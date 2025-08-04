"""
ARS.jl implements an adaptive rejection sampler.

Functions and structs intended for public use are marked as such.
"""
module ARS

import Base: OneTo
using DocStringExtensions
import SpecialFunctions: loggamma
import Base.Iterators: drop, take
using StatsBase
import Random: default_rng, AbstractRNG

using DifferentiationInterface
import Mooncake

using Compat



include("doctemplates.jl")

@compat public Objective, ARSampler, sample!

"""
$TYPEDEF

Non-mutable weights used in the `sample` function in this package in order to avoid allocations.

$TYPEDFIELDS
"""
struct AllocFreeWeights{S<:Real,T<:Number,V<:AbstractVector{T}} <: AbstractWeights{S,T,V}
    values::V
    sum::S

    function AllocFreeWeights(ws::V, sum::S) where {S<:Real,T<:Number,V<:AbstractVector{T}}
        new{S,T,V}(ws, sum)
    end
end

function AllocFreeWeights(ws::V) where {T<:Number,V<:AbstractVector{T}}
    AllocFreeWeights(ws, sum(ws))
end

function alphatest(k, n, a)
    alpha = exp(a)
    a * (k - (3 / 2)) + (-1 / (2 * alpha)) + loggamma(alpha) - loggamma(n + alpha)
end


#=
Objective function including its gradient
=#

"""
Struct representing an (unnormalized) log-concave density from which to sample.

$(TYPEDFIELDS)

Both `f` and `grad` should be single-argument functions.

!!! warning
    Observe that `f` should be in its
    log-concave form and that no checks are performed in order to verify this.
"""
struct Objective{F<:Function,G<:Function}
    "The (log-concave) function defining the density `f(x) -> y`"
    f::F
    "Its gradient `grad(x) -> y'`"
    grad::G

    @doc """
    $(TYPEDSIGNATURES)

    Create an `Objective` directly defined by its function `f` and custom gradient `grad`.


    !!! warning
        Observe that `f` should be in its
        log-concave form and that no checks are performed in order to verify this.
    """
    function Objective(f::Function, grad::Function)
        new{typeof(f),typeof(grad)}(f, grad)
    end
end

@doc """
$(SIGNATURES)

Create an [`Objective`](@ref) for a function automatically generating its gradient.
Gradients are calculated using `DifferentiationInterface.jl` using the backend of choice
with `Mooncake.jl` being the default. In order to prepare the gradient an initial value is
required. By default this is `one(Float64)`. If a gradient for a different type is
desired, it should be specified. 


!!! warning
    Observe that `f` should be in its
    log-concave form and that no checks are performed in order to verify this.
"""
function Objective(f::Function, adbackend=AutoMooncake(; config=nothing), init=one(Float64))
    let f = f, backend = adbackend
        gradprep = prepare_gradient(f, backend, init)
        Objective(
            f,
            x::AbstractFloat -> gradient(f, gradprep, backend, x)
        )
    end
end

#=
Hulls for sampling and squeezing
=#

abstract type AbstractHull end

struct UpperHull{T} <: AbstractHull
    intercepts::Vector{T}
    slopes::Vector{T}
    intersections::Vector{T}
    abscissae::Vector{T}
    segment_weights::Vector{T}
    domain::Tuple{T,T}
end

Base.broadcastable(h::UpperHull) = Ref(h)

slopes(h::UpperHull) = h.slopes
intercepts(h::UpperHull) = h.intercepts
intersections(h::UpperHull) = h.intersections
abscissae(h::UpperHull) = h.abscissae
"Get line `i` from the hull. Returns a tuple consisting of `(slope, intercept)`."
line(h::UpperHull, i::Integer) = (h.slopes[i], h.intercepts[i])
lineinds(h::UpperHull) = OneTo(length(slopes(h)))
n_lines(h::UpperHull) = length(slopes(h))
segment_weights(h::UpperHull) = h.segment_weights

"""
Eval hull `h` at `x`.
"""
function eval_hull(h::UpperHull, x)
    i = searchsortedfirst(intersections(h), x) - 1
    sl, int = line(h, i)
    sl * x + int
end

function calc_intersects(slopes::AbstractVector{T}, intercepts::AbstractVector{T}) where {T}
    [intersection(slopes[i], intercepts[i], slopes[i+1], intercepts[i+1])
     for i in OneTo(length(slopes) - 1)]
end

# Calculate intersections, storing the result in out
function calc_intersects!(out::AbstractVector{T}, slopes::AbstractVector{T}, intercepts::AbstractVector{T}) where {T}
    for i in eachindex(out)
        out[i] = intersection(slopes[i], intercepts[i], slopes[i+1], intercepts[i+1])
    end
    return nothing
end

function calc_slopes_and_intercepts(obj::Objective, abscissae::AbstractVector{T}) where {T}
    issorted(abscissae) ||
        throw(ArgumentError("`abscissae` should be sorted in ascending order."))
    slopes = [obj.grad(abscissae[i]) for i in eachindex(abscissae)]
    intercepts = [slopes[i] * (-abscissae[i]) + obj.f(abscissae[i])
                  for i in eachindex(abscissae)]
    return slopes, intercepts
end

function calc_domain_integral_exp(slopes::Vector{T}, intercepts::Vector{T}, intersects::Vector{T}) where {T}
    res = zero(T)
    for i in 1:length(slopes)
        res += exp_integral_line(slopes[i], intercepts[i], intersects[i], intersects[i+1])
    end
    return res
end

"""
    calc_domain_integral_exp(hull::UpperHull)

Calculate the domain integral of `hull`.
"""
function calc_domain_integral_exp(hull::UpperHull)
    calc_domain_integral_exp(hull.slopes, hull.intercepts, hull.intersections, hull.domain)
end

"""
    exp_integral_line(slope, intercept, x1, x2)

Calculate the integral of `exp(slope * x + intercept)` between `x1` and `x2`. Handles a slope of 0.
"""
function exp_integral_line(slope, intercept, x1, x2)
    if !iszero(slope)
        (exp(slope * x2 + intercept) - exp(slope * x1 + intercept)) / slope
    else
        (x2 - x1) * exp(intercept)
    end
end

function UpperHull(obj::Objective, abscissae::Vector{T}, domain::Tuple{T,T}) where {T}
    slopes, intercepts = calc_slopes_and_intercepts(obj, abscissae)

    intersects = [domain[1]; calc_intersects(slopes, intercepts); domain[2]]

    wgts = Vector{T}(undef, length(slopes))
    for i in 1:length(wgts)
        wgts[i] = exp_integral_line(slopes[i], intercepts[i], intersects[i], intersects[i+1])
    end

    UpperHull(intercepts, slopes, intersects, abscissae, wgts, domain)
end

# Calculate the inverse CDF of a segment, handling a slope of zero.
@inline function inv_cdf_seg(slope, intercept, r, w, intersect, intersect2)
    if !iszero(slope)
        log(exp(-intercept) * r * w * slope + exp(slope * intersect)) / slope
    else
        r * (intersect2 - intersect) + intersect
    end
end


# Draw a single sample from `h`.
function sample_hull(rng::AbstractRNG, h::UpperHull)
    ws = segment_weights(h)
    ind = sample(rng, ARS.AllocFreeWeights(ws, sum(ws)))
    sl, int = line(h, ind)
    inv_cdf_seg(sl, int, rand(rng), segment_weights(h)[ind], intersections(h)[ind], intersections(h)[ind+1])
end
sample_hull(h::UpperHull) = sample_hull(default_rng(), h)

# Draw `n` samples from `h`, storing the result in `out`.
function sample_hull!(rng::AbstractRNG, out::AbstractVector{T}, h::UpperHull{T}, n::Integer) where {T}
    inds = sample(rng, 1:n_lines(h), weights(segment_weights(h)), n)
    rands = rand(rng, n)
    for i in eachindex(inds, out, rands)
        ind = inds[i]
        sl, int = line(h, ind)
        out[i] = inv_cdf_seg(sl, int, rands[i], segment_weights(h)[ind], intersections(h)[ind], intersections(h)[ind+1])
    end
end
sample_hull!(out::AbstractVector{T}, h::UpperHull{T}, n::Integer) where {T} = sample_hull!(default_rng(), out, h, n)

function sample_hull!(rng, out::AbstractVector{T}, h::UpperHull) where {T}
    sample_hull!(rng, out, h, length(out))
end
sample_hull!(out::AbstractVector{T}, h::UpperHull) where {T} = sample_hull!(default_rng(), out, h)

# Draw `n` samples from `h`.
function sample_hull(rng::AbstractRNG, h::UpperHull{T}, n::Integer) where {T}
    out = Vector{T}(undef, n)
    sample_hull!(rng, out, h, n)
    return out
end
sample_hull(h::UpperHull, n::Integer) = sample_hull(default_rng(), h, n)

"""
Returns the intersection abscissa between 2 lines as defined by their slopes and intercepts. Returns NaN if the lines are paralell.
"""
function intersection(slope1::T, intercept1::T, slope2::T, intercept2::T) where {T}
    (intercept2 - intercept1) / (slope1 - slope2)
end

struct LowerHull{T}
    intercepts::Vector{T}
    slopes::Vector{T}
    intersections::Vector{T}
end

Base.broadcastable(h::LowerHull) = Ref(h)


intercepts(h::LowerHull) = h.intercepts
slopes(h::LowerHull) = h.slopes
intersections(h::LowerHull) = h.intersections
line(h::LowerHull, i) = (h.slopes[i], h.intercepts[i])

function calc_lower_slopes_and_intercepts!(
    slout::AbstractVector{T}, intout::AbstractVector{T},
    intersections::AbstractVector{T}, f::Function) where {T}

    for i in eachindex(slout, intout)
        slout[i] = (f(intersections[i+1]) - f(intersections[i])) /
                   (intersections[i+1] - intersections[i])
        intout[i] = slout[i] * (-intersections[i]) + f(intersections[i])
    end
    return nothing
end

function LowerHull(upper::UpperHull{T}, obj::Objective) where {T}
    intersections = abscissae(upper) # NOTE: This makes them alias each other!
    n_segs = length(intersections) - 1
    sl = Vector{T}(undef, n_segs)
    int = Vector{T}(undef, n_segs)

    calc_lower_slopes_and_intercepts!(sl, int, intersections, obj.f)

    return LowerHull(int, sl, intersections)
end

function eval_hull(h::LowerHull, x)
    i = searchsortedfirst(intersections(h), x)
    if isone(i) || i == lastindex(intersections(h)) + 1
        return -Inf
    end
    sl, int = line(h, i - 1)
    sl * x + int
end

"""
Struct for holding an objective function to sample from, an upper and a lower hull for adaptive rejection sampling.

See also: [`ARS.sample!`](@ref)
"""
struct ARSampler{T,F,G}
    objective::Objective{F,G}
    upper_hull::UpperHull{T}
    lower_hull::LowerHull{T}
end

"""
$(TYPEDSIGNATURES)

Initialize an adaptive rejection sampler over a (log) objective function from `obj`. `initial_points` should be a vector of abscissae defining the initial segments of the sampler. At least 2 of the points should be on opposite sides of the objective function's maximum.

$(METHODLIST)
"""
function ARSampler(
    obj::Objective{F,G},
    initial_points::Vector{T},
    domain::Tuple{T,T}
) where {T<:AbstractFloat,F<:Function,G<:Function}

    u = UpperHull(obj, initial_points, domain)
    l = LowerHull(u, obj)

    return ARSampler{T,F,G}(obj, u, l)
end

# Adds a segment with abscissa at `x` to `s`
function add_segment!(s::ARSampler{T}, x::T) where {T<:AbstractFloat}

    # Calculate slope, intercept and index of new segment
    new_slope = s.objective.grad(x)
    new_intercept = (new_slope * -x) + s.objective.f(x)
    new_ind = searchsortedfirst(abscissae(s.upper_hull), x)


    # Insert new slope and intercept
    all_inters = intersections(s.upper_hull)
    all_slopes = slopes(s.upper_hull)
    all_intercepts = intercepts(s.upper_hull)
    insert!(all_slopes, new_ind, new_slope)
    insert!(all_intercepts, new_ind, new_intercept)

    # Insert new abscissa
    insert!(abscissae(s.upper_hull), new_ind, x)


    # TODO: Only calculate the intersections and weights that actually change.
    # Recalculate intersection points for segments, given the new segment
    push!(all_inters, all_inters[end]) # Extend intercepts by one
    calc_intersects!(@view(all_inters[begin+1:end-1]), all_slopes, all_intercepts)

    # Recalculate segment weights
    all_weights = segment_weights(s.upper_hull)
    push!(all_weights, zero(T))
    for i in eachindex(all_weights)
        all_weights[i] = exp_integral_line(all_slopes[i], all_intercepts[i], all_inters[i], all_inters[i+1])
    end

    #= Lower hull time =#
    # We don't need to add anything to the lower intersections as it is the same vector used for abscissae 
    # in the upper hull

    all_inters_lower = intersections(s.lower_hull)
    lowerslopes = slopes(s.lower_hull)
    lowerintercepts = intercepts(s.lower_hull)
    push!(lowerslopes, zero(T))
    push!(lowerintercepts, zero(T))
    calc_lower_slopes_and_intercepts!(
        lowerslopes,
        lowerintercepts,
        all_inters_lower,
        s.objective.f
    )
    return nothing
end

# Draw samples from `s`, filling `out`
function __sample!(rng::AbstractRNG, out::Vector{T}, s::ARSampler{T}, add_segments::Bool) where {T<:AbstractFloat}
    n = length(out)
    n_accepted = 0
    while n_accepted < n
        x = sample_hull(rng, s.upper_hull)
        up = eval_hull(s.upper_hull, x)
        lo = eval_hull(s.lower_hull, x)
        w = rand(rng)
        # Squeeze test
        if w <= exp(lo - up)
            out[n_accepted+1] = x
            n_accepted += 1
        elseif w <= exp(s.objective.f(x) - up)
            # Accept sample i
            out[n_accepted+1] = x
            n_accepted += 1
            if add_segments
                add_segment!(s, x)
            end
        elseif add_segments
            add_segment!(s, x)
        end
    end
    return nothing
end
__sample!(out::Vector{T}, s::ARSampler{T}, add_segments::Bool) where {T} = __sample!(default_rng(), out, s, add_segments)

# Draw `n` samples, returning a newly allocated vector.
function __sample!(rng::AbstractRNG, s::ARSampler{T}, n::Integer, add_segments::Bool) where {T<:AbstractFloat}
    out = Vector{T}(undef, n)
    __sample!(rng, out, s, add_segments)
    return out
end
__sample!(s::ARSampler{T}, n::Integer, add_segments::Bool) where {T<:AbstractFloat} = __sample!(default_rng(), s, n, add_segments)


"""
    sample!([rng=default_rng()], s::ARSampler, n::Integer, add_segments::Bool=true)
    sample!([rng=default_rng()], v::AbstractVector, s::ARSampler, add_segments::Bool=true)

Draw samples from `s`. If supplied, a vector `v` will be filled with samples. Otherwise the number of samples is specified with `n`.
"""
function sample! end

function sample!(rng::AbstractRNG, s::ARSampler{T}, n::Integer, add_segments::Bool=true) where {T<:AbstractFloat}
    __sample!(rng, s, n, add_segments)
end
sample!(s::ARSampler{T}, n::Integer, add_segments::Bool=true) where {T<:AbstractFloat} = sample!(default_rng(), s, n, add_segments)

function sample!(rng::AbstractRNG, v::AbstractVector{T}, s::ARSampler{T}, add_segments::Bool=true) where {T}
    __sample!(rng, v, s, add_segments)
    return nothing
end

sample!(v::AbstractVector{T}, s::ARSampler{T}, add_segments::Bool=true) where {T} = sample!(default_rng(), v, s, add_segments)

end
