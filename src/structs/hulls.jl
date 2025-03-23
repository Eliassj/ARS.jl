"""
Abstract type for lines.

Should contain the fields `intercept` and `slope`.
"""
abstract type AbstractLine{T} end

struct Line{T <: Number} <: AbstractLine{T}
    slope::T
    intercept::T
end

Base.broadcastable(o::AbstractLine) = Ref(o)

# TODO: Deprecate this
"""
Evaluate a `Line` at `x`.
"""
function (l::Line)(x)
    l.slope * x + l.intercept
end

function eval_line(l::Line, x)
    l.slope * x + l.intercept
end

"""
    eval_line(l::Line, x::Vector{T}) where {T}

Evaluate a line at points in `x`.
"""
function eval_line(l::Line, x::Vector{T}) where {T}
    out = similar(x)
    eval_line!(out, l, x)
    return out
end

"""
    eval_line!(out::Vector{T}, l::Line, x::Vector{T}) where {T}

Evaluate a line at points in `x` and store the result in `out`.

**Does not check bounds nor that `out` is the same length as `x`!**
"""
function eval_line!(out::Vector{T}, l::Line, x::Vector{T}) where {T}
    slope = l.slope
    intercept = l.intercept
    @inbounds @simd for i in eachindex(out)
        out[i] = slope * x[i] + intercept
    end
    return nothing
end

"Returns slope of an `AbstractLine`"
function slope(l::AbstractLine)
    l.slope
end
intercept(l::AbstractLine) = l.intercept

"""
Returns the intersection abscissa between `line1` and `line2`. Returns NaN if the lines are paralell.
"""
function intersection(line1::Line, line2::Line)
    (intercept(line2) - intercept(line1)) / (slope(line1) - slope(line2))
end

function intersection(lines::AbstractVector{<:Line})
    v1 = @view lines[begin:(end - 1)]
    v2 = @view lines[(begin + 1):end]
    intersection.(v1, v2)
end

# Calculates the integral for `line` between points x1 and x2.
function line_exp_integral(line::Line, x1::Number, x2::Number)
    slope = line.slope
    intercept = line.intercept
    # Handle case when slope is zero
    if iszero(slope)
        return exp(intercept) * (x2 - x1)
    else
        return ((exp(intercept + x2 * slope) -
                 exp(intercept + x1 * slope)) / slope)
    end
end

function line_exp_integral_at(line::Line, x::Number)
    if iszero(line.slope)
        exp(line.intercept) * x
    else
        exp(line.slope * x + line.intercept) / line.slope
    end
end

function line_inv_exp_integral(line::Line, x::Number)
    k = line.slope
    m = line.intercept

    (log(x / k) - m) / k
end

abstract type AbstractHull{T} end

function lines(hull::AbstractHull)
    hull.lines
end
function intersections(hull::AbstractHull)
    hull.intersections
end

# Struct for upper hull

"""
$TYPEDEF

Struct for holding the upper hull of a `RejectionSampler`.

$TYPEDFIELDS
"""
struct UpperHull{T} <: AbstractHull{T}
    "Points where the lines tangent the objective."
    abscissae::Vector{T}
    "Vector of lines."
    lines::Vector{Line{T}}
    "Points where lines intersect."
    intersections::Vector{T}

    function UpperHull(obj::Objective, abscissae::Vector{T}) where {T}
        lines = upperlines(obj, abscissae)
        intersections = intersection(lines)

        new{T}(abscissae, lines, intersections)
    end
end

function UpperHull(f::Function, abscissae::Vector{T}) where {T}
    UpperHull(Objective(f), abscissae)
end

Base.broadcastable(o::AbstractHull) = Ref(o)

"""
Evaluate a hull at `x`.
"""
function eval_hull(hull::UpperHull, x::Number)
    v = searchsortedfirst(hull.intersections, x)
    if v > length(hull.lines)
        v -= 1
    end
    return hull.lines[v](x)
end

"""
Evaluate the area/CDF under `exp(u(x))` between `ld` and `ud` where `u(x)` is the upper hull.
"""
function hull_exp_integral(
        hull::UpperHull{T}, ld = -Inf, ud = Inf) where {T <: AbstractFloat}
    breakpoints = hull.intersections
    res = zero(T)

    vstart = searchsortedfirst(breakpoints, ld)
    vend = searchsortedfirst(breakpoints, ud)

    lins = @view lines(hull)[vstart:min(vend, length(lines(hull)))]

    @views begin
        res += line_exp_integral(lins[begin], ld, breakpoints[vstart])
        res += line_exp_integral(lins[end], breakpoints[min(length(breakpoints), vend)], ud)

        for (line, i) in zip(lins[(begin + 1):(end - 1)], (vstart + 1):(vend - 1))
            res += line_exp_integral(line, breakpoints[i - 1], breakpoints[i])
        end
    end
    return res
end

function hull_piece_at(hull::UpperHull, x)
    v = searchsortedfirst(hull.intersections, x)
    v > length(hull.lines) ? v - 1 : v
end

function hull_exp_cdf_inv(hull::UpperHull, lower_support::T, x::T) where {T}
    breakpoints = hull.intersections
    res = zero(T)

    lins = lines(hull)

    last_breakpoint = zero(T)

    seg_with_x = 1
    @views begin
        res += line_exp_integral(lins[1], lower_support, breakpoints[begin])
        @show res
        i = 2
        for line in lins[(begin + 1):end]
            @show i
            tmp = line_exp_integral(line, breakpoints[i - 1], breakpoints[i])
            # If we've passed x or is at the last segment, break
            if res + tmp >= x
                break
            else
                res += tmp
                @show res
                last_breakpoint = breakpoints[i]
                @show last_breakpoint
                i += 1
                @show seg_with_x = i
                @show length(breakpoints)
                if i > length(breakpoints)
                    break
                end
            end
        end
    end

    k = slope(lins[seg_with_x])
    m = intercept(lins[seg_with_x])

    if iszero(k)
        (x - res) / (exp(m)) + last_breakpoint
    else
        (log(k * (x - res) + exp(m + last_breakpoint * k)) - m) / k
    end
end

function abscissae(hull::UpperHull)
    hull.abscissae
end

# NOTE: Avoided broadcasting here since it introduces type instability for the objective function
function upperlines(f::Objective, points::AbstractVector{T}) where {T <: Number}
    issorted(points) || throw(ArgumentError("`points` should be sorted."))
    grads = [f.grad(points[i]) for i in eachindex(points)]
    ints = [grads[i] * (-points[i]) + f.f(points[i]) for i in eachindex(points)]
    [Line(grads[i], ints[i]) for i in eachindex(points)]
end

# Struct for lower hull

struct LowerHull{T} <: AbstractHull{T}
    "Vector of lines."
    lines::Vector{Line{T}}
    "Points where lines intersect each other and the upper hull."
    intersections::Vector{T}

    function LowerHull(obj::Objective, upper::UpperHull{T}) where {T}
        absc = abscissae(upper)
        slopes = slope(obj, absc)

        xv = @view absc[begin:(end - 1)]
        ints = [slopes[i] * (-xv[i]) + obj.f(xv[i]) for i in eachindex(slopes)]

        lines = [Line(slopes[i], ints[i]) for i in eachindex(slopes)]

        new{T}(lines, absc)
    end
end

function eval_hull(hull::LowerHull, x::Number)
    v = searchsortedfirst(intersections(hull), x)
    if v == 1 || v > length(intersections(hull))
        return -Inf
    end
    return hull.lines[v - 1](x)
end

"Slope of line between to points `(x1, ob.f(x1))` and `(x2, ob.f(x2))`"
function slope(ob::Objective, x1::T, x2::T) where {T}
    (ob.f(x2) - ob.f(x1)) / (x2 - x1)
end

@doc """
 Calculate the slopes of lines between all points `(x[i], ob.f(x[i]))` and `(x[i + 1], ob.f(x[i+1]))` for all `x` in `xs`.
 """
function slope(ob::Objective, xs::Vector{T}) where {T <: AbstractFloat}
    out = Vector{T}(undef, length(xs) - 1)
    slope!(out, ob, xs)
    return out
end

@doc """
 Calculate the slopes of lines between all points `(x[i], ob.f(x[i]))` and `(x[i + 1], ob.f(x[i+1]))` for all `x` in `xs`.
Result is placed in `out` which should have length `length(xs) - 1` (not checked). Explicitly returns `nothing`.
 """
function slope!(out::Vector{T}, ob::Objective, xs::Vector{T}) where {T <: AbstractFloat}
    v1 = @view xs[begin:(end - 1)]
    v2 = @view xs[(begin + 1):end]
    @. out = (ob.f(v2) - ob.f(v1)) / (v2 - v1)
    for i in eachindex(out)
        if isnan(out[i])
            out[i] = zero(T)
        end
    end
    return nothing
end
