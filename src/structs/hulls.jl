"""
Abstract type for lines.

Should contain the fields `intercept` and `slope`.
"""
abstract type AbstractLine{T} end

struct Line{T <: Number} <: AbstractLine{T}
    slope::T
    intercept::T
end

"""
Evaluate a `Line` at `x`.
"""
function (l::Line)(x)
    l.slope * x + l.intercept
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

"""
Evaluate a hull at `x`.
"""
function (hull::UpperHull)(x::Number)
    v = findfirst(>=(x), hull.intersections)
    isnothing(v) && return hull.lines[end](x)
    return hull.lines[v](x)
end

function abscissae(hull::UpperHull)
    hull.abscissae
end

function upperlines(f::Objective, points::AbstractVector{T}) where {T <: Number}
    issorted(points) || throw(ArgumentError("`points` should be sorted."))
    grads = f.grad.(points)
    ints = @. grads * (-points) + f.f(points)
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
        ints = @. slopes * (-xv) + obj.f(xv)

        lines = [Line(slopes[i], ints[i]) for i in eachindex(slopes)]

        new{T}(lines, absc)
    end
end

"Slope of line between to points `(x1, ob.f(x1))` and `(x2, ob.f(x2))`"
function slope(ob::Objective, x1::T, x2::T) where {T}
    (x2 - x1) / (ob.f(x2) - ob.f(x1))
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
