module ARS

import ForwardDiff: derivative
using DocStringExtensions

include("doctemplates.jl")

include("structs/objective.jl")
include("structs/hulls.jl")
include("structs/sampler.jl")

const DEFAULT_MIN_SLOPE = 1e-6
const DEFAULT_MAX_SLOPE = 1e6

struct Lines{T}
    lines::Vector{Line{T}}
    intersections::Vector{T}

    function Lines(lines::Vector{Line{T}}) where {T}
        intersections = Vector{T}(undef, length(lines) - 1)

        for i in eachindex(intersections)
            intersections[i] = intersection(lines[i], lines[i + 1])
        end

        new{T}(lines, intersections)
    end
end

lines(lines::Lines) = lines.lines
lines(lines::Lines, i::Integer) = lines.lines[i]
intersections(lines::Lines) = lines.intersections
intersections(lines::Lines, i::Integer) = lines.intersections[i]

nlines(lines::Lines) = length(lines(lines))
nintersections(lines::Lines) = length(intersections(lines))

function squeezelines(ob::Objective, upper_lines::Lines{T}) where {T}
    lins = Vector{Line{T}}(undef, nintersections(upper_lines))

    grads = Vector{T}(undef, length(lins))
    for i in eachindex(grads)
        grads[i] = slope(
            ob, tangent(lines(upper_lines, i)), tangent(lines(upper_lines, i + 1)))
    end
    ints = []
end

function linefun(l::Line)
    f = let k = l.slope, m = l.intercept
        x -> k * x + m
    end
    f
end

#=
y = kx + b
y = b
=#

end
