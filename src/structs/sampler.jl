
struct RejectionSampler{T}
    objective::Objective
    upperhull::UpperHull{T}
    lowerlines::LowerHull{T}
    support::Tuple{T, T}
    max_segments::Int64
    max_failed_rate::Float64

    function RejectionSampler(f::Objective, initial_points::AbstractVector{T},
            support::Tuple{T, T} = (typemin(T), typemax(T))) where {T}
        upper = UpperHull(f, initial_points)
        lower = LowerHull(f, initial_points)
    end
end
