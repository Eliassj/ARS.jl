
struct RejectionSampler{T}
    objective::Objective
    upperhull::UpperHull{T}
    lowerhull::LowerHull{T}
    support::Tuple{T, T}
    max_segments::Int64
    max_failed_rate::Float64

    function RejectionSampler(f::Objective, initial_points::AbstractVector{T},
            support::Tuple{T, T} = (typemin(T), typemax(T)), max_segments::Integer = 25, max_failed_rate::AbstractFloat = 0.001) where {T}
        upper = UpperHull(f, initial_points)
        lower = LowerHull(f, upper)

        new{T}(f, upper, lower, support, max_segments, max_failed_rate)
    end
end

objective(s::RejectionSampler) = s.objective
upperhull(s::RejectionSampler) = s.upperhull
lowerhull(s::RejectionSampler) = s.lowerhull
support(s::RejectionSampler) = s.support
max_segments(s::RejectionSampler) = s.max_segments
max_failed_rate(s::RejectionSampler) = s.max_failed_rate

function sample!(sampler::RejectionSampler, n_samples::Integer)
    
end

function plotsampler end

function samplerplot end
function samplerplot! end
