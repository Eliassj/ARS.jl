
struct RejectionSampler{T}
    objective::Objective
    upperhull::UpperHull{T}
    upperhull_integral::Ref{T}
    lowerhull::LowerHull{T}
    support::Tuple{T, T}
    max_segments::Int64
    max_failed_rate::Float64

    function RejectionSampler(f::Objective, initial_points::AbstractVector{T},
            support::Tuple{T, T} = (typemin(T), typemax(T)), max_segments::Integer = 25, max_failed_rate::AbstractFloat = 0.001) where {T}
        upper = UpperHull(f, initial_points)
        upper_integral = hull_exp_integral(upper, support[1], support[2])
        lower = LowerHull(f, upper)

        new{T}(f, upper, Ref(upper_integral), lower, support, max_segments, max_failed_rate)
    end
end

objective(s::RejectionSampler) = s.objective
upperhull(s::RejectionSampler) = s.upperhull
lowerhull(s::RejectionSampler) = s.lowerhull
support(s::RejectionSampler) = s.support
max_segments(s::RejectionSampler) = s.max_segments
max_failed_rate(s::RejectionSampler) = s.max_failed_rate

"""
    sample_hull(hull::UpperHull)

Draws a single sample from the upper hull
"""
function sample_upper_hull(sam::RejectionSampler)
    # Sample point
    hull_exp_cdf_inv(sam.upperhull, sam.support[1], rand()) / sam.upperhull_integral[]
end

function sample_upper_hull!(
        out::Vector{T}, sam::RejectionSampler{T}, n_samples::Integer) where {T}
    rs = rand(n_samples)
    @. out = hull_exp_cdf_inv(sam.upperhull, sam.support[1], sam.upperhull_integral[], rs) /
             sam.upperhull_integral[]
end

function sample_upper_hull(
        sam::RejectionSampler{T}, n_samples::Integer) where {T}
    out = Vector{T}(undef, n_samples)
    @. out = hull_exp_cdf_inv(
        sam.upperhull, sam.support[1], sam.upperhull_integral[], $rand(n_samples)) /
             sam.upperhull_integral[]
end

function sample!(
        sampler::RejectionSampler{T}, outv::Vector{T}, n_samples::Integer) where {T}
    w_og = rand(n_samples)
    sample_upper_hull!(outv, sampler, n_samples)
    not_accepted = ones(Bool, n_samples)
    n_accepted = 0
    n_passes = 0
    while n_accepted < n_samples
        n_passes += 1
        w = @view w_og[not_accepted]
        out = @view outv[not_accepted]
        for i in eachindex(w, out)
            if w[i] <= exp(eval_hull(lowerhull(sampler), out[i]) -
                   eval_hull(upperhull(sampler), out[i]))
                # Accept x[i]
                not_accepted[i] = false
                n_accepted += 1
            elseif w[i] <= exp(objective(sampler).f(out[i]) -
                       eval_hull(upperhull(sampler), out[i]))
                # Accept x[i]
                not_accepted[i] = false
                n_accepted += 1
            else
                # Reject x[i] and sample new values
                out[i] = sample_upper_hull(sampler)
            end
        end
    end
end

function sample(sampler::RejectionSampler{T}, n_samples::Integer) where {T}
    out = Vector{T}(undef, n_samples)
    sample!(sampler, out, n_samples)
    return out
end

function plotsampler end

function samplerplot end
function samplerplot! end
