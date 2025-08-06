using ARS
using Test
using Aqua
using JET
using Distributions
using Random

function test_dist(x)
    logpdf(Normal(), x)
end


# Unnormalized distributions
normal(mu, sigma, x) = exp(-((x - mu)^2) / (2 * sigma^2))
beta(alpha, beta, x) = x^(alpha - 1) * (1 - x)^(beta - 1)

@testset "ARS.jl" begin
    @testset "Catch uninitialized mem" begin
        samp_norm = ARS.ARSampler(Objective(x -> normal(0.0, 1.0, x)), [-0.5, 0.5], (-Inf, Inf))
        Random.seed!(1)
        v1 = let s = samp_norm, v = Vector{Float64}(undef, 1000)
            ARS.sample!(s, v)
        end
        Random.seed!(1)
        v2 = let s = samp_norm, v = Vector{Float64}(undef, 1000)
            ARS.sample!(s, v)
        end
        @test v1 == v2
    end
    @testset "Code quality (Aqua.jl)" begin
        Aqua.test_all(ARS)
    end
    @testset "Code linting (JET.jl)" begin
        JET.test_package(ARS; target_defined_modules=true)
    end
end
