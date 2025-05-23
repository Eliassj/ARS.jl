using ARS
using Test
using Aqua
using JET
using Distributions

function test_dist(x)
    logpdf(Normal(), x)
end

@testset "ARS.jl" begin
    @testset "Code quality (Aqua.jl)" begin
        Aqua.test_all(ARS)
    end
    @testset "Code linting (JET.jl)" begin
        JET.test_package(ARS; target_defined_modules=true)
    end
    # Write your tests here.

end
