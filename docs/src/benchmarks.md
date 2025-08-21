
# Benchmarks

## Comparison with `AdaptiveRejectionSampling.jl`

```@example 1
using ARS, AdaptiveRejectionSampling
using DifferentiationInterface, Distributions
using ForwardDiff
using Chairmarks

# Define function to sample from
f(x) = logpdf(Laplace(0., 0.5), x) + logpdf(Normal(0.0, 2.0), x)
```

### ARS.jl

```@example 1

#= ARS.jl =#
sam_ARS = ARS.ARSampler(ARS.Objective(f, AutoForwardDiff()), [-0.5, 0.5], (-Inf, Inf))

@be deepcopy(sam_ARS) ARS.sample!(_, 100000, true, 25) samples=100 evals=1
```

### AdaptiveRejectionSampling.jl

```@example 1

#= AdaptiveRejectionSampling.jl =#

sam_other = RejectionSampler(f, (-Inf, Inf), (-0.5, 0.5); logdensity=true, max_segments=25)

@be deepcopy(sam_other) run_sampler!(_, 100000) samples=100 evals=1 seconds=1000
```
