module MakieExt

using Makie
import Makie: Observable
using ARS
import ARS: samplerplot, samplerplot!

@recipe(SamplerPlot, r) do scene
    Attributes(
        objectivecolor = :blue,
        lowerhullcolor = :green,
        upperhullcolor = :red
    )
end

const Point2f64 = Makie.Point{2, Float64}

function Makie.plot!(samplerplt::SamplerPlot{<:Tuple{
        <:ARS.RejectionSampler, <:AbstractRange}})
    sam = samplerplt[1]
    rang = samplerplt[2]

    lower = Observable(Float64[])
    lowerneg1 = Observable(Point2f64[])
    lowerneg2 = Observable(Point2f64[])
    upper = Observable(Float64[])
    obj = Observable(Float64[])

    function update_plot(sampler, range)
        lower[] = ARS.eval_hull.(ARS.lowerhull(sampler), range)
        upper[] = ARS.eval_hull.(ARS.upperhull(sampler), range)
        obj[] = ARS.objective(sampler).f.(range)
        minmin = min(minimum(upper[]), minimum(obj[]))
        lastinf = findfirst(!isinf, lower[])
        firstinf = findfirst(isinf, @view(lower[][(lastinf):end]))
        if !isnothing(firstinf)
            firstinf -= (2 + lastinf)
        else
            firstinf = lastinf
        end

        lowerneg1[] = [
            Point2f64(range[lastinf], lower[][lastinf]), Point2f64(range[lastinf], minmin)]
        lowerneg2[] = [
            Point2f64(range[firstinf], lower[][firstinf]), Point2f64(
                range[firstinf], minmin)]
    end

    onany(update_plot, sam, rang)
    update_plot(sam[], rang[])

    lines!(samplerplt, rang, obj, color = samplerplt[:objectivecolor])
    lines!(
        samplerplt, rang, lower, color = samplerplt[:lowerhullcolor])
    @show lowerneg1 lowerneg2
    linesegments!(samplerplt, lowerneg1, color = samplerplt[:lowerhullcolor])
    linesegments!(samplerplt, lowerneg2, color = samplerplt[:lowerhullcolor])
    lines!(samplerplt, rang, upper, color = samplerplt[:upperhullcolor])

    samplerplt
end

end
