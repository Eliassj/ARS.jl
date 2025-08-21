using ARS
using Documenter
using DocumenterCitations

DocMeta.setdocmeta!(ARS, :DocTestSetup, :(using ARS); recursive=true)

bib = CitationBibliography(joinpath(@__DIR__, "src", "refs.bib"))

makedocs(;
    modules=[ARS],
    authors="Elias Sjölin <elias.sjolin@gmail.com> and contributors",
    sitename="ARS.jl",
    format=Documenter.HTML(;
        canonical="https://Eliassj.github.io/ARS.jl",
        edit_link="master",
        assets=["assets/favicon.ico"],
    ),
    pages=[
        "Home" => "index.md",
        "Benchmarks" => "benchmarks.md",
        "Public API" => "api.md",
        "Internals" => "devdocs.md",
        "References" => "references.md"
    ],
    plugins=[bib],
    remotes=nothing,
    checkdocs=:public,
    warnonly=[:missing_docs],
)

deploydocs(;
    repo="github.com/Eliassj/ARS.jl",
    devbranch="main",
)
