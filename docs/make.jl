using ARS
using Documenter

DocMeta.setdocmeta!(ARS, :DocTestSetup, :(using ARS); recursive=true)

makedocs(;
    modules=[ARS],
    authors="Eliassj <elias.sjolin@gmail.com> and contributors",
    sitename="ARS.jl",
    format=Documenter.HTML(;
        canonical="https://Eliassj.github.io/ARS.jl",
        edit_link="master",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/Eliassj/ARS.jl",
    devbranch="master",
)
