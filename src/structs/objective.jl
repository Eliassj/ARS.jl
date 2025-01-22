"""
$TYPEDEF

Struct holding the function to be sampled from and its gradient function.

Gradient function may be provided. If not it will be determined using automatic differentiation.

# Fields

$TYPEDFIELDS
"""
struct Objective{F, G} <: Function
    "Function to sample from"
    f::F
    "Derivative of `f`"
    grad::G

    @doc """
     Construct an `Objective` with a function `f` and its gradient `grad`.
     """
    function Objective(f::F, grad::G) where {F <: Function, G <: Function}
        new{F, G}(f, grad)
    end

    @doc """
     Construct an `Objective` with a function `f` and automatically determine its gradient.
     """
    function Objective(f::F) where {F <: Function}
        gr = x -> derivative(f, x)
        new{F, typeof(gr)}(f, gr)
    end
end
