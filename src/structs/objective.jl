"""
$TYPEDEF

Struct holding the function to be sampled from and its gradient function.

Gradient function may be provided. If not it will be determined using automatic differentiation.

# Fields

$TYPEDFIELDS
"""
struct Objective
    "Function to sample from"
    f::Function
    "Derivative of `f`"
    grad::Function

    @doc """
     Construct an `Objective` with a function `f` and its gradient `grad`.
     """
    function Objective(f::Function, grad::Function)
        new(f, grad)
    end

    @doc """
     Construct an `Objective` with a function `f` and automatically determine its gradient.
     """
    function Objective(f::Function)
        new(f, x -> derivative(f, x))
    end
end
