# ⛓️ Constrained Categories

A `Category` hierarchy with constraints on the objects.

## What?

The standard `Category` definition in `base` looks like this:

```
type Category :: (t -> t -> Type) -> Constraint
class Category k where
  id :: k x x
  (.) :: k y z -> k x y -> k x z
```

This is fine until you want to specify that all objects in your category are
constrained in some way. For example, perhaps I want to be able to `Show`
intermediate values in my computation: in this formulation, I can't make this
demand of my categories. So, in this library, we abstract over some `c`
constraint that is applied to all objects in the category. If the category is
cartesian, cocartesian, or closed, we similarly apply these constraints to the
products, sums, and morphisms.
