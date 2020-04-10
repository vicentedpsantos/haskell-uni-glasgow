#### Keep your programs tidy

##### Scoping
Scoping i an important way to keep your programs tidy. It involves limiting the reigon of the program in which names exist an can be used.

###### Let clauses

In _Haskell_ a __let__ expression provides local scope. A let expression has a series of equations defining variable vlaues and a final expression (after the __in__ key word). E.g.:

```haskell
let x = 2
in x * x
```

Multiple variables can be defined in a single __let__:
```haskell
let x = 2
    y = 3
in x + y
```

Sometimes the value of a varialbe might depend on another variable being defined in the same let statement, as such:

```haskell
journeyCost :: Float -> Float -> Float
journeyCost miles fuelCostPerLitre =
  let milesPerGallon = 35
      litresPerGallon = 4.55
      gallons = miles / milesPerGallon
  in (gallons * litresPerGallon * fuelCostPerLitre)
```

another example could be:
```haskell
let diameter = 2 * radius
  circumference = pi * diameter
in (diameter, circumference)
```

p.s. the constant _pi_ is defined in the Haskell Prelude

###### Where clauses

Another syntax for introducing local varibles. This is another way Haskell provides us of defining variables being used in an equation. E.g.:

```haskell
squarePlusOne :: Int -> Int
squarePlusOne x = xSquared + 1
  where xSquared = x * x
```

Just like _let_, __where__ allows us to have multiple variable definitions in the same clause, as long as they are properly indented (mandatory).

```haskell
celToFahr :: Float -> Float
celToFahr x = (x * scalingFactor) + freezingPoint
  where scalingFactor = 9.0 / 5.0
        freezingPoint = 32
```

###### Scoping methods similarities
- Both introduce a __local scope__
- Both allow any number of equations to be written
- Both allow the equations to be written in any order, and variables defined in any equation can be used ("are in scope") in other equations

###### Scoping methods differences
- __let__ expressions are expressions; __let__ can be used anywhere an expression is allowed
- __where__ clauses are not expresions; they can be used only to provide some local variables for a top level equation
