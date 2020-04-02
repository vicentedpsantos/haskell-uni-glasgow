#### Computations over lists

Many computatations that would be for/while loops in an imperative language are naturally expressed as list computations in a functional language.

There are some common cases:

- Perform a computation on each element of a list:  𝑚𝑎𝑝
- Iterate over a list, from left to right:  𝑓𝑜𝑙𝑑𝑙
- Iterate over a list, from right to left:  𝑓𝑜𝑙𝑑𝑟

#### Function composition
We can express a large computation by __“chaining together”__ a sequence of functions that perform smaller computations
- Start with an argument of type  𝑎 
- Apply a function  𝑔::𝑎−>𝑏  to it, getting an intermediate result of type  𝑏 
- Then apply a function  𝑓::𝑏−>𝑐  to the intermediate result, getting the final result of type  𝑐 
- The entire computation (first  𝑔 , then  𝑓 ) is written as  𝑓∘𝑔 .

This is traditional mathematical notation; just remember that in  𝑓∘𝑔 , the functions are used in right to left order.
Haskell uses . as the function composition operator

```haskell
(.) :: (b->c) -> (a->b) -> a -> c
(f . g) x = f (g x)
```

#### Performing an operation on every element of a list: map
__map__ applies a function to every element of a list

```haskell
map f [x0,x1,x2] -- > [f x0, f x1, f x2]
```

##### Composition of maps
- map is one of the most commonly used tools in your functional toolkit
- A common style is to define a set of simple computations using map, and to compose them.
```haskell
map f (map g xs) = map (f . g) xs
```

__Recursive definition of map__
```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

#### Folding a list
- An iteration over a list to produce a singleton value is called a fold
- There are several variations: folding from the left, folding from the right, several variations having to do with “initialisation”, and some more advanced variations.
- Folds may look tricky at first, but they are extremely powerful, and they are used a lot! And they aren’t actually very complicated.

Left fold: __foldl__
foldl is fold from the left
Think of it as an iteration across a list, going left to right.
- A typical application is  𝑓𝑜𝑙𝑑𝑙𝑓𝑧𝑥𝑠
- The  𝑧::𝑏  is an initial value
- The  𝑥𝑠::[𝑎]  argument is a list of values which we combine systematically using the supplied function  𝑓
- A useful intuition: think of the  𝑧::𝑏  argument as an “accumulator”.
- The function  𝑓  takes the current value of the accumulator and a list element, and gives the new value of the accumulator.

```haskell
foldl :: (b->a->b) -> b -> [a] -> b
```

__Examples of foldl with infix notation__
In this example, + denotes an arbitrary operator for f; it isn’t supposed to mean specifically addition.

```haskell
foldl (+) z []          -- > z
foldl (+) z [x0]        -- > z + x0
foldl (+) z [x0,x1]     -- > (z + x0) + x1
foldl (+) z [x0,x1,x2]  -- > ((z + x0) + x1) + x2
```

__Recursive definition of foldl__

```haskell
foldl        :: (b -> a -> b) -> b -> [a] -> b
foldl f z0 xs0 = lgo z0 xs0
             where
                lgo z []     =  z
                lgo z (x:xs) = lgo (f z x) xs
```
