##### Returning functions as values
- Functions can return functions as values
- For example, partial application of a function in Haskell:

```haskell
sum = foldl (+) 0
```

More explicitly we can write this as:
```haskell
sum = \xs -> foldl (+) 0 xs
```

##### Function generators
- We can use this concept to generate parameterised functions

For example, the following function generates functions that adds a constant number to their argument:
```haskell
genAddN = \n ->
            \x ->
              x+n

addThree = genAddN 3
addSeven = genAddN 7

addThree 5 -- 8
addSeven 4 -- 11
```

This is of course not limited to numeric constants

For example, the following function generates functions that performa  given arithmetic operation on a constant number and their argument.

```haskell
genOpN = \op n ->
    \x -> x `op` n

addThree = genOpN (+) 3
multSeven = genOpN (*) 7

addThree 5 --> 8
multSeven 4 --> 28
```

##### Practical Parsing

To make the parsing problem more concrete, suppose you have to parse the following recipe and identify the different steps required  in the preparation.

> Bring a large pot of water up to a boil. Unlike Italian pasta, you do not need to salt the water. Once it’s boiling, hold the noodles over the water and sprinkle them in strand by strand. Once all the noodles are in, stir gently so that they are all immersed in the water. Bring the water back up to a gentle boil, then lower the heat so that the water is just simmering. (This differs from the ’rolling boil’ that’s recommended for pasta.) If the water threatens to boil over, add about 1/2 cup of cold water (but if you lower the heat to the gentle simmer, and have a big enough pot, this shouldn’t be necessary). Cook for about 7 to 8 minutes, or following the package directions (for thinner noodles 5 to 6 minutes may be enough. Test by eating a strand - it should be cooked through, not al dente, but not mushy either).

__Parser Combinators__

- Parser combinators are functions that allow you to combine smaller parsers into bigger ones.
- They are higher-order functions that take functions as arguments and return functions
- A parser combinator library provides both basic parsers (for words, numbers etc.) and combinators.

__Parsec: monadic parsing combinators__
- There are many parsing libraries for Haskell.
- One of the most widely used is Parsec, which is robust, flexible, expressive, and efficient.
- Parsec operates in a monad.



