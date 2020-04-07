-- Computations over lists
--
-- Many computations that would be for/while loops in an imperative
-- language are naturally expressed as list comaputations in a functional language.
-- These are some common cases.
--  - Perform a computation on each element of a list: map
--  - Iterate over a list, from left to right: foldl
--  - Iterate over a list, from right to left: foldr
--
--  It is good practice to use functions like filter and these three functions when applicable
--  Lets look at maps and folds in some more detail.

-- The following relationship is really useful to refactor haskell code

-- map f (map g xs) = map (f . g) xs


