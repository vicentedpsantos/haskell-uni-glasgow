-- In Haskell there are several ways to define conditional functions.
-- The easiest of them being to write a definition for each case, as is done
-- in the previous notes. E.g.

length [] = 0
length x:xs = 1 + length xs

-- There are other ways to define them such as an if-then-else expression

length lst =
  if lst == []
    then 0
    else let x:xs = lst in 1 + length xs

-- Alternatively you can use what is known as guards, e.g.

length lst
  | lst == [] = 0
  | otherwise = let x:xs = lst in 1 + length xs

-- finally you can define these functions using multiline functions and where clauses with
-- semicolons, e.g.:

f = f' where f' 1 = 0; f' x = x + f' (x - 1)

-- Recursive definition of filter
-- The function filter is given a predicate (function that gives a Boolean result)

filter (< 5) [3, 9, 2, 12, 6, 4]

myFilter condition lst
  | null lst = []
  | otherwise = if condition x
                  then x:myFilter condition xs
                  else myFilter condition xs
                    where x:xs = lst

myFilter (>5) [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- or in a single line

myFilter condition lst | null lst = [] | otherwise = if condition x then x:filter condition xs else filter condition xs where x:xs=lst
