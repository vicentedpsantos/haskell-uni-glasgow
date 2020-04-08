data SimpleNum = One | Two | Many deriving Show

-- Any Type must derive Show if we ought to print out its values

let convert 1 = One
    convert 2 = Two
    convert _ = Many

convert 1 -- One
convert 300 -- Many
map convert [1..5] -- [One, Two, Many, Many, Many]

-- 

data CricketScore = Score [Char] Int Int deriving Show

let x = Score "New Zealand" 350 4

x -- Score "New Zealand" 350 4
-- x is of type CricketScore
--
-- These kinds of custom data types are called algebraic data types
-- The alternative values relate to algebraic sums
-- and the record values relate to algebraic products

