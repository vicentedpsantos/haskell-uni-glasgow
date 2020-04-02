length :: [a] -> Int           -- function type
length [] = 0                  -- base case
length (x:xs) = 1 + Main.length xs  -- recursion case

filter :: (a->Bool) -> [a] -> [a]
filter pred []    = []
filter pred (x:xs)
  | pred x         = x : Main.filter pred xs
  | otherwise      = Main.filter pred xs

-- filter (<5) [3,9,2,12,6,4] -- > [3,2,4]

