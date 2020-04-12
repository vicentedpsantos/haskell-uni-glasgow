-- find the max value in an Int list

maxHelper :: Int -> [Int] -> Int
maxHelper x [] = x
maxHelper x (y:ys) = maxHelper
  (if x > y then x else y) ys

-- if the list argument comes empty
-- then the function returns Nothing
maxFromList :: [Int] -> Maybe Int
maxFromList [] = Nothing
maxFromList (x:xs) = Just(maxHelper x xs)

maxFromList [1,2,3] -- Just 3
maxFromList []      -- Nothing

-- Once we've generated Maybe values, how do we propagate them in programs?
-- (monads)
--
-- To map functions over Maybe values we can use fmap

inc :: Num a => a -> a
let inc = (1+)

inc 1             -- 2
inc (Just 1)      -- error
fmap inc (Just 1) -- Just 2

