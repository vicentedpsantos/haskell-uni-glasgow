class Eq a where
  (==) :: a -> a -> Bool

instance Eq Int where
  i1 == i2 = eqInt i1 i2

instance (Eq a) => Eq [a] where
  []     == [] = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)

-- definition of a function that checks whether
-- or not a value is a member of a list recursively

member :: Eq a => a -> [a] -> Bool
member x []                 = False
member x (y:ys) | x == y    = True
                | otherwise = member x ys
