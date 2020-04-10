-- example functions being tested

propRev :: [Int] -> Bool
propRev xs = reverse (reverse xs) == xs

propRevApp :: [Int] -> [Int] -> Bool
propRevApp xs ys = reverse (xs ++ ys) ==
                    reverse ys ++ reverse xs

--
-- and then you might run quick test like so
-- ghci > quickCheck propRev
-- ghci > OK: passed 100 tests
--
-- ghci > quickCheck propRevApp
-- ghci > OK: passed 100 tests
--

quickCheck :: Test a => a -> IO ()

class Test a where
  test :: a -> Rand -> Bool

class Arby a where
  arby :: Rand -> a

instance (Arby a, Test b) => Test (a -> b) where
  test f r = test (f (arby r1)) r2
          where (r1, r2) = split r

instance Test Bool where
  test b r = b
