#### Infinite Data Structures
Infinite list of consecutive integers can be defined as follows:

```haskell
[1..]
```

We could evaluate this list, but it won't print out in its entirety because it goes on forever. To repeat a set of identical values, you can use the repeat function
```haskell
repeat 'a'
```

The reason Haskell can work with infinite lists is it evalutes lists in a lazy fashion which means elements are evaluated as they become needed.

##### Fibonacci Numbers
The nth Fibonacci is the sum of the previous two Fibonacci numbers.
```haskell
1, 1, 2, 3, 5, 8, 13, 21,...
```

in Haskell we can create a list of infinite Fibonacci numbers as follows:
```haskell
let fibs = 1:1:(zipWith (+) fibs(tail fibs))
```

##### Prime Numbers
Below is a series of filter expressions to calculate an infinite list of prime numbers:
```haskell
properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(x-1)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..]
```

##### Factorials
Infinite list of factorials
```haskell
facts = map (\x-> (foldr (*) 1 [1..x])) [1..]
```
