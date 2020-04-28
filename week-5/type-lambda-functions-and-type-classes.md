#### Function Types
- Ordinary data types for primitive data (like _Int_ and _Char_) and basic data structures (like _[Int]_ and _[Char]_)
- Algebraic data types are types tht combine other types either as records ('products') e.g.
```haskell
data Pair = Pair Int Double
```

or as variants ('sums') e.g.
```haskell
data Bool = False | True
```

- Functions have types containing an arrow, e.g. 
```haskell
Int -> String
```

- We now look at function types in more detail

#### Lambda expressions
##### Named and anonymous expressions

You can give a name __sum__ to an expression 2 + 2:
```haskell
sum = 2 + 2
```

But you can also write __anonymous__ expressions:
```haskell
(-b) + sqrt (b^2 - 4 * a *c)
```

Without anonymous expressions, writing this would almost feel like assembly language:
```haskell
e1 = (-b)
e2 = b^2
e3 = 4*a
e4 = e3*c
e5 = e2-e4
e6 = e1+e5
```

##### Some background
- Sometimes in a mathematics or physics book, there are statements like __the function x^2 is continuous...__
- This is ok when the context makes it clear what x is
- But it can lead to problems. What does __x * y__ mean?

   - Is it a constant, because both _x_ and _y_ have fixed values?
   - Is it a function of _x_, with a fixed value of _y_?
   - is it a function of _y_, with a fixed value of _x_?
   - is it a function of both _y_ and _x_?

In mathematical logic (and computer programming) we need to be precise about this
- A lambda expression
```haskell
\x -> e
```
contains
   - An explicit statement that the formal parameter is _x_, and
   - the expression _e_ that defines the value of the function

##### Anonymous functions
A function can be defined and given a name using an equation
```haskell
f :: Int -> Int
f x = x + 1
```

- Since functions are "first class", they are ubiquitous, and it's often useful to denote a function anonymously.
- This is done using _lambda expressions_
```haskell
\x -> x + 1
```

#### Monomorphic and Polymorphic functions

##### Monomorphic functions
Monomorphic means 'having one form'
```haskell
f :: Int -> Char
f i = ['a'..'z'] !! (i - 1)

x :: Int
x = 3

f x :: Char
f x --> 'c'
```


##### Polymorphic
Polymorphic means 'having many forms'
```haskell
fst :: (a,b) -> a
fst (x,y) = x

snd :: (a,b) -> b
snd (x,y) = y

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b
```

#### Currying
- Most programming languages allow functions to have any number of arguments.
- But this turns out to be unnecessary: we can restrict all functions to have just one argument, without losing any expressiveness.

##### A function with two arguments
You can write a definition like this, which appears to have two arguments:
```haskell
f :: Int -> Int -> Int
f x y = 2*x + y
```

But it actually means the following:
```haskell
f :: Int -> (Int -> Int)
f 5 :: Int -> Int
```

The function takes its arguments one at a time:
```haskell
f 3 4 = (f 3) 4

g :: Int -> Int
g = f 3
g 10 -- > (f 3) 10 -- > 2*3 + 10
```
