#### Guards

###### Haskell provides a notation for defining functions based on predicate values.
```haskell
f x
  | predicate1 = expression1
  | predicate2 = expression2
  | predicate3 = expression3
```

For instance, the _absolute_ value of a number is its magnitude, i.e. ignoring its sign. You could define a function to calculate the absolute value with an if/then/else conditional

```haskell
absolute x = if (x< 0) then (-x) else x
```

or with guards:

```haskell
absolute x
  | x < 0     = -x
  | otherwise = x
```

Notice how there is no equals sign on the first line of the function definition -- but there is an equals sign after each guard.

For instance, think about scoring in the sport of Golf. For a single hole, a player takes a number of _strokes_. There is a _par_ score for the hole which is the expected number of _strokes_.

```haskell
holeScore :: Int -> Int -> String
holeScore strokes par
  | strokes < par  = show (par - strokes) ++ " under par"
  | strokes == par = "level par"
  | strokes > par  = show (strokes - par) ++ " over par"
```

How could we tidy this up? Maybe we could turn the final guard into __otherwise__ and also refactor with __where__ clause.

```haskell
holeScore :: Int -> Int -> String
holeScore strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show (score_ ++ " over par")
where score = strokes - par
```

#### Case Expressions

Case expressions are like guards, but they select based on the form of the value, i.e. it does pattern matching.

Here is a sum data type for my pets

```haskell
data Pet = Cat | Dog | Fish
```

and here is how I greet my pets

```haskell
hello :: Pet -> String
hello pet =
  case pet of
    Cat  -> "meow"
    Dog  -> "woof"
    Fish -> "bubble"
```

Now suppose we want to make the data type a bit more sophisticated and add a __Parrot__ with a __String__ name.

```haskell
data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello pet =
  case pet of
    Cat         -> "meow"
    Dog         -> "woof"
    Fish        -> "bubble"
    Parrot name -> "pretty " ++ name
```

Now the pattern include a variable, which is associated with the concrete value for the Parrot's name.

```haskell
hello (Parrot "polly") -- "pretty polly"
```

In the same way as there is a catch-all case for guards(otherwise), we can have a catch-all pattern for __case__. It's the underscore character, _ which means 'don't care' or 'match anything', so we could redefine hello as:

```haskell
hello :: Pet -> String
hello pet
  case pet of
    Parrot name -> "pretty " ++ name
    _           -> "grunt"
```









