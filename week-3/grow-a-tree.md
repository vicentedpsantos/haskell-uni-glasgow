##### Grow a Tree

__In computer science, trees grow upside-down. The unique root is at the top, and the leaves are at the bottom__

The binary tree data type is often used for storing data in a sorted order, to allow efficient searching - for example, a telephone directory.

Defining a data type for trees storing integer values could be done as follows:

```haskell
data Tree = Leaf | Node Int Tree Tree deriving Show
```

A __Tree__ value might be either a __Leaf__ or a __Node__. Note tha tthis is a _recursive_ data type, since a __Node__ stores an __Int__ payload and has branches to two subtrees (sometimes called children).

Here is the simplest tree - it's just a single leaf

```haskell
Leaf
```

Here is a tree with one __Node__ containing value 3, and two leaves:
```haskell
Node 3 Leaf Leaf
```

Now let's write a function to compute the depth of a __Tree__ -- this is the maximum number of branches from the root to any leaf. To write this function, we will do pattern matching on the different kinds of __Tree__, i.e. __Leaf__ and __Node__ values. Each __Leaf__ is a base case, but for each __Node__, we need to recursively process the two child trees.

```haskell
treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)
```

And here is a function that traverses all the branches and sums up the value at each __Node__:

```haskell
treeSum :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node value leftSubtree rightSubtree) =
  value + (treeSum leftSubtree) + (treeSum rightSubtree)
```

How about a function to check whether a tree is sorted properly? The data structure invariant we want is that, for any __Node__ storing value __x__, all values in its left subtree are _< x_, and all values in its right subtree are _>= x_.

So this function will take in a __Tree__, a minimum value, a maximum value and it will return a __Bool__. isSortedTree :: Tree -> Int -> Int -> Bool

A Leaf is automatically sorted, since it does not contain a value. For each __Node__, we have to check the value in it is between the min and max values, which start off as far apart as possible, then get split into smaller ranges based on the value at the Node.

```haskell
isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted   = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted
```
