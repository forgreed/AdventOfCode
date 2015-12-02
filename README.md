# Advent of Code in Haskell

## Introduction

I've decided to work my way through each [advent of code](http://adventofcode.com/) challenge in Haskell. I don't know the problems ahead of time, so I can't be sure Haskell is at all a good tool for each. In any case, I expect it will make the daily puzzles much more interesting.

## Day 1

### Part 1

> Given a list of parentheses, find the balance offset. `(` is +1 and `)` is -1.

Strings are lists of characters, and we are reducing this list to a single value. It would be the perfect problem for the strict, tail-recursive `foldl'`.

``` hs
Prelude> import Data.List
Prelude Data.List> let parens = "(()))(((("
Prelude Data.List> foldl' (\acc x -> if x == '(' then acc + 1 else acc - 1) 0 parens
3
```

### Part 2

> Find the position of the first character produces the total of -1, where the starting element has position 1

This is a vastly different problem, and I have yet to come up with a very performant purely-functional solution. We will start by manipulating the `foldl'` function from step 1 to form a list with the accumulating value. A right fold is not possible for this problem, so `takeWhile` will have to be applied to reversed result. The length of the result will give us the correct solution because the initial `0` satisfies the problem's starting index of 1.

``` hs
Prelude Data.List> length . takeWhile (> -1) . reverse $ foldl' (\acc@(y:ys) x -> if x == '(' then y + 1 : acc else y - 1 : acc) [0] input
5
```

## Day 2

### Part 1

> Find the total amount of material needed given a list of dimensions, where the formula is `2*l*w + 2*w*h + 2*h*l` plus the area of the smallest two sides

``` hs 
import Data.List

input = "4x23x21 22x29x19 11x4x11 8x10x5 24x18x16 11x25x22"

data Box = Box { l :: Int , w :: Int , h :: Int }

sortBox :: Box -> Box
sortBox box = Box (sorted!!0) (sorted!!1) (sorted!!2)
    where sorted = sort [l box, w box, h box]

splitBy :: (Eq t1, Foldable t) => t1 -> t t1 -> [[t1]]
splitBy delimiter = foldr (\c l@(x:xs) -> if c == delimiter then []:l else (c:x):xs) [[]]

area :: Box -> Int
area box = 2 * l box * w box + 2 * w box * h box + 2 * h box * l box

extra :: Box -> Int
extra box = l sBox * w sBox
     where sBox = sortBox box

buildBoxes :: [Box]
buildBoxes = map (\xs -> Box (read (xs!!0)) (read (xs!!1)) (read (xs!!2))) splits
    where splits = [x | x <- map (splitBy 'x') $ words input]
```

The answer is the sum of the map of `area + extra` over each set of dimensions.

``` hs
Prelude> sum $ map (\x -> area x + extra x) buildBoxes
10748
```

### Part 2

> Find the total amount of ribbon needed, which is 2 * smallest + 2 * medium dimension plus an extra l * w * h.

The dimensions must be sorted, so the `extra` function can be modified to suit the formula.

``` hs
ribbon :: Box -> Int
ribbon box = 2 * l sBox + 2 * w sBox + l sBox * w sBox * h sBox
    where sBox = sortBox box
```

Like part 1, use a map to compute the answer:

``` hs
Prelude> sum $ map ribbon buildBoxes
28222
```