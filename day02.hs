import Data.List

input = "4x23x21 22x29x19 11x4x11 8x10x5 24x18x16 11x25x22"

data Box = Box { s1 :: Int , s2 :: Int , s3 :: Int }

sortBox :: Box -> Box
sortBox box = Box (sorted!!0) (sorted!!1) (sorted!!2)
    where sorted = sort [s1 box, s2 box, s3 box]

splitBy :: (Eq t1, Foldable t) => t1 -> t t1 -> [[t1]]
splitBy delimiter = foldr (\c l@(x:xs) -> if c == delimiter then []:l else (c:x):xs) [[]]

area :: Box -> Int
area box = 2 * s1 box * s2 box + 2 * s2 box * s3 box + 2 * s3 box * s1 box

extra :: Box -> Int
extra box = s1 sBox * s2 sBox
     where sBox = sortBox box

ribbon :: Box -> Int
ribbon box = 2 * s1 sBox + 2 * s2 sBox + s1 sBox * s2 sBox * s3 sBox
    where sBox = sortBox box

buildBoxes :: [Box]
buildBoxes = map (\xs -> Box (read (xs!!0)) (read (xs!!1)) (read (xs!!2))) splits
    where splits = [x | x <- map (splitBy 'x') $ words input]

part1 = sum $ map (\x -> area x + extra x) buildBoxes
part2 = sum $ map ribbon buildBoxes