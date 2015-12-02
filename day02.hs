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

ribbon :: Box -> Int
ribbon box = 2 * l sBox + 2 * w sBox + l sBox * w sBox * h sBox
    where sBox = sortBox box

buildBoxes :: [Box]
buildBoxes = map (\xs -> Box (read (xs!!0)) (read (xs!!1)) (read (xs!!2))) splits
    where splits = [x | x <- map (splitBy 'x') $ words input]

part1 = sum $ map (\x -> area x + extra x) buildBoxes
part2 = sum $ map ribbon buildBoxes