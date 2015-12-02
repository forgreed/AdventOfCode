import Data.List

input = "4x23x21 22x29x19 11x4x11 8x10x5 24x18x16 11x25x22"

splitBy :: (Eq t1, Foldable t) => t1 -> t t1 -> [[t1]]
splitBy delimiter = foldr (\c l@(x:xs) -> if c == delimiter then []:l else (c:x):xs) [[]]

ints :: [[Int]]
ints = [map read x | x <- map (splitBy 'x') $ words input]

area :: [Int]-> Int
area (l:w:h:[]) = 2*l*w + 2*w*h + 2*h*l

extra :: [Int]-> Int
extra xs = s * m
    where (s:m:l:[]) = sort xs

ribbon :: [Int] -> Int
ribbon xs = 2*s + 2*m + s*m*l
    where (s:m:l:[]) = sort xs

part1 = sum $ map (\x -> area x + extra x) ints
part2 = sum $ map ribbon ints