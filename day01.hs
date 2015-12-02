import Data.List

input = "(()))(((("

part1 = foldl' (\acc x -> if x == '(' then acc + 1 else acc - 1) 0 input

part2 = length . takeWhile (> -1) . reverse $ foldl' (\acc@(y:ys) x -> if x == '(' then y + 1 : acc else y - 1 : acc) [0] input