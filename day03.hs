import Data.List

start = [(0,0)]

trav inp = foldl p [(0,0)] inp
    where 
        p acc x | x == '>' = (first acc + 1, second acc)     : acc
                | x == '<' = (first acc - 1, second acc)     : acc
                | x == '^' = (first acc,     second acc + 1) : acc
                | x == 'v' = (first acc,     second acc - 1) : acc
        first acc  = fst (head acc)
        second acc = snd (head acc)

split1 []       = []
split1 (x:y:ys) = x : split1 ys

split2 []       = []
split2 (x:y:ys) = y : split2 ys

part1 = length . nub $ trav input
part2 = length . nub $ (trav $ split1 input) ++ (trav $ split2 input)

input = ">^^v^<>v<<<v<v^>>v^^^<v<>^^><^<<^vv>>>^<<^>><vv<<v^<^^><>>><>v<><>"