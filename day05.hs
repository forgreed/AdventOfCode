import Data.List

vowelRule 'a' = True
vowelRule 'e' = True
vowelRule 'i' = True
vowelRule 'o' = True
vowelRule 'u' = True
vowelRule _   = False

badRule "ab" = True
badRule "cd" = True
badRule "pq" = True
badRule "xy" = True
badRule _    = False

countVowels = length . filter vowelRule

findBad :: String -> Bool
findBad ([]) = True
findBad (x:[]) = True
findBad (x:y:ys)
    | badRule [x,y] = False
    | otherwise     = findBad (y:ys)

findDouble ([])   = False
findDouble (x:[]) = False
findDouble (x:y:ys)
    | x == y    = True
    | ys == []  = False
    | otherwise = findDouble (y:ys)

findTwoPairs []     = False
findTwoPairs (x:[]) = False
findTwoPairs (x:y:ys)
    | isInfixOf [x,y] ys = True
    | otherwise          = findTwoPairs (y:ys)

findAltRepeat ([])     = False
findAltRepeat (x:[])   = False
findAltRepeat (x:y:[]) = False
findAltRepeat (x:y:z:zs)
    | x == z    = True
    | zs == []  = False
    | otherwise = findAltRepeat (y:z:zs)

part1 = length [x | x <- words input, findBad x, findDouble x, countVowels x > 2]
part2 = length [x | x <- words input, findTwoPairs x, findAltRepeat x]

input = "ugknbfddgicrmopn aaa bvggvrdgjcspkkyj qjhvhtzxzqqjkmpb uurcxstgmygtbstg"