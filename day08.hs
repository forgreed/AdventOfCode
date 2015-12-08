memLength :: String -> Int
memLength ('\\':'\"':xs)    = 2 + memLength xs
memLength ('\\':'\\':xs)    = 2 + memLength xs
memLength ('\\':'x':_:_:xs) = 4 + memLength xs
memLength (_:xs)            = 1 + memLength xs
memLength []                = 0

litLength :: String -> Int
litLength ('\"':xs)         = 0 + litLength xs
litLength ('\\':'\\':xs)    = 1 + litLength xs
litLength ('\\':'x':_:_:xs) = 1 + litLength xs
litLength (_:xs)            = 1 + litLength xs
litLength []                = 0

encLength :: String -> Int
encLength ('\"':xs)         = 3 + encLength xs
encLength ('\\':'\\':xs)    = 4 + encLength xs
encLength ('\\':'x':_:_:xs) = 5 + encLength xs
encLength (_:xs)            = 1 + encLength xs
encLength []                = 0

diff :: (String -> Int) -> (String -> Int) -> [String] -> Int
diff a b input = (sum . map a $ input) - (sum . map b $ input)

part1 :: IO ()
part1 = do
    input <- lines <$> readFile "input08.txt"
    print $ diff memLength litLength input

part2 :: IO ()
part2 = do
    input <- lines <$> readFile "input08.txt"
    print $ diff encLength memLength input
