solve xs = length $ filter (\(a,b) -> a < b) ys
    where ys = zip xs $ tail xs

main = do
    str <- readFile "input.txt"
    print $ solve [read x :: Int | x <- lines str]
