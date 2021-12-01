solve xs = length $ filter (\(a,b) -> a < b) ys
    where ys = zip xs $ tail xs

window xs = [x+y+z | (x,y,z) <- ys]
    where ys = zip3 xs (tail xs) (drop 2 xs)

main = do
    str <- readFile "input.txt"
    print $ solve $ window [read x :: Int | x <- lines str]
