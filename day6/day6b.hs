import Day6

main :: IO ()
main = do
    str <- getContents
    let xs = parse' str
    let p = foldl (\a _ -> nextDay' a) xs [1..256]
    print $ sum p
