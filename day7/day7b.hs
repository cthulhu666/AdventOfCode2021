import Day7

solve :: [Int] -> Int
solve xs = minimum . map (fuelUsage' xs) $ [x1..x2]
    where x1 = minimum xs
          x2 = maximum xs

main :: IO ()
main = do
    str <- getContents
    let xs = parse str
    print $ solve xs
