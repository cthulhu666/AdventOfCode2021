import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List (transpose)

main :: IO ()
main = do
    str <- getContents
    let input = lines str
    print $ oxygen input * co2 input


oxygen :: [String] -> Int
oxygen xs = str2int $ head $ ys
    where ys = f xs 0

co2 :: [String] -> Int
co2 xs = str2int $ head $ ys
    where ys = f' xs 0

filter' :: (String -> Char) -> [String] -> Int -> [String]
filter' modeF xs i = filter (\x -> x!!i == c) xs
    where c = modeF $ (!!i) . transpose $ xs

mode :: String -> Char
mode xs = if l0 > l1
            then '0'
            else '1'
    where l0 = length $ filter (=='0') xs
          l1 = length $ filter (=='1') xs

mode' :: String -> Char
mode' xs = if l0 <= l1
            then '0'
            else '1'
    where l0 = length $ filter (=='0') xs
          l1 = length $ filter (=='1') xs

str2int :: String -> Int
str2int = foldl' (\acc x -> acc * 2 + digitToInt x) 0

f :: [String] -> Int -> [String]
f [x] _ = [x]
f xs i = f ys (i+1)
    where ys = filter' mode xs i

f' :: [String] -> Int -> [String]
f' [x] _ = [x]
f' xs i = f' ys (i+1)
    where ys = filter' mode' xs i
