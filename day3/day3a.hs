import Data.Char (digitToInt)
import Data.List (foldl')
import Data.List (transpose)

gamma :: [String] -> Int
gamma xs = str2int $ map mode $ transpose xs

epsilon :: [String] -> Int
epsilon xs = str2int $ map mode' $ transpose xs

str2int :: String -> Int
str2int = foldl' (\acc x -> acc * 2 + digitToInt x) 0

mode :: String -> Char
mode xs = if l0 > l1
            then '0'
            else '1'
    where l0 = length $ filter (=='0') xs
          l1 = length $ filter (=='1') xs

mode' :: String -> Char
mode' = mode . map flip
    where flip x = if x == '0'
                    then '1'
                    else '0'

main :: IO ()
main = do
    str <- getContents
    let input = lines str
    print $ gamma input * epsilon input
