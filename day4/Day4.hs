module Day4 where
import Data.List.Split (splitOn)
import Data.List (find, intersect, transpose)

type Board = [Int]

parse :: [String] -> ([Int], [Board])
parse (xs:xss) = (input, boards)
    where boards = parseBoards $ filter (/="") xss
          input  = [read x :: Int | x <- splitOn "," xs]

parseBoards :: [String] -> [Board]
parseBoards [] = []
parseBoards xs = parseBoard y : parseBoards ys
    where (y, ys) = splitAt 5 xs

parseBoard :: [String] -> Board
parseBoard xs = foldl parseLine [] xs
    where parseLine acc l = acc ++ [read w :: Int | w <- words l]

rows :: Board -> [[Int]]
rows [] = []
rows b = take 5 b : (rows $ drop 5 b)

cols :: Board -> [[Int]]
cols = transpose . rows

checkBoards :: [Board] -> [Int] -> Maybe Board
checkBoards bs numbers = find (checkBoard numbers) bs

checkBoard ::  [Int] -> Board -> Bool
checkBoard numbers b = any (match numbers) xs
    where xs = cols b ++ rows b

match :: [Int] -> [Int] -> Bool
match numbers rowOrCol = length xs == 5
    where xs = intersect rowOrCol numbers
