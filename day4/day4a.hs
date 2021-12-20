import Data.List (find, inits, (\\))
import Data.Maybe (isJust)

import Day4


solve :: [Int] -> [Board] -> Int
solve numbers boards = sumOfUnmarked * lastCalledNumber
    where Just winningNumbers = find (\xs -> isJust $ checkBoards boards xs) $ inits numbers
          Just winningBoard = checkBoards boards winningNumbers
          lastCalledNumber = head . reverse $ winningNumbers
          sumOfUnmarked = sum $ winningBoard \\ winningNumbers


main :: IO ()
main = do
    str <- getContents
    let (input, boards) = parse . lines $ str
    print $ solve input boards
