import Data.List (find, findIndex, inits, sort,  (\\))
import Day4


solve :: [Int] -> [Board] -> Int
solve numbers boards = sumOfUnmarked * lastCalledNumber
    where   n = maximum rs
            rs = map (winningTurn numbers) boards
            Just i = findIndex (==n) rs
            lastWinningBoard = boards!!i
            lastCalledNumber = case n of
                                Just x  -> numbers!!(x-1)
                                Nothing -> error "boo"
            sumOfUnmarked = sum $ lastWinningBoard \\ winningNumbers
            winningNumbers = case n of
                                Just x -> head . drop x . inits $ numbers
                                Nothing -> error "boo"

winningTurn :: [Int] -> Board -> Maybe Int
winningTurn numbers b = length <$> xs
    where   xs = find (\x -> checkBoard x b) $ inits numbers

main :: IO ()
main = do
    str <- getContents
    let (input, boards) = parse . lines $ str
    print $ solve input boards
