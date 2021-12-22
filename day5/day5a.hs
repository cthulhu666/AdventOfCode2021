import qualified Data.Vector as V

import Day5

main :: IO ()
main = do
    str <- getContents
    let m = readMap . parse $ str
    print $ length . V.filter (>=2) $ content m
