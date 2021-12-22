module Day5 where

import Data.List.Split (splitOn)
import qualified Data.Vector as V

type Coords = (Int,Int,Int,Int)

data OceanFloor = OceanFloor { content :: V.Vector Int, width   :: Int } deriving (Show, Eq)

parse :: String -> [Coords]
parse = map parseLine . lines

-- 498,436 -> 498,932
parseLine :: String -> Coords
parseLine s = (x1,y1,x2,y2)
    where [w1, _, w2] = words s
          [x1, y1] = map (\x -> read x :: Int) $ splitOn "," w1
          [x2, y2] = map (\x -> read x :: Int) $ splitOn "," w2

isHorizontal :: Coords -> Bool
isHorizontal (x1,_,x2,_) = x1 == x2

isVertical :: Coords -> Bool
isVertical (_,y1,_,y2) = y1 == y2

maxXY :: [Coords] -> (Int, Int)
maxXY xs = (maximum mxs, maximum mys)
    where (mxs, mys) = foldl (\(ax, ay) (x1,y1,x2,y2) -> (x1:x2:ax, y1:y2:ay)) ([], []) xs

inc :: OceanFloor -> [(Int,Int)] -> OceanFloor
inc o xs = o { content = V.update c $ V.fromList ys }
    where w = width o
          c = content o
          ys = map (\(x,y) -> (x + w * y, get o (x,y) + 1)) xs

get :: OceanFloor -> (Int,Int) -> Int
get o (x,y) = (V.!) c (x + w * y)
    where c = content o
          w = width o

empty :: (Int, Int) -> OceanFloor
empty (w, h) = OceanFloor { content = V.replicate (w * h) 0, width = w }

readMap :: [Coords] -> OceanFloor
readMap xs = foldl setLine o ys
    where o = empty (x+1, y+1)
          ys = filter (\(x1,y1,x2,y2) -> x1==x2 || y1==y2) xs
          (x, y) = maxXY xs

readMap' :: [Coords] -> OceanFloor
readMap' xs = foldl setLine o xs
    where o = empty (x+1, y+1)
          (x, y) = maxXY xs

setLine :: OceanFloor -> Coords -> OceanFloor
setLine o (x1,y1,x2,y2) = inc o $ line (x1,y1) (x2,y2)

goto :: (Int,Int) -> (Int,Int) -> Maybe (Int, Int)
goto (x1,y1) (x2,y2)
    | (x1,y1) == (x2,y2) = Nothing
    | otherwise          = Just (x1+dx, y1+dy)
        where dx = signum (x2-x1)
              dy = signum (y2-y1)

line :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
line s d = case goto s d of
                Nothing    -> [s]
                Just c     -> s:(line c d)
