module Day6 where

import Data.List.Split (splitOn)
import qualified Data.Vector as V

maxTimer = 8

parse :: String -> [Int]
parse s = [read x :: Int | x <- splitOn "," s]

nextDay :: [Int] -> [Int]
nextDay = foldl f []
    where f a x
            | x == 0    = 6:8:a
            | otherwise = (x-1):a

-- [3,4,3,1,2] becomes [0, 1, 1, 2, 1]
--                      0  1  2  3  4 age

type Population = V.Vector Int

parse' :: String -> Population
parse' xs = V.fromList [length . filter (==x) $ ys | x <- [0..maxTimer]]
    where ys = parse xs

nextDay' :: Population -> Population
nextDay' p = V.update (V.fromList ys) $ V.fromList [(6,n)]
    where ys = tail xs ++ [head xs]
          xs = V.toList p
          n = (V.!) p 0 + (V.!) p 7
