module Day7 where

import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = map read . splitOn ","

fuelUsage :: [Int] -> Int -> Int
fuelUsage xs pos = sum [abs(x - pos) | x <- xs]

fuelUsage' :: [Int] -> Int -> Int
fuelUsage' xs pos = sum [sum [0..abs(x - pos)] | x <- xs]
