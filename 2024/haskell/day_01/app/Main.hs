module Main where

import Data.List (sort)

processFile :: String -> ([Int], [Int])
processFile contents =
  let rows = lines contents
      columns = map (map read . words) rows
      firstColumn = map head columns
      secondColumn = map (!! 1) columns
   in (sort firstColumn, sort secondColumn)

sumList :: ([Int], [Int]) -> Int
sumList (first, second) =
  sum (zipWith (\x y -> abs (x - y)) first second)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (first, second) = processFile contents
  let sumL = sumList (first, second)

  print sumL
