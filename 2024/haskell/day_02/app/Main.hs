module Main where

processFile :: String -> [[Int]]
processFile input =
  let rows = lines input
      items = map (map read . words) rows
   in items

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = processFile input
  print input
  print numbers
