module Main where

processFile :: String -> [[Int]]
processFile input =
  let rows = lines input
      items = map (map read . words) rows
   in items

compareSucessor :: (a -> a -> b) -> [a] -> [b]
compareSucessor f xs = zipWith f xs (tail xs)

isRange :: (Ord a, Num a) => a -> a -> Char
isRange x y
  | x < y && calc <= 3 = '<'
  | x > y && calc <= 3 = '>'
  | otherwise = '='
  where
    calc = abs (x - y)

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = processFile input
  let resultList = [compareSucessor isRange row | row <- numbers]
  let result = [x | x <- resultList, allSame x]

  print $ length result
