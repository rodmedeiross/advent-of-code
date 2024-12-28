module Main where

processFile :: String -> [[Int]]
processFile input =
  let rows = lines input
   in map (map read . words) rows

processFile' :: String -> [[Int]]
processFile' input = map (map read . words) rows
  where
    rows = lines input

compareSucessor :: (a -> a -> b) -> [a] -> [b]
compareSucessor f xs = zipWith f xs (tail xs)

isRange :: (Ord a, Num a) => a -> a -> Char
isRange x y
  | x < y && calc <= 3 = '<'
  | x > y && calc <= 3 = '>'
  | otherwise = '='
  where
    calc = abs (x - y)

isRange' :: (Ord a, Num a) => a -> a -> Bool
isRange' x y
  | x < y && calc <= 3 = True
  | x > y && calc <= 3 = True
  | otherwise = False
  where
    calc = abs (x - y)

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

removeOne :: [a] -> [[a]]
removeOne xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

main :: IO ()
main = do
  input <- readFile "test.txt"
  let numbers = processFile' input

  let resultList1 = [compareSucessor isRange row | row <- numbers]
   in print $ length [x | x <- resultList1, allSame x]

  let resultList2 = [removeOne row | row <- numbers]
      filteredItems = [[compareSucessor isRange y | y <- item] | item <- resultList2]
      result = [x | x <- filteredItems, any allSame x]
   in print $ length result
