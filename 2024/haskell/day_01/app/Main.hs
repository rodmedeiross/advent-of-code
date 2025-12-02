module Main where

import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List (foldl', sort)

processFile :: String -> ([Int], [Int])
processFile contents =
  let rows = lines contents
      columns = map $ map read . words rows
      firstColumn = map head columns
      secondColumn = map (!! 1) columns
   in (sort firstColumn, sort secondColumn)

sumList :: ([Int], [Int]) -> Int
sumList (first, second) =
  sum (zipWith (\x y -> abs (x - y)) first second)

countOccurrences :: (Eq k, Hashable k) => [k] -> HM.HashMap k Int
countOccurrences = foldr (\x acc -> HM.insertWith (+) x 1 acc) HM.empty

sumOccurrences :: ([Int], [Int]) -> Int
sumOccurrences (first, second) =
  let firstC = countOccurrences first
      secondC = countOccurrences second

      result =
        sum $
          map
            ( \n ->
                let count1 = HM.lookupDefault 0 n firstC
                    count2 = HM.lookupDefault 0 n secondC
                 in n * count1 * count2
            )
            (HM.keys firstC)
   in result

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (first, second) = processFile contents
  let sumL = sumList (first, second)

  let occurrences = sumOccurrences (first, second)

  print sumL
  print occurrences
