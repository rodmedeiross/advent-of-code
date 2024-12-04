module Main where

import Data.Maybe (mapMaybe)
import Text.Regex.TDFA (getAllTextMatches, (=~))

processFile :: String -> [String]
processFile i = lines i

getStatements :: [String] -> [(Int, Int)]
getStatements lines = concatMap (getTuple . parseGroup) lines

getTuple :: [[String]] -> [(Int, Int)]
getTuple = map (\[_, a, b] -> (read a, read b))

sumList :: [Int] -> Int
sumList xs = sum xs

sufferMultiply :: [(Int, Int)] -> Int
sufferMultiply = sumList . map (\(a, b) -> a * b)

-- parseMatch :: String -> [String]
-- parseMatch line = getAllTextMatches (line =~ "mul\\(([0-9]+),([0-9]+)\\)") :: [String]

parseGroup :: String -> [[String]]
parseGroup line = line =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]

main :: IO ()
main = do
  i <- readFile "input.txt"
  let matches = getStatements (processFile i)
  let numbers = sufferMultiply matches
  print numbers
