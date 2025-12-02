parseFile :: String -> IO [String]
parseFile s = return $ lines s

findXMAS :: [String] -> Int
findXMAS xs = 

main :: IO ()
main = do
  input <- readFile "input.txt" >>= parseFile
  print input
