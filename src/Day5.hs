module Day5 where


import Utils ( getLines, toInt )
import Data.List ( (\\) )


readLine :: String -> Int
readLine s = 8*r+c
  where
    (r, c) = (toInt $ (=='B') <$> reverse (take 7 s), toInt $ (=='R') <$> drop 7 s)


day5 :: IO ()
day5 = do
  ls <- getLines 5
  let ids = readLine <$> ls 
      mid = maximum ids 
      missingSeats = [0..47] ++ [833..1024]
      frontRow = [48..55]
      possibles = [0..(8*128 - 1)] \\ (ids ++ missingSeats  ++ frontRow)
  putStrLn $ "Day5: part1: " ++ show mid
  putStrLn $ "Day5: part2: " ++ show possibles



