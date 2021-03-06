
module Day1 where


import Utils (getLines)


day1 :: IO ()
day1 = do
  inLines <- getLines 1
  let transactions :: [Int]
      transactions = read <$> inLines
      t1 :: [(Int, Int)]            
      t1 = [(x, y) | x <- transactions, y <- transactions, (x+y) == 2020]
      t2 :: [(Int, Int, Int)]            
      t2 = [(x, y, z) | x <- transactions, y <- transactions, z <- transactions, (x+y+z) == 2020]
      (x, y) = head t1
      (p, q, r) = head t2
  putStrLn $ "Day1: part1: " ++ show (x*y)
  putStrLn $ "Day1: part2: " ++ show (p*q*r)

  return ()


