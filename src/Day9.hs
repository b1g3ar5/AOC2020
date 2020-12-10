module Day9 where


import Utils ( getLines )
import Data.Maybe ( fromJust )
import System.TimeIt


twinSum :: [Int] -> Int -> Bool
twinSum [] _ = False
twinSum (x:xs) target = not (null xSums) || twinSum xs target
  where
    xSums = [ y | y <- xs, x+y == target]
  

findInvalid :: Int -> [Int] -> Maybe Int
findInvalid n = go
  where
    go ns 
      | length ns <= n = Nothing 
      | twinSum group target = go (tail ns) 
      | otherwise = Just target 
      where
        group = take n ns
        target = ns!!n
  

findWindow :: Int -> [Int]-> [Int]
findWindow target = go [] 
  where
    go acc rem@(n:ns)
      | sum (n:acc) == target = acc ++ [n] 
      | sum (n:acc) < target = go (acc++[n]) ns
      | otherwise = go (tail acc) rem 


day9 :: IO ()
day9 = do
  ls <- getLines 9
  let ns :: [Int]
      ns = read <$> ls
      n = 25
      target = fromJust $ findInvalid n ns
      rng = findWindow target ns

  putStrLn $ "Day9: part1: " ++ show target
  timeIt $ putStrLn $ "Day9: part2: " ++ show (minimum rng + maximum rng)

