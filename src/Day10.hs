module Day10 where


import Utils ( getLines )
import Data.List ( group, sort, foldl' )
import Memo ( memoise )
import System.TimeIt ( timeIt )


count1 :: [Int] -> Int
count1 ns = head ls * ls!!1
  where
    xs = sort ns
    ls = length <$> group (sort $ zipWith (-) (tail xs) (init xs))

-- For the split lists when none of the gaps are 3
count2 :: [Int] -> Int -> Int
count2 xs n = go [head ys] (tail ys)
  where
    ys = drop n xs 
    go :: [Int] -> [Int] -> Int
    go _ [] = 1
    go acc [x] = if x > last acc + 3 then 0 else 1
    go acc rem = go (acc ++ [head rem]) (tail rem) + go acc (tail rem)


-- Naive solution of the whole lot - to test memoisation
count3 :: [Int] -> Int -> Int
count3 xs n = fromIntegral $ memoisedGo 0
  where
    ys = drop n xs 

    memoisedGo = memoise go toInteger fromIntegral toInteger fromIntegral

    -- Recursion factored out...
    go :: (Int -> Int) -> Int -> Int
    go f i
      | i == (l - 1) = 1
      | otherwise = foldl' (\acc x -> acc + f x) 0 js
      where
        x = xs!!i
        js = filter (\k -> xs!!k - x <=3) [(i+1)..(l-1)]
        l = length ys



split :: [Int] -> [[Int]]
split xs = reverse $ go (head xs) [] [head xs] (tail xs)
  where
    go _ acc _ [] = acc
    go last acc next rem
      | head rem > last+2 = go (head rem) (next:acc) [head rem] (tail rem)
      | otherwise = go (head rem) acc (next ++ [head rem]) (tail rem)



day10 :: IO ()
day10 = do
  ls <- getLines 10
  let ns :: [Int]
      ns = read <$> ls
      xs = sort $ 0:ns ++ [maximum ns + 3]
      splits = split xs
      cs = flip count2 0 <$> splits

  
  timeIt $ putStrLn $ "Day10: part1: " ++ show (count1 xs)
  timeIt $ putStrLn $ "Day10: part2: splitting: " ++ show (product cs)
  timeIt $ putStrLn $ "Day10: part2: memoisation: " ++ show (count3 xs 0)
