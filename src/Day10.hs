module Day10 where


import Utils ( getLines )
import Data.List ( group, sort, foldl', sortOn, groupBy )
import Memo ( memoise )
import System.TimeIt ( timeIt )


countGaps :: [Int] -> Int
countGaps ns = head ls * ls!!1
  where
    xs = sort ns
    ls = length <$> group (sort $ zipWith (-) (tail xs) (init xs))


-- For the split lists when none of the gaps are 3
countPerms :: [Int] -> Int
countPerms ys = go [head ys] (tail ys)
  where
    go :: [Int] -> [Int] -> Int
    go _ [] = 1
    go acc [x] = if x > last acc + 3 then 0 else 1
    -- We can either use the next one or skip it
    go acc rem = go (acc ++ [head rem]) (tail rem) + go acc (tail rem)


-- Naive solution of the whole lot - to test memoisation
countPerms2 :: [Int] -> Int
countPerms2 ys = fromIntegral $ memoisedGo 0
  where
    -- Memoise for recursive functions
    memoisedGo = memoise go id id id id 

    -- Recursion factored out...
    go :: (Integer -> Integer) -> Integer -> Integer
    go f i = if i == fromIntegral (l - 1) then 1 else foldl' (\acc x -> acc + f x) 0 js
      where
        ii = fromIntegral i
        l = length ys
        x = ys!!ii
        -- possible next items...
        js = toInteger <$> filter (\k -> ys!!k - x <=3) [(ii+1)..(l-1)]



-- Split at all gaps of 3 - so we can just
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
      p2 = product $ zipWith (^) [1,1,2,4,7] $ length <$> group (sort $ length <$> split xs)

  
  timeIt $ putStrLn $ "Day10: part1: " ++ show (countGaps xs)
  timeIt $ putStrLn $ "Day10: part2: splitting: " ++ show (product $ countPerms <$> split xs)
  timeIt $ putStrLn $ "Day10: part2: memoisation: " ++ show (countPerms2 xs)
  timeIt $ putStrLn $ "Day10: part2: maths: " ++ show p2
