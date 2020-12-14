module Day13 where


import Utils ( getLines, splitOnStr )
import Data.List ( sortOn )
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad ( guard )
import Data.Ord ( Down(Down) )


seive :: (Integer,Integer) -> [(Integer, Integer)] -> (Integer, Integer)
seive xs [] = xs
seive (start, inc) ((y,r):ts) = seive (head go, inc * y) ts
  where
    go ::[Integer]
    go = do
      x <- [start, start+inc..] 
      guard $ x `mod` y == r 
      return x


day13 :: IO ()
day13 = do
  ls <- getLines 13
  let target :: Integer
      target = read $ head ls
      buses :: [(Integer, Integer)]
      buses = sortOn (Down . fst) $ (\(x, y) -> (y, (y-x) `mod` y)) . second read <$> filter (\(_,y)-> y /= "x") (zip [0..] $ splitOnStr "," (ls!!1))

  putStrLn $ "day12: part1: " ++ show (uncurry (*) $ head $ (\x -> (x * (1 + target `div` x) - target,x)) . fst <$> buses) 
  putStrLn $ "day12: part2: Seive: " ++ show (fst $ seive (1,1) buses) 
  putStrLn $ "day12: part2: Euclid: " ++ show (snd $ euclid buses) 


-- Maths...
euclid :: [(Integer, Integer)] -> (Integer, Integer)
euclid [] = error "No input"
euclid [x] = x
euclid (t1:t2:ts) = euclid $ rem2 t1 t2:ts


-- Reverse Euclidian algorithm?
gcd2 :: (Integer, Integer) -> (Integer, Integer)
gcd2 (_, 0) = (1, 0)
gcd2 (a, b) = (t, s - q * t)
  where 
    (q, r) = quotRem a b
    (s, t) = gcd2 (b, r)    


-- Given 2 numbers and their rems, work out the rem of the product of the numbers
-- We know that n1*m1+n2*m2=1 (that's what gcd2 does)
-- Look at x = a1*m2*n2+a2*n1*m1
-- x `mod` n1 = a1 * (n2*m2 `mod` n1) = a1 * 1 = a1
-- x `mod` n2 = a2 * (n1*m1 `mod` n2) = a2 * 1 = a2
-- as required so x is the number we want.
rem2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
rem2 (n1, a1) (n2, a2) = (n1*n2, (a1*m2*n2+a2*m1*n1) `mod` (n1*n2))
  where
    (m1,m2) = gcd2 (n1, n2)
