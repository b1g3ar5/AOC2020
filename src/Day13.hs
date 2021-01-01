module Day13 where


import Utils ( getLines, splitOn )
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
      buses = sortOn (Down . fst) $ (\(x, y) -> (y, (y-x) `mod` y)) . second read <$> filter (\(_,y)-> y /= "x") (zip [0..] $ splitOn "," (ls!!1))

  putStrLn $ "Day13: part1: " ++ show (uncurry (*) $ head $ (\x -> (x * (1 + target `div` x) - target,x)) . fst <$> buses) 
  putStrLn $ "Day13: part2: Seive: " ++ show (fst $ seive (1,1) buses) 
  putStrLn $ "Day13: part2: Euclid: " ++ show (snd $ euclid buses) 


-- Maths...
euclid :: [(Integer, Integer)] -> (Integer, Integer)
euclid [] = error "No input"
euclid [x] = x
euclid (t1:t2:ts) = euclid $ rem2 t1 t2:ts


-- | Euclids algorithm:
-- Given an x find and a st a*w=1 (ie. a modular inverse)
-- GCD2 is:
-- Given x and y find s and t st s*x+t*y = 1
-- a sort of inverse for 2 numbers
gcd2 :: (Integer, Integer) -> (Integer, Integer)
gcd2 (_, 0) = (1, 0)
gcd2 (a, b) = (t, s - q * t)
  where 
    (q, r) = quotRem a b
    (s, t) = gcd2 (b, r)    


-- | Given:
-- z `mod` x1 = a1
-- z `mod` x2 = a2
-- Work out a12 st
-- x `mod` x1*x2 = a12

-- We find s, t st. s*x1+t*x2=1 (that's what gcd2 does)
-- We claim that x = a1*t*x2 + a2*s*x1 works
-- Mod both sides with x1
-- x `mod` x1 = a1*(t*x2 `mod` x1) = a1*1,  as required
-- Mod both sides with x2
-- x `mod` x2 = a2*(s*x1 `mod` x2) = a2*1, as required
rem2 :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
rem2 (n1, a1) (n2, a2) = (n1*n2, (a1*m2*n2+a2*m1*n1) `mod` (n1*n2))
  where
    (m1,m2) = gcd2 (n1, n2)
