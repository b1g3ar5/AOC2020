module Day13 where


import Utils ( getLines, splitOnStr )
import Data.List ( sortOn )
import Data.Bifunctor ( Bifunctor(second) )
import Control.Monad ( guard )
import Data.Ord ( Down(Down) )


checkNext :: [Int] -> (Int, Int) -> [Int]
checkNext ys (y,0) = filter (\x -> x `mod` y == 0) ys
checkNext ys (y,r) = do
  x <- ys
  guard $ x `mod` y == (y-r)
  return x


loop :: (Int,Int) -> [(Int, Int)] -> (Int, Int)
loop xs [] = xs
loop (start, inc) ((x,r):ts) = loop (head nxt, nxt!!1- head nxt) ts
  where
    nxt = checkNext [start, start+inc..] (x,r)


day13 :: IO ()
day13 = do
  ls <- getLines 13
  --let ls = test
  let target :: Int
      target = read $ head ls
      buses :: [(Int, Int)]
      buses = sortOn (Down . fst) $ (\(x, y) -> (y, x `mod` y)) . second read <$> filter (\(_,y)-> y /= "x") (zip [0..] $ splitOnStr "," (ls!!1))

  putStrLn $ "day12: part1: " ++ show (uncurry (*) $ head $ (\x -> (x * (1 + target `div` x) - target,x)) . fst <$> buses) 
  putStrLn $ "day12: part2: " ++ show (fst $ loop (1,1) buses) 
