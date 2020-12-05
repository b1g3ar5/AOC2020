module Day3s where


import Utils (getLines, Coord)
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V ((!))
import Control.Comonad.Store ( store, ComonadStore(peek), Store )


-- A version using Store comonad - not really using it's best bits!

type Grid a = Store Coord a


-- The store knows nothing about it's bounds, so if the index is too big we need
-- these to do the wrapping
gWidth :: Int
gWidth = 31
gHeight :: Int
gHeight = 323


readGrid :: [String] -> Grid Bool
readGrid ls = store (v !) (0, 0)
  where
    v :: Vector (Vector Bool)
    v = fromList ( fromList . go <$> ls)
    go :: String -> [Bool]
    go s = (=='#') <$> s


-- We need this lookup function to tell the store how to look given a coordinate
(!) :: Vector (Vector a) -> Coord -> a
v ! (x, y) = (v V.! y) V.! x


countTrees :: Grid Bool -> (Int, Int) -> Int
countTrees g (ix, iy) = go 0 (0,0)
  where
    go :: Int -> (Int, Int) -> Int
    go n p@(x, y)
      | y >= gHeight = n
      -- if peek is True we add one...
      -- notice the bounds are used here
      | peek p g = go (n+1) ((x+ix) `mod` gWidth, y+iy)
      | otherwise = go n ((x+ix) `mod` gWidth, y+iy)


day3s :: IO ()
day3s = do
  ls <- getLines 3
  let g = readGrid ls
      numTrees1 :: Int
      numTrees1 = countTrees g (3, 1)
  putStrLn $ "Day3: part1: " ++ show numTrees1

  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      numTrees2 = product $ countTrees g <$> slopes
  putStrLn $ "Day3: part2: " ++ show numTrees2
