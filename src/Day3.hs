module Day3 where


import Utils (getLines, Coord)
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V ((!))


-- This is the width height and the ground data
type Env = (Coord, Vector (Vector Bool))


-- This doesn't need to check bounds because the go function
-- checks the hieght and uses `mod` on the width
(!) :: Vector (Vector a) -> Coord -> a
v ! (x, y) = (v V.! y) V.! x


readGrid :: [String] -> Env
readGrid ls = ((length $ v V.! 0, length v), v)
  where
    v = fromList ( fromList . go <$> ls)
    go :: String -> [Bool]
    go s = (=='#') <$> s


countTrees :: Env -> Coord -> Int
countTrees ((width, height), g) (ix,iy) = go 0 (0,0)
  where
    go n (x, y)
      | y >= height = n
      | g ! (x,y) = go (n+1) ((x+ix) `mod` width, y+iy)
      | otherwise = go n ((x+ix) `mod` width, y+iy)


day3:: IO ()
day3 = do
  ls <- getLines 3
  let g = readGrid ls
      numTrees1 = countTrees g (3,1)
  putStrLn $ "Day3: part1: " ++ show numTrees1

  let slopes :: [Coord]
      slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      numTrees2 = product $ countTrees g <$> slopes
  putStrLn $ "Day3: part2: " ++ show numTrees2
