module Day3m where


import Utils (getLines, Coord)
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V ((!))
import Control.Monad.Reader (asks, runReader, Reader)


-- A version, just for practise, using the reader monad for the environment
-- We don't need to change it so we don't need state

data Env = Env { bounds :: Coord, ground ::Vector (Vector Bool)} deriving (Show)


readGrid :: [String] -> Env
readGrid ls = Env (length $ v V.! 0, length v) v
  where
    v = fromList ( fromList . go <$> ls)
    go :: String -> [Bool]
    go s = (=='#') <$> s


(!) :: Vector (Vector a) -> Coord -> a
v ! (x, y) = (v V.! y) V.! x


countTrees :: Coord -> Reader Env Int
countTrees (ix,iy) = do
  g <- asks ground
  (width, height) <- asks bounds
  let go :: Int -> (Int, Int) -> Int
      go n (x, y)
        | y>=height = n
        | g ! (x,y) = go (n+1) ((x+ix) `mod` width, y+iy)
        | otherwise = go n ((x+ix) `mod` width, y+iy)
  return $ go 0 (0,0)


day3m :: IO ()
day3m = do
  ls <- getLines 3
  let env = readGrid ls
      numTrees1 :: Int
      numTrees1 = runReader (countTrees (3, 1)) env
  putStrLn $ "Day3: part1: " ++ show numTrees1

  let slopes :: [Coord]
      slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      numTrees2 = product $ ($ env) . runReader . countTrees <$> slopes 
  putStrLn $ "Day3: part2: " ++ show numTrees2
