module Day3m where


import Utils (getLines)
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V ((!))
import Control.Monad.Reader (asks, runReader, Reader)


-- A version using the reader monad for the environment

type Coord = (Int, Int)


data Env = Env { bnds :: Coord
                , ground ::Vector (Vector Bool)
               } deriving (Show)


readGrid :: [String] -> Env
readGrid ls = Env (width, height) v
  where
    v = fromList ( fromList . go <$> ls)
    height = length v
    width = length $ v V.! 0
    go :: String -> [Bool]
    go s = (=='#') <$> s


(!) :: Vector (Vector a) -> Coord -> a
v ! (x, y) = (v V.! y) V.! x


countTrees :: (Int, Int) -> Reader Env Int
countTrees (ix,iy) = do
  g <- asks ground
  (width, height) <- asks bnds
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
