module Day15 where


import qualified Data.Map as M
import System.TimeIt


type Key = Int
type Position = Int
type Loops = Int
data MapWithLast = ML Loops (Key, Position) (M.Map Key Position)


makeMap :: Int -> [Int] -> MapWithLast
makeMap n seeds = ML (n-length seeds) (0, length seeds) (M.fromList $ zip seeds [0..])


nxt :: MapWithLast -> (Key, Position)
nxt (ML _ (key, pos) mp) = case mp M.!? key of
                             Nothing -> (0, pos+1)
                             Just ix -> (pos-ix, pos+1)


get :: MapWithLast ->  (Key, Position)
get ml@(ML n (key, pos) mp)
  | 0 == (n-1) = (key, pos+1)
  | otherwise = get (ML (n-1) (nxt ml) $ M.insert key pos mp)


day15 :: IO ()
day15 = do
  let ns :: [Key]
      ns = [0,1,4,13,15,12,16]

  timeIt $ putStrLn $ "Day15: " ++ show (get $ makeMap 2020 ns)
  timeIt $ putStrLn $ "Day15: " ++ show (get $ makeMap 30000000 ns)

