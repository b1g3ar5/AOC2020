module Day15 where


import qualified Data.IntMap.Strict as M
import System.TimeIt ( timeIt )


-- Neat code but it takes 180 seconds to do part 2 - too slow!
-- See day15v

type Key = Int
type Position = Int
type Loops = Int
-- A map with a counter (so it knows whether it's full)
-- and the last item not yet inserted
data MapWithLast = ML Loops (Key, Position) (M.IntMap Position) deriving (Show)

makeMap :: Int -> [Int] -> MapWithLast
makeMap n seeds = ML (n-length seeds) (0, length seeds) (M.fromList $ zip seeds [0..])


-- The rule for the next item
nxt :: MapWithLast -> (Key, Position)
nxt (ML _ (key, pos) mp) = case mp M.!? key of
                             Nothing -> (0, pos+1)
                             Just ix -> (pos-ix, pos+1)


-- See if we've finished or loop
--get :: MapWithLast ->  (Key, Position)
get ml@(ML n (key, pos) mp)
  | 1 == n = (key, pos+1)
  | otherwise = get (ML (n-1) (nxt ml) $ M.insert key pos mp)


day15 :: IO ()
day15 = do
  let ns :: [Key]
      ns = [0,1,4,13,15,12,16]

  timeIt $ putStrLn $ "Day15: part1: " ++ show (get $ makeMap 2020 ns)
  --timeIt $ putStrLn $ "Day15: " ++ show (get $ makeMap 30000000 ns)

