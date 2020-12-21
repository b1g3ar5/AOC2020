{-# LANGUAGE TupleSections #-}

module Day17m where


import Prelude hiding (elem)
import Utils (getLines)
import Data.List ( foldl', nub, delete )
import qualified Data.HashSet as S
import Data.Hashable ( Hashable )
import Control.Monad (replicateM)


-- | Reimplemented with a simple HashSet to make it quicker - still slow 2s for part 2  = 3-4s
-- The HashMap was 2-3s hardly worth it...

parse3 :: [String] -> [(Coord3, Char)]
parse3 ls = concat $ (\(y, cs) -> (\(x, c) -> ((x,y,0), c)) <$> zip [0..] cs) <$> zip [0..] ls


parse4 :: [String] -> [(Coord4, Char)]
parse4 ls = concat $ (\(y, cs) -> (\(x, c) -> ((x,y,0,0), c)) <$> zip [0..] cs) <$> zip [0..] ls


day17m :: IO ()
day17m = do
  ls <- getLines 17
  let g3 :: Grid Coord3
      g3 = S.fromList $ fst <$> filter (\(p, c) -> c == '#') (parse3 ls)
      
      gend3 = iterate (step (neighbourCoords3 `at3`) $ basicRule (neighbourCoords3 `at3`)) g3 !! 6
      
      g4 :: Grid Coord4
      g4 = S.fromList $ fst <$> filter (\(p, c) -> c == '#') (parse4 ls)
      
      gend4 = iterate (step (neighbourCoords4 `at4`) $ basicRule (neighbourCoords4 `at4`)) g4 !! 6

  putStrLn $ "Day17m: part1: " ++ show (S.size gend3)
  putStrLn $ "Day17m: part2: " ++ show (S.size gend4)
  

type Grid a = S.HashSet a


-- Stay alive if you've got 2 or 3 neighbours, come back to life if there are 3 neighbours
-- Tkaes a function to calculate neighbours, the grid and a position
basicRule :: (Eq i, Hashable i) => (i -> [i]) -> S.HashSet i -> i -> Bool
basicRule f g pos = neighboursAlive == 3 || (alive && (neighboursAlive == 2))
  where
    alive = pos `S.member` g
    neighbours = (`S.member` g) <$> f pos
    neighboursAlive = length (filter id neighbours)


allNeighbours :: (Eq i, Hashable i) => (i -> [i]) -> Grid i -> S.HashSet i
allNeighbours f g = S.fromList ng
    where
      ng = concatMap f g


step :: (Eq i,  Hashable i)  => (i -> [i]) -> (S.HashSet i -> i -> Bool) -> S.HashSet i -> Grid i
step f r g = S.filter (r g) $ allNeighbours f g


type Coord3 = (Int, Int, Int)
type Coord4 = (Int, Int, Int, Int)

toCoord3 :: [Int] -> Coord3
toCoord3 ns = (\[x,y,z] -> (x,y,z)) $ take 3 ns


toCoord4 :: [Int] -> Coord4
toCoord4 ns = (\[w, x,y,z] -> (w, x,y,z)) $ take 4 ns


neighbourCoords3 :: [Coord3]
neighbourCoords3 = toCoord3 <$> delete [0,0,0] (replicateM 3 [-1..1])


neighbourCoords4 :: [Coord4]
neighbourCoords4 = toCoord4 <$> delete [0,0,0,0] (replicateM 4 [-1..1])


addCoords3 :: Coord3 -> Coord3 -> Coord3
addCoords3 (x, y, z) (x', y', z') = (x + x', y + y', z + z')


addCoords4 :: Coord4 -> Coord4 -> Coord4
addCoords4 (w, x, y, z) (w', x', y', z') = (w + w', x + x', y + y', z + z')


at3 :: [Coord3] -> Coord3 -> [Coord3]
coords `at3` origin = map (addCoords3 origin) coords


at4 :: [Coord4] -> Coord4 -> [Coord4]
coords `at4` origin = map (addCoords4 origin) coords

