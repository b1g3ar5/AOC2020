{-# LANGUAGE TypeFamilies #-}

module Day17 where

import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, peek)
import Data.Distributive (Distributive(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Data.Vector (Vector, (!), generate, fromList)
import Control.Monad (replicateM)
import Data.List (delete)
import Data.Bool (bool)
import System.TimeIt ( timeIt )


-- | First implementation with the Store comonad as per Penner
-- It's a bit slow 5s for part 2


test :: [String]
test = [".#.", "..#", "###"]

actual :: [String]
actual = ["######.#", "##.###.#", "#.###.##", "..#..###", "##.#.#.#", "##...##.", "#.#.##.#", ".###.###"]


day17 :: IO ()
day17 = do 
  let ls = actual
  let cs3 :: [Coord3]
      cs3 = fst <$> filter (\(pos, x) -> x=='#') (concat $ (\(y, cs) -> (\(x, c) -> ((x,(y,0)), c)) <$> zip [0..] cs) <$> zip [0..] ls)
      cs4 :: [Coord4]
      cs4 = fst <$> filter (\(pos, x) -> x=='#') (concat $ (\(y, cs) -> (\(x, c) -> ((x,(y,(0,0))), c)) <$> zip [0..] cs) <$> zip [0..] ls)
      g3 = mkGrid3 cs3
      g4 = mkGrid4 cs4

      g3_6 = iterate (step3 basicRule3) g3 !! 6
      g4_6 = iterate (step4 basicRule4) g4 !! 6
  
  timeIt $ putStrLn $ "part1: " ++ show (count3 g3_6)
  timeIt $ putStrLn $ "part2: " ++ show (count4 g4_6)


type Coord3 = (Int, (Int, Int))
type Coord4 = (Int, (Int, (Int, Int)))


type Grid3 a = Store (Compose Vector (Compose Vector Vector)) a
type Grid4 a = Store (Compose Vector (Compose Vector (Compose Vector Vector))) a


gridSize :: Int
gridSize = 20


mkGrid3 :: [Coord3] -> Grid3 Bool
mkGrid3 xs = store (`elem` xs) (0, (0, 0))
mkGrid4 :: [Coord4] -> Grid4 Bool
mkGrid4 xs = store (`elem` xs) (0, (0, (0, 0)))


type Rule3 = Grid3 Bool -> Bool
type Rule4 = Grid4 Bool -> Bool


instance Distributive Vector where
  distribute = distributeRep


instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize


-- Stay alive if you've got 2 neighbours, come back to life if there are 3 neighbours
basicRule3 :: Rule3
basicRule3 g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at3 neighbourCoords3) g
    numNeighboursAlive = length (filter id neighbours)


basicRule4 :: Rule4
basicRule4 g = numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at4 neighbourCoords4) g
    numNeighboursAlive = length (filter id neighbours)


step3 :: Rule3 -> Grid3 Bool -> Grid3 Bool
step3 = extend
step4 :: Rule4 -> Grid4 Bool -> Grid4 Bool
step4 = extend


count3 :: Grid3 Bool -> Int
count3 (StoreT (Identity (Compose g)) _) = foldMap (foldMap (bool 0 1)) g
count4 :: Grid4 Bool -> Int
count4 (StoreT (Identity (Compose g)) _) = foldMap (foldMap (bool 0 1)) g


instance Semigroup Int where
  x <> y = x + y


instance Monoid Int where
  mempty = 0


toCoord3 :: [Int] -> Coord3
toCoord3 ns = (\[x,y,z] -> (x,(y,z))) $ take 3 ns
toCoord4 :: [Int] -> Coord4
toCoord4 ns = (\[w, x,y,z] -> (w, (x,(y,z)))) $ take 4 ns


neighbourCoords3 :: [Coord3]
neighbourCoords3 = toCoord3 <$> delete [0,0,0] (replicateM 3 [-1, 0, 1])
neighbourCoords4 :: [Coord4]
neighbourCoords4 = toCoord4 <$> delete [0,0,0,0] (replicateM 4 [-1, 0, 1])


addCoords3 :: Coord3 -> Coord3 -> Coord3
addCoords3 (x, (y, z)) (x', (y', z')) = (x + x', (y + y', z + z'))
addCoords4 :: Coord4 -> Coord4 -> Coord4
addCoords4 (w, (x, (y, z))) (w', (x', (y', z'))) = (w + w', (x + x', (y + y', z + z')))


at3 :: [Coord3] -> Coord3 -> [Coord3]
coords `at3` origin = map (addCoords3 origin) coords

at4 :: [Coord4] -> Coord4 -> [Coord4]
coords `at4` origin = map (addCoords4 origin) coords

