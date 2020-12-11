{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Day11s where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Comonad.Representable.Store (Store(..), store, experiment, peek, pos)
import Control.Comonad ( Comonad(extract, extend) )
import Data.Distributive (Distributive(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Maybe ()
import System.TimeIt ( timeIt )
import Data.Function (fix)


-- The global grid size - so that I can write tabluate for the representative instance
gRows :: Int
gRows = 90
gCols :: Int
gCols = 95
inBounds :: Coord -> Bool
inBounds (x,y) = x >= 0 && y >= 0 && x < gCols && y < gRows
allCoords :: [[(Int, Int)]]
allCoords = (\c -> (c,) <$> [0..(gRows-1)]) <$> [0..(gCols-1)]


type Grid a = Store (Compose HV VV) a


instance Eq a => Eq (Grid a) where
  g == h = and $ (\c -> peek c g == peek c h) <$> concat allCoords


showGrid :: Grid Char -> String
showGrid g = intercalate "\n" $ (\r -> (\c -> (c,r) `peek`g) <$> [0..(gCols-1)]) <$> [0..(gRows-1)]


-- I need these for the recangular fixed grid
newtype HV a = HV (Vector a) deriving (Eq, Show, Functor)
newtype VV a = VV (Vector a) deriving (Eq, Show, Functor)


hat :: HV a -> Int -> a
(HV v) `hat` ix = v V.! ix


vat :: VV a -> Int -> a
(VV v) `vat` ix = v V.! ix


vgenerate :: (Int -> a) -> VV a
vgenerate f = VV $ V.generate gRows f

hgenerate :: (Int -> a) -> HV a
hgenerate f = HV $ V.generate gCols f


mkGrid :: M.Map Coord Char -> Grid Char
mkGrid mp = store (mp `lu`) (0, 0)


lu :: M.Map Coord Char -> Coord -> Char
mp `lu` c@(x, y) = mp M.! c


type Rule a = Grid a -> a


instance Distributive HV where
  distribute = distributeRep


instance Representable HV where
  type Rep HV = Int
  index v i = v `hat` i
  tabulate = hgenerate


instance Distributive VV where
  distribute = distributeRep


instance Representable VV where
  type Rep VV = Int
  index v i = v `vat` i
  tabulate = vgenerate


count :: (a -> Bool) -> Grid a -> Int
count p g = length $ filter p $ (`peek` g) <$> concat allCoords


step :: Rule a -> Grid a -> Grid a
step = extend


rule1 :: Rule Char
rule1 g
  | cell == '.'                                 = '.'
  | (cell == 'L') && numNeighboursOccupied == 0 = '#'
  | (cell == '#') && numNeighboursOccupied >= 4 = 'L'
  | otherwise                                   = cell
  where
    cell = extract g
    neighbours = experiment (safeAt neighbourCoords) g
    numNeighboursOccupied = length (filter (== '#') neighbours)


rule2 :: Rule Char
rule2 g
  | cell == '.' = '.'
  | (cell == 'L') && numNeighboursOccupied == 0 = '#'
  | (cell == '#') && numNeighboursOccupied >= 5 = 'L'
  | otherwise = cell
  where
    cell = extract g

    numNeighboursOccupied = length $ filter id seatOccupied

    seatOccupied :: [Bool]
    seatOccupied = (\cs -> or ((=='#') <$> cs)) . find (/= '.') . ((`peek` g) <$> ) <$> filter (/=[]) (ray (pos g) <$> directions)


safeAt :: [Coord] -> Coord -> [Coord]
coords `safeAt` origin = filter inBounds $ map (addCoords origin) coords


stepUntil :: Eq a => Rule a -> Grid a -> Grid a
stepUntil r = go
  where
    go g
      | nxt == g = g
      | otherwise = go nxt
      where
        nxt = step r g


day11s :: IO ()
day11s = do
  ls <- getLines 11
  let g0 = readSeats ls
  timeIt $ putStrLn $ "Day11s: Part1: " ++ show (count (== '#') $ stepUntil rule1 g0)
  timeIt $ putStrLn $ "Day11s: Part1: " ++ show (count (== '#') $ stepUntil rule2 g0)


readSeats :: [String] -> Grid Char
readSeats css =  mkGrid $ M.fromList g
  where
    g :: [(Coord, Char)]
    g = concat $ (\(row, rs) -> (\(col, c) -> ((col,row),c)) <$> zip xs rs) <$> zip xs css

    xs :: [Int]
    xs = [0..]

test = [
    "L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"]    