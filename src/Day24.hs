{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}

module Day24 where


import Utils hiding (neighbouts, neighbourCoords)

import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store (Store(..), StoreT(..), store, experiment, peek)
import Data.Distributive (Distributive(..))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Functor.Identity (Identity(..))
import Control.Monad (replicateM)
import Data.List (delete, group, sort, nub)
import Data.Bool (bool)
import qualified Data.Set as S
import Data.Maybe
import qualified TotalMap as M



data Dir = E | SE | SW | W | NW | NE deriving (Eq, Show)

readDir :: String -> Dir
readDir s
  | s == "e" = E
  | s == "se" = SE
  | s == "sw" = SW
  | s == "w" = W
  | s == "nw" = NW
  | s == "ne" = NE


toCoord :: Dir -> Coord
toCoord E = (2, 0)
toCoord W = (-2, 0)
toCoord NE = (1, -1)
toCoord SE = (1, 1)
toCoord NW = (-1, -1)
toCoord SW = (-1, 1)

parseLine :: String -> [Dir]
parseLine [] = []
parseLine [c]
  | c == 'e' = [E] 
  | c == 'w' = [W] 
parseLine (c:d:cs)
  | c == 'e' = E : parseLine (d:cs)
  | c == 'w' = W : parseLine (d:cs)
  | [c,d] == "ne" = NE : parseLine cs
  | [c,d] == "nw" = NW : parseLine cs
  | [c,d] == "se" = SE : parseLine cs
  | [c,d] == "sw" = SW : parseLine cs


allcoords = [(x, y) | x<-[-200..200], y<-[-100..100], even (x+y)]



day24 :: IO ()
day24 = do
  ls <- getLines 24
  --let ls = test
  let ds = parseLine <$> ls
  let p1 = length $ filter (/=2) $ length <$> group (sort $ sum . (toCoord <$>) <$> ds)
  putStrLn $ "Day24: part1: " ++ show p1

  let black :: [Coord]
      black = (head <$>) <$> filter (\xs -> length xs /= 2) $ group (sort $ sum . (toCoord <$>) <$> ds)
      g :: Grid
      --g = M.fromPartial False $ M.fromList $ (, True) <$> black
      g = M.fromPartial False $ M.fromList $ (\c -> (c,c `elem` black)) <$> allcoords
      g1 = apply basicRule g
      g2 = apply basicRule g1
      g3 = apply basicRule g2
      g4 = apply basicRule g3
      g100 = (iterate (apply basicRule) g) !! 100
  --putStrLn $ "Day24: part2: " ++ show black
  --putStrLn $ "Day24: part2: " ++ show mp
  --putStrLn $ "Day24: part2: " ++ showGrid g
  --putStrLn $ "Day24: part2: " ++ showGrid g
  --putStrLn $ "Day24: part2: " ++ showGrid g1
  putStrLn $ "Day24: part2: " ++ show (count $ (iterate (apply basicRule) g) !! 100)



  return ()


neighbourCoords :: [Coord]
neighbourCoords = [(2, 0), (-2, 0), (1, -1), (1, 1), (-1, -1), (-1, 1)]



apply :: Rule -> Grid -> Grid
apply r g = M.mapWithKey (\k h -> r g k) g


type Grid = M.TMap Coord Bool


type Rule = Grid -> Coord ->  Bool


basicRule :: Rule
basicRule g hex = (not alive && neighboursAlive == 2) || (alive && (neighboursAlive == 1 || neighboursAlive == 2))
  where
    alive = g M.! hex
    neighbours = (+ hex) <$> neighbourCoords
    neighboursAlive = length $ filter id $ (g M.!) <$> neighbours

{-
Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
-}


count :: Grid -> Int
count tmp = foldMap (bool 0 1) mp 
  where
    mp :: M.Map Coord Bool
    mp = M._map tmp

showGrid :: Grid -> String
showGrid tmp = show $ M.filter id mp --foldMap (maybe 0 (bool 0 1)) mp 
  where
    mp :: M.Map Coord Bool
    mp = M._map tmp


instance Semigroup Int where
  x <> y = x + y


instance Monoid Int where
  mempty = 0


test = ["sesenwnenenewseeswwswswwnenewsewsw"
  , "neeenesenwnwwswnenewnwwsewnenwseswesw"
  , "seswneswswsenwwnwse"
  , "nwnwneseeswswnenewneswwnewseswneseene"
  , "swweswneswnenwsewnwneneseenw"
  , "eesenwseswswnenwswnwnwsewwnwsene"
  , "sewnenenenesenwsewnenwwwse"
  , "wenwwweseeeweswwwnwwe"
  , "wsweesenenewnwwnwsenewsenwwsesesenwne"
  , "neeswseenwwswnwswswnw"
  , "nenwswwsewswnenenewsenwsenwnesesenew"
  , "enewnwewneswsewnwswenweswnenwsenwsw"
  , "sweneswneswneneenwnewenewwneswswnese"
  , "swwesenesewenwneswnwwneseswwne"
  , "enesenwswwswneneswsenwnewswseenwsese"
  , "wnwnesenesenenwwnenwsewesewsesesew"
  , "nenewswnwewswnenesenwnesewesw"
  , "eneswnwswnwsenenwnwnwwseeswneewsenese"
  , "neswnwewnwnwseenwseesewsenwsweewe"
  , "wseweeenwnesenwwwswnew"]