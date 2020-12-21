{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# Language BlockArguments, QuasiQuotes #-}

module Day20 where


import Prelude hiding (flip)
import Utils ( getParagraphs, Coord, neighbours, above, below, leftOf, rightOf, clockTurn, antiTurn )
import Data.List
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Maybe
import Data.Bifunctor
import Data.Foldable (for_)
import Control.Monad (guard)
import qualified Data.Set as S


-- For part one I thought (overnight) that you could just count the 
-- occurances of each edge, then 4 tiles will have 2 not recurring edges
-- these are the corners. It might help to turn all the edges into a
-- Int for the orientation with most initial .s


-- Globals!
gridSize :: Int
gridSize = 12
coordList :: [Coord]
coordList = [(x,y) | x <- [0..(gridSize-1)], y <- [0..(gridSize-1)]]


-- A Tile is a list of strings - where there are '#'s
type Tile = [Coord]


parseTile :: [String] -> (Int, Tile)
parseTile (n:css) = (read $ take 4 $ drop 5 n, cds)
  where
    cds :: [Coord]
    cds = fst <$> filter snd (concat $ (\(y, cs) -> (\(x, c) -> ((x, y), c=='#')) <$> zip [0..] cs)<$> zip [0..] css)


showTile :: Tile -> String
showTile cs = unlines $ (\y -> (\x -> if (x,y) `elem` cs then '#' else '.') <$> [0..mx]) <$> [0..my]
  where
    mx = maximum $ fst <$> cs
    my = maximum $ snd <$> cs



-- | rotate n quarter turns clockwise - and translate back to the same square
rotate :: Tile -> Int -> Tile
rotate cs 0 = cs
rotate cs 1 = (+ (9, 0)) . clockTurn <$> cs
rotate cs 2 = (+ (9, 9)) . clockTurn . clockTurn <$> cs
rotate cs 3 = (+ (0, 9)) . antiTurn <$> cs
  where
    mx = maximum $ fst <$> cs
    my = maximum $ snd <$> cs


-- flip tile over and again move back to [0,(n-1)]*[0,(n-1)]
flip :: Tile -> Tile
flip cs = (+ (9, 0)) . (\(x,y) -> (-x,y)) <$> cs


-- All possible orientations of a tile - 8 of them
orientations :: Tile -> [Tile]
orientations t = ts ++ fts
  where
    ft = flip t
    ts = rotate t <$> [0..3]
    fts = rotate ft <$> [0..3]


-- | An Arrangement is a set of tiles placed at a coordinate
type Arrangement = M.Map Coord (Int, Tile)


pick :: [a] -> [(a, [a])]
pick xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]


build :: Arrangement -> [(Int, Tile)] -> [Coord] -> [Arrangement]
build arr remainingTiles [] = [arr]
build arr remainingTiles (nextPosition:remainingPositions) = do
  ((tix, baseTile), rem) <- pick remainingTiles

  tile <- orientations baseTile

  -- This tile must match existing tiles
  -- We are tiliig from the top left, so they will be above or left

  for_ (M.lookup (above nextPosition) arr) 
    (\(_,l) -> guard (sort [x | (x, 0) <- tile] == sort [x | (x, 9) <- l]))

     -- match the tile to the left if there is one
  for_ (M.lookup (leftOf nextPosition) arr) 
    (\(_,l) -> guard (sort [y | (0, y) <- tile] == sort [y | (9, y) <- l]))

  build (M.insert nextPosition (tix, tile) arr) rem remainingPositions

  
makeImage :: Arrangement -> [Coord]
makeImage arr = concat $ (\((cx, cy), (_, tcs)) -> bimap ((cx * 8) +) ((cy * 8) +) <$> removeEdges tcs) <$> cs 
  where
    cs = M.toList arr
    removeEdges :: Tile -> Tile
    removeEdges cs = (\c -> c - (1,1)) <$> filter (\(x,y) -> x/=0 && x/=9 && y/=0 && y/=9 ) cs


snake :: Tile
snake = snd $ parseTile img
  where
    img = ["Snak 2001:", "                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]


rotateSnake :: Tile -> Int -> Tile
rotateSnake cs n
  | n==0 = cs
  | n==1 = (+ (my, 0)) . clockTurn <$> cs
  | n==2 = (+ (mx, my)) . clockTurn . clockTurn <$> cs
  | n==3 = (+ (0, mx)) . antiTurn <$> cs
  where
    mx = maximum $ fst <$> cs
    my = maximum $ snd <$> cs

flipSnake :: Tile -> Tile
flipSnake cs = (+ (mx, 0)) . (\(x,y) -> (-x,y)) <$> cs
  where
    mx = maximum $ fst <$> cs


orientationsSnake :: Tile -> [Tile]
orientationsSnake t = ts ++ fts
  where
    ft = flipSnake t
    ts = rotateSnake t <$> [0..3]
    fts = rotateSnake ft <$> [0..3]


day20 :: IO ()
day20 = do
  gs <- getParagraphs 20
  let tiles = parseTile <$> tail gs

      arr :: Arrangement
      arr = head $ build M.empty tiles coordList

      arrayIx = product [fst $ arr M.! (x, y) | x<-[0, gridSize-1], y<-[0, gridSize-1]]

  putStrLn $ "Day20: part1: " ++ show arrayIx
  
  let img :: Tile
      img = makeImage arr
      simg :: S.Set Coord
      simg = S.fromList img
      -- To get all snakes add all possible coordinate to all possible snake orientations
      allSnakes = [(\s -> s + (x,y)) <$>  s | s <- orientationsSnake snake, x <- [0..(8*gridSize)], y <- [0..(8*gridSize)]]
      reducedImage = foldl' remove simg allSnakes

  putStrLn $ "Day20: part2: " ++ show (S.size reducedImage)


  return ()


remove :: S.Set Coord-> Tile -> S.Set Coord
remove s t
  | st `S.isSubsetOf` s =  s S.\\ st
  | otherwise  = s
  where
    st = S.fromList t


test = [
    ".####...#####..#...###.."
  , "#####..#..#.#.####..#.#."
  , ".#.#...#.###...#.##.##.."
  , "#.#.##.##.#.##.##.#####"
  , "..##.###.####..#.####.##"
  , "...#.#..##.##...#..#..##"
  , "#.##.#..#.#..#..##.#.#.."
  , ".###.##.....#...###.#..."
  , "#.####.#.#....##.#..#.#."
  , "##...#..#....#..#...####"
  , "..#.##...###..#.#####..#"
  , "....#.##.#.#####....#..."
  , "..##.##.###.....#.##..#."
  , "#...#...###..####....##."
  , ".#.##...#.##.#.#.###...#"
  , "#.###.#..####...##..#..."
  , "#.###...#.##...#.######."
  , ".###.###.#######..#####."
  , "..##.#..#..#.#######.###"
  , "#.#..##.########..#..##."
  , "#.#####..#.#...##..#...."
  , "#....##..#.#########..##"
  , "#...#.....#..##...###.##"
  , "#..###....##.#...##.##.#"
  ]    