module Day12 where


import Utils ( getLines, Coord )
import Data.List ( foldl' )

data Direction = N | S | E | W | F | L | R deriving (Eq, Show)

type Inst = (Direction, Int)

type Orientation = Int

parse :: String -> Inst
parse s 
  | head s == 'N' = (N, read $ tail s)
  | head s == 'S' = (S, read $ tail s)
  | head s == 'E' = (E, read $ tail s)
  | head s == 'W' = (W, read $ tail s)
  | head s == 'F' = (F, read $ tail s)
  | head s == 'L' = (L, read $ tail s)
  | head s == 'R' = (R, read $ tail s)


type State1 = (Coord, Orientation)
type State2 = (Coord, Coord)

apply1 :: State1 -> Inst -> State1
apply1 ((x, y), orientation) (d,n)
  | d == N = ((x, y-n), orientation)
  | d == S = ((x, y+n), orientation)
  | d == E = ((x+n, y), orientation)
  | d == W = ((x-n, y), orientation)
  | d == F = case orientation of
                0   -> ((x, y-n), orientation)
                90  -> ((x+n, y), orientation)
                180 -> ((x, y+n), orientation)
                270 -> ((x-n, y), orientation)
                _ -> error "Orientation gone wrong"
  | d == L = ((x, y), (orientation - n) `mod` 360)
  | d == R = ((x, y), (orientation + n) `mod` 360)


apply2 :: State2 -> Inst -> State2
apply2 ((sx, sy), (wx, wy) ) (d,n)
  | d == N = ((sx, sy), (wx, wy-n))
  | d == S = ((sx, sy), (wx, wy+n))
  | d == E = ((sx, sy), (wx+n, wy))
  | d == W = ((sx, sy), (wx-n, wy))
  | d == F = ((sx + n * wx, sy + n * wy), (wx, wy))
  | d == L = onLeft n
  | d == R = onLeft $ (-n) `mod` 360
  where
    onLeft n = case n of 
               0 -> ((sx, sy), (wx, wy))
               90 -> ((sx, sy), (wy, -wx))
               180 -> ((sx, sy), (-wx, -wy))
               270 -> ((sx, sy), (-wy, wx))
               _  -> error $ "I didn't code L: " ++ show n

day12 :: IO ()
day12 = do
  ls <- getLines 12
  --let ls = test
  let instructions :: [Inst]
      instructions = parse <$> ls
      start1 = ((0, 0), 90)
      start2 = ((0, 0), (10, -1))
      ((x,y), _) = foldl' apply1 start1 instructions
      ((sx,sy), _) = foldl' apply2 start2 instructions
  putStrLn $ "Day12: part1: " ++ show (abs x + abs y)
  putStrLn $ "Day12: part2: " ++ show (abs sx + abs sy)

