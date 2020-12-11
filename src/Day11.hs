{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Day11 where


import Utils ( getLines, Coord, neighbourCoords, at, directions, Direction(..), findTarget, ray )
import Data.List ( find, intercalate )
import Data.Vector (Vector, fromList)
import qualified Data.Vector as V
import Control.Comonad ( Comonad(..) )
import Data.Maybe (catMaybes)
import System.TimeIt ( timeIt )
import Debug.Trace


-- The global grid size - so that I can write tabluate for the representative instance
gRows :: Int
gRows = 90
gCols :: Int
gCols = 95

inBounds :: Coord -> Bool
inBounds (x,y) = x >= 0 && y >= 0 && x < gCols && y < gRows


data Grid a = Grid { focus :: Coord
                   , getVector :: Vector (Vector a)
                   } deriving (Eq, Functor)


instance (Show a) => Show (Grid a) where
  show (Grid f xss) = intercalate "\n" $ (\xs -> concat $ show <$> xs) <$> V.toList (V.toList <$> xss)


instance Foldable Grid where
  foldMap f (Grid _ xss) = foldMap (foldMap f) xss


(!) :: Grid a -> Coord -> a
(!) g (x,y) 
  | x<0 = error $ "x<0 in grid !, (x,y): " ++ show (x,y)
  | y<0 = error $ "y<0 in grid !, (x,y): " ++ show (x,y)
  | x>=gCols = error $ "x>gCols im grid !, (x,y): " ++ show (x,y)
  | y>=gRows = error $ "y>gRows in grid !, (x,y): " ++ show (x,y)
  | otherwise = (getVector g V.! y) V.! x


(!?) :: Grid a -> Coord -> Maybe a
g !? (x,y) 
  | x<0 = Nothing
  | y<0 = Nothing
  | x>=gCols = Nothing
  | y>=gRows = Nothing
  | otherwise = Just $ (getVector g V.! y) V.! x


count :: (a -> Bool) -> Grid a -> Int
count p g = sum $ (\c -> if p c then 1 else 0) <$> g


instance Comonad Grid where
  extract g = g ! focus g
  duplicate (Grid f xss) = Grid f $ V.fromList $ (\row -> V.fromList $ (\col -> Grid (col, row) xss) <$> [0..(gCols-1)]) <$> [0..(gRows-1)]


step :: Rule a -> Grid a -> Grid a
step = extend


type Rule a = Grid a -> a


rule1 :: Rule Char
rule1 g
  | cell == '.' = '.'
  | (cell == 'L') && numNeighboursOccupied == 0 = '#'
  | (cell == '#') && numNeighboursOccupied >= 4 = 'L'
  | otherwise = cell
  where
    cell = extract g
    neighbours = filter inBounds $ neighbourCoords `at` focus g
    numNeighboursOccupied = length $ filter (\c -> g ! c == '#') neighbours 


rule2 :: Rule Char
rule2 g
  | cell == '.' = '.'
  | (cell == 'L') && seatsOccupied == 0 = '#'
  | (cell == '#') && seatsOccupied >= 5 = 'L'
  | otherwise = cell
  where
    cell = extract g

    seatsOccupied :: Int
    --seatsOccupied = trace ("x: " ++ show xx) $ length $ filter (=='#') $ (g !) <$> xx
    seatsOccupied = length $ filter (=='#') $ (g !) <$> xx

    xx = catMaybes (findTarget (\c -> inBounds c && g ! c == '#') (focus g) <$> directions)


stepUntil :: Eq a => Rule a -> Grid a -> Grid a
stepUntil r = go
  where
    go g
      | nxt == g = g
      | otherwise = go nxt
      where
        nxt = step r g


day11 :: IO ()
day11 = do
  ls <- getLines 11
  let g0 = readSeats ls

  timeIt $ putStrLn $ "Day11: Part1: " ++ show (count (== '#') $ stepUntil rule1 g0)
  --timeIt $ putStrLn $ "Day11: Part2: " ++ show (count (== '#') $ stepUntil rule2 g0)

  let cell = extract g0
      seatsOccupied :: Int
      seatsOccupied = length $ filter (=='#') $ (g0 !) <$> xx
      xx = catMaybes (findTarget (\c -> g0 ! c == '#') (focus g0) <$> directions)
      yy = findTarget (\c -> inBounds c && g0 ! c == '#') (focus g0) <$> directions
      zz = filter (\c -> inBounds c && g0 ! c == '#') $ ray (0, 0) N
      ww = ray (0, 0) N

  putStrLn $ "Day11: Part2: " ++ show (yy)


readSeats :: [String] -> Grid Char
readSeats ls = Grid (0, 0) $ fromList $ fromList <$> ls
