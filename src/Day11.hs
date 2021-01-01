module Day11 where


import Utils ( getLines, Coord(..), neighbourCoords, race, directions, fixpoint )
import Data.Map ( Map, (!?), fromList, notMember, mapWithKey, (!) )
import System.TimeIt ( timeIt )


-- | Straight forward version mapping the rules over the grid
-- The grid is represented with a map


type Grid a = Map Coord a


count :: (a -> Bool) -> Grid a -> Int
count p g = sum $ (\c -> if p c then 1 else 0) <$> g


rule1 :: Grid Char -> Coord -> Char
rule1 g c
  | cell == '.' = '.'
  | (cell == 'L') && numNeighboursOccupied == 0 = '#'
  | (cell == '#') && numNeighboursOccupied >= 4 = 'L'
  | otherwise = cell
  where
    cell = g ! c
    neighbours = (c+) <$> neighbourCoords
    numNeighboursOccupied = length $ filter (\c -> g !? c == Just '#') neighbours 


rule2 :: Grid Char -> Coord -> Char
rule2 g c
  | cell == '.' = '.'
  | (cell == 'L') && seatsOccupied == 0 = '#'
  | (cell == '#') && seatsOccupied >= 5 = 'L'
  | otherwise = cell
  where
    cell = g ! c

    seatsOccupied :: Int
    seatsOccupied = length $ filter id ((\d -> race isFinished isOccupied c (+ d)) <$> directions)
    isFinished c =  notMember c g || g ! c == 'L'
    isOccupied c = g ! c == '#'


stepUntil :: (Grid Char -> Coord -> Char) -> Grid Char -> Grid Char
stepUntil f = fixpoint (step f)


step :: (Map t a -> t -> b) -> Map t a -> Map t b
step f g = mapWithKey (\k _ -> f g k) g


day11 :: IO ()
day11 = do
  ls <- getLines 11
  let g = readSeats ls

  timeIt $ putStrLn $ "Day11: Part1: " ++ show (count (== '#') $ stepUntil rule1 g)
  timeIt $ putStrLn $ "Day11: Part2: " ++ show (count (== '#') $ stepUntil rule2 g)


readSeats :: [String] -> Grid Char
readSeats css = fromList $ concat $ (\(row, cs) -> (\(col, c) -> ((col, row), c)) <$> zip [0..] cs) <$> zip [0..] css


