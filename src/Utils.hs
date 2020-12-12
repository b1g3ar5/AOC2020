{-# LANGUAGE TupleSections #-}


module Utils where

import Debug.Trace

getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./Data/Day" ++ show n ++ ".txt"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getGroups :: Int -> IO [[String]]
getGroups = getF (splitOnChar "" . lines)


-- This does conversion units at the front of the list
toInt :: [Bool] -> Int
toInt [] = 0
toInt bs = 2 * toInt (tail bs) + if head bs then 1 else 0


-- Split a string into 2 at a character
split1 :: (Eq a) => a -> [a] -> ([a], [a])
split1 ch = go []
  where
    go acc (x:xs)
      | x == ch = (acc, xs)
      | otherwise = go (acc++[x]) xs


-- Like words but you specify the character
splitOnChar :: Eq a => a -> [a] -> [[a]]
splitOnChar c = go []
  where
    go acc [] = acc
    go [] (x:xs)
      | x==c = go [] xs
      | otherwise = go [[x]] xs
    go acc@(w:ws) (x:xs)
      | x==c = go ([]:acc) xs
      | otherwise = go ((w++[x]):ws) xs


splitOnStr :: Eq a => [a] -> [a] -> [[a]]
splitOnStr s = reverse . go [[]]
  where
    n = length s
    go acc [] = acc
    go acc@(w:ws) xs
      | take n xs == s = go ([]:acc) $ drop n xs
      | otherwise = go ((w ++ [head xs]):ws) $ tail xs


-- A 2D coordinate or a vector
type Coord = (Int, Int)


neighbourCoords :: [Coord]
neighbourCoords = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]


add :: Coord -> Coord -> Coord
add (x, y) (x', y') = (x + x', y + y')


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (add origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


-- This takes 2 predicates isFinished and isOK
-- and a start value and a next function
-- it returns True if isOK happens before isFinished
race :: (a -> Bool) -> (a -> Bool) -> a -> (a -> a) -> Bool
race isFinished isOk x next 
  | isFinished nxt = False
  | isOk nxt = True
  | otherwise = race isFinished isOk nxt next
  where
    nxt = next x



directions :: [Coord]
directions = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]