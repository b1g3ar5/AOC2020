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


addCoords :: Coord -> Coord -> Coord
addCoords (x, y) (x', y') = (x + x', y + y')


at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords


mul :: Int -> Coord -> Coord
mul c (x,y) = (c*x, c*y)


findTarget :: (Coord -> Bool) -> Coord -> Direction -> Maybe Coord
findTarget p pos dir = case filter p $ ray pos dir of
                         [] -> Nothing
                         (x:_) -> trace ("findTarget returns: " ++ show x) $ Just x


data Direction = N | S | E | W | NE | SE | SW | NW
    
directions :: [Direction]
directions = [N,S,E,W,NE,SE,SW,NW]


ray :: Coord -> Direction -> [Coord]
ray (_, 0) N = []
ray (_, 0) NE = []
ray (_, 0) NW = []
ray (0, _) W = []
ray (0, _) SW = []
ray (0, _) NW = []
ray (px, py) N =  (px, ) <$> [(py-1)..0]
ray (px, py) S =  (px, ) <$> [(py+1)..]
ray (px, py) E =  (, py) <$> [(px+1)..]
ray (px, py) W =  (, py) <$> [(px-1)..0]
ray (px, py) NE = (\i -> (px+i, py-i)) <$> [1..py]
ray (px, py) SE = (\i -> (px+i, py+i)) <$> [1..]
ray (px, py) SW = (\i -> (px-i, py+i)) <$> [1..px]
ray (px, py) NW = (\i -> (px-i, py-i)) <$> [1..(min py px)]
