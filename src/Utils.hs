module Utils where


getRaw :: Int -> IO String
getRaw n = readFile $ "./Data/Day" ++ show n ++ ".txt"


getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- getRaw n
  return $ f s


getWords = getF words
getLines = getF lines


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

