{-# LANGUAGE TupleSections #-}

module Day7s where


import Data.Tuple
import Data.List hiding (reverse)
import Utils ( getLines, splitOn )

-- This is a simple implementation - recursing down the tree

type Colour = String

data Bag = Bag { col :: Colour
               , needs :: [(Int, Colour)]
               } deriving (Show)


parse :: String -> Bag
parse s = Bag col $ if ps!!1 == "no other bags." then [] else parseBag <$> bs 
  where
    ps = splitOn " bags contain " s
    col = head ps
    bs = splitOn ", " $ ps!!1
    parseBag :: String -> (Int, Colour)
    parseBag s = (n, col)
      where
        n = read $ take 1 s
        col = unwords $ take 2 $ words $ drop 2 s


day7s :: IO ()
day7s = do
  ls <- getLines 7
  let bs = parse <$> ls
  putStrLn $ "Day7s: part1: " ++ show (length $ filter (doesItNeed bs) bs)
  putStrLn $ "Day7s: part2: " ++ show (howManyDoesItNeed bs (head $ filter (\(Bag x _) -> x == "shiny gold") bs)-1)


doesItNeed :: [Bag] -> Bag -> Bool
doesItNeed _ (Bag _ []) = False
doesItNeed allBags (Bag _ ns)
  -- if the bag needs a shiny gold directly ...
  | "shiny gold" `elem` cs = True
  -- if any included bag needs a shiny gold ...
  | or (doesItNeed allBags <$> bs) = True
  | otherwise = False
  where
    cs = snd <$> ns
    -- Convert colour list to bag list
    bs = filter (\(Bag c _) -> c `elem` cs) allBags


howManyDoesItNeed :: [Bag] -> Bag -> Int
howManyDoesItNeed _ (Bag _ []) = 1
howManyDoesItNeed allBags b = go 0 b
  where
    go n (Bag _ []) = n
    go n (Bag _ ns) = n + 1 + sum xx
      where
        xx :: [Int]
        xx = (\(m, b) -> m * howManyDoesItNeed allBags b) . fromC <$> ns

    fromC :: (Int, Colour) -> (Int, Bag)
    fromC (n, c) = (n, head $ filter (\(Bag x _) -> x == c) allBags)
