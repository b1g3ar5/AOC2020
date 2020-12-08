{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Day7a where

import Data.List (delete)
import Utils ( getLines, splitOnStr )

newtype Fix f = Fix { unFix :: f (Fix f) }
type Coalgebra f a = a -> f a
ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g


type Colour = String

data OldBag = OldBag { col :: Colour
               , needs :: [(Int, Colour)]
               } deriving (Show)


-- Bag with no recursion - we will want the a to be a Bag!
data BagF a = BagF Colour [(Int, a)] deriving (Eq, Show, Functor)


type Bag = Fix BagF


-- When the carrier is [Int] we will have
-- Bag Colour [(Int, [Int])] that is [Int] replaces the a on the RHs of the definition
-- We have to make a [Int] to return 
howMany :: BagF [Int] -> [Int]
howMany (BagF c bs) =  concat $ (\(n, xs) -> (n*)<$> xs) <$> bs


type Pool = [OldBag]

lu :: Colour -> [OldBag] -> Maybe [(Int, Colour)]
lu c = go
    where
      go [] = Nothing
      go (b@(OldBag d ds):bs)
        | d == c = Just ds
        | otherwise = go bs


deleteWith :: (a -> Bool) -> [a] -> [a]
deleteWith p [] = []
deleteWith p xs = go [] xs
  where
    go acc [] = acc
    go acc (y:ys)
      | p y = acc ++ ys
      | otherwise = go (acc ++ [y]) ys


-- Unfolding the tree from the pool
mkBagF :: Coalgebra BagF (Colour, Pool)
mkBagF (col, pool) =
  case lu col pool of
    Nothing  -> BagF col []
    Just ncols -> 
      let newPool = deleteWith (\(OldBag d _) -> d == col) pool
      in BagF col (fmap (\(i, c) -> (i, (c, newPool))) ncols) 



parseLine :: String -> OldBag
parseLine s = OldBag col $ if ps!!1 == "no other bags." then [] else parseBag <$> bs 
  where
    ps = splitOnStr " bags contain " s
    col = head ps
    bs = splitOnStr ", " $ ps!!1
    parseBag :: String -> (Int, Colour)
    parseBag s = (n, col)
      where
        n = read $ take 1 s
        col = unwords $ take 2 $ words $ drop 2 s


day7a :: IO ()
day7a = do
  --ls <- getLines 7
  let ls = test1
  let bs = parseLine <$> ls
      bags = ana mkBagF ("shiny gold", bs)
      count = cata howMany bags
  putStrLn $ "bags: " ++ show count


doesItNeed :: [OldBag] -> OldBag -> Bool
doesItNeed _ (OldBag _ []) = False
doesItNeed allBags (OldBag _ ns)
  -- if the bag needs a shiny gold directly ...
  | "shiny gold" `elem` cs = True
  -- if any included bag needs a shiny gold ...
  | or (doesItNeed allBags <$> bs) = True
  | otherwise = False
  where
    cs = snd <$> ns
    -- Convert colour list to bag list
    bs = filter (\(OldBag c _) -> c `elem` cs) allBags


howManyDoesItNeed :: [OldBag] -> OldBag -> Int
howManyDoesItNeed _ (OldBag _ []) = 1
howManyDoesItNeed allBags b = go 0 b
  where
    go n (OldBag _ []) = n
    go n (OldBag _ ns) = n + 1 + sum xx
      where
        xx :: [Int]
        xx = (\(m, b) -> m * howManyDoesItNeed allBags b) . fromC <$> ns

    fromC :: (Int, Colour) -> (Int, OldBag)
    fromC (n, c) = (n, head $ filter (\(OldBag x _) -> x == c) allBags)



test1 = ["light red bags contain 1 bright white bag, 2 muted yellow bags."
  , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
  , "bright white bags contain 1 shiny gold bag."
  , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
  , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
  , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
  , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
  , "faded blue bags contain no other bags."
  , "dotted black bags contain no other bags."
  ]