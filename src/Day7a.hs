{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Day7a where

import Data.List (delete)
import Utils ( getLines, splitOnStr )


-- This is an attempt to use algebra to build a tree of bags
-- and then comsume it to get the results...

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


-- Bag with recursion factored out - we will want the a to be a Bag!
data BagF a = BagF Colour [(Int, a)] deriving (Eq, Show, Functor)


type Bag = Fix BagF


mkBagF :: Coalgebra BagF (Colour, Pool)
mkBagF (col, pool) =
  case lu pool col of
    Nothing  -> BagF col []
    Just ncols -> 
      let newPool = deleteWith (\(OldBag d _) -> d == col) pool
      in BagF col ((\(i, c) -> (i, (c, newPool))) <$> ncols) 


-- Each BagF has a list [(Int, a)] in the second parameter, and we can choose the a
howMany :: BagF Int -> Int
howMany (BagF _ bs) =  if null bs then 1 else 1 + sum (uncurry (*) <$> bs)


type Pool = [OldBag]

lu :: [OldBag] -> Colour -> Maybe [(Int, Colour)]
lu ob c = go ob
    where
      go [] = Nothing
      go (OldBag d ds : bs)
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
  ls <- getLines 7
  let bs = parseLine <$> ls
  putStrLn $ "bags: " ++ show (hylo howMany mkBagF ("shiny gold", bs) - 1)


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


