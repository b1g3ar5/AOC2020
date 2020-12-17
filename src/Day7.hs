{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module Day7 where

import Prelude hiding (reverse)
import qualified Data.Map as M
import Data.List ( groupBy, sort, nub )
import Utils ( getLines, splitOnStr )

-- An implementatiin using hyolmorphisms, I've bben waiting for this one

-- There is one coalgebra to build the tree and then
-- an algebra to count it for each part.
-- The tree can be inverted from it's original "contains" version
-- to a "is contained" passive version.


-- A recursion library
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


-- "has in" map
type Pool = M.Map Colour [(Int, Colour)] 

-- convert to "is in" map - all the ints are 1, you can only be in one bag
reverse :: Pool -> Pool
reverse bs = M.fromList ret 
  where
    ret :: [(Colour, [(Int, Colour)])]
    ret = (\ts -> (fst $ head ts, (\t -> (1, snd t)) <$> ts)) <$> 
                      groupBy (\x y -> fst x == fst y) (
                        sort ( 
                              concat $ 
                                (\(c, ts) -> (\t -> (snd t, c)) <$> ts) <$> M.toList bs
                             )
                      )


data BagF a b = BagF a [(Int, b)] deriving (Eq, Show, Functor)


type Bag a = Fix (BagF a)


-- We need the colour of the bag to make and a pool so we can work out what bags are in it.
-- We then lookup the colour in the map and
--    - if the bag is empty we make a 'leaf' bag
--    - if the bag has bags inside we make a list of the colours and the 
--      reduced map to pass on...
makeBag :: Coalgebra (BagF Colour) (Colour, Pool)
makeBag (col, pool) =
  case pool M.!? col of
    Nothing  -> BagF col []
    Just ncols -> BagF col ((\(i, c) -> (i, (c, newPool))) <$> ncols)
                    where
                      newPool = M.delete col pool


-- This uses the "is in" tree
-- The carrier type is a list of all the contained in colours (this time we need to keep track of the
-- colours so that we only count each colour once).
-- If there are no contained in bags just return this bag in a list
-- For all contained in bags - each will have a (1, [Colour]) - so we just concat them together
-- add this colour and nub to get rid of repeats
insideHowMany :: BagF Colour [Colour] -> [Colour]
insideHowMany (BagF c bs) =  if null bs then [c] else nub $ c : concat (snd <$> bs)


-- Each BagF has a list [(Int, a)] in the second parameter, and we can choose the "a"
-- If we choose it to be an Int we can add up the number of bags
howManyInside :: BagF Colour Int -> Int
howManyInside (BagF _ bs) =  if null bs then 1 else 1 + sum (uncurry (*) <$> bs)


day7 :: IO ()
day7 = do
  ls <- getLines 7
  let bags = M.fromList $ parseLine <$> ls

  putStrLn $ "\npart1: " ++ show (length (hylo insideHowMany makeBag ("shiny gold", reverse bags)) - 1)
  putStrLn $ "\npart2: " ++ show (hylo howManyInside makeBag ("shiny gold", bags) - 1)


parseLine :: String -> (Colour, [(Int, Colour)])
parseLine s = (col, if ps!!1 == "no other bags." then [] else parseBag <$> bs)
  where
    ps = splitOnStr " bags contain " s
    col = head ps
    bs = splitOnStr ", " $ ps!!1
    parseBag :: String -> (Int, Colour)
    parseBag s = (n, col)
      where
        n = read $ take 1 s
        col = unwords $ take 2 $ words $ drop 2 s

