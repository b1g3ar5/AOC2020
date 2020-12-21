{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Day11v where


import Data.List ( intercalate)
import Data.Grid ( autoConvolute, omitBounds, fromNestedLists', toNestedLists, partitionFocus, Grid, IsGrid )
import Data.Grid.Internal.Convolution ()
import Data.Grid.Internal.Shapes ( Centered )
import Data.Functor.Compose ( Compose(Compose, getCompose) )
import Data.Foldable ( Foldable(toList) )
import Control.Monad ( join )
import Data.Maybe ( fromJust, isJust )
import GHC.TypeNats ( KnownNat )
import System.TimeIt ( timeIt )
import Utils ( getLines, fixpoint )


-- | Using another grid library with fixed size grids and boundary options
-- This time it works, but it's a bit slow - about 15s for part 1.
-- I've not bothered to implement part 2 again.


readSeats :: [String] -> [((Integer, Integer), Char)]
readSeats css = concat $ (\(row, cs) -> (\(col, c) -> ((col, row), c)) <$> zip [0..] cs) <$> zip [0..] css


day11v :: IO ()
day11v = do
  ls <- getLines 11
  --let ls = test
  let g :: Grid '[90, 95] Char
      g = fromNestedLists' ls
      final = fixpoint step g

  timeIt $ putStrLn $ "Day11v: part1: " ++ show (count (=='#') final)


count ::(a -> Bool) -> Grid '[x, y] a -> Int
count p g = sum $ (\c -> if p c then 1 else 0) <$> g


test :: [String]
test = ["L.LL.LL.LL"
  , "LLLLLLL.LL"
  , "L.L.L..L.."
  , "LLLL.LL.LL"
  , "L.LL.LL.LL"
  , "L.LLLLL.LL"
  , "..L.L....."
  , "LLLLLLLLLL"
  , "L.LLLLLL.L"
  , "L.LLLLL.LL"]


showGrid :: (IsGrid '[x, y]) => Grid '[x, y] Char -> String
showGrid = intercalate "\n" . toNestedLists 


simulate :: (KnownNat x, KnownNat y) => Int -> Grid '[x, y] Char -> Grid '[x, y] Char
simulate i g = iterate step g !! i


step :: (IsGrid dims) => Grid dims Char -> Grid dims Char
step = autoConvolute @[3, 3] omitBounds (rule1 . getCompose)


rule1 :: forall dims a. (Centered dims, IsGrid dims) => Grid dims (Maybe Char) -> Char
rule1 g
  | here == Just '.' = '.'
  | (here == Just 'L') && occupied == 0 = '#'
  | (here == Just '#') && occupied >= 4 = 'L'
  | isJust here = fromJust here
  | otherwise = '!'
  where
    (here, neighbours) = partitionFocus g
    occupied = length . filter (== '#') . toList . Compose $ join <$> neighbours



