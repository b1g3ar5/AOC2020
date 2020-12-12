{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}

module Day11v where


--import System.Console.ANSI
import Data.List ( intercalate)
import Data.Grid ( autoConvolute, omitBounds, fromNestedLists', toNestedLists, partitionFocus, Grid, IsGrid )
import Data.Grid.Internal.Convolution ()
import Data.Grid.Internal.Shapes ( Centered )
import Data.Functor.Compose
import Data.Foldable
import Control.Monad
import Data.Maybe
import GHC.TypeNats
import System.TimeIt

import Utils

-- I can't get this to work with Ordinal - only with wrapped grid
-- So it's no good - going to try with linear V...

readSeats :: [String] -> [((Integer, Integer), Char)]
readSeats css = concat $ (\(row, cs) -> (\(col, c) -> ((col, row), c)) <$> zip [0..] cs) <$> zip [0..] css


day11v :: IO ()
day11v = do
  ls <- getLines 11
  --let ls = test
  let g :: Grid '[90, 95] Char
      g = fromNestedLists' ls
      final = loop g

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


loop :: (KnownNat x, KnownNat y) => Grid '[x, y] Char -> Grid '[x, y] Char
loop g = if g == nxt then g else loop nxt
  where
    nxt = step g


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



