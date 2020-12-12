{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Day11g where


import Control.Comonad
import Control.Lens
import Control.Comonad.Store
--import Data.AdditiveGroup
import Data.AffineSpace
import GHC.TypeLits
import qualified GHC.TypeLits as GHC
--import System.Console.ANSI
import SizedGrid
import Data.List (foldl', transpose)


-- I can't get this to work with Ordinal - only with wrapped grid
-- So it's no good - going to try with linear V...

readSeats :: [String] -> [((Integer, Integer), Char)]
readSeats css = concat $ (\(row, cs) -> (\(col, c) -> ((col, row), c)) <$> zip [0..] cs) <$> zip [0..] css


day11g :: IO ()
day11g = do
  --ls <- getLines 11
  let ls = test
  let g :: Grid '[Periodic 10, Periodic 10] Char
      g = start ls
      gg = loop 2 rule1 g

  putStrLn $ "Day11: Part1:\n" ++ displayGrid gg


type Rule = Char -> String -> Char

rule1 :: Rule
rule1 here neigh
  | here == '.' = '.'
  | (here == 'L') && occupied == 0 = '#'
  | (here == '#') && occupied >= 4 = 'L'
  | otherwise = here
  where
    occupied = length $ filter (== '#') neigh 


applyRule :: ( All IsCoordLifted cs, All Monoid cs, All Semigroup cs, All AffineSpace cs, All Eq cs, AllDiffSame Integer cs, AllSizedKnown cs, IsGrid cs (grid cs))
          => Rule -> grid cs Char -> grid cs Char
applyRule rule = over asFocusedGrid $ 
    extend $ \fg -> rule (extract fg) $ map (`peek` fg) $ 
                      filter (/= pos fg) $ moorePoints (1 :: Integer) $ pos fg


loop :: ( Eq (grid cs Char), All IsCoordLifted cs, All Monoid cs, All Semigroup cs, All AffineSpace cs, All Eq cs, AllDiffSame Integer cs, AllSizedKnown cs, IsGrid cs (grid cs)) 
     =>  Int -> Rule -> grid cs Char -> grid cs Char
loop n rule g
  | n == 0 = g
  | newGrid == g = g
  | otherwise = loop (n-1) rule newGrid
  where
    newGrid = applyRule rule g


count ::(Foldable (grid cs), Functor (grid cs)) => (a -> Bool) -> grid cs a -> Int
count p g = sum $ (\c -> if p c then 1 else 0) <$> g


displayGrid :: (KnownNat (x GHC.* y), KnownNat x, KnownNat y) => Grid '[f x, g y] Char -> String
displayGrid = unlines . transpose . collapseGrid 


start :: ( KnownNat (CoordNat x GHC.* CoordNat y), Semigroup x, Semigroup y, Monoid x, Monoid y, IsCoordLifted x, IsCoordLifted y, AffineSpace x, AffineSpace y, Diff x ~ Integer, Diff y ~ Integer) 
         => [String] -> Grid '[x,y] Char
start css = foldl' update (pure '.') $ concat $ (\(row, cs) -> (\(col, c) -> ((col,row), c)) <$> zip [0..] cs) <$> zip [0..] css


update :: ( KnownNat (CoordNat x GHC.* CoordNat y), Semigroup x, Semigroup y, Monoid x, Monoid y, IsCoordLifted x, IsCoordLifted y, AffineSpace x, AffineSpace y, Diff x ~ Integer, Diff y ~ Integer) 
          => Grid '[x,y] Char -> ((Integer, Integer), Char) -> Grid '[x,y] Char
update acc (cd, c) = acc & gridIndex (offset .+^ cd) .~ c
  where
    offset = mempty


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
