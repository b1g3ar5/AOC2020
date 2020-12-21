{-# LANGUAGE FlexibleContexts #-}

module Day6 where

import Prelude hiding (sum, product)
import qualified Data.List as L
import Data.Semiring
import Utils

alphabet = ['a'..'z']


day6 :: IO ()
day6 = do
  gs <- getParagraphs 6
  let c1 = L.sum $ length . foldl L.union [] <$> gs
      c2 = L.sum $ length . foldl L.intersect alphabet <$> gs

  putStrLn $ "Day6: part1: " ++ show c1
  putStrLn $ "Day6: part2: " ++ show c2

  -- With a semiring instance. Why not?
  let hs :: [[Set]]
      hs = (mkSet <$>) <$> gs
      apply :: ([Set] -> Set) -> [[Set]] -> Int
      apply f xs = L.sum $ size . f <$> xs
      d1, d2 :: Int
      d1 = apply sum  hs
      d2 = apply product hs

  putStrLn $ "Day6: part1: " ++ show d1
  putStrLn $ "Day6: part2: " ++ show d2


-- A newtype so we can add a semiring instance
newtype Set = Set String


-- Smart constructor
mkSet :: String -> Set
mkSet xs = Set $ L.nub xs

-- We need the size of the set
size :: Set -> Int
size (Set xs) = L.length xs


instance Semiring Set where
  (Set x) `plus` (Set y) = Set $ L.union x y
  (Set x) `times` (Set y) = Set $ L.intersect x y
  -- Empty set
  zero = Set []
  -- Universal set
  one = Set alphabet
