{-# LANGUAGE FlexibleContexts #-}

module Day6 where

import Prelude hiding (sum, product)
import qualified Data.List as L
import Data.Semiring
import Utils

alphabet = "abcdefghijklmnopqrstuvwxyz"


day6 :: IO ()
day6 = do
  gs <- getGroups 6
  let c1 = L.sum $ length . foldl L.union [] <$> gs
      c2 = L.sum $ length . foldl L.intersect alphabet <$> gs

  putStrLn $ "Day6: part1: " ++ show c1
  putStrLn $ "Day6: part2: " ++ show c2

  -- With a semiring instance. Why not?
  let hs :: [[Set]]
      hs = (mkSet <$>) <$> gs
      d1, d2 :: Int
      d1 = L.sum $ order . sum <$> hs
      d2 = L.sum $ order . product <$> hs

  putStrLn $ "Day6: part1: " ++ show d1
  putStrLn $ "Day6: part2: " ++ show d2


-- A newtype so we can add a monooid instance
newtype Set = Set String


-- Smart constructor
mkSet :: String -> Set
mkSet xs = Set $ L.nub xs

-- We need the size of the set
order :: Set -> Int
order (Set xs) = L.length xs


instance Semiring Set where
  (Set x) `plus` (Set y) = Set $ L.union x y
  (Set x) `times` (Set y) = Set $ L.intersect x y
  -- Empty set
  zero = Set []
  -- Universal set
  one = Set alphabet
