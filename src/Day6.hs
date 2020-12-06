{-# LANGUAGE FlexibleContexts #-}

module Day6 where

import Prelude hiding (sum, product)
import qualified Data.List as L
import Data.Semiring
import Utils

alphabet = "abcdefghijklmnopqrstuvwxyz"


day6 :: IO ()
day6 = do
  txt <- getRaw 6
  let ls = splitOnChar '\n' txt
      gs = splitOnChar "" ls
      c1 = L.sum $ length . foldl L.union [] <$> gs
      c2 = L.sum $ length . foldl L.intersect alphabet <$> gs

  putStrLn $ "Day6: part1: " ++ show c1
  putStrLn $ "Day6: part2: " ++ show c2

  let hs :: [[Set]]
      hs = (mkSet <$>) <$> gs
      d1, d2 :: Int
      d1 = L.sum $ len . sum <$> hs
      d2 = L.sum $ len . product <$> hs

  putStrLn $ "Day6: part1: " ++ show d1
  putStrLn $ "Day6: part2: " ++ show d2


newtype Set = Set String


mkSet :: String -> Set
mkSet xs = Set $ L.nub xs

len :: Set -> Int
len (Set xs) = L.length xs

instance Semiring Set where
  (Set x) `plus` (Set y) = Set $ L.union x y
  (Set x) `times` (Set y) = Set $ L.intersect x y
  zero = Set []
  one = Set alphabet
