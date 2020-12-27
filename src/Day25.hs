
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Day25 where


import Math.NumberTheory.Moduli.Singleton ( CyclicGroup, cyclicGroup )
import Data.Maybe
import Data.Foldable (traverse_)
import GHC.TypeNats (KnownNat, SomeNat(SomeNat), someNatVal, Nat)
import Math.NumberTheory.Moduli ((^%), Mod, discreteLogarithm, getVal, isMultElement, isPrimitiveRoot)
import Numeric.Natural (Natural)
import Data.Proxy

doorKey = 1327981 --17807724
cardKey = 2822615 --5764801



modn :: Natural
modn = 20201227


findLoopSize :: Integer -> Integer -> Integer
findLoopSize mul target = go 0 1 
  where
    go n x
      | x == target = n
      | otherwise = go (n+1) ((x*mul) `mod` 20201227)


transform :: Integer -> Integer  -> Integer 
transform mul loopSize = go 1 mul
  where
    go n x
      | n == loopSize = x
      | otherwise = go (n+1) ((x*mul) `mod` 20201227)



day25 :: IO ()
day25 = do
  let subjectNumber = 7
      divider = 20201227
      doorLoops = findLoopSize 7 doorKey
      cardLoops = findLoopSize 7 cardKey

  putStrLn $ "Day24: part1: " ++ show (doorLoops, cardLoops)
  putStrLn $ "Day24: part2: " ++ show (transform doorKey cardLoops)
  putStrLn $ "Day24: part2: " ++ show (transform cardKey doorLoops)

  return ()


