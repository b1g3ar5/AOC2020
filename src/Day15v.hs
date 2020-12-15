{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}

module Day15v where


import Control.Monad ( forM_ )
import Control.Monad.ST (runST, ST(..))
import System.TimeIt (timeIt)
import Data.Foldable (foldlM)
import qualified Data.Vector.Unboxed.Mutable as VM


{- Faster code using the Vector -}

day15v:: IO ()
day15v = do
  let seeds :: [Int]
      seeds = [0,1,4,13,15,12,16]
  
  timeIt $ putStrLn $ "day15v: part1: Vector: " ++ show (run seeds 2020)
  timeIt $ putStrLn $ "day15v: part1: Vector: " ++ show (run seeds 30000000)


run :: [Int] -> Int -> Int
run input target
  | target <= length input = input !! (target - 1)
  | otherwise = runST $ do
                  -- initialise with 0s
                  v <- VM.replicate (maximum (target : input) + 1) 0
                  -- write the input into the vector
                  forM_ (zip (init input) [1..]) $ uncurry (VM.write v)
                  -- 
                  foldlM (go v) (last input) [len..target-1]
    where
      len = length input


go :: VM.MVector s Int -> Int -> Int -> ST s Int
go !v !val pos = do
  -- find the previous position
  prevPos <- VM.unsafeRead v val
  -- save the new position
  VM.write v val pos
  -- return the next value
  return $ if prevPos == 0 then 0 else pos - prevPos
