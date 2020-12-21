{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module Day15m where


import qualified Data.Massiv.Array  as A
import Data.Int (Int32)
import Control.Monad ( forM_ )
import Control.Monad.ST ( runST )
import System.TimeIt (timeIt)


{- Faster code using the Array in the Massiv library -}

day15m:: IO ()
day15m = do
  let seeds32 :: [Int32]
      seeds32 = [0,1,4,13,15,12,16]
  putStrLn $ "day15m: part1: Massiv: " ++ show (work 2020 seeds32)
  putStrLn $ "day15m: part2: Massiv: " ++ show (work 30000000 seeds32)
  

type Task = [Int32]


work :: Int32 -> Task -> Int32
work last_index seeds = runST $ do
  mp <- A.new @A.U (A.Sz1 $ fromIntegral $ last_index + 2)

  -- initialise with seeds
  forM_ (zip [1..] seeds) $ \(idx, x) -> A.writeM mp (fromIntegral x) idx
     -- write val at pos and work out the nxt
  let go pos val 
        |   pos == last_index = pure val
        | otherwise = do
                        -- read the previous pos of val and work out the nxt value
                        nxt <- A.readM mp (fromIntegral val) >>= \case
                          0 -> pure 0
                          prev_pos -> pure (pos - prev_pos)
                        -- tell the array that val is at pos
                        A.writeM mp (fromIntegral val) pos
                        -- repeat...
                        go (pos + 1) nxt

  go (fromIntegral $ length seeds) (last seeds)

