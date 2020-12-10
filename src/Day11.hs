module Day11 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day11 :: IO ()
day11 = do
  ls <- getLines 11
  putStrLn $ "Day11: " ++ show ls
