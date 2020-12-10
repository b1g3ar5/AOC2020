module Day12 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day12 :: IO ()
day12 = do
  ls <- getLines 12
  putStrLn $ "Day12: " ++ show ls
