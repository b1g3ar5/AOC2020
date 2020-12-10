module Day15 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day15 :: IO ()
day15 = do
  ls <- getLines 15
  putStrLn $ "Day15: " ++ show ls
