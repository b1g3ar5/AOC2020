module Day19 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day19 :: IO ()
day19 = do
  ls <- getLines 19
  putStrLn $ "Day19: " ++ show ls
