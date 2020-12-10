module Day20 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day20 :: IO ()
day20 = do
  ls <- getLines 20
  putStrLn $ "Day20: " ++ show ls
