module Day17 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day17 :: IO ()
day17 = do
  ls <- getLines 17
  putStrLn $ "Day17: " ++ show ls
