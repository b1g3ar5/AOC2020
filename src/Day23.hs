module Day23 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day23 :: IO ()
day23 = do
  ls <- getLines 23
  putStrLn $ "Day23: " ++ show ls
