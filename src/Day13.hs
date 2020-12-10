module Day13 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day13 :: IO ()
day13 = do
  ls <- getLines 13
  putStrLn $ "Day13: " ++ show ls
