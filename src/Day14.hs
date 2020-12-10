module Day14 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day14 :: IO ()
day14 = do
  ls <- getLines 14
  putStrLn $ "Day14: " ++ show ls
