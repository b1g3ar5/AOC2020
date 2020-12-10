module Day24 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day24 :: IO ()
day24 = do
  ls <- getLines 24
  putStrLn $ "Day24: " ++ show ls
