module Day22 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day22 :: IO ()
day22 = do
  ls <- getLines 22
  putStrLn $ "Day22: " ++ show ls
