module Day16 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day16 :: IO ()
day16 = do
  ls <- getLines 16
  putStrLn $ "Day16: " ++ show ls
