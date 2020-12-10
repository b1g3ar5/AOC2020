module Day21 where


import Utils
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V


day21 :: IO ()
day21 = do
  ls <- getLines 21
  putStrLn $ "Day21: " ++ show ls
