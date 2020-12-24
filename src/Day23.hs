{-# LANGUAGE  TupleSections #-}
{-# LANGUAGE  StandaloneDeriving #-}

module Day23 where


import Utils
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Sequence (Seq(..), (><), (|>), (<|))
import System.TimeIt
import qualified Data.IntMap.Strict as M
import Data.IntMap.Strict (IntMap, (!))

import Debug.Trace

gameSize = 1000000

-- To get the kill messages
-- dmesg | grep -i kill

-- input : 614752839

--Day23: part1: (178512,119169)
--CPU time: 378.46s
--Day23: part1: 21273394210

-- This only works on my computer with no apps open - otherwise it gets killed

day23 :: IO ()
day23 = do
  let ll = mkLL [5,0,3,6,4,1,7,2,8]
      ll1 = iterate updateLL ll !! 100

  --timeIt $ putStrLn $ "Day23: part1: " ++ showLL ll1

  let mm = mkLL $ [5,0,3,6,4,1,7,2,8] ++ [9..(gameSize-1)]
      mm500 = iterate updateLL mm !! 10000000
      n1 = snd mm500 ! 0
      n2 = snd mm500 ! n1

  timeIt $ putStrLn $ "Day23: part1: " ++ show (n1, n2)
  putStrLn $ "Day23: part1: " ++ show ((n1+1)*(n2+1))


type LL = (Int, IntMap Int)


showLL :: LL -> String 
showLL ll@(f, mp) = go 20 f (show f) (moveLL ll)
  where
    go :: Int -> Int -> String -> LL -> String
    go n stop acc gll@(gf, _) 
      | n==0 = acc
      | gf == stop = acc
      | otherwise = go (n-1) stop (acc ++ " " ++ show gf) (moveLL gll)


mkLL :: [Int] -> LL
mkLL xs = (head xs, mp)
  where
    mp = M.fromList $ zip xs $ tail xs ++ [head xs]

moveLL :: LL -> LL
moveLL (f, mp) = (mp!f, mp)    


showE :: (Int , Int) -> String
showE (x, y) = show x ++ "->" ++ show y ++ " "


updateLL :: LL -> LL
updateLL (f, mp) = --trace (showE (f, mp!t3) ++ showE (t3, mp!next) ++ showE (next, t1))
  (mp!t3, M.insert f (mp!t3) $ M.insert t3 (mp!next) $ M.insert next t1 mp)
  where
    t1 = mp ! f
    t2 = mp ! t1
    t3 = mp ! t2 -- this needs to point to mp !next
    pred x = if x == 0 then gameSize-1 else x-1
    getNext y = if y `elem` [t1,t2,t3] then getNext (pred y) else y
    next = getNext (pred f) -- this needs to point to t1




