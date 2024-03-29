{-# LANGUAGE  BangPatterns, StandaloneDeriving, TupleSections #-}

module Day23 where


import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))

gameSize :: Int
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
  let mm = mkLL $ [5,0,3,6,4,1,7,2,8] ++ [9..(gameSize-1)]
      mm500 = myIterate 10000000 updateLL mm
      n1 = snd mm500 ! 0
      n2 = snd mm500 ! n1

  putStrLn $ "Day23: part2: " ++ show (n1, n2)
  putStrLn $ "Day23: part2: " ++ show ((n1+1)*(n2+1))


-- Linked list with focus implemented with and IntMap
type LL = (Int, Map Int Int)


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


-- move focus to the next position
moveLL :: LL -> LL
moveLL (f, mp) = (mp!f, mp)    


showE :: (Int , Int) -> String
showE (x, y) = show x ++ "->" ++ show y ++ " "


-- f points to where t3 pointed, t3 points to the one after next (==f-1), next points to t1
updateLL :: LL -> LL
updateLL (f, mp) = (mp!t3, newMap)
  where
    t1 = mp ! f
    t2 = mp ! t1
    t3 = mp ! t2 -- this needs to point to mp !next
    pred x = if x == 0 then gameSize-1 else x-1
    getNext y = if y `elem` [t1,t2,t3] then getNext (pred y) else y
    !next = getNext (pred f) -- this needs to point to t1
    !newMap = M.insert f (mp!t3) $ M.insert t3 (mp!next) $ M.insert next t1 mp


myIterate :: Int -> (a -> a) -> a -> a
myIterate n f = go n
  where
    go 0 x = x
    go n x = go (n-1) y
      where
        !y = f x

