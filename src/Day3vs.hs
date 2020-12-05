{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}


module Day3vs where


import GHC.TypeNats ( KnownNat, Nat, natVal )
import Data.Proxy ( Proxy(Proxy) )
import Data.Finite (finite )
import Data.Vector (fromList)

import Utils (getLines, Coord)
import Vec (Mat(..), mindex, withMat)


-- Another version with my type level sized vector

-- The Grid is a continuation - you need to pass a function to it
-- but your function can reify the size of the Grid
type Grid m n r = (forall m n. (KnownNat m, KnownNat n) => Mat m n Bool -> r) -> r


readGrid :: [String] -> Grid m n r
readGrid ls = withMat v
  where
    v = fromList ( fromList . go <$> ls)
    go :: String -> [Bool]
    go s = (=='#') <$> s


-- This lookup function wraps the x coord and return Nothing if the y coord is too big
at :: forall (m :: Nat) (n :: Nat) a. (KnownNat m, KnownNat n) => Mat m n a -> Coord -> Maybe a
g `at` (x, y)
  | y >= intM = Nothing
  | otherwise = Just $ g `mindex` (finite mx, finite $ toInteger y)
  where
    mx = toInteger $ x `mod` intN
    intN = fromIntegral $ toInteger (natVal (Proxy @n))
    intM = fromIntegral $ toInteger (natVal (Proxy @m))


countTrees :: forall (m :: Nat) (n :: Nat) a. (KnownNat m, KnownNat n) => Coord -> Mat m n Bool -> Int
countTrees (ix,iy) g = go 0 (0,0)
  where
    go ntrees (x, y) = case g `at` (x, y) of
                         Nothing -> ntrees
                         Just b -> if b then
                                       go (ntrees+1) (x+ix, y+iy)
                                     else
                                       go ntrees (x+ix, y+iy)


day3vs:: IO ()
day3vs = do
  ls <- getLines 3
  let g = readGrid ls
      numTrees1 = g (countTrees (3,1))
  putStrLn $ "Day3: part1: " ++ show numTrees1

  let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
      numTrees2 = product $ (\s -> g (countTrees s)) <$> slopes
  putStrLn $ "Day3: part2: " ++ show numTrees2
