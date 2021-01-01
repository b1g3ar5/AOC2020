{-# LANGUAGE TupleSections#-}

module Day14 where


import Utils ( fromInt, getRaw, pad, splitOn, toInt )

import qualified Data.Map as M
import Data.List ( foldl' )
import Control.Monad ( zipWithM )
import Data.Tuple ( swap )


type Memory = M.Map Integer Integer


type Bit = Bool
type MBit = Maybe Bool -- 3 inhabitants
type Mask = [MBit]
type Write = (Integer, Integer)
type Set = (Mask, [Write])


type Rule = MBit -> Bit -> [Bit]


-- Rule for changing the int to write
rule1 :: Rule
rule1 Nothing b = [b]
rule1 (Just False) _ = [False]
rule1 (Just True) _ = [True]


-- Rule for changing the address to write to
rule2 :: Rule
rule2 Nothing _ = [True, False]
rule2 (Just False) b = [b]
rule2 (Just True) _ = [True]


toBits :: Integer -> [Bool]
toBits x = pad 36 False (reverse $ fromInt x )


fromBits :: [Bool] -> Integer
fromBits bs = toInt $ reverse bs


-- Note: zipWithM does the 'sequence' call that we need
-- to make all the lists
apply :: Rule -> Mask -> Write -> [Write]
apply f m (a, i) = (a,) . fromBits <$> zipWithM f m (toBits i)


apply2 :: Mask -> Write -> [Write]
apply2 m t = swap <$> apply rule2 m (swap t)


runSet :: (Mask -> Write -> [Write]) -> Memory -> Set -> Memory
runSet f mem (mask, writes) = newmem `M.union` mem
  where
    newmem = M.fromList $ concat $ f mask <$> writes


day14 :: IO ()
day14 = do
  s <- getRaw 14
  let sets :: [Set]
      sets = parse s

  putStrLn $ "Day14: Part1: " ++ show (sum $ foldl' (runSet $ apply rule1) M.empty sets)
  putStrLn $ "Day14: Part2: " ++ show (sum $ foldl' (runSet apply2) M.empty sets)


-- Paring the input...
parseBit :: Char -> MBit
parseBit '0' = Just False
parseBit '1' = Just True
parseBit 'X' = Nothing


parse :: String -> [Set]
parse s = parseSet . lines <$> pieces
  where
    pieces = tail $ splitOn "mask = " s


parseSet :: [String] -> Set
parseSet ls = (parseMask $ head ls, parseWrite <$> tail ls)


parseWrite :: String -> Write
parseWrite s = (read $ head pieces, read $ pieces !!1)
  where
    pieces = splitOn "] = " $ drop 4 s


parseMask :: String -> [MBit]
parseMask s = parseBit <$> s
      
