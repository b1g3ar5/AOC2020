{-# LANGUAGE TupleSections#-}

module Day14 where


import Utils

import qualified Data.Map as M
import Data.List

type Memory = M.Map Integer Integer

type Bit = Bool
type MBit = Maybe Bool
type Mask = [MBit]
type Address = Integer
type Write = (Address, Integer)
type Set = (Mask, [Write])


-- Rule for changing the int to write
rule1 :: MBit -> Bit -> Bit
rule1 Nothing b = b
rule1 (Just False) _ = False
rule1 (Just True) _ = True


-- Rule for changing the address to write to
rule2 :: MBit -> Bit -> [Bit]
rule2 Nothing _ = [True, False]
rule2 (Just False) b = [b]
rule2 (Just True) _ = [True]


toBits :: Integer -> [Bool]
toBits x = pad 36 False (reverse $ fromInt x )


fromBits :: [Bool] -> Integer
fromBits bs = toInt $ reverse bs


apply :: Mask -> Write -> Write
apply m (a, i) = (a, fromBits $ zipWith rule1 m $ toBits i)


runSet1 :: Memory -> Set -> Memory
runSet1 mem (mask, writes) = newmem `M.union` mem
  where
    newmem = M.fromList $ apply mask <$> writes


apply2 :: Mask -> Write -> [Write]
apply2 m (a, i) = (,i) . fromBits <$> sequence bss
  where
    bss = zipWith rule2 m $ toBits a


runSet2 :: Memory -> Set -> Memory
runSet2 mem (mask, writes) = newmem `M.union` mem
  where
    newmem = M.fromList $ concat $ apply2 mask <$> writes


day14 :: IO ()
day14 = do
  s <- getRaw 14
  let sets :: [Set]
      sets = parse s

  putStrLn $ "Day14: Part1: " ++ show (sum $ foldl' runSet1 M.empty sets)
  putStrLn $ "Day14: Part2: " ++ show (sum $ foldl' runSet2 M.empty sets)


-- Paring the input...
parseBit :: Char -> MBit
parseBit '0' = Just False
parseBit '1' = Just True
parseBit 'X' = Nothing


parse :: String -> [Set]
parse s = parseSet . lines <$> pieces
  where
    pieces = tail $ splitOnStr "mask = " s


parseSet :: [String] -> Set
parseSet ls = (parseMask $ head ls, parseWrite <$> tail ls)


parseWrite :: String -> Write
parseWrite s = (read $ head pieces, read $ pieces !!1)
  where
    pieces = splitOnStr "] = " $ drop 4 s


parseMask :: String -> [MBit]
parseMask s = parseBit <$> s
      
