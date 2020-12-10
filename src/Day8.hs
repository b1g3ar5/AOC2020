module Day8 where

import Prelude hiding (filter, (++))
import qualified Prelude as P ((++))
import Utils ( getLines )
import Data.Vector (Vector, (!), fromList, findIndices, filter, (++))
import Computer


day8 :: IO ()
day8 = do
  ls <- getLines 8
  let tape = fromList $ zip (parse <$> ls) $ repeat False
      comp = Comp 0 0 tape
  putStrLn $ "Day8: part1: " P.++ show (run comp)

  let jmps, nops :: Vector Int
      jmps = findIndices (isJmp . fst) tape
      nops = findIndices (isNop . fst) tape
      jmpTapes, nopTapes :: Vector (Vector (Inst, Bool))
      jmpTapes = (\ix -> tape `update` (ix, (Nop $ get $ fst $ tape!ix, False))) <$> jmps
      nopTapes = (\ix -> tape `update` (ix, (Jmp $ get $ fst $ tape!ix, False))) <$> nops
      computers = Comp 0 0 <$> (jmpTapes ++ nopTapes)
      rets = run <$> computers
  putStrLn $ "Day8: part2: " P.++ show (filter snd rets)


