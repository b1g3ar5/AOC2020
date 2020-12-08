module Day8 where

import Prelude hiding (filter)
import Utils
import Data.Vector (Vector, (//), (!), fromList, findIndices, filter)
import Computer


day8 :: IO ()
day8 = do
  ls <- getLines 8
  let instructions = fromList $ zip (parse <$> ls) $ repeat False
      tape = Computer 0 0 instructions
  putStrLn $ "Day8: part1: " ++ show (run tape)

  let jmps, nops :: Vector Int
      jmps = findIndices (isJmp . fst) instructions
      nops = findIndices (isNop . fst) instructions
      tapes :: Vector (Vector (Inst, Bool))
      tapes = (\ix -> instructions // [(ix, (Nop $ get $ fst $ instructions!ix, False))]) <$> (jmps `mappend` nops)
      computers = Computer 0 0 <$> tapes
      rets = run <$> computers
  putStrLn $ "Day8: part2: " ++ show (filter snd rets)


