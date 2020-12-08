

module Computer where


import Data.Vector (Vector, (//), (!))


data Inst = Acc Int | Jmp Int | Nop Int deriving (Show, Eq)

isJmp, isNop, isAcc :: Inst -> Bool
isJmp (Jmp _) = True
isJmp _ = False
isNop (Nop _) = True
isNop _ = False
isAcc (Acc _) = True
isAcc _ = False


get :: Inst -> Int
get (Jmp x) = x
get (Acc x) = x
get (Nop x) = x


parse :: String -> Inst
parse s
  | ins == "acc" = Acc $ sgn * read (drop 5 s)
  | ins == "jmp" = Jmp $ sgn * read (drop 5 s)
  | ins == "nop" = Nop $ sgn * read (drop 5 s)
  where
    ins = take 3 s
    sgn = if s!!4 == '+' then 1 else -1


-- The memory has a position, an accumulator and an instruction set
-- and a list of instructions visited
data Computer = Computer { pos :: Int
                         , acc :: Int
                         , instructions :: Vector (Inst, Bool)
                         } deriving (Show)


run :: Computer -> (Int, Bool)
run (Computer p acc tape)
  | p==n = (acc, True)  -- off the end of the tape so HALT
  | b    = (acc, False) -- already visited this instruction so HALT
  | otherwise = case ins of
                  -- Add to accumulator, move to next instruction, mark instruction as visited
                  Acc x -> run (Computer (p+1) (acc+x) $ tape // [(p, (ins, True))])
                  Jmp x -> run (Computer (p+x) acc $ tape // [(p, (ins, True))])
                  Nop _ -> run (Computer (p+1) acc $ tape // [(p, (ins, True))])
  where
    (ins, b) = tape!p
    n = length tape
