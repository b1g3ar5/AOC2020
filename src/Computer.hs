

module Computer where


import Data.Vector (Vector, (//), (!))


data Inst = Acc Int 
          | Jmp Int 
          | Nop Int deriving (Show, Eq)


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


-- A Tape is a set of instructions
type Tape = Vector (Inst, Bool)

-- The memory has a position, a register and an instruction set
-- which records whether eash instruction has been executed
data Comp = Comp { pos :: Int
                 , reg :: Int
                 , instructions :: Tape
                 } deriving (Show)


update :: Tape -> (Int, (Inst, Bool)) -> Tape
update tape (pos, ins) = tape // [(pos, ins)]


run :: Comp -> (Int, Bool)
run (Comp pos acc tape)
  | pos==n = (acc, True)  -- off the end of the tape so HALT
  | b    = (acc, False) -- already visited this instruction so HALT
  | otherwise = case ins of
                  -- Add to accumulator, move to next instruction, mark instruction as visited
                  Acc x -> run (Comp (pos+1) (acc+x) $ tape `update` (pos, (ins, True)))
                  Jmp x -> run (Comp (pos+x) acc     $ tape `update` (pos, (ins, True)))
                  Nop _ -> run (Comp (pos+1) acc     $ tape `update` (pos, (ins, True)))
  where
    (ins, b) = tape!pos
    n = length tape
