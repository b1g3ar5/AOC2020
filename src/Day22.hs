module Day22 where


import Prelude hiding (round)
import Utils ( getParagraphs )
import qualified Data.Sequence as Q
import Data.Sequence (Seq(..), (|>), (><))
import qualified Data.Set as S


-- An opportunity to learn about the Seq type...

type Hand = Seq Int
type HandBank = S.Set (Hand, Hand)


parsePlayer :: [String] -> Hand
parsePlayer ls = Q.fromList $ read <$> tail ls


score :: Hand -> Int
score h = sum $ Q.zipWith (*) (Q.reverse h) $ Q.fromList [1..(length h)]


-- Game1 finishes when a player has no cards
game1 :: (Hand, Hand) -> (Bool, Int)
game1 (Q.Empty, h2) = (False, score h2)
game1 (h1, Q.Empty) = (True, score h1)
game1 (h1, h2) = game1 $ round1 (h1, h2)


-- Each round the highest card wins bot cards
round1 :: (Hand, Hand) -> (Hand, Hand)
round1 (c :<| cs, d :<| ds)
  | c > d = (cs |> c |> d, ds)
  | otherwise = (cs, ds |> d |>c)


-- Game 2 finishes when a player has no cards, or the 
-- position is a repeat of a previous position
game2 :: ((Hand, Hand), HandBank) -> (Bool, Int)
game2 ((Q.Empty, h2), _) = (False, score h2)
game2 ((h1, Q.Empty), _) = (True, score h1)
game2 ((h1, h2), bank)
  | (h1, h2) `S.member` bank = (True, score h1)
  | otherwise = game2 $ round2 ((h1, h2), bank)


-- The hands won't be null because game2 checks this
-- If the cards are low enough to mkae a new game play that 
-- game for the cards, otherwise the same as round1
round2 :: ((Hand, Hand), HandBank) -> ((Hand, Hand), HandBank)
round2 (hs@(c :<| cs, d :<| ds), seen)
  | c <= Q.length cs && d <= Q.length ds = 
      if fst $ game2 ((Q.take c cs, Q.take d ds), S.empty) 
        then cwins
        else dwins
  | c>d = cwins
  | c<d = dwins
  | otherwise = error "This can't happen!"
  where
    next = hs `S.insert` seen
    cwins = ((cs |> c |> d, ds), next)
    dwins = ((cs, ds |> d |> c), next)


day22 :: IO ()
day22 = do
  ps <- getParagraphs 22
  let (p1:p2:_) = parsePlayer <$> ps

  putStrLn $ "Day22: part1: " ++ show (game1 (p1, p2))
  putStrLn $ "Day22: part2: " ++ show (game2 ((p1, p2), S.empty))
 
  return ()

