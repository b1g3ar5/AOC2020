module Day19 where


import Utils hiding (toInt)
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!), fromList, keys, elems)
import Control.Monad (sequence)
import System.TimeIt


-- Does part1 using loeb! Too slow for part 2 - also the loops don't help


type Message = String

type Irule  = Map Int [Int] -> [Int]


toInt :: Message -> Int
toInt s = foldl' (\ acc i -> i + 3 * acc) 0 is
  where
    ctoi 'a' = 1
    ctoi 'b' = 2
    is = ctoi <$> s


len3 :: Int -> Int
len3 0 = 0
len3 x = if x < 3 then 1 else 1 + len3 (x `div` 3)


-- concatting for the messages converted to ints....
iconcat :: [Int] -> Int
iconcat is = foldl' (\acc i -> (3 ^ len3 acc) * i + acc) 0 $ reverse is 


mkIrule :: [[Int]] -> Irule
mkIrule iss = \mp -> concat $ (\r -> r mp) . go <$> iss
  where
    go :: [Int] -> Map Int [Int] -> [Int]
    go is = \mp -> (iconcat <$>) <$> sequence $ (mp!) <$> is


parseIrule :: String -> (Int, Irule)
parseIrule s = (ix, go $ p!!1) 
  where
    p = splitOn ":" s
    ix :: Int
    ix = read $ head p

    go :: String -> Irule
    go s
      | s == " \"a\"" = const [1]
      | s == " \"b\"" = const [2]
      | otherwise = mkIrule $ (read <$>) . words <$> q
          where
            q = splitOn "|" s


loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x


day19 :: IO ()
day19 = do
  ls1 <- getLines 19
  let pieces = splitOn [""] ls1
      ruleLs = head pieces
      testStrings = pieces!!1
      irules1 :: Map Int Irule
      irules1 = fromList $ parseIrule <$> ruleLs
      imapOfStrings = loeb irules1
      iallStrings = concat $ elems imapOfStrings
      itestStrings = toInt <$> pieces!!1

  timeIt $ putStrLn $ "Day19: " ++ show (length $ filter id $ (`elem` iallStrings) <$> itestStrings)
