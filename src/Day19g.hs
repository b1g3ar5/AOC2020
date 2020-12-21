{-# Language OverloadedStrings #-}

module Day19g  where

import Utils
import Control.Applicative
import Data.Foldable (asum, traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Text.ParserCombinators.ReadP  as R
import Text.Megaparsec (parseMaybe, setInput, anySingle, satisfy, parse, Parsec, eof, sepBy, endBy, sepBy1, endBy1, manyTill)
import Data.Void
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char (newline, letterChar)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.List


loeb :: Functor f => f (f a -> a) -> f a
loeb = moeb fmap


moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
--moeb f x = let go = f ($ go) x in go
moeb f x = go where go = f ($ go) x


spaces :: Parser ()
spaces = " " *> spaces <|> pure ()


-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc+1 else acc) 0


getParsedInput :: Int -> Parser a -> IO a
getParsedInput i p =
  do input <- getRaw i
     case parse p "input" input of
       Left e -> fail (errorBundlePretty e)
       Right a -> return a


type Parser = Parsec Void String


-- | Rules either match a literal string, or match a sum of product of sub-rules.
-- These are the rules as defined in the problem
type Rule = Either String [[Int]]


-- | Lexeme parser that trims trailing spaces
l :: Parser a -> Parser a
l p = p <* spaces


-- | List of rules followed by examples
format :: Parser ([(Int,Rule)], [String])
format =
  (,) <$> endBy rule "\n"
      <*  "\n"
      <*> endBy (many letterChar) "\n"


-- | Number-prefixed 'rhs'
rule :: Parser (Int, Rule)
rule = (,) <$> decimal <* l ":" <*> rhs


-- | Matches 'literal' or 'alts'
rhs :: Parser Rule
rhs = Left <$> literal <|> Right <$> alts


-- | Matches @|@ separated 'seqs': @1 2 | 3 4@
alts :: Parser [[Int]]
alts = seqs `sepBy` l "|"


-- | Matches lists of integers: @1 2 3 4@
seqs :: Parser [Int]
seqs = many (l decimal)


-- | Matches string literals: @"example"@
literal :: Parser String
literal = "\"" *> many letterChar <* "\""


day19g :: IO ()
day19g =
  do (rs,ws) <- getParsedInput 19 format

     let rules1 = IntMap.fromList rs
         rules2 = IntMap.insert  8 (Right [[42   ],[42, 8   ]])
                $  IntMap.insert 11 (Right [[42,31],[42,11,31]]) rules1

     putStrLn $  "Day19g: part1: " ++ show (run rules1 ws)
     putStrLn $  "Day19g: part2: " ++ show (run rules2 ws)


-- run :: parse rules * input strings -> number of matching strings
run :: IntMap Rule -> [String] -> Int
run rules = count (not . null . R.readP_to_S parser)
  where
    rpr :: IntMap (IntMap (R.ReadP ()) -> R.ReadP ())
    rpr = ruleParser <$> rules

    rp :: IntMap (R.ReadP ())
    rp = loeb rpr

    parser :: R.ReadP ()
    parser = rp IntMap.! 0 *> R.eof

    ruleParser :: Rule -> IntMap (R.ReadP ()) -> R.ReadP ()
    ruleParser (Left  s  ) _   = () <$ R.string s
    ruleParser (Right xss) sub = asum [traverse_ (sub IntMap.!) xs | xs <- xss]