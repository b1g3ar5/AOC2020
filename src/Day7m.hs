{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com
<https://adventofcode.com/2020/day/7>
The problem gives us a list of rules about the immediate contents
of each color of bag. We use this to compute the tnrasitive
closure of bag contents in order to answer queries about a shiny
gold bag.
-}
module Day7m where


import Control.Applicative (many, some, optional, (<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec (Parsec, sepBy1, parse )
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (letterChar)
import Text.Megaparsec.Error (errorBundlePretty)
import Data.Void ( Void )
import Data.List ( foldl' )

import Utils ( getRaw )


löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap


möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f = \x -> let go = f ($ go) x in go


type Bag = String
type Rule = (Bag, [(Int, Bag)])


type Parser = Parsec Void String


bag :: Parser Bag
bag = some letterChar <> " " <> some letterChar <* " bag" <* optional "s"

bags :: Parser (Int, Bag)
bags = (,) <$> decimal <* " " <*> bag

bagss :: Parser [(Int, Bag)]
bagss = [] <$ "no other bags" <|> bags `sepBy1` ", "

rule :: Parser Rule
rule = (,) <$> bag <* " contain " <*> bagss <* ".\n"

------------------------------------------------------------------------

day7m :: IO ()
day7m =
  do rules <- getParsedInput (many rule)
     let tc = transClosBags rules
     print (count (Map.member "shiny gold") tc)
     print (sum (tc Map.! "shiny gold"))


count :: Foldable t => (a -> Bool) -> t a -> Int
count p = foldl' (\acc x -> if p x then acc+1 else acc) 0


transClosBags :: [Rule] -> Map Bag (Map Bag Int)
transClosBags rules = löb (expand <$> Map.fromList rules)


-- This puts the bag itself in the list of bags and then ...
expand :: [(Int,Bag)] -> Map Bag (Map Bag Int) -> Map Bag Int
expand inside tc = Map.unionsWith (+) [(n*) <$> Map.insertWith (+) b 1 (tc Map.! b) | (n,b) <- inside]


getParsedInput :: Parser a -> IO a
getParsedInput p =
  do input <- getRaw 7
     case parse p "input" input of
       Left e -> fail (errorBundlePretty e)
       Right a -> return a    