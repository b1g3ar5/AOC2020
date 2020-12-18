{-# LANGUAGE FlexibleContexts #-}

module Day18p where


import Utils ( getRaw )
import Text.Parsec.Expr
import Text.Parsec.Char
import qualified Text.Parsec as P


type Parser = P.Parsec String ()

-- Doing the whole thing using Parsec's buildExpressionParser

parse :: P.Parsec String () a -> String -> Either P.ParseError a
parse p = P.parse p ""

parselist :: P.Parsec String () b -> [String] -> [b]
parselist p = either (error . show) id . mapM (parse p)


integer :: Parser Int
integer = read <$> P.many1 digit


expr, term, paren :: Parser Int
expr = buildExpressionParser table term
term = paren P.<|> integer
paren = char '(' *> expr <* char ')'

table = [[Infix (char '+' >> return (+)) AssocLeft], [Infix (char '*' >> return (*)) AssocLeft]]

day18p :: IO ()
day18p = do
  s <- getRaw 18
  let part2 = sum . parselist expr . lines . filter (/=' ') $ s
  putStrLn $ "day18p: part2: " ++ show part2
  return ()
