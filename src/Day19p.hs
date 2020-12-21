module Day19p where


import Utils hiding (toInt)
import Data.List
import qualified Data.Map as M
import Data.Map (Map, (!), fromList, keys, elems)
import Control.Monad (sequence, void)
import System.TimeIt
import Text.Parsec
import Text.Parsec.String
import Data.Either (isRight)


-- | New approach - make string parsers out of each rule and then try to parse
-- each string with each parser to see if any work. Much better approach!
-- Now the second part is easy because we just make 2 new parsers from the rule change


data Rule = A | B | And Rule Rule | Or Rule Rule | Get Int deriving (Show, Eq)


-- | Parse the rule strings and make a Rule
parseRule :: String -> (Int, Rule)
parseRule s 
  | head nss == "\"a\"" = (ix, A)
  | head nss == "\"b\"" = (ix, B)
  | otherwise = (ix, pss)
    where
      ix = read $ init ns
      (ns:nss) = words s
      pss :: Rule
      pss = foldl1 Or $ foldl1 And . (Get . read <$>) <$> splitOnChar "|" nss


-- | Make a rule map from all the rule strings
mkRuleMap :: [String] -> Map Int Rule
mkRuleMap ls = fromList $ parseRule <$> ls


-- | From a ruleMap and a rule make a string parser
-- we can use this to see if the strings are legit
messageParser :: Map Int Rule -> Rule -> Parser ()
messageParser mp A = void $ char 'a'
messageParser mp B = void $ char 'b'
messageParser mp (And x y) = messageParser mp x >> messageParser mp y
messageParser mp (Or x y) = try (messageParser mp x) <|> messageParser mp y
messageParser mp (Get ix) = messageParser mp (mp ! ix)


day19p :: IO ()
day19p = do
  ls <- getLines 19
  let ruleMap :: Map Int Rule
      ruleMap = mkRuleMap $ take 128 ls

      messages :: [String]
      messages = drop 129 ls

      -- Make a parse out of the ruleMap
      messageP :: Parser ()
      messageP = messageParser ruleMap (Get 0)

      check :: String -> Bool
      check s = isRight $ parse (messageP >> eof) "" s

  putStrLn $ "Day19p: part1: " ++ show ( length $ filter id $ check <$> messages)

    
  let p42, p31, p :: Parser ()
      p42 = messageParser ruleMap $ ruleMap ! 42
      p31 = messageParser ruleMap $ ruleMap ! 31
      p = do
        r42 <- many1 $ try p42
        r31 <- many1 p31
        if length r42 > length r31 then return () else fail "nope"

      check2 :: String -> Bool
      check2 s = isRight $ parse (p >> eof) "" s

  putStrLn $ "Day19p: part2: " ++ show ( length $ filter id $ check2 <$> messages)

  return ()