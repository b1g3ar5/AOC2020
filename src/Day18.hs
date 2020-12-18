module Day18 where


import Utils ( getLines )
import Data.Bifunctor ( Bifunctor(first) )

day18 :: IO ()
day18 = do
  ls <- getLines 18
  let xs1 = eval . fst . parseUntil parse1 '\n' . (++ "\n") <$> ls
      xs2 = eval . fst . parseUntil parse2 '\n' . (++ "\n") <$> ls
      xs3 = eval . fst . parseUntil parse3 '\n' . (++ "\n") <$> ls
  putStrLn $ "Day18: part1 - l-r precedence: " ++ show (sum xs1)
  putStrLn $ "Day18: part2 - add precedence: " ++ show (sum xs2)
  putStrLn $ "Day18: part3 - mul precedence: " ++ show (sum xs3)


data Expr = Empty -- So we can start the parser?!
          | Val Int 
          | Add Expr Expr
          | Mul Expr Expr
          | Rt Expr -- To put round expressions in parentheses to stop add injection
          deriving (Show, Eq)


eval :: Expr -> Int
eval Empty = error "Empty in final expression"
eval (Rt e) = eval e
eval (Val x) = x
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y


isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']


parseUntil :: ((Expr, String) -> (Expr, String)) -> Char -> String -> (Expr, String)
parseUntil pr ch = go Empty
  where
    go :: Expr -> String -> (Expr, String)
    go acc [] = (acc, "")
    go acc (x:xs) | x == ch = (Rt acc, xs)
    go e s = go pe ps
      where
        (pe, ps) = pr (e, s)


-- Parse left to right - with parentheses
parse1 :: (Expr, String) -> (Expr, String)
parse1 (e, ' ':more) = parse1 (e, more)
parse1 (e, '+':more) = first (Add e) $ parse1 (Empty, more)
parse1 (e, '*':more) = first (Mul e) $ parse1 (Empty, more)
parse1 (e, s@(num:more)) | isDigit num = first Val $ parseNumber s
parse1 (e, '(':more) = parseUntil parse1 ')' more
parse1 (e, ')':more) = (e, more)
parse1 (e, s) = error ("Unexpected input in parse1: '" ++ s ++ "'")


-- Give Add precedence by injecting into a previous Mul
injectAdd :: Expr -> Expr -> Expr
injectAdd Empty e = e
injectAdd (Mul a b) e = Mul a (Add b e)
injectAdd a b = Add a b


parse2 :: (Expr, String) -> (Expr, String)
parse2 (e, ' ':more) = parse2 (e, more)
parse2 (e, '+':more) = first (injectAdd e) $ parse2 (Empty, more)
parse2 (e, '*':more) = first (Mul e) $ parse2 (Empty, more)
parse2 (e, s@(num:more)) | isDigit num = first Val $ parseNumber s
parse2 (e, '(':more) = parseUntil parse2 ')' more
parse2 (e, ')':more) = (e, more)
parse2 (e, s) = error ("unexpected input: " ++ s)


injectMul :: Expr -> Expr -> Expr
injectMul Empty e = e
injectMul (Add a b) e = Add a (Mul b e)
injectMul a b = Mul a b


parse3 :: (Expr, String) -> (Expr, String)
parse3 (e, ' ':more) = parse3 (e, more)
parse3 (e, '+':more) = first (Add e) $ parse3 (Empty, more)
parse3 (e, '*':more) = first (injectMul e) $ parse3 (Empty, more)
parse3 (e, s@(num:more)) | isDigit num = first Val $ parseNumber s
parse3 (e, '(':more) = parseUntil parse3 ')' more
parse3 (e, ')':more) = (e, more)
parse3 (e, s) = error ("unexpected input: " ++ s)


parseNumber :: Read a => String -> (a, String)
parseNumber s = (read $ takeWhile (\c -> c /=' ' && c /=')') s, dropWhile (\c -> c /=' ' && c /=')') s)


