module Day4p where


import Utils (getRaw)
import Data.Map.Strict (Map, (!), fromList)
import Data.Void ( Void )
import Text.Megaparsec ( oneOf, runParser, single, choice, manyTill, Parsec, MonadParsec(eof), chunk, ParseErrorBundle)


day4p :: IO ()
day4p = do
  inString <- getRaw 4
  let ps :: [PassportData]
      ps = either (const []) id $ runP parsePassports inString
  let valids1 = filter valid1 ps
  let valids2 = filter valid2 ps
  putStrLn $ "Day4p: part1: " ++ show (length valids1)
  putStrLn $ "Day4p: part2: " ++ show (length valids2)


type PassportData = [(Field, String)]


valid1 :: PassportData -> Bool
valid1 fs
  | length fs == 8 = True
  | length fs == 7 && CID `notElem` (fst <$> fs) = True
  | otherwise = False


valid2 :: PassportData -> Bool
valid2 fs = valid1 fs && and ((\(fld, dat) -> (validatorMap ! fld) dat ) <$> fs)
  where
    validatorMap :: Map Field (String -> Bool)
    validatorMap = fromList [(BYR, byrOK), (CID, const True) , (ECL, eclOK), (EYR, eyrOK), (HCL, hclOK), (HGT, hgtOK), (IYR, iyrOK), (PID, pidOK)]

      
runP :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
runP p = runParser p ""


type Parser = Parsec Void String


parsePassports :: Parser [PassportData]
parsePassports = manyTill parsePassport eof


parsePassport :: Parser PassportData
parsePassport = manyTill parseFieldWithValue $ single '\n'


parseFieldWithValue :: Parser (Field, String)
parseFieldWithValue = do
  f <- choice parseF 
  single ':'
  s <- manyTill (oneOf "#abcdefghijklmnopqrstuvwxyz0132456789") $ oneOf "\n " -- note, space of newline
  return (f, s)


parseF :: [Parser Field]
parseF = (\s -> chunk s >> return (read s))  <$> ["byr", "cid", "ecl", "eyr", "hcl", "hgt", "iyr", "pid"]


data Field = BYR | CID | ECL | EYR | HCL | HGT | IYR | PID deriving (Eq, Show, Ord)


instance Read Field where
  readsPrec _ s 
    | t == "byr" = [(BYR, d)]
    | t == "cid" = [(CID, d)]
    | t == "ecl" = [(ECL, d)]
    | t == "eyr" = [(EYR, d)]
    | t == "hcl" = [(HCL, d)]
    | t == "hgt" = [(HGT, d)]
    | t == "iyr" = [(IYR, d)]
    | t == "pid" = [(PID, d)]
    where
      t = take 3 s
      d = drop 3 s
  readsPrec _ _ = []  


-- Validation ...
hex, dec :: String
hex = "0123456789abcdef"
dec = "0123456789"


boundsCheck :: Int -> Int -> String -> Bool
boundsCheck mn mx s = (n>=mn) && (n<=mx)
  where
    n = read s


byrOK, iyrOK, eyrOK, hgtOK, hclOK, eclOK, pidOK :: String -> Bool
byrOK = boundsCheck 1920 2002
iyrOK = boundsCheck 2010 2020
eyrOK = boundsCheck 2020 2030

hgtOK s
  | u == "cm" = boundsCheck 150 193 n
  | u == "in" = boundsCheck 59 76 n
  | otherwise = False
  where
    len = length s
    u = drop (len-2) s
    n = take (len-2) s

hclOK (c:cs)
  | c /= '#' = False
  | length cs /= 6 = False
  | and $ (`elem` hex) <$> cs = True
  | otherwise = False

eclOK s = s == "amb" || s == "blu" || s == "brn" || s == "gry" || s == "grn" || s == "hzl" || s == "oth"

pidOK s = length s == 9 && and ((`elem` dec) <$> s)
