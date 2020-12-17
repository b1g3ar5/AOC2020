module Day4 where


import Utils (getLines, split1, splitOnStr)
import Data.Bifunctor (first)


day4 :: IO ()
day4 = do
  ls <- getLines 4

  let ps = getPassportData ls
  let valids1 = filter valid1 ps
  let valids2 = filter valid2 ps

  putStrLn $ "Day4: part1: " ++ show (length valids1)
  putStrLn $ "Day4: part2: " ++ show (length valids2)


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


data Colour = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Eq, Show, Ord)


instance Read Colour where
  readsPrec _ s 
    | t == "amb" = [(AMB, d)]
    | t == "blu" = [(BLU, d)]
    | t == "brn" = [(BRN, d)]
    | t == "gry" = [(GRY, d)]
    | t == "grn" = [(GRN, d)]
    | t == "hzl" = [(HZL, d)]
    | t == "oth" = [(OTH, d)]
    where
      t = take 3 s
      d = drop 3 s
  readsPrec _ _ = []  


boundsCheck :: Int -> Int -> String -> Bool
boundsCheck mn mx s = (n>=mn) && (n<=mx) 
  where
    n = read s


byrOK, iyrOK, eyrOK :: String -> Bool
byrOK = boundsCheck 1920 2002
iyrOK = boundsCheck 2010 2020
eyrOK = boundsCheck 2020 2030


hgtOK :: String -> Bool
hgtOK s
  | u == "cm" = boundsCheck 150 193 n
  | u == "in" = boundsCheck 59 76 n
  | otherwise = False
  where
    len = length s
    u = drop (len-2) s
    n = take (len-2) s


hclOK :: String -> Bool
hclOK (c:cs)
  | c /= '#' = False
  | length cs /= 6 = False
  | and $ (`elem` hex) <$> cs = True
  | otherwise = False


hex, dec :: String
hex = "0123456789abcdef"
dec = "0123456789"


eclOK :: String -> Bool
eclOK "amb" = True
eclOK "blu" = True 
eclOK "brn" = True 
eclOK "gry" = True 
eclOK "grn" = True 
eclOK "hzl" = True 
eclOK "oth" = True
eclOK _ = False


pidOK :: String -> Bool
pidOK s = length s == 9 && and ((`elem` dec) <$> s)


type PassportData = [(Field, String)]


valid1 :: PassportData -> Bool
valid1 fs
  | length fs == 8 = True
  | length fs == 7 && CID `notElem` (fst <$> fs) = True
  | otherwise = False


valid2 :: PassportData -> Bool
valid2 fs = valid1 fs && and (uncurry validField <$> fs)


validField :: Field -> String -> Bool
validField BYR = byrOK
validField CID = const True
validField ECL = eclOK
validField EYR = eyrOK
validField HCL = hclOK
validField HGT = hgtOK
validField IYR = iyrOK
validField PID = pidOK


getPassportData :: [String] -> [PassportData]
getPassportData ss = (first read <$>) . (split1 ':' <$>) . words . unwords <$> splitOnStr [""] ss
