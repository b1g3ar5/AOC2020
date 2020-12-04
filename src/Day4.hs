module Day4 where


import Utils (getLines)
import Data.List.Split (splitOn)
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


readField :: String -> Field
readField "byr" = BYR
readField "cid" = CID
readField "ecl" = ECL 
readField "eyr" = EYR
readField "hcl" = HCL
readField "hgt" = HGT
readField "iyr" = IYR
readField "pid" = PID


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


split1 :: (Eq a) => a -> [a] -> ([a], [a])
split1 ch = go []
  where
    go acc (x:xs)
      | x == ch = (acc, xs)
      | otherwise = go (acc++[x]) xs


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
getPassportData ss = (first readField <$>) . (split1 ':' <$>) . words . unwords <$> splitOn [""] ss
