module Day2 where


import Utils (getLines, span)


data Rule = Rule { mn ::Int, mx ::  Int, ch :: Char, pwd :: String} deriving (Eq, Show)


readRule :: String -> Rule
readRule s = Rule (read smn) (read smx) ch pwd
  where
    ws = words s
    ch = head $ ws!!1
    pwd = ws!!2
    (smn, smx) = span (=='-') $ head ws
    

valid1 :: Rule -> Bool
valid1 (Rule mn mx ch pwd) = (n>=mn) && (n<=mx)
  where
    n = length $ filter (==ch) pwd


valid2 :: Rule -> Bool
valid2 (Rule mn mx ch pwd) = p1 /= p2
  where
    p1 = pwd !! (mn-1) == ch
    p2 = pwd !! (mx-1) == ch


day2 :: IO ()
day2 = do
  ls <- getLines 2
  let rs = readRule <$> ls
      n1 = length $ filter id $ valid1 <$> rs
      n2 = length $ filter id $ valid2 <$> rs
  putStrLn $ "Day2: part1: " ++ show n1
  putStrLn $ "Day2: part2: " ++ show n2
  return ()
