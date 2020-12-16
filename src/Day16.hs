module Day16 where


import Utils ( getLines, splitOnStr )
import Data.List ( intersect, sortOn, transpose )
import Data.Bifunctor (second)


-- Numbers in the ticket that break all the rules
notValid :: [Rule] -> Ticket -> [Int]
notValid rs t = foldr1 intersect $ (t `breakers`) <$> rs


-- Numbers in the ticket that break a rule
breakers :: Ticket -> Rule -> [Int]
breakers ns (_, (l1, h1), (l2, h2)) = filter (\n -> (n<l1 || n >h1) && (n<l2 || n>h2)) ns


-- Rules that the numbers don't break
possibleRules :: [Rule] -> [Int] -> [Rule]
possibleRules rs ns = filter (null . breakers ns) rs


-- Given a list of the rules that might work in each position
-- allocate all the rules to a position that they work in
allocate :: [[Rule]] -> [(Int, Rule)]
allocate rs = second head <$> go [] sorted
  where
    -- Sort according to the number of rules that work
    sorted = sortOn (\(_,r) -> length r) $ zip [0..] rs
    -- For each rule set take the first rule and add it to the 
    -- accumulated list, delete it from the sets remaining and loop
    go acc [] = acc
    go acc (r:rs) = go (acc ++ [r]) $ second (filter (/= f)) <$> rs
      where
        f = head $ snd r



day16 :: IO ()
day16 = do
  ls <- getLines 16
  let (rules, myTicket, tickets) = parse ls
      goodTickets = filter (null . notValid rules) tickets
      -- The set of numbers for each field
      numbers = transpose goodTickets
      -- The rules that are not broken by each set
      possibles :: [[Rule]]
      possibles = possibleRules rules <$> numbers
      -- Rules allocated to fields
      allocation :: [(Int, Rule)]
      allocation = allocate possibles
      -- The "departure" field indexes
      departureFieldIxs = fst <$> filter (\(_, (n,_,_)) -> "departure" == take 9 n) allocation

  putStrLn $ "Day16: part1: " ++ show (sum $ concat $ notValid rules <$> tickets)
  putStrLn $ "Day16: part2: " ++ show (product $ (myTicket!!) <$> departureFieldIxs)


-- Parsing...

type Range = (Int, Int)
type Rule = (String, Range, Range)
type Ticket = [Int]

parse :: [String] -> ([Rule], Ticket, [Ticket])
parse ls = (parseRule <$> head gps, parseTicket $ (gps!!1)!!1, parseTicket <$> tail (gps!!2))
  where
    gps = splitOnStr [""] ls


parseTicket :: String -> Ticket
parseTicket s = read <$> splitOnStr "," s

parseRule :: String -> Rule
parseRule s = (head pieces, parseRange $ head rgs, parseRange $ rgs!!1)
  where
    pieces = splitOnStr ":" s
    rgs = splitOnStr " or " $ pieces!!1    


parseRange :: String -> Range
parseRange s = (read $ head pieces, read $ pieces!!1)
  where
    pieces = splitOnStr "-" s    

