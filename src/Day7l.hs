module Day7l where


import Utils ( getLines, splitOnStr )


-- This is an attempt to use Loeb to sort out the bags in bag...
-- TODO


type Colour = String

type Bag = ((Int, Colour), [(Int, Colour)])


parse :: String -> Bag
parse s = ((1, col), parseBag <$> bs)
  where
    ps = splitOnStr " bags contain " s
    col = head ps
    bs = splitOnStr ", " $ ps!!1
    parseBag :: String -> (Int, Colour)
    parseBag s = (n, col)
      where
        n = read $ take 1 s
        col = unwords $ take 2 $ words $ drop 2 s



day7l :: IO ()
day7l = do
  ls <- getLines 7
  let bs = parse <$> ls
  --putStrLn $ "Day7: part1: " ++ show (length $ filter (doesItNeed bs) bs)
  --putStrLn $ "Day7: part2: " ++ show (howManyDoesItNeed bs (head $ filter (\(Bag x _) -> x == "shiny gold") bs)-1)
  return ()

