module Day7l where


import qualified Data.Map as M
import Utils ( getLines, splitOnStr )


-- This is an attempt to use Loeb to sort out the bags in bag...

loeb :: Functor f => f (f a -> a) -> f a
loeb x = go where go = fmap ($ go) x

-- So to use loeb we need an f ( f a -> a)
-- The functor will be a (Map Colour), so we need a

-- Map Colour (Map Colour e -> e) 

-- What can this be - well for each colour in the outer map we could make a list
-- of all the bags it's in

makeSheet :: Bags -> M.Map Colour (M.Map Colour [(Int, Colour)] -> [(Int, Colour)])
makeSheet bs = undefined
  where
    ks = M.keys bs

type Colour = String

type Bag = (Colour, [(Int, Colour)])

type Bags = M.Map Colour [(Int, Colour)]


day7l :: IO ()
day7l = do
  ls <- getLines 7
  let bs :: Bags 
      bs = M.fromList $ parse <$> ls
  putStrLn $ "Day7: part1: " ++ show bs

  return ()


parse :: String -> Bag
parse s = (col, parseBag <$> bs)
  where
    ps = splitOnStr " bags contain " s
    col = head ps
    bs = splitOnStr ", " $ ps!!1
    parseBag :: String -> (Int, Colour)
    parseBag s = (n, col)
      where
        n = read $ take 1 s
        col = unwords $ take 2 $ words $ drop 2 s



