module Day25 where


import Numeric.Natural (Natural)

doorKey :: Integer
doorKey = 1327981 --17807724
cardKey :: Integer
cardKey = 2822615 --5764801


modn :: Natural
modn = 20201227


findLoopSize :: Integer -> Integer -> Integer
findLoopSize mul target = go 0 1 
  where
    go n x
      | x == target = n
      | otherwise = go (n+1) ((x*mul) `mod` 20201227)


transform :: Integer -> Integer  -> Integer 
transform mul loopSize = go 1 mul
  where
    go n x
      | n == loopSize = x
      | otherwise = go (n+1) ((x*mul) `mod` 20201227)



day25 :: IO ()
day25 = do
  let subjectNumber = 7
      divider = 20201227
      doorLoops = findLoopSize 7 doorKey
      cardLoops = findLoopSize 7 cardKey

  putStrLn $ "Day25: part1: " ++ show (doorLoops, cardLoops)
  putStrLn $ "Day25: part2: " ++ show (transform doorKey cardLoops)
  putStrLn $ "Day25: part2: " ++ show (transform cardKey doorLoops)

  return ()


