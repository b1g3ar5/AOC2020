module Utils where


getRaw :: Int -> IO String
getRaw n = readFile $ "./Data/Day" ++ show n ++ ".txt"


getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- getRaw n
  return $ f s


getWords = getF words
getLines = getF lines



