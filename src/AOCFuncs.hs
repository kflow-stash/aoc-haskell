module AOCFuncs
    ( readInt, wordsWhen
    ) where

readInt :: String -> Int
readInt = read

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'