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


{- visibleRowSplit ix xs = leftV || rightV
    where   (x_pre, x:x_post) = splitAt ix xs
            leftV = not (null x_pre) && (x > maximum x_pre)
            rightV = not (null x_post) && (x > maximum x_post) -}