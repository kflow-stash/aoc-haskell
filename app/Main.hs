

import qualified Day1
import qualified Day2a
import qualified Day2b

import System.Environment (getArgs)
import Data.Maybe (fromJust)


dispatch = [
    ("Day1", Day1.main),
    ("Day2a", Day2a.main),
    ("Day2b", Day2a.main)]

main = do
    --args <- getArgs
    --let day = head args
    --print $ "starting " ++ day
    print "Enter Day Number to Run:"
    day <- getLine
    let mainFn = fromJust $ lookup day dispatch
    mainFn