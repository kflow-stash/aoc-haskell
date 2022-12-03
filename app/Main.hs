

import qualified Day1
import qualified Day2
import qualified Day3


import System.Environment (getArgs)
import Data.Maybe (fromJust)


dispatch = [
    ("Day1", Day1.main),
    ("Day2", Day2.main),
    ("Day3", Day3.main)]

main = do
    --args <- getArgs
    --let day = head args
    --print $ "starting " ++ day
    print "Enter Day Number to Run:"
    day <- getLine
    let mainFn = fromJust $ lookup day dispatch
    mainFn