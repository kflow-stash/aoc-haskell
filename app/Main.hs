

import qualified Day1

import System.Environment (getArgs)
import Data.Maybe (fromJust)


dispatch = [
    ("Day1", Day1.main)]

main = do
    --args <- getArgs
    --let day = head args
    --print $ "starting " ++ day
    print "Enter Day Number to Run:"
    day <- getLine
    let mainFn = fromJust $ lookup day dispatch
    mainFn