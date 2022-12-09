

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

import System.Environment (getArgs)
import Data.Maybe (fromJust)

import qualified AOCFuncs


dispatch = [
    ("Day1", Day1.main),
    ("Day2", Day2.main),
    ("Day3", Day3.main),
    ("Day4", Day4.main),
    ("Day5", Day5.main),
    ("Day6", Day6.main),
    ("Day7", Day7.main)]

main = do
    --args <- getArgs
    --let day = head args
    --print $ "starting " ++ day
    print "Enter Day Number to Run:"
    day <- getLine
    let mainFn = fromJust $ lookup day dispatch
    mainFn