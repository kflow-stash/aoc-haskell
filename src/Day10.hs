module Day10 (main) where

import System.IO 
import AOCFuncs
import Data.List (intercalate)
import Data.List.Split

getSignal n cycles = (sum (take (n-2) cycles) + 1) * n

getSignalVal n cycles = if (n_mod>=signal_val) && (n_mod<=(signal_val + 2)) then '#' else '.'
    where   n_mod = if n `mod` 40 == 0 then 40 else n `mod` 40
            signal_val = sum (take (n-2) cycles) + 1


main = do  
    contents <- readFile "data/day10.txt"
    let input_ = map words $ lines contents
        cycles = concat [if length xs > 1 then [readInt $ last xs,0] else [0]| xs <- input_]

        x_20 = getSignal 20 cycles
        x_60 = getSignal 60 cycles
        x_100 = getSignal 100 cycles
        x_140 = getSignal 140 cycles
        x_180 = getSignal 180 cycles
        x_220 = getSignal 220 cycles

        part1 = sum [x_20,x_60,x_100,x_140,x_180,x_220]

        signals = map (`getSignalVal` cycles) [1..240]
        signal_rows = intercalate "\n" $ chunksOf 40 signals

    writeFile "data/day10_out.txt" signal_rows

    print part1
