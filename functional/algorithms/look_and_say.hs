{- The look-and-say sequence:
	1
	11 (prev is "one 1")
	21 (prev is "two 1s")
	1211 (prev is "one 2; one 1")
	111221 (prev is "one 1; one 2; two 1s")
	312211 (prev is "three 1s; two 2s; one 1")
	etc.
-}

import qualified Data.List

next_look_and_say :: [Int] -> [Int]
next_look_and_say
    = concatMap (\l -> [length l, head l])
    . Data.List.group

look_and_say_sequence :: [[Int]]
look_and_say_sequence = iterate next_look_and_say [1]
