module Tool where

--this needs more special libs

import Math.OEIS
import Data.List


oeis_lookup seq= putStrLn $ maybe "NOT FOUND" id $ lookupSequence seq >>=
        (\s -> return $ head (catalogNums s) ++ " ("
                ++ concat (intersperse " / " (tail (catalogNums s))) ++ ")\n"
                ++ description s ++"\n"
                ++ (show.sequenceData) s
        )

