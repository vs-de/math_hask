module MTGen where

import Bits
import System.Random.Mersenne.Pure64
import Char
import List
import qualified Data.ByteString as BS
import StdLib


mt_str_upto = mt_str 0

mt_str min max =
    map (chr.fromIntegral) $ 
        concatMap (
            (\n ->  map ((.&. 0xff).(shiftR n)) [0,8..56]).(fst.randomWord64.pureMT)
        ) [min..max]


-- new stuff
--just let the machine run ;)
mt_all = (\(a,b) -> [a]++(mt_all b)) . randomWord64


--len is num of 64 bit chunks so for byte count it must be multiplied by 8
mt2bs len = BS.concat. take len . map int2bs . mt_all

--some hard coded old shit from here :(
write_files = 
  mapM 
    (\n -> writeFile ("mt_dump"++show n++"-"++show (n+2^20-1)++".bin") $
      mt_str n (n+2^20-1)
    ) [0,2^20..2^32]

-- FILE GENERATION
dump_file_names start step = map (\n -> "mt_dump"++show n++"-"++show (n+step-1)++".bin") [start, start+step..]

--main = write_files

