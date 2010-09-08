module PlayGround where

import StdLib
import Prime
import Numeric
import Ratio
import List
import Char
import Random

show32 num = showIntAtBase 32 (\n -> if (n<10) then chr(0x30+n) else chr(ord('A')+n-10)) num ""

goat_game = 
    randomRIO (1,3) >>= --should be clear
        (\w -> putStr "Choose a Gate: " >>
            readLn >>= (\c -> 
                putStr ("(HINT: "++(show w)++")you took gate [" ++ (show (c::Integer)) ++ "]...\n") >>
                sleep 0.8 >> 
                putStr "i show you what's in gate number [" >>
                (((\lst -> randomRIO (0,length lst-1) >>= (return.((!!) lst))).filter (\n -> n/=w && n/=c)) [1,2,3]) >>= (\o ->
                    putStr ((show o) ++ "]: ") >>
                    sleep 0.8 >>
                    putStr ("it's a goat!\n Do you want to change your gate? (Y/N)\n> ") >>
                    getChar >>= (\c2 -> 
                        if (toUpper c2) == 'Y' then
                            putStr $ "\nyou changed from ["++(show c)++"] to [" ++ (show((head.(filter (\n -> n/=c && n/=o))) [1,2,3]))
                        else
                            putStr $ "you stay at [" ++ (show c)
                        ) >>
                    putStrLn "]"
                )
            )
        )

