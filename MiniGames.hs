module MiniGames where

import StdLib

game_2_exp n = mapM (\(f,e) -> print e >> f >>= print.(\n -> n == (2^e)) >> print (2^e)) $ take 10 $ zip (cycle [readLn]) [n..]

--digits of num compared
--VERY simple implementation
game_dig_cmp = (mapM (\c -> getChar >>= print . (==c)) . show)

edigits = game_dig_cmp . floor . (r_exp 2000 1 *) . (10^)

