module Visual where

import List

--console code nums
ccn_red = 31
ccn_green = 32
ccn_yellow = 33
ccn_bold = 1

cc_reset = "\x1b[0m"

setc_prfx = "\x1b[1;"
setc_psfx = "m"

{-
cc_codes = [
  ("green", 32)
  ("yello", )
]
-}

setc n = setc_prfx ++ show n ++ setc_psfx

cc_green str = setc ccn_green ++ str ++ cc_reset

hl = cc_green

list_show' lst = putStrLn $ "[" ++ (concat . intersperse ",") lst ++ "]"

hl_idx idx lst = list_show' $ map (\(e,i) -> if i == idx then (hl.show) e else show e ) $ zip lst [1..]

hl_elem elem lst = list_show' $ map (\e -> if e == elem then (hl.show) e else show e ) lst

hl_if f lst = list_show' $ map (\e -> if f e then (hl.show) e else show e) lst

