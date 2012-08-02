module Graph where

import StdLib
import Data.Ratio
import Data.List

dot = '#'

flat_values n lst = map (floor.(*(fromInteger n)).(%(maximum lst))) lst



--vis n lst = (((mapM (putStrLn)).(reverse.transpose.(map (\k -> (flip replicate dot) k ++ (flip replicate ' ') (n-k))))) (flat_values n lst))

vis n = (map ((\k -> (flip replicate dot) k ++ (flip replicate ' ') ((fromInteger n)-k)).fromInteger)).(flat_values n)

vis_t = (.) (reverse.transpose).vis

putv = (.) (mapM putStrLn) . vis
putvt = (.) (mapM putStrLn) . vis_t

