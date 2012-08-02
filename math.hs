module Math where
import MathLib

-- this doesn't import whats imported there, so let's list it
--

import Prime
import Pi
import Polynom
import Numeric
import StdLib
import Const
import Data.Ratio
import Data.List
import Graph
import Visual

--comment this out, if you don't have the libs
import Tool

import qualified Data.Map as Map

{-
 -
 - some main ideas ;)
 -
 -}

--simple tests/benchmarks/fun outputs

--main = putStr (showHex (bbp_seq 2000) "" )
--main = is_prime fat_prime
--main = mapM (\n -> putStrLn $ show n ) $ take 1000000 primes

-- n i c e
--main = mapM (putStrLn.show) $ map (\n -> fpf $ (big_prime 12)+n) [1..]
