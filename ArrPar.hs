module ArrPar where

import Control.Arrow
import Control.Concurrent

import System.Cmd
import Text.Printf

--eventually makes noise in the output stream because of the returned thread ids
par_run_ret = mapM forkIO 


