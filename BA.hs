module BA where -- Binary Analysis

--import Random.Mersenne
import StdLib
import qualified Data.ByteString as BS
import qualified GHC.Word as W
import MTGen
import Bits --hiding rotate
import List

--cmp_funcs :: (Int -> [(W.Word8 -> W.Word8 -> Bool)])
cmp_funcs a 
  | a == 0 = [
          (==)
        , ((==).(+1))
        , ((==).(+(-1)))
        , ((==).(xor 0xff))
        ]
  | a == 1 = [
          ((==).(flip shiftR 1))
        , ((==).(flip shiftR 2))
        , ((==).(flip shiftR 3))
        , ((==).(flip shiftR 4))
        , ((==).(flip shiftR 5))
        , ((==).(flip shiftR 6))
        , ((==).(flip shiftR 7))
        , ((==).(flip shiftR 8)) -- yes, we check for zero here
        ]
   | a == 2 = [
          ((==).(flip shiftL 1))
        , ((==).(flip shiftL 2))
        , ((==).(flip shiftL 3))
        , ((==).(flip shiftL 4))
        , ((==).(flip shiftL 5))
        , ((==).(flip shiftL 6))
        , ((==).(flip shiftL 7))
        , ((==).(.|. 0xff)) -- k, then we check for ff here
        ]
  
  | otherwise = []


bs_every' n bs = let len = BS.length bs in map (BS.index bs) $ takeWhile (< len) [0,n..]
bs_every n = BS.pack . bs_every' n

eval_bs' n = (flip map) (map pair_map (cmp_funcs n)) . (flip apply)
eval_bs n = eval_bs' n . BS.unpack

eval_bs_every n m = eval_bs' n . bs_every' m

show_eval_bs n = mapM (putStrLn.show) . eval_bs n

show_eval_bs_to n bs = mapM (flip show_eval_bs bs) [0..n]

show_eval_bs_short n = mapM (putStrLn.show) . map or . eval_bs n

show_eval_bs_count n = mapM (putStrLn.show.length) . map (findIndices id). eval_bs n

show_eval_bs_count_every n m = mapM (putStrLn.show.length) . map (findIndices id). eval_bs_every n m

