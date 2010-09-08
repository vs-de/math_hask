module RBT where
import Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString as BS
import qualified Data.CryptoHash.MD5 as MD5
import qualified Data.CryptoHash.SHA1 as SHA1
import qualified Data.Map as Map

import StdLib

import Char
import Numeric
--import Bits

type Chain = (ByteString, ByteString)

data RBT = RBT {
  name :: String,
  table :: [Chain],
  iterations :: Int,
  hash_func :: (ByteString -> ByteString),
  reduct_func :: (ByteString -> ByteString)
}

instance Show RBT where
  show (RBT n tab it hsh red) = "RBT " ++ n ++ ":" ++ " " ++ show it ++ " " ++ show tab

rbt = RBT

gen_entry' hsh reduct len init = last $ take len $ iterate (reduct.hsh) init
gen_entry (RBT _ _ len hsh reduct) init = gen_entry' hsh reduct len init

-- just a testing reduction
reduct_0 = BS.take 8 . BS.drop 2

gen_table name hsh rdc len inits = RBT {
    name = name,
    table = map (\init -> (init, gen_entry' hsh rdc len init)) inits,
    iterations = len,
    hash_func = hsh,
    reduct_func = rdc
}

-- build a very simple MD5 table
mini_md5 = gen_table "MD5_simple" MD5.hash reduct_0 10000 $ Prelude.map (pack.(:[])) [0..255]
--beautified list output
mini_md5' = map (\(key, hsh) -> bs2hex hsh) (table mini_md5)

--hash_lookup(RBT name table cnt hsh_func red_func) hsh = Map.fromList (table RBT)

-- some general tasks
slice_file filename chunk_size = BS.readFile filename >>= return . flip slice_bs chunk_size

slice_bs bs chunk_size = map (\n -> ((bs2hex . BS.take chunk_size. BS.drop n) bs)) [0..(BS.length bs)-chunk_size]

