module Finance where

import qualified Data.Map as Map
import Data.Map as Map (toList, fromList)
import Finance.Quote.Yahoo
import List
import Data.Time
import Char
import Text.Printf
import StdLib
--import Prelude hiding (readFile)
import Data.ByteString as BS (split)
import Text.CSV.ByteString
import Monad

-- yahoo queries

yahoo_fields = fromList
  [     ("c", "change")
  ,     ("g", "day's low")
  ,     ("h", "day's high")
  ,     ("l1", "last Trade")
  ,     ("n", "name")
  ,     ("x", "exchange")
  ,     ("v", "volume")
  ]

-- just to make a simple query
stock_query sym = 
      getQuote [sym] ["n", "l1", "c", "g", "h", "x"] >>=
        (\mb -> return $ maybe "" (\mp -> concatMap (uncurry (\a b -> maybe "??" id (Map.lookup (snd a) yahoo_fields) ++ ": " ++ b ++ "\n")) $ toList mp) mb)

--
-- Options
--

data OptionValues = OV
     {
        
        base :: Double,
        base_name :: String

    }
data Option = 
    Call { values :: OptionValues } | 
    Put {values :: OptionValues}
    
    --deriving (Eq,Show)



--
-- I B A N calculations
--

-- institute identification
type IID = Integer
-- + bank account number
type BAN = Integer
-- = basic bank account number:
type BBAN = (IID, BAN)

-- ("CC", (LEN, BAN_LEN))
iban_sizes = 
    [   ("BE", (16, 9))
    ,   ("DK", (18, 10))
    ,   ("DE", (22, 10))
    ,   ("FI", (18, 8))
    ,   ("GB", (22, 8))
    ,   ("IE", (22, 8))
    ,   ("IS", (26, 8)) -- ?
    ,   ("IT", (27, 12)) -- ?
    ,   ("LU", (20, 12))
    ,   ("NL", (18, 10))
    ,   ("NO", (15, 7))
    ,   ("AT", (20, 11))
    ,   ("PL", (28, 17))
    ,   ("PT", (25, 13))
    ,   ("SE", (21, 12))
    ,   ("CH", (21, 12))
    ,   ("ES", (24, 12))
    ,   ("TR", (26, 16))
    ]

data Account = IBAN {
    country_code :: String,
    checksum :: Integer,
    bban :: BBAN
  }
  deriving (Show)

bban_combine :: String -> BBAN -> Integer
bban_combine cc = (\(iid,ban) -> 10^(maybe (length $ show ban) id (lookup cc iban_sizes >>= (return.snd))) * iid + ban)

iban :: String -> BBAN -> Account
iban cc bban' = IBAN {
    country_code = cc,
    bban = bban',
    checksum = 98 - (((bban_combine cc bban' * 10000 + iban_c2n cc) * 100) `mod` 97)
    }

iban_c2n = sum . map (\(m,v) -> m * (subtract 55 . toInteger . ord . toUpper) v) . zip [100,1]

iban_std_show :: Account -> String
iban_std_show iban = let cc = country_code iban in
    concat $ intersperse " " $ chunks 4 $ printf "%s%02.d%d" cc (checksum iban) (bban_combine cc (bban iban))
  
-- DiBa csv dump parsing
-- this is wrong :( shit csv parser
-- would have to modify sth... too lazy
-- diba_read_depot_rating = (=<<) (return . maybe [] (map $ map (BS.split 59)) . parseCSV) . readFile

-- but i make this instead:

stock_query_from_list_file = (=<<) (mapM stock_query) . liftM lines . readFile
--from_list_file kind = (=<<) (mapM (flip yh_get_quote_field kind)) . liftM lines . readFile
from_list_file kind = (=<<) (sequence.map (uncurry ((>>).putStrLn)).keep_map (flip yh_get_quote_field kind)) . liftM lines . readFile
-- step by step

yh_get_quote_field = (\sym -> liftM (snd.last.maybe ([]) (toList)) . (\b -> getQuote (sym:[]) (b:[])))

quote_watch sym = mapM ((=<<) (putStrLn)) $ repeat (sleep 2 >> yh_get_quote_field sym "l1")
