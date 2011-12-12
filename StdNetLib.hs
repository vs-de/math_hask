module StdNetLib where

import Network.Socket
import Network.HTTP
import Control.Arrow

import Text.HTML.TagSoup


fetchUrl = (getResponseBody =<<) . simpleHTTP . getRequest
-- std tcp socket
new_std_socket = socket AF_INET Stream defaultProtocol

-- std tcp addr info
std_addr_info host port = getAddrInfo (Just defaultHints) (Just host) (Just port)

std_addr host port = return . addrAddress . head =<< std_addr_info host port
-- std tcp connectios
std_connect host port = std_addr_info host port >>= ( \aInf ->
    new_std_socket >>= (\s -> connect s (addrAddress $ head aInf) >> return s)
  )

-- with direct monadic input -> ...M

connectM msock maddr = msock >>= (\s -> (maddr >>= (\a -> connect s a >> return s)))
sendM msock str = (msock, msock >>= (\s -> return . flip send str))
recvM msock len = msock >>= (\s -> recv s len >>= (\str -> return (s,str)))
closeM msock = msock >>= return . sClose

-- this means send and then recv
simple_query host port str recv_len = std_connect host port >>=
  (\s -> send s str >> recv s recv_len)

-- with arr
simple_queryA host port = (.) (std_connect host port >>=) .
  curry ((flip send) *** (flip recv) >>> (\(s,r) sck -> s sck >> r sck))

-- with arr 2
simple_queryB host port str recv_len = std_connect host port >>=
  (send &&& recv >>> (\(s,r) -> s str >> r recv_len))

-- with arr 3
simple_queryC host port str recv_len = std_connect host port >>=
  (send &&& recv >>> uncurry (\s -> flip ((.)(s str >>) . ($)) recv_len))

-- arrow abstractions
{-
arr_connectM addr = (addr >>= return . flip connect) >>= return . arr
arr_connect addr = arr (flip connect addr)
arr_send str = arr (flip send str)
-}
-- connect with socket

-- test
addr = std_addr "localhost" "4567"

--  H T M L / T a g s o u p - S t u f f  --

-- some short hand funcs
-- tags should be canonicalized first or they will only match downcase ones
href = fromAttrib "href"
src = fromAttrib "src"

is_div = isTagOpenName "div"
is_li = isTagOpenName "li"
is_ul = isTagOpenName "ul"
is_ol = isTagOpenName "ol"
is_a = isTagOpenName "a"

has_attrib_value attr = flip $ (==) . fromAttrib attr
has_class = flip $ (==) . fromAttrib "class"
has_id = flip $ (==) . fromAttrib "id"

