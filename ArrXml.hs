module ArrXml where

import Data.Maybe
import Text.XML.HXT.Arrow
import Text.XML.HXT.XPath

-- jff
import Network.HTTP

xml2node = runX.readString []

text_from tag = deep (isElem >>> hasName tag) >>> getChildren >>> getText

--xpath: runX $ readDocument [] "test.xml" >>> arr (getXPath "/")

