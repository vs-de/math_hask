import Network.HTTP
import Text.HTML.TagSoup
import List
import StdLib
import StdNetLib

data ResultEntry = ResultEntry {
    result_id :: Integer,
    result_title :: String,
    result_excerpt :: String,
    result_link :: String
}
    deriving (Show)

type SearchResult = [ResultEntry]


-- gooogle related
-- result struct is:
-- <li class="g">
--      <h3 class="r">
--      <div class="s">

protocol_string = "http://"
ggl_search_string = "/search?q="
ggl_host = "www.google.com"

-- yo, we go on

ggl_search' = fetchUrl . (++) (protocol_string++ggl_host++ggl_search_string) . urlEncode

-- small proof of concept here
ggl_first_hit_url str = ggl_search' str >>= return . fromAttrib "href" . head . drop 1 . dropWhile ((/= "r") .fromAttrib "class") . filter (isTagOpen) . parseTags
--

-- now something cleaner
ggl_search str = ggl_search' str >>= 
    return . pack_results . zip [1..] . drop 1 . (\tags -> 
        splitAt' tags $ map toInteger $ findIndices (flip has_all_criteria [is_li, has_class "g"]) tags
    ) . parseTags
    where 
        pack_results = map (\(a, b) -> 
                ResultEntry {
                    result_id = a,
                    result_link = fromAttrib "href" $ head $ dropWhile (not.is_a) b,
                    result_title = concatMap fromTagText $ filter isTagText $ takeWhile (not.isTagCloseName "h3") b,
                    result_excerpt = concatMap fromTagText $ filter isTagText $ takeWhile (not.isTagOpenName "span") $ drop 1 $dropWhile (not.isTagOpenName "div") b
                }
            )

ggl_show_search = (mapM (\res -> putStrLn $ show (result_id res) ++ ") \t" ++ show (result_title res) ++ "\t\t[" ++ take 40 (show (result_link res)) ++ "]") =<<) . ggl_search

