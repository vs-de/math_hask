module Parse where

import Text.ParserCombinators.Parsec
import Monad
import Text.ParserCombinators.Parsec.Token 
-- as PT
import Text.ParserCombinators.Parsec.Language




emptyTP = makeTokenParser emptyDef

testLex = (lexeme emptyTP) (braces emptyTP (symbol emptyTP "/"))

testParse a = getLine >>= parseTest a

--from parsec doc, there with do notation
nesting = 
    (
        (char '(') >> nesting >>= (\i -> 
            char ')' >> (liftM (+i) nesting) >>= (\k ->
                return (max (i+1) k)
            )
        )
    ) <|> return 0


-- try some php style shit

data PHPTextData = NonPHP String |
                PHPCode String

  deriving (Show)


php_start = string "<?php"
php_end = string "?>"

notPHP = NonPHP

php_code = php_start >>  manyTill anyChar (try php_end) >>= return . PHPCode

-- php_code = between php_start php_end (try $ many anyChar) >>= return . PHPCode

php_out_parse = try php_code <|> (anyChar >>= (\a -> return $ notPHP [a]))


 -----------------
 -- simple ruby --
 -----------------

ruby_lang = emptyDef {
  commentStart = "=begin",
  commentEnd = "=end",
  commentLine = "#",
  nestedComments = False,
  caseSensitive = True,
  reservedNames = ["class", "module", "def", "end"]
}

-- ----------- --
-- back to trs --
-- ----------- --

data Rule = Rule (String, String)
  deriving (Show)

rule = char '<' >> many1 letter >>= (\t1 -> skipMany space >> char ',' >> skipMany space >> many1 letter >>= (\t2 -> return (Rule (t1,t2))))

f_syms = letter



