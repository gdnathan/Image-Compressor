module File.Lexer (FileToken (..), tokenize, getDigit) where

import Control.Exception (throw)
import Error (CompressorException (LexingException))
import Data.Char (isDigit)

data FileToken = OPEN | CLOSE | SPACE | COMMA | Value String

tokenize :: [String] -> [[FileToken]]
tokenize = map tokenizeLine

tokenizeLine :: String -> [FileToken]
tokenizeLine ('(':rest) = OPEN  : tokenizeLine rest
tokenizeLine (')':rest) = CLOSE : tokenizeLine rest
tokenizeLine (',':rest) = COMMA : tokenizeLine rest
tokenizeLine (' ':rest) = SPACE : tokenizeLine rest
tokenizeLine []         = []
tokenizeLine line@(char:_)
    | isDigit char      = let (digit, rest) = getDigit line in Value digit : tokenizeLine rest
    | otherwise         = throw $ LexingException "Invalid line format"

getDigit :: String -> (String, String)
getDigit line@(char:rest)
    | isDigit char = let (nbrs, end) = getDigit rest in (char : nbrs, end)
    | otherwise    = ([], line)
getDigit _         = ([], [])
