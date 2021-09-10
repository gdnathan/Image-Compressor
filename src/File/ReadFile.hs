module File.ReadFile (getPixelsFromFile) where

import File.Lexer (tokenize)
import File.Parser (Pixel, parse)

getPixelsFromFile :: String -> IO [Pixel]
getPixelsFromFile file = parsePixelsFromFileContent <$> readFile file

parsePixelsFromFileContent :: String -> [Pixel]
parsePixelsFromFileContent = parse . tokenize . lines
