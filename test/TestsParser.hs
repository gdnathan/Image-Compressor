module TestsParser where

import File.Parser (parse, Point (..), Short (..), Color (..), Pixel (..))
import File.Lexer (FileToken (..))
import Test.Hspec ( describe, it, shouldThrow, Spec ) 
import Control.Exception (evaluate)
import Error (CompressorException (..)) 

instance Eq CompressorException where
    (==) (ParsingException _) (ParsingException _) = True
    (==) (LexingException _) (LexingException _) = True
    (==) (ArgumentException _) (ArgumentException _) = True
    (==) _ _ = False

instance Eq Pixel where
    (==) (Pixel (Point a1 b1) color1) (Pixel (Point a2 b2) color2) = a1 == a2 && b1 == b2 && color1 == color2

parserSpec :: Spec
parserSpec = describe "Parser" $ do
    it "Valid format" $ do 
        parse [[OPEN, Value "1", COMMA, Value "2", CLOSE, SPACE, OPEN, Value "1", COMMA, Value "2", COMMA, Value "3", CLOSE]] == [Pixel (Point 1 2) (Color (Short 1) (Short 2) (Short 3))]
    it "empty" $ do
        parse [[]] == []
    it "Invalid" $ do
        evaluate (parse [[OPEN, Value "1", OPEN]]) `shouldThrow` (==ParsingException "")