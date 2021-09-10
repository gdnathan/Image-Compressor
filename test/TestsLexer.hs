module TestsLexer (lexerSpec) where

import File.Lexer ( FileToken (..), tokenize, getDigit )
import Test.Hspec ( describe, it, shouldThrow, Spec ) 

import Error (CompressorException (..)) 
import Control.Exception (evaluate)

instance Eq FileToken where
    (==) OPEN OPEN = True
    (==) CLOSE CLOSE = True
    (==) COMMA COMMA = True
    (==) (Value str) (Value str2) = str == str2
    (==) SPACE SPACE = True
    (==) _ _ = False

instance Eq CompressorException where
  (==) (ParsingException _) (ParsingException _) = True
  (==) (LexingException _) (LexingException _) = True
  (==) (ArgumentException _) (ArgumentException _) = True
  (==) _ _ = False


--le truc load jsp si mon test est correcte
--regarde dans testsDisplay aussi
--faut aussi des instances Eq pour les LexingException
-- ouais jsais pas faire d'instance Eq non plus mdr

lexerSpec :: Spec
lexerSpec = describe "Test Lexer module:" $ do
    it "Invalid random line" $ do
        evaluate (tokenize ["jambon"]) `shouldThrow` (== (LexingException ""))
    it "Line with format error" $ do 
        tokenize ["(1,2 (1,2,3)"] == [[OPEN, Value "1", COMMA, Value "2", SPACE, OPEN, Value "1", COMMA, Value "2", COMMA, Value "3", CLOSE]]
    it "Line with invalid digit" $ do 
        evaluate (tokenize ["(1,b) (1,2,3)"]) `shouldThrow` (== (LexingException ""))
    it "good line" $ do 
        tokenize ["(1,2) (1,2,3)"] == [[OPEN, Value "1", COMMA, Value "2", CLOSE, SPACE, OPEN, Value "1", COMMA, Value "2", COMMA, Value "3", CLOSE]]
