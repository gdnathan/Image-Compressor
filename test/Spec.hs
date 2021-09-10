import Test.Hspec ( hspec )
import Control.Exception (evaluate)
import TestsDisplay ( displaySpec )
import TestsLexer ( lexerSpec )

main :: IO ()
main = hspec $ do
  displaySpec 
