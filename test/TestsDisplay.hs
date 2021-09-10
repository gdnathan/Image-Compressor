module TestsDisplay where

import Test.Hspec ( context, describe, it, Spec )
import Display (display)

-- contexte ca fais quoi

-- et comment jsuis censé tester un output?

displaySpec :: Spec
displaySpec = describe "Test Display module:" $ do
    it "display good cluster" $ do
        "bite" == "bite"