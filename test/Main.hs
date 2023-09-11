import Relude

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty qualified

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

import Data.String.Interpolate
import Text.Megaparsec

import Lam
import Lib qualified

main :: IO ()
main = do
    test <- testSpec "pipes" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel do
    it "Test Parsing Identity" do
        let
            in1 = "x =  x"
            in2 = "(x= x)"
            in3 :: Text
            in3 =
                [__i|
                  -- Identity
                  (x= x)
                |]
            output = mkLams [mkVar "x"] (mkVal "x")
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test Parsing Const" do
        let
            in1 = "x y =  x"
            in2 = "(x = (y = x))"
            in3 :: Text
            in3 =
                [__i|
                  -- Const
                  (x= (y = x))
                |]
            output = mkLams [mkVar "x", mkVar "y"] (mkVal "x")
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test parsing flip" do
        let
            in1 = "fn x y = fn y x"
            output =
                mkLams
                    [mkVar "fn", mkVar "x", mkVar "y"]
                    (mkApp (mkVal "fn") (mkApp (mkVal "y") (mkVal "x")))
        parse Lam.expr "" in1 `shouldParse` output

    it "Test Parsing Application" do
        let
            in1 = "(x y = x) z = z"
            in2 = "(x= (y = x)) (z= z)"
            in3 :: Text
            in3 =
                [__i|
                  -- Application
                  x y = x
                    z = z
                |]
            output = mkApp (mkLams [mkVar "x", mkVar "y"] (mkVal "x")) (mkLams [mkVar "z"] (mkVal "z"))
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

-- it "Test binding infix operators" do
--   let
--     in1 = "(\\ x y = x \\ y) (a b = a b)"
--     flipFnApp = "(a b = b a)"
--     out = mkApp (mkLams [mkVar "\\", mkVar "x", mkVar "y"] (mkApp (mkVar "x"))) (mkVal "y")
--   parse Lam.expr "" in1 `shouldParse` out

-- it "parses a Tuple.lam" do
--     result <- Lib.lam "test/Tuple.lam"
--     result `shouldParse` (mkLams ["a", ])
