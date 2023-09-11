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

import Ast
import Lam qualified

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
            output = lam ["x"] (ref "x")
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
            output = lam ["x", "y"] (ref "x")
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test parsing flip" do
        let
            in1 = "fn x y = fn y x"
            output =
                lam
                    ["fn", "x", "y"]
                    (app (app (ref "fn") (ref "y")) (ref "x"))
        parse Lam.expr "" in1 `shouldParse` output

    it "Test Parsing Application" do
        let
            in1 = "(x y = x) (z = z)"
            in2 = "(x= (y = x)) (z= z)"
            in3 :: Text
            in3 =
                [__i|
                  -- Application
                  (x y = x)
                    z = z
                |]
            output = app (lam ["x", "y"] (ref "x")) (lam ["z"] (ref "z"))
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test binding infix operators" do
        let
            -- current = "\\ x y = (x \\) y"
            -- desired = "\\ x y = (\\) x y"
            in1 = "(\\ x y = x \\ y) ((fn a b = fn b a) (a b = a b))"
            out = app interface (app flip dollar)

            interface = lam ["\\", "x", "y"] (app (app (ref "x") (ref "\\")) (ref "y"))
            flip = lam ["fn", "a", "b"] (app (app (ref "fn") (ref "b")) (ref "a"))
            dollar = lam ["a", "b"] (app (ref "a") (ref "b"))

            nf =
                -- ???
                -- \ x y . x |> (\a b -> b |> a) |> y
                lam ["x", "y"] $
                    ref "x"
                        `app` ( lam ["a", "b"] $
                                    ref "b" `app` ref "a"
                              )
                        `app` (ref "y")
        parse Lam.expr "" in1 `shouldParse` out
        (Ast.nf <$> parse Lam.expr "" in1) `shouldParse` nf

-- it "parses a Tuple.lam" do
--     result <- Lib.lam "test/Tuple.lam"
--     result `shouldParse` (lam ["a", ])
