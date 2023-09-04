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

import Text.Megaparsec
import Data.String.Interpolate

import Calc
import Lam
import Lib qualified

main :: IO ()
main = do
    test <- testSpec "pipes" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel do
    it "Test Parsing Main.calc" do
        result <- Lib.calc "test/Main.calc"
        result
            `shouldParse` do
                Or
                    ( Add
                        (Nat 123)
                        ( Eq
                            (Mul (Nat 1) (Nat 5))
                            (Boo True)
                        )
                    )
                    (Eq (Nat 4) (Nat 5))

    it "Test Parsing Identity" do
        let
          in1 = "\\ x . x"
          in2 = "(\\x. x)"
          in3 :: Text
          in3 = [__i|
            -- Identity
            (\\x. x)
          |]
          output = Lam (Var "x") (Var "x")
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test Parsing Const" do
        let
          in1 = "\\ x. (\\ y. x)"
          in2 = "(\\x. (\\y . x))"
          in3 :: Text
          in3 = [__i|
            -- Const
            (\\x. (\\y . x))
          |]
          output = Lam (Var "x") (Lam (Var "y") (Var "x"))
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test Parsing Application" do
        let
          in1 = "(\\ x. (\\ y. x)) (\\z. z)"
          in2 = "(\\x. (\\y . x)) (\\z. z)"
          in3 :: Text
          in3 = [__i|
            -- Application
            (\\x. (\\y . x)) (\\z. z)
          |]
          output = App (Lam (Var "x") (Lam (Var "y") (Var "x"))) (Lam (Var "z") (Var "z"))
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output


    -- it "parses a Main.lam" do
    --     result <- Lib.lam "test/Main.lam"
    --     result `shouldParse` Lam (Var "x") (Var "y")
