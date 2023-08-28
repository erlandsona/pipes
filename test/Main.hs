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

    it "Test Parsing Main.lam" do
        result <- Lib.lam "test/Main.lam"
        result `shouldParse` App (Lam (Var "x") (Var "x")) (Lam (Var "x") (Lam (Var "y") (Var "x")))
