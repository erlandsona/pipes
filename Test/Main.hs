import Relude

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import Test.Tasty qualified

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Test.Hspec
-- import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

import Text.Gigaparsec
import Data.String.Interpolate

import Lam qualified
import Ast

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
          in3 = [__i|
            -- Identity
            (x= x)
          |]
          output = lams [ name "x" ] (ref "x")
        parse Lam.expr in1 `shouldParse` (output)
        parse Lam.expr in2 `shouldParse` (output)
        parse Lam.expr in3 `shouldParse` (output)

    it "Test Parsing Const" do
        let
          in1 = "x y =  x"
          in2 = "(x = (y = x))"
          in3 = [__i|
            -- Const
            (x= (y = x))
          |]
          output = lams [ name "x", name "y" ] (ref "x")
        parse Lam.expr in1 `shouldParse` (output)
        parse Lam.expr in2 `shouldParse` (output)
        parse Lam.expr in3 `shouldParse` (output)

    it "Test Parsing Application" do
        let
          in1 = "(x y = x) z = z"
          in2 = "(x= (y = x)) (z= z)"
          in3 = [__i|
            -- Application
            x y = x
              z = z
          |]
          output = app (lams [ name "x", name "y" ] (ref "x")) (lams [ name "z" ] (ref "z"))
        parse Lam.expr in1 `shouldParse` (output)
        parse Lam.expr in2 `shouldParse` (output)
        parse Lam.expr in3 `shouldParse` (output)

    it "Test parsing flip" do
      let
        in1 = "fn x y = fn y x"
        output = lams [name "fn", name "x" , name "y"]
          (app (ref "fn") (app (ref "y") (ref "x")))
      parse Lam.expr in1 `shouldParse` (output)

    -- TODO: Work out flip above.
    -- it "Test binding infix operators" do
    --   let
    --     in1 =
    --       [__i|
    --         (\\ x y = x \\ y)
    --           -- flip ($) aka: (&)
    --           ((fn a b = fn b a) (fn a = fn a))
    --       |]
    --     out = app (lams [name "\\", name "x", name "y"] (app (ref "x"))) (ref "y")
    --   parse Lam.expr in1 `shouldParse` out


shouldParse
    :: (Show a, Eq a) =>
  -- | Result of parsing as returned by function like 'parse'
  Result a ->
  -- | Desired result
  a ->
  Expectation
r `shouldParse` v = case r of
  Failure ->
    expectationFailure $ "expected: " ++ show v
  Success x -> x `shouldBe` v
