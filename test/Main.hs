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
    describe "Parsing" do
        syntax
        semantics

syntax :: Spec
syntax = do
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
                lam ["fn", "x", "y"] $
                    ref "fn"
                        `app` ref "y"
                        `app` ref "x"
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
            output =
                lam ["x", "y"] (ref "x")
                    `app` lam ["z"] (ref "z")
        parse Lam.expr "" in1 `shouldParse` output
        parse Lam.expr "" in2 `shouldParse` output
        parse Lam.expr "" in3 `shouldParse` output

    it "Test binding infix operators" do
        let
            -- current = "\\ x y = (x \\) y"
            -- desired = "\\ x y = (\\) x y"
            in1 = "(\\ x y = x \\ y) ((fn a b = fn b a) (a b = a b))"
            out = interface `app` (flip' `app` dollar)

            interface =
                lam ["\\", "x", "y"] $
                    ref "x"
                        `app` ref "\\"
                        `app` ref "y"
            nf' =
                -- ???
                -- \ x y . x |> (\a b -> b |> a) |> y
                lam ["x", "y"] $
                    ref "x"
                        `app` ampersand
                        `app` ref "y"
        parse Lam.expr "" in1 `shouldParse` out
        (Ast.nf <$> parse Lam.expr "" in1) `shouldParse` nf'

semantics :: Spec
semantics = do
    it "Test semantics infix operators" do
        -- current = "\\ x y = (x \\) y"
        -- desired = "\\ x y = (\\) x y"
        let
            notFalse = "(p a b = p b a) (a b = b)"
        -- in1 =
        --     [__i|
        --       ((\\ -- Infix flip application for free?
        --         x y =
        --           x
        --            \\ y
        --         )
        --         (
        --           -- Flip
        --           (fn a b = fn b a)
        --           -- Application
        --           (a b = a b)
        --         )
        --       )
        --       -- not
        --       (p a b = p b a)
        --       -- true
        --       (a b = a)
        --     |]
        -- out = interface `app` (flip' `app` dollar)
        -- interface =
        --     lam ["\\", "x", "y"] $
        --         ref "x"
        --             `app` ref "\\"
        --             `app` ref "y"

        (Ast.nf <$> parse Lam.expr "" notFalse) `shouldParse` true

-- it "parses a Tuple.lam" do
--     result <- Lib.lam "test/Tuple.lam"
--     result `shouldParse` (lam ["a", ])

-- Helpers

flip' :: Expr
flip' =
    lam ["fn", "a", "b"] $
        ref "fn" `app` ref "b" `app` ref "a"

dollar :: Expr
dollar =
    lam ["a", "b"] $
        ref "a" `app` ref "b"

ampersand :: Expr
ampersand =
    lam ["a", "b"] $
        ref "b" `app` ref "a"

true :: Expr
true = lam ["a", "b"] $ ref "a"

false :: Expr
false = lam ["a", "b"] $ ref "b"

not' :: Expr
not' = lam ["p", "a", "b"] $ ref "p" `app` ref "b" `app` ref "a"
