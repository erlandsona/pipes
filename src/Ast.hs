module Ast where

import Relude hiding (many, show, some, takeWhile)

import Control.Monad.Combinators.Expr
import Data.Char
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Show

type Parser = Parsec Void Text

data Expr
    = -- Terms
      Var Text
    | Boo Bool
    | Nat Word
    | Eq Expr Expr
    | -- Bool Operations
      Neg Expr
    | And Expr Expr
    | Or Expr Expr
    | -- Elementary Algebra
      Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    deriving (Show, Eq)

-- instance (Show) => Show (Expr) where
--     show = \case
--         -- Terms
--         Var x -> show x
--         Boo x -> show x
--         Nat x -> show x
--         -- Operations
--         Eq x y -> [i|#{x} == #{y}|]
--         Neg x -> [i|-#{x}|]
--         And x y -> [i|#{x} && #{y}|]
--         Or x y -> [i|#{x} || #{y}|]
--         Add x y -> [i|#{x} + #{y}|]
--         Sub x y -> [i|#{x} - #{y}|]
--         Mul x y -> [i|#{x} * #{y}|]
--         Div x y -> [i|#{x} / #{y}|]

expr :: Parser Expr
expr =
    makeExprParser terms operators
    where
        terms :: Parser Expr
        terms =
            choice
                [ parens expr
                , -- , Var <$> var
                  Nat <$> nat
                , Boo <$> boo
                ]
        operators :: [[Operator Parser Expr]]
        operators =
            [
                [ prefix '-' Neg
                ]
            , -- , [suffix "++" (+ 1)]

                [ infix' '*' Mul
                , infix' '/' Div
                , infix' '&' And
                , infix' '|' Or
                , infix' '=' Eq
                ]
            ,
                [ infix' '+' Add
                , infix' '-' Sub
                ]
            ]

-- var :: Parser Text
-- var =
--     lexeme (T.cons <$> alphaNumChar <*> takeWhileP Nothing isAlphaNum <?> "variable")

-- -- 0000123 is technically valid :thinking:
nat :: Parser Word
nat = fromIntegral @Integer <$> lexeme L.decimal

boo :: Parser Bool
boo = true <|> false
    where
        true = True <$ string "true"
        false = False <$ string "false"

-- Affixes: pre-fix (before stuff), inf-fix (middle stuff), suf-fix (after stuff)

prefix :: Char -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ symbol name)

suffix :: Char -> (a -> a) -> Operator Parser a
suffix name f = Postfix (f <$ symbol name)

-- Parens implied to the right
infixr' :: Char -> (a -> a -> a) -> Operator Parser a
infixr' name f = InfixR (f <$ symbol name)

-- Parens implied to the left
infixl' :: Char -> (a -> a -> a) -> Operator Parser a
infixl' name f = InfixL (f <$ symbol name)

-- Non-Associative
infix' :: Char -> (a -> a -> a) -> Operator Parser a
infix' name f = InfixN (f <$ symbol name)

ternary :: Char -> Char -> (a -> a -> a -> a) -> Operator Parser a
ternary if' else' f =
    TernR ((f <$ symbol if') <$ symbol else')

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')

braces :: Parser a -> Parser a
braces = between (symbol '{') (symbol '}')

angles :: Parser a -> Parser a
angles = between (symbol '<') (symbol '>')

brackets :: Parser a -> Parser a
brackets = between (symbol '[') (symbol ']')

semicolon :: Parser Text
semicolon = symbol ';'

comma :: Parser Text
comma = symbol ','

colon :: Parser Text
colon = symbol ':'

pipe :: Parser Text
pipe = symbol '|'

elbow :: Parser Text
elbow = symbol '\\'

dot :: Parser Text
dot = symbol '.'

eq :: Parser Text
eq = symbol '='

-- Whitespace consuming primitives
lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Char -> Parser Text
symbol = L.symbol space . T.singleton

-- data Expr where
--   Var :: Text -> Expr
--   Lam :: (Bind Expr) -> Expr
--   App :: Expr -> Expr -> Expr
--   deriving (Eq, Generic)

-- instance VarC Expr where
--   var = Var
--   isvar (Var v) = Just v
--   isvar _ = Nothing

-- instance FreeVarsC Expr
-- instance SubstC Expr Expr
