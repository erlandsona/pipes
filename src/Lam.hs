module Lam where

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
    = Var Text
    | Lam Expr Expr
    | App Expr Expr
    deriving (Show, Eq)

expr :: Parser Expr
expr = app

app :: Parser Expr
app = do
    left <- lam
    rightOpt <- optional $ char ' ' *> expr
    case rightOpt of
        Nothing -> pure left
        Just right ->
            pure $ App left right

lam :: Parser Expr
lam = (Lam <$> between (symbol '\\') (symbol '.') var <*> value) <|> value

value :: Parser Expr
value = parens expr <|> var

var :: Parser Expr
var = Var <$> var'

var' :: Parser Text
var' =
    lexeme (T.cons <$> alphaNumChar <*> takeWhileP Nothing isAlphaNum <?> "variable")

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

backslash :: Parser Text
backslash = symbol '\\'

slash :: Parser Text
slash = symbol '/'

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
