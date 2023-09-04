module Lam where

import Relude hiding (many, show, some, takeWhile)

import Control.Monad.Combinators.Expr
import Data.Char
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

expr :: Parser Expr
expr = commentableSpace *> app <* eof

app :: Parser Expr
app = try (mkApp <*> lam <*> app) <|> lam

lam :: Parser Expr
lam = (mkLam <*> between (symbol '\\') (symbol '.') var <*> value) <|> value

value :: Parser Expr
value = parens app <|> var

var :: Parser Expr
var = mkVar <*> lexeme (alphaNumChar <:> takeWhileP Nothing isAlphaNum <?> "variable")

data Expr
    = Var Text
    | Lam Expr Expr
    | App Expr Expr
    deriving (Show, Eq)

-- Add Smart Constructors
mkVar :: Parser (Text -> Expr)
mkVar = pure Var

mkLam :: Parser (Expr -> Expr -> Expr)
mkLam = pure Lam

mkApp :: Parser (Expr -> Expr -> Expr)
mkApp = pure App

(<:>) :: Parser Char -> Parser Text -> Parser Text
(<:>) = liftA2 T.cons

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

commentableSpace :: Parser ()
commentableSpace =
    L.space
        space1
        (L.skipLineComment "--")
        (L.skipBlockComment "---" "---")

-- Whitespace consuming primitives
lexeme :: Parser a -> Parser a
lexeme = L.lexeme commentableSpace

symbol :: Char -> Parser Text
symbol = L.symbol commentableSpace . T.singleton
