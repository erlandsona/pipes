module Lam where

import Relude hiding (many, show, some, takeWhile)

import Control.Monad.Combinators.Expr
import Data.Char
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Unbound.Generics.LocallyNameless qualified as U

expr :: Parsec Void Text Expr
expr = toParsec $ commentableSpace *> app <* eof

app :: Parser Expr
app = try (appP <*> lam <*> app) <|> lam

lam :: Parser Expr
lam = lamP <*> vars <*> value <|> value
    where
        vars :: Parser Var
        vars = "\\" *> varP <* "."

value :: Parser Expr
value = parens app <|> valP

nameP :: Parser Text
nameP = lexeme (alphaNumChar <:> takeWhileP Nothing isAlphaNum <?> "variable")

type Var = U.Name Expr

data Expr
    = Val Var
    | Lam (U.Bind Var Expr)
    | App Expr Expr
    deriving (Show, Generic)

instance Eq Expr where
    e1 == e2 = U.aeq e1 e2

-- | With generic programming, the default implementation of Alpha
-- provides alpha-equivalence, open, close, and free variable calculation.
instance U.Alpha Expr where
    {-# SPECIALIZE instance U.Alpha Expr #-}

-- | The subst class uses generic programming to implement capture
-- avoiding substitution. It just needs to know where the variables
-- are.
instance U.Subst Expr Expr where
    {-# SPECIALIZE instance U.Subst Expr Expr #-}
    isvar (Val x) = Just (U.SubstName x)
    isvar _ = Nothing
    {-# INLINE U.isvar #-}

-- Normalization must be done in a freshness monad.
-- We use the one from unbound-generics
nf :: Expr -> Expr
nf = U.runFreshM . nfd

nfd :: Expr -> U.FreshM Expr
nfd e@(Val _) = return e
nfd (Lam e) =
    do
        (x, e') <- U.unbind e
        e1 <- nfd e'
        return $ Lam (U.bind x e1)
nfd (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            nfd (U.subst x a b')
        _ -> App <$> nfd f' <*> nfd a

whnf :: Expr -> U.FreshM Expr
whnf e@(Val _) = return e
whnf e@(Lam _) = return e
whnf (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            whnf (U.subst x a b')
        _ -> return $ App f' a

-- Add Smart Constructors
mkVar :: Text -> Var
mkVar = U.s2n . T.unpack

varP :: Parser Var
varP = mkVar <$> nameP

mkVal :: Text -> Expr
mkVal = Val . mkVar

valP :: Parser Expr
valP = mkVal <$> nameP

mkLam :: (Var -> Expr -> Expr)
mkLam name e = Lam (U.bind name e)

lamP :: Parser (Var -> Expr -> Expr)
lamP = pure mkLam

mkApp :: (Expr -> Expr -> Expr)
mkApp = App

appP :: Parser (Expr -> Expr -> Expr)
appP = pure mkApp

-- Wrapper to disambiguate the OverloadedStrings IsString instance below.
newtype Parser a = Parser {toParsec :: Parsec Void Text a}
    deriving
        ( Monad
        , Alternative
        , Applicative
        , Functor
        , MonadParsec Void Text
        , MonadPlus
        )

instance u ~ () => IsString (Parser u) where
    fromString str
        | str `elem` keywords = void $ keyword (T.pack str)
        | otherwise = void $ text (T.pack str)

keywords :: [String]
keywords = []

(<:>) :: Parser Char -> Parser Text -> Parser Text
(<:>) = liftA2 T.cons

-- Affixes: pre-fix (before stuff), inf-fix (middle stuff), suf-fix (after stuff)

prefix :: Parser () -> (a -> a) -> Operator Parser a
prefix name f = Prefix (f <$ name)

suffix :: Parser () -> (a -> a) -> Operator Parser a
suffix name f = Postfix (f <$ name)

-- Parens implied to the right
infixr' :: Parser () -> (a -> a -> a) -> Operator Parser a
infixr' name f = InfixR (f <$ name)

-- Parens implied to the left
infixl' :: Parser () -> (a -> a -> a) -> Operator Parser a
infixl' name f = InfixL (f <$ name)

-- Non-Associative
infix' :: Parser () -> (a -> a -> a) -> Operator Parser a
infix' name f = InfixN (f <$ name)

ternary :: Parser () -> Parser () -> (a -> a -> a -> a) -> Operator Parser a
ternary if' else' f =
    TernR (f <$ if' <$ else')

parens :: Parser a -> Parser a
parens p = "(" *> p <* ")"

braces :: Parser a -> Parser a
braces p = "{" *> p <* "}"

angles :: Parser a -> Parser a
angles p = "<" *> p <* ">"

brackets :: Parser a -> Parser a
brackets p = "[" *> p <* "]"

commentableSpace :: Parser ()
commentableSpace =
    L.space
        space1
        (L.skipLineComment "--")
        (L.skipBlockComment "---" "---")

-- Whitespace consuming primitives
lexeme :: Parser a -> Parser a
lexeme = L.lexeme commentableSpace

text :: Text -> Parser Text
text = L.symbol commentableSpace

keyword :: Text -> Parser ()
keyword t = text t *> notFollowedBy alphaNumChar
