module Lam where

import Relude hiding (many, show, some, takeWhile)

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
lam = lamsP <*> vars <*> value <|> value
    where
        vars :: Parser [Var]
        vars = many varP <* "\\"

value :: Parser Expr
value = "(" *> app <* ")" <|> valP

nameP :: Parser Text
nameP = lexeme (alphaNumChar <:> takeWhileP Nothing isAlphaNum <?> "variable")

(<:>) :: Parser Char -> Parser Text -> Parser Text
(<:>) = liftA2 T.cons

-- module :: Parser Expr
-- module = lexeme (upperChar <:> takeWhileP NOthing isAlphaNum <?> "module")
-- newtype Module = Module {unModule :: Expr}

type Var = U.Name Expr

data Expr
    = Val Var
    | Lam (U.Bind Var Expr)
    | App Expr Expr
    deriving (Generic, Show)

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
nfd e@(Val _) = pure e
nfd (Lam e) = do
    (x, e') <- U.unbind e
    e1 <- nfd e'
    pure $ Lam (U.bind x e1)
nfd (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            nfd (U.subst x a b')
        _ -> App <$> nfd f' <*> nfd a

whnf :: Expr -> U.FreshM Expr
whnf e@(Val _) = pure e
whnf e@(Lam _) = pure e
whnf (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            whnf (U.subst x a b')
        _ -> pure $ App f' a

-- Add Smart Constructors
mkVar :: Text -> Var
mkVar = U.s2n . T.unpack

varP :: Parser Var
varP = mkVar <$> nameP

mkVal :: Text -> Expr
mkVal = Val . mkVar

valP :: Parser Expr
valP = mkVal <$> nameP

mkLams :: [Var] -> Expr -> Expr
mkLams = flip (foldr mkLam)

mkLam :: Var -> Expr -> Expr
mkLam name = Lam . U.bind name

lamsP :: Parser ([Var] -> Expr -> Expr)
lamsP = pure mkLams

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

-- Whitespace consuming primitives
lexeme :: Parser a -> Parser a
lexeme = L.lexeme commentableSpace

text :: Text -> Parser Text
text = L.symbol commentableSpace

keyword :: Text -> Parser ()
keyword t = text t *> notFollowedBy alphaNumChar

keywords :: [String]
keywords = []

commentableSpace :: Parser ()
commentableSpace =
    L.space
        space1
        (L.skipLineComment "--")
        (L.skipBlockComment "---" "---")
