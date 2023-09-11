module Lam where

import Relude hiding (many, show, some, takeWhile)

import Data.Char
import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

import Ast (Expr)
import Ast qualified

expr :: Parsec Void Text Expr
expr = toParsec $ fully app

-- parseTerm :: Parser Expr
-- parseTerm = chainl1 parseNonApp $ do
--   space
--   pos <- getPosition
--   return $ TmApp (infoFrom pos)

app :: Parser Expr
app = chainl1 parseNonApp (pure Ast.app)

parseNonApp :: Parser Expr
parseNonApp =
    try ("(" *> app <* ")") -- (M)
        <|> try (lam app)
        <|> varP -- x

lam :: Parser Expr -> Parser Expr
lam body = lamsP <*> vars <*> body
    where
        vars :: Parser [Text]
        vars = many nameP <* "="

infixl1 :: (a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = (wrap <$> p) <**> rest
    where
        rest =
            flip (.)
                <$> (flip <$> op <*> p)
                <*> rest
                    <|> pure id

infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainl1 = infixl1 id

chainr1 :: Parser b -> Parser (b -> b -> b) -> Parser b
chainr1 = infixr1 id

nameP :: Parser Text
nameP =
    lexeme (satisfy availableChars <:> takeWhileP Nothing availableChars <?> "variable")

(<:>) :: Parser Char -> Parser Text -> Parser Text
(<:>) = liftA2 T.cons

keyChars :: Text
keyChars = "= ()"

availableChars :: Char -> Bool
availableChars c = isPrint c && not (T.elem c keyChars) && not (isSpace c)

varP :: Parser Expr
varP = Ast.var <$> nameP

lamsP :: Parser ([Text] -> Expr -> Expr)
lamsP = pure Ast.lam

appP :: Parser (Expr -> Expr -> Expr)
appP = pure Ast.app

-- Wrapper to disambiguate the OverloadedStrings IsString instance below.
newtype Parser a = Parser {toParsec :: Parsec Void Text a}
    deriving
        ( Monad
        , Alternative
        , Applicative
        , Functor
        , MonadParsec Void Text
        , MonadParsecDbg Void Text
        , MonadPlus
        )

instance u ~ () => IsString (Parser u) where
    fromString str
        | str `elem` keywords = void $ keyword (T.pack str)
        | otherwise = void $ text (T.pack str)

fully :: Parser a -> Parser a
fully p = commentableSpace *> p <* eof

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
