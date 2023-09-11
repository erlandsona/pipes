{-# LANGUAGE OverloadedLists #-}

module Lam where

import Relude hiding (many, show, some, takeWhile)

import Data.Char
import Text.Gigaparsec
import Text.Gigaparsec.Char
import Text.Gigaparsec.Errors.Combinator ((<?>))

import Ast (Expr, Name)
import Ast qualified

expr :: Parsec Expr
expr = fully app

app :: Parsec Expr
app = atomic (appP <*> value <*> value) <|> lam

lam :: Parsec Expr
lam = lamsP <*> vars <*> value <|> value
    where
        vars :: Parsec [Name]
        vars = many nameP <* "="

value :: Parsec Expr
value = "(" *> app <* ")" <|> (Ast.var <$> nameP)

appP :: Parsec (Expr -> Expr -> Expr)
appP = pure Ast.app

lamsP :: Parsec ([Name] -> Expr -> Expr)
lamsP = pure Ast.lams

nameP :: Parsec Name
nameP = Ast.name <$> lexeme (satisfy availableChars <:> some (satisfy availableChars) <?> ["variable"])
    where
        availableChars :: Char -> Bool
        availableChars c = isPrint c && notElem c keyChars && not (isSpace c)

        keyChars :: String
        keyChars = "= ()"

instance u ~ () => IsString (Parsec u) where
    fromString str
        | str `elem` keywords = void $ keyword str
        | otherwise = void $ token (string str)

fully :: Parsec a -> Parsec a
fully p = whitespace *> p <* eof

lexeme :: Parsec a -> Parsec a
lexeme p = p <* many whitespace

token :: Parsec a -> Parsec a
token = lexeme . atomic

keyword :: String -> Parsec ()
keyword k = token (string k *> notFollowedBy letterOrDigit)

keywords :: [String]
keywords = []

-- Whitespace consuming primitives
-- lexeme :: Parser a -> Parser a
-- lexeme = L.lexeme commentableSpace

-- text :: Text -> Parser Text
-- text = L.symbol commentableSpace

-- commentableSpace :: Parser ()
-- commentableSpace =
--     space
--         space1
--         (L.skipLineComment "--")
--         (L.skipBlockComment "---" "---")
