module Lib where

import Relude hiding (fmap, many, pure, some, (<$>), (<|>))

import Data.Char
import Parsley
import Parsley.Char
import Data.Text qualified as T

import Ast

stuff = undefined


varParser :: Parser (Exp String)
varParser = varQ <$> (some letter)

varQ :: WQ (String -> Exp t)
varQ = makeQ Var [||Var||]

-- boolParser = Boo <$> string "true" <|> string "false"

-- -- 0000123 is technically valid :thinking:
-- natParser = pure (_code Nat) <$> some digit
