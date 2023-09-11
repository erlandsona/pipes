module Lib where

import Relude

import Data.Text ()
import Data.Void ()
import Text.Gigaparsec

import Ast qualified
import Lam qualified

-- type Errs = ParseErrorBundle Text Void

lam :: FilePath -> IO (Result Ast.Expr)
lam file = do
    contents <- decodeUtf8 <$> readFileBS file
    pure $ parse Lam.expr contents
