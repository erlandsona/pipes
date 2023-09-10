module Lib where

import Relude

import Data.Text ()
import Data.Void ()
import Text.Megaparsec

import Lam qualified

type Errs = ParseErrorBundle Text Void

lam :: FilePath -> IO (Either Errs Lam.Expr)
lam file = do
    contents <- decodeUtf8 <$> readFileBS file
    pure $ parse Lam.expr file contents
