module Lib where

import Relude

import Data.Text qualified as T
import Data.Void
import Text.Megaparsec

import Ast

go :: FilePath -> IO (Either (ParseErrorBundle Text Void) Expr)
go file = do
    contents <- decodeUtf8 <$> readFileBS file
    pure $ parse expr file contents
