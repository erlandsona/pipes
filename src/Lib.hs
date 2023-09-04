module Lib where

import Relude

import Data.Text ()
import Data.Void ()
import Text.Megaparsec

import Calc qualified
import Lam qualified

type Errs = ParseErrorBundle Text Void

calc :: FilePath -> IO (Either Errs Calc.Expr)
calc file = do
    contents <- decodeUtf8 <$> readFileBS file
    pure $ parse Calc.expr file contents

lam :: FilePath -> IO (Either Errs Lam.Expr)
lam file = do
    contents <- decodeUtf8 <$> readFileBS file
    pure $ parse Lam.expr file contents
