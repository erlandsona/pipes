module Lib where

import Relude

import Data.Text qualified as T
import Parsley (parseFromFile)

import Ast

go :: FilePath -> IO (Maybe (Exp String))
go = $$(parseFromFile varParser)

