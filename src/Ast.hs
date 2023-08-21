{-# LANGUAGE DatatypeContexts #-}

module Ast where

import Relude hiding (fmap, many, pure, show, some, (<$>), (<|>))

import Data.Char
import Data.String.Interpolate (i)
import Parsley
import Parsley.Char
import Text.Show

data Exp t where
  -- Terms
  Var :: String -> Exp t
  Boo :: Bool -> Exp Bool
  Nat :: Word -> Exp Word
  Eq :: (Show t) => Exp t -> Exp t -> Exp Bool
  -- Bool Operations
  Neg :: Exp t -> Exp t
  And :: Exp Bool -> Exp Bool -> Exp Bool
  Or :: Exp Bool -> Exp Bool -> Exp Bool
  -- Elementary Algebra
  Add :: Exp Word -> Exp Word -> Exp Word
  Sub :: Exp Word -> Exp Word -> Exp Word
  Mul :: Exp Word -> Exp Word -> Exp Word
  Div :: Exp Word -> Exp Word -> Exp Word

instance (Show t) => Show (Exp t) where
  show = \case
    -- Terms
    Var x -> show x
    Boo x -> show x
    Nat x -> show x
    -- Operations
    Eq x y -> [i|#{x} == #{y}|]
    Neg x -> [i|-#{x}|]
    And x y -> [i|#{x} && #{y}|]
    Or x y -> [i|#{x} || #{y}|]
    Add x y -> [i|#{x} + #{y}|]
    Sub x y -> [i|#{x} - #{y}|]
    Mul x y -> [i|#{x} * #{y}|]
    Div x y -> [i|#{x} / #{y}|]

varParser :: Parser (Exp String)
varParser = varQ <$> (some letter)

-- boolParser = Boo <$> string "true" <|> string "false"

-- -- 0000123 is technically valid :thinking:
-- natParser = pure (_code Nat) <$> some digit

varQ :: WQ (String -> Exp t)
varQ = makeQ Var [||Var||]

-- data Exp where
--   Var :: Text -> Exp
--   Lam :: (Bind Exp) -> Exp
--   App :: Exp -> Exp -> Exp
--   deriving (Eq, Generic)

-- instance VarC Exp where
--   var = Var
--   isvar (Var v) = Just v
--   isvar _ = Nothing

-- instance FreeVarsC Exp
-- instance SubstC Exp Exp
