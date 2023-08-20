module Ast where

import Relude hiding (show)

import Data.String.Interpolate (i)
import Text.Show

data Exp t where
  -- Terms
  Var :: String -> Exp t
  Boo :: Bool -> Exp Bool
  Nat :: Word -> Exp Word
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
    Neg x -> [i|-#{x}|]
    And x y -> [i|#{x} && #{y}|]
    Or x y -> [i|#{x} || #{y}|]
    Add x y -> [i|#{x} + #{y}|]
    Sub x y -> [i|#{x} - #{y}|]
    Mul x y -> [i|#{x} * #{y}|]
    Div x y -> [i|#{x} / #{y}|]

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
