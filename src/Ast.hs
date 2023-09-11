module Ast (Expr, Name, var, app, nf, lam, name, ref) where

import Relude

import Unbound.Generics.LocallyNameless qualified as U

data Expr
    = App Expr Expr
    | Lam (U.Bind Name Expr)
    | Var Name
    deriving (Generic, Show)

type Name = U.Name Expr

-- Add Smart Constructors

app :: Expr -> Expr -> Expr
app = App

lam :: [Text] -> Expr -> Expr
lam = flip (foldr (\n -> Lam . U.bind (name n)))

ref :: Text -> Expr
ref = Var . name

var :: Text -> Expr
var = Var . name

name :: Text -> Name
name = U.s2n . toString

instance Eq Expr where
    e1 == e2 = U.aeq e1 e2

-- | With generic programming, the default implementation of Alpha
-- provides alpha-equivalence, open, close, and free variable calculation.
instance U.Alpha Expr where
    {-# SPECIALIZE instance U.Alpha Expr #-}

-- | The subst class uses generic programming to implement capture
-- avoiding substitution. It just needs to know where the variables
-- are.
instance U.Subst Expr Expr where
    {-# SPECIALIZE instance U.Subst Expr Expr #-}
    isvar (Var x) = Just (U.SubstName x)
    isvar _ = Nothing
    {-# INLINE U.isvar #-}

-- Normalization must be done in a freshness monad.
-- We use the one from unbound-generics
nf :: Expr -> Expr
nf = U.runFreshM . nfd

nfd :: Expr -> U.FreshM Expr
nfd e@(Var _) = pure e
nfd (Lam e) = do
    (x, e') <- U.unbind e
    e1 <- nfd e'
    pure $ Lam (U.bind x e1)
nfd (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            nfd (U.subst x a b')
        _ -> App <$> nfd f' <*> nfd a

whnf :: Expr -> U.FreshM Expr
whnf e@(Var _) = pure e
whnf e@(Lam _) = pure e
whnf (App f a) = do
    f' <- whnf f
    case f' of
        Lam b -> do
            (x, b') <- U.unbind b
            whnf (U.subst x a b')
        _ -> pure $ App f' a
