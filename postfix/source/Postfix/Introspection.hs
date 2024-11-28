{-# LANGUAGE LambdaCase #-}

-- |
-- TemplateHaskell functions for introspecting wrapper types.
module Postfix.Introspection where

import Language.Haskell.TH

-- | Given the name of a single-constructor single-field type, find the type
-- that it wraps.
wrapped :: Name -> Q Name
wrapped name =
  reify name >>= \case
    TyConI (DataD _ _ _ _ [NormalC _ [(_, ConT t)]] _) -> pure t
    TyConI (DataD _ _ _ _ [RecC _ [(_, _, ConT t)]] _) -> pure t
    TyConI (NewtypeD _ _ _ _ (NormalC _ [(_, ConT t)]) _) -> pure t
    TyConI (NewtypeD _ _ _ _ (RecC _ [(_, _, ConT t)]) _) -> pure t
    _ -> fail (show name ++ " is not a single-field single-constructor type.")

-- | Given the name of a single-constructor single-field type, find the
-- value-level name of its constructor.
constructor :: Name -> Q Name
constructor name =
  reify name >>= \case
    TyConI (DataD _ _ _ _ [NormalC n [(_, _)]] _) -> pure n
    TyConI (DataD _ _ _ _ [RecC n [(_, _, _)]] _) -> pure n
    TyConI (NewtypeD _ _ _ _ (NormalC n [(_, _)]) _) -> pure n
    TyConI (NewtypeD _ _ _ _ (RecC n [(_, _, _)]) _) -> pure n
    _ -> fail (show name ++ " is not a single-field single-constructor type.")
