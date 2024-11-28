{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Splices for generating aliases for postfix constructors.
module Postfix.Aliases where

import Control.Monad (guard)
import Data.Char (toLower)
import Data.List (uncons, unsnoc)
import Language.Haskell.TH qualified as TH
import Postfix.Introspection (constructor, wrapped)

-- | Make a lowercase pattern synonym for a constructor.
lowercase :: TH.Name -> TH.DecsQ
lowercase name = do
  uppercase <- constructor name

  case makeLowercaseName name of
    Just prepared -> do
      let inner :: TH.Q TH.Type
          inner = wrapped name >>= TH.conT

      let outer :: TH.Q TH.Type
          outer = TH.conT name
      
      signature <- fmap (TH.SigD prepared) [t| $inner -> $outer |]
      statement <- [d| $(TH.varP prepared) = $(TH.conE uppercase) |]

      pure (signature : statement)

    Nothing -> pure []

-- | Make a singular pattern synonym for a constructor.
singular :: TH.Name -> TH.DecsQ
singular name = do
  original <- constructor name

  case makeSingularName original of
    Just prepared -> do
      let inner :: TH.Q TH.Type
          inner = wrapped name >>= TH.conT

      let outer :: TH.Q TH.Type
          outer = TH.conT name

      let variable :: TH.Name
          variable = TH.mkName "__inner"

      signature <- fmap (TH.PatSynSigD prepared) [t| $inner -> $outer |]
      statement <- TH.patSynD prepared (TH.prefixPatSyn [variable]) TH.implBidir do
        TH.conP original [TH.varP variable]

      pure [signature, statement]

    Nothing -> pure []

-- | Make a lowercase, singular pattern synonym for a constructor.
lowercaseSingular :: TH.Name -> TH.DecsQ
lowercaseSingular name = do
  uppercase <- constructor name

  case makeSingularName uppercase >>= makeLowercaseName of
    Just prepared -> do
      let inner :: TH.Q TH.Type
          inner = wrapped name >>= TH.conT

      let outer :: TH.Q TH.Type
          outer = TH.conT name
      
      signature <- fmap (TH.SigD prepared) [t| $inner -> $outer |]
      statement <- [d| $(TH.varP prepared) = $(TH.conE uppercase) |]

      pure (signature : statement)

    Nothing -> pure []

-- | Make the first letter of a 'TH.Name' lowercase. Returns 'Nothing' for
-- empty names.
makeLowercaseName :: TH.Name -> Maybe TH.Name
makeLowercaseName = fmap go . uncons . TH.nameBase
  where go (c, cs) = TH.mkName (toLower c : cs)

-- | Make a name singular by removing the 's' on the end. Fails for names that
-- don't end with an 's'. This could be improved a lot.
makeSingularName :: TH.Name -> Maybe TH.Name
makeSingularName name = unsnoc (TH.nameBase name) >>= go
  where go (cs, c) = TH.mkName cs <$ guard (c == 's')
