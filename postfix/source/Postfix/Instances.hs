{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Splices for generating the postfix instances.
module Postfix.Instances where

import Language.Haskell.TH (DecsQ, Q, Name, Type)
import Language.Haskell.TH qualified as TH

-- | Generate a `Num` instance for a given postfix unit.
num :: Name -> Name -> DecsQ
num source target = do
  let written :: Q Type
      written = TH.conT source

      inferred :: Q Type
      inferred = TH.conT target

  [d|
      instance Num i => Num ((i -> $written) -> $inferred) where
        fromInteger x k = from (k (fromInteger x))

        (+) = liftA2 (+)
        (*) = liftA2 (*)
        (-) = liftA2 (-)

        abs = fmap abs

        negate = fmap negate
        signum = fmap signum
    |]

-- | Generate a `Fractional` instance for a given postfix unit.
fractional :: TH.Name -> TH.Name -> TH.DecsQ
fractional source target = do
  let written :: Q Type
      written = TH.conT source

      inferred :: Q Type
      inferred = TH.conT target

  [d|
      instance Fractional i => Fractional ((i -> $written) -> $inferred) where
        fromRational x k = from (k (fromRational x))
        recip = fmap recip
        (/)   = liftA2 (/)
    |]

-- | Generate a `Floating` instance for a given postfix unit.
floating :: TH.Name -> TH.Name -> TH.DecsQ
floating source target = do
  let written :: Q Type
      written = TH.conT source

      inferred :: Q Type
      inferred = TH.conT target

  [d|
      instance Floating i => Floating ((i -> $written) -> $inferred) where
        pi k = from (k pi)

        exp   = fmap exp
        log   = fmap log
        sin   = fmap sin
        cos   = fmap cos
        tan   = fmap tan
        asin  = fmap asin
        acos  = fmap acos
        atan  = fmap atan
        sinh  = fmap sinh
        cosh  = fmap cosh
        tanh  = fmap tanh
        asinh = fmap asinh
        acosh = fmap acosh
        atanh = fmap atanh
    |]

