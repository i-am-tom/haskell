{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Orphan instances for better type inference.
module Postfix.Instances.Orphans where

instance
  {-# INCOHERENT #-}
  (x ~ y, Num i, Num y) =>
  Num ((i -> x) -> y)
  where
  fromInteger x k = k (fromInteger x)

  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)

  abs = fmap abs

  negate = fmap negate
  signum = fmap signum

instance
  {-# INCOHERENT #-}
  (x ~ y, Fractional i, Fractional y) =>
  Fractional ((i -> x) -> y)
  where
  fromRational x k = k (fromRational x)
  recip = fmap recip
  (/) = liftA2 (/)

instance
  {-# INCOHERENT #-}
  (x ~ y, Floating i, Floating y) =>
  Floating ((i -> x) -> y)
  where
  pi k = k pi

  exp = fmap exp
  log = fmap log
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh
