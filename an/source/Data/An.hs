{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}

-- | A wrapper for some existential 'Type', along with the most general
-- possible instances for some of the basic classes.
module Data.An
  ( A, An (A, An)

  , forget

  , type (&&)
  , type (:-)

  , Ghost (..)
  ) where

import Data.Hashable (Hashable (..))
import Data.Kind (Constraint, Type)
import Type.Reflection ((:~~:) (..), SomeTypeRep (..), Typeable, eqTypeRep, typeOf)

-- | An existential type that adheres to some 'Constraint'. For example, a
-- value of type @'A' 'Typeable'@ is a value along with its 'Typeable'
-- evidence.
type An :: (Type -> Constraint) -> Type
data An c where An :: c x => x -> An c

-- | A type synonym for 'An' for linguistic fluidity.
type A :: (Type -> Constraint) -> Type
type A = An

-- | A pattern synonym for 'An' for linguistic fluidity.
pattern A :: forall c. forall x. c x => x -> An c
pattern A x = An x
{-# COMPLETE A #-}

-- | If we have a type with evidence of X, and X is enough to convince us of Y,
-- then we can have a type with evidence of Y.
forget :: forall c d. c :- d => A c -> A d
forget (An x) = An (Ghost x)

-- | A product of constraints. For example, @'An' ('Eq' '&&' 'Typeable')@ holds
-- a value along with evidence of 'Eq' and 'Typeable'.
type (&&) :: (Type -> Constraint) -> (Type -> Constraint) -> (Type -> Constraint)
class (c x, d x) => (c && d) x
instance (c x, d x) => (c && d) x

-- | Here's where things get a little uglier: we want to write typeclass
-- instances for 'An'. The easiest way to do this would be to require that the
-- evidence precisely match what we want. For example:
--
--     instance 'Eq' ~ c => 'Eq' ('An' c)
--
-- Now, what if we have @'An' ('Eq' '&&' 'Typeable')@? Well, we could 'forget'
-- the 'Typeable' constraint and /then/ make use of 'Eq', but why should we?
-- What we really want is a quantified constraint - something like this:
--
--     instance (forall x. c x => 'Eq' x) => 'Eq' ('An' c)
--
-- This would be ideal, but we can't write this - I've talked about the reasons
-- before, which you can read about here: https://gitlab.haskell.org/ghc/ghc/-/issues/17939
--
-- So, for now, this is the best we can do: we define quantified instances that
-- say our constraint implies a lesser constraint /on a transparent newtype/,
-- and then our instances use that constraint to get by.
type Ghost :: Type -> Type
newtype Ghost x = Ghost x deriving newtype (Eq, Hashable, Ord, Show)

-- | A shorthand for a quantified constraint implication. See 'Ghost' for more
-- information as to why this isn't as simple as we'd like.
type (:-) :: (Type -> Constraint) -> (Type -> Constraint) -> Constraint
type c :- d = forall x. c x => d (Ghost x)

infix 1 :-

instance c :- Eq && Typeable => Eq (A c) where
  An x == An y = do
    let xs = typeOf (Ghost x)
        ys = typeOf (Ghost y)
    
    case eqTypeRep xs ys of
      Just HRefl -> Ghost x == Ghost y
      Nothing -> False

instance c :- Hashable && Typeable => Hashable (A c) where
  hashWithSalt d (An x) = hashWithSalt d (Ghost x)
  hash (An x) = hash (Ghost x)

instance c :- Ord && Typeable => Ord (A c) where
  compare (An x) (An y) = do
    let xs@(SomeTypeRep -> xs') = typeOf (Ghost x)
        ys@(SomeTypeRep -> ys') = typeOf (Ghost y)
    
    case eqTypeRep xs ys of
      Just HRefl -> compare (Ghost x) (Ghost y)
      Nothing -> compare xs' ys'

instance (c :- Show) => Show (An c) where
  show (An x) = show (Ghost x)
