{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A @Category@ class with constraints upon its objects.
--
-- In the @base@ definition of "Base.Category", it is assumed that the objects
-- in the category are identical to those of @Hask@, _or_ that users will not
-- be able to instantiate arrows in the category freely. For example, if I want
-- to describe the category of things that have `Show` instances, the @base@
-- definition requires that I introduce a smart constructor to guarantee that
-- composition is always well-defined.
--
-- With this definition, we can impose constraints directly on the objects as
-- part of the category definition. This means that you can instantiate arrows
-- freely and the typechecker will ensure that the constraints are met.
module Control.Category.Constrained where

import Control.Arrow (Kleisli)
import Control.Category qualified as Base
import Data.Functor.Contravariant (Op)
import Data.Kind (Constraint, Type)
import Data.Type.Coercion (Coercion)
import Data.Type.Equality ((:~:), (:~~:))
import Prelude hiding (id, (.))

-- | A category is a monoidal structure with identity element @id@ for an
-- associative binary function @.@.
type Category :: (t -> Constraint) -> (t -> t -> Type) -> Constraint
class (Obj k ~ c) => Category c (k :: t -> t -> Type) | k -> c where
  -- | Having the constraint as a class parameter allows us to use it in
  -- quantified constraints. However, in practice, the constraint is always
  -- determined by the category, so we also have a type family for unquantified
  -- constraints.
  type Obj (k :: t -> t -> Type) :: t -> Constraint

  type Obj k = Trivial

  -- | Composition of category arrows.
  (.) :: (c x, c y, c z) => k y z -> k x y -> k x z

  -- | Identity on category arrows.
  id :: (c x) => k x x

-- | Because the constraint is determined by the functional dependency, we can
-- use this synonym in constraints to save us a type variable.
type Category' :: (t -> t -> Type) -> Constraint
type Category' k = Category (Obj k) k

-- | A trivial constraint satisfied by all objects.
type Trivial :: t -> Constraint
class Trivial x

instance Trivial x

-- | A newtype via which we can derive instances for unconstrained categories.
-- In other words, if you have a 'Control.Category.Category' according to the
-- @base@ library, you can derive our 'Category' class via this type.
type Base :: (t -> t -> Type) -> t -> t -> Type
newtype Base k a b = Base (k a b)

instance (Base.Category k) => Category Trivial (Base k) where
  (.) :: Base k y z -> Base k x y -> Base k x z
  Base f . Base g = Base (f Base.. g)

  id :: Base k x x
  id = Base Base.id

deriving via
  Base (->)
  instance
    Category Trivial (->)

deriving via
  Base Op
  instance
    Category Trivial Op

deriving via
  Base (Kleisli m)
  instance
    (Monad m) => Category Trivial (Kleisli m)

deriving via
  Base Coercion
  instance
    Category Trivial Coercion

deriving via
  Base (:~:)
  instance
    Category Trivial (:~:)

deriving via
  Base (:~~:)
  instance
    Category Trivial (:~~:)

-- | A type for natural transformations.
type (~>) :: (Type -> Type) -> (Type -> Type) -> Type
newtype f ~> g = NT (forall x. f x -> g x)

instance Category Functor (~>) where
  type Obj (~>) = Functor

  (.) :: (y ~> z) -> (x ~> y) -> (x ~> z)
  NT f . NT g = NT (f . g)

  id :: x ~> x
  id = NT id

-- | Endomorphisms in a category @k@ form a 'Monoid'.
type Endo :: (t -> t -> Type) -> t -> Type
newtype Endo k a = Endo {unEndo :: k a a}

instance (Category' k, Obj k a) => Semigroup (Endo k a) where
  Endo f <> Endo g = Endo (g . f)

instance (Category' k, Obj k a) => Monoid (Endo k a) where
  mempty = Endo id
