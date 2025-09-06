{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Helpers for creating generic optics.
module Data.Optics.Helper where

import Data.Kind (Type)
import GHC.Generics
import GHC.TypeLits (ErrorMessage (..), Symbol, TypeError)

-- | It's 'Left' or 'Right'. Unnecessarily cute.
type Step :: Type
type Step = () -> Either () ()

-- | Calculate the path to the given element through a generic representation.
type Path :: Type -> Symbol -> (Type -> Type) -> Type -> Maybe [Step]
type family Path i x rep s where
  Path S x (S1 ('MetaSel ('Just x) _ _ _) _) s = 'Just '[]
  Path C x (C1 ('MetaCons x _ _) _) s = 'Just '[]
  Path i x (D1 ________________________ rep) s = Path i x rep s
  Path S x (C1 ________________________ rep) s = Path S x rep s
  Path C x (l :+: r) s = Prepend 'Left (Path C x l s) <|> Prepend 'Right (Path C x r s)
  Path S x (l :*: r) s = Prepend 'Left (Path S x l s) <|> Prepend 'Right (Path S x r s)

type Prepend :: Step -> Maybe [Step] -> Maybe [Step]
type family Prepend x xs where
  Prepend x ('Just xs) = 'Just (x ': xs)
  Prepend x 'Nothing = 'Nothing

-- | Choose the first non-@Nothing@ value.
type (<|>) :: Maybe [Step] -> Maybe [Step] -> Maybe [Step]
type family xs <|> ys where
  'Just xs <|> _ = 'Just xs
  'Nothing <|> y = y

-- | If the value is @Nothing@, raise an error.
type OnNothing :: Maybe [Step] -> ErrorMessage -> [Step]
type family OnNothing x e where
  OnNothing 'Nothing e = TypeError e
  OnNothing ('Just xs) _ = xs

-- | An error for missing fields.
type FieldPath :: Symbol -> Type -> [Step]
type FieldPath x s =
  Path S x (Rep s) s
    `OnNothing` ('Text x ':<>: 'Text " ∉ " ':<>: 'ShowType s)

-- | An error for missing constructors.
type ConstructorPath :: Symbol -> Type -> [Step]
type ConstructorPath x s =
  Path C x (Rep s) s
    `OnNothing` ('Text x ':<>: 'Text " ∉ " ':<>: 'ShowType s)
