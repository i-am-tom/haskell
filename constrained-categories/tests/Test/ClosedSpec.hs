{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}

module Test.ClosedSpec where
import Control.Category.Constrained (Category (..), type (~>) (..))
import Control.Closed.Constrained (curry, uncurry, End (..))
import Data.Functor.Const (Const (..))
import GHC.Generics ((:*:) (..))
import Prelude hiding ((.), curry, id, uncurry)
import Test.Orphans ()
import Test.QuickCheck (Fun (..))

function :: Fun x y -> (x -> y)
function (Fun _ f) = f

prop_function_evaluation :: Fun (Int, String) Bool -> (Int, String) -> Bool
prop_function_evaluation (function -> f) x = uncurry (curry f) x == f x

prop_function_extensionality :: Fun Int (Fun String Bool) -> (Int, String) -> Bool
prop_function_extensionality (fmap function . function -> f) (x, y) = curry (uncurry f) x y == f x y

---

prop_nt_evaluation :: (Fun (Int, String) Bool) -> (Const Int :*: Const String) Int -> Bool
prop_nt_evaluation f x = runNT (uncurry (curry (nt f))) x == runNT (nt f) x
  where
    nt :: (Fun (x, y) z) -> ((Const x :*: Const y) ~> Const z)
    nt (Fun _ g) = NT \(Const a :*: Const b) -> Const (g (a, b))

prop_nt_extensionality :: (Fun Int (Fun String Bool)) -> (Const Int :*: Const String) Int -> Bool
prop_nt_extensionality f (x :*: y) = unEnd (runNT (curry (uncurry (nt f))) x) id y == unEnd (runNT (nt f) x) id y
  where
    nt :: (Fun Int (Fun String Bool)) -> (Const Int ~> End (Const String) (Const Bool))
    nt (Fun _ g) = NT \(Const i) -> End \_ (Const s) -> let Fun _ h = g i in Const (h s)