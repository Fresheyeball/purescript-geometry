module Test.Bifunctor where

import Data.Bifunctor
import Test.QuickCheck
import Debug.Trace

checkBifunctor :: forall f a b.
  ( Bifunctor f
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary (f a b)
  , Show a
  , Eq        (f a b))
  => f a b -> QC Unit
checkBifunctor t = do
  trace "Bifunctor identity"
  quickCheck $ identity t
  trace "Bifunctor composition"
  quickCheck $ composition t

  where

  identity :: forall f a b.
    ( Bifunctor f
    , Arbitrary a
    , Arbitrary b
    , Show a
    , Eq (f a b)) => f a b -> f a b -> Boolean
  identity _ f = bimap id id f == id f

  composition :: forall f a b.
    ( Bifunctor f
    , Arbitrary a
    , Arbitrary b
    , Show a
    , Eq (f a b))
    => f a b
    -> f a b
    -> (a -> a) -- f1
    -> (a -> a) -- f2
    -> (b -> b) -- g1
    -> (b -> b) -- g2
    -> Boolean
  composition _ f f1 f2 g1 g2 = (bimap f1 g1 <<< bimap f2 g2) f == (bimap (f1 <<< f2) (g1 <<< g2)) f
