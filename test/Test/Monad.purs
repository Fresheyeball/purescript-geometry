module Test.Monad where

import Test.QuickCheck
import Debug.Trace

checkFunctor :: forall f a.
  ( Functor f
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Eq (f a))
  => f a -> QC Unit
checkFunctor t = do
  trace "Functor identity"
  quickCheck $ identity t
  trace "Functor associativity"
  quickCheck $ associativity t

  where

  identity :: f a -> Boolean
  identity f = id <$> f == id f

  associativity :: f a -> (a -> a) -> (a -> a) -> Boolean
  associativity f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f

checkApplicative :: forall f a b c.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a
  , Eq (f a)
  , Eq (f b)
  , Eq (f c))
  => f a -> f b -> f c -> QC Unit
checkApplicative ta tb tc = do
  quickCheck $ identity ta
  quickCheck $ composition ta tb tc
  quickCheck $ homomorphism ta tb
  quickCheck $ interchange ta tb

  where

  identity :: forall f a. (Applicative f, Arbitrary (f a), Eq (f a)) => f a -> f a -> Boolean
  identity _ v = (pure id <*> v) == v

  composition :: forall f a b c. (Applicative f, Arbitrary (f (b -> c)), Arbitrary (f (a -> b)), Arbitrary (f a), Eq (f c)) => f a -> f b -> f c -> f (b -> c) -> f (a -> b) -> f a -> Boolean
  composition _ _ _ u v w = (pure (<<<) <*> u <*> v <*> w) == (u <*> (v <*> w))

  homomorphism :: forall f a b. (Applicative f, Arbitrary b, CoArbitrary a, Arbitrary a, Eq (f b)) => f a -> f b -> (a -> b) -> a -> Boolean
  homomorphism _ tb f x = (pure f <*> pure x) == (pure (f x) `asTypeOf` tb)

  interchange :: forall f a b. (Applicative f, Arbitrary a, Arbitrary (f (a -> b)), Eq (f b)) => f a -> f b -> a -> f (a -> b) -> Boolean
  interchange _ _ y u = (u <*> pure y) == (pure (\x -> x y) <*> u)

checkMonad :: forall m a.
  ( Monad m
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (m a)
  , Eq (m a))
  => m a -> QC Unit
checkMonad t = do
  quickCheck $ leftIdentity t
  quickCheck $ rightIdentity t
  quickCheck $ associativity t

  where

  leftIdentity :: forall m a. (Monad m, Arbitrary a, Eq (m a)) => m a -> a -> (a -> m a) -> Boolean
  leftIdentity _ x f = (return x >>= f) == (f x)

  rightIdentity :: forall m a. (Monad m, Arbitrary a, Eq (m a)) => m a -> m a -> Boolean
  rightIdentity _ m = (m >>= return) == m

  associativity :: forall m a. (Monad m, Arbitrary a, Eq (m a)) => m a -> m a -> (a -> m a) -> (a -> m a) -> Boolean
  associativity _ m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
