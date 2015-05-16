module Test.Monad where

import Test.Binary
import Test.QuickCheck
import Debug.Trace

checkFunctor' :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a))
  => CustomEq (f a)
  -> ((a -> a) -> f a -> f a)
  -> QC Unit
checkFunctor' (==) (<$>) = do
  trace "Functor identity"
  quickCheck identity
  trace "Functor associativity"
  quickCheck associativity

  where

  identity :: f a -> Boolean
  identity f = id <$> f == id f

  associativity :: f a -> (a -> a) -> (a -> a) -> Boolean
  associativity f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f

checkFunctor :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Eq (f a))
  => ((a -> a) -> f a -> f a) -> QC Unit
checkFunctor = checkFunctor' (==)

checkFunctorInstance' :: forall f a.
  ( Functor f
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a))
  => CustomEq (f a) -> QC Unit
checkFunctorInstance' (==) = checkFunctor' (==) (<$>)

checkFunctorInstance :: forall f a.
  ( Functor f
  , Eq (f a)
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a))
  => f a -> QC Unit
checkFunctorInstance _ = checkFunctorInstance' ((==) :: CustomEq (f a))

-- checkApplicative :: forall f a b c.
--   ( Applicative f
--   , Arbitrary (f a)
--   , Arbitrary (f (a -> b))
--   , Arbitrary (f (b -> c))
--   , CoArbitrary a
--   , Arbitrary b
--   , Arbitrary a
--   , Eq (f a)
--   , Eq (f b)
--   , Eq (f c))
--   => f a -> f b -> f c -> QC Unit
-- checkApplicative ta tb tc = do
--   quickCheck $ identity ta
--   quickCheck $ composition ta tb tc
--   quickCheck $ homomorphism ta tb
--   quickCheck $ interchange ta tb
--
--   where
--
--   identity :: f a -> f a -> Boolean
--   identity _ v = (pure id <*> v) == v
--
--   composition ::  f a -> f b -> f c -> f (b -> c) -> f (a -> b) -> f a -> Boolean
--   composition _ _ _ u v w = (pure (<<<) <*> u <*> v <*> w) == (u <*> (v <*> w))
--
--   homomorphism :: f a -> f b -> (a -> b) -> a -> Boolean
--   homomorphism _ tb f x = (pure f <*> pure x) == (pure (f x) `asTypeOf` tb)
--
--   interchange :: f a -> f b -> a -> f (a -> b) -> Boolean
--   interchange _ _ y u = (u <*> pure y) == (pure (\x -> x y) <*> u)
--
-- checkMonad :: forall m a.
--   ( Monad m
--   , Arbitrary a
--   , CoArbitrary a
--   , Arbitrary (m a)
--   , Eq (m a))
--   => m a -> QC Unit
-- checkMonad t = do
--   quickCheck $ leftIdentity t
--   quickCheck $ rightIdentity t
--   quickCheck $ associativity t
--
--   where
--
--   leftIdentity :: m a -> a -> (a -> m a) -> Boolean
--   leftIdentity _ x f = (return x >>= f) == (f x)
--
--   rightIdentity ::  m a -> m a -> Boolean
--   rightIdentity _ m = (m >>= return) == m
--
--   associativity ::  m a -> m a -> (a -> m a) -> (a -> m a) -> Boolean
--   associativity _ m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
