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

checkApplicative' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c)

  -- identity
  -> (            f (a -> a) -> f a -> f a) -- <*>
  -> ((a -> a) -> f (a -> a)) -- pure

  -- composition
  -> ( f  (a -> c) -> f a ->       f       c) -- v <*> w
  -> ( f ((a -> b) ->   a ->               c)
    -> f               (a -> b) -> f (a -> c) ) -- u <*> v
  -> ( f ((b -> c) ->  (a -> b) ->    a -> c)
    -> f ( b -> c)
    -> f              ((a -> b) ->    a -> c) ) -- pure (<<<) <*> u
  -> (   ((b -> c) ->  (a -> b) ->    a -> c)
    -> f ((b -> c) ->  (a -> b) ->    a -> c) ) -- pure

  -> (f (a ->   b)
    -> f a -> f b) -- (v <*> w)
  -> (f        (b ->   c)
    ->        f b -> f c) -- u <*> (v

  -- homomorphism
  -> (b -> f b)
  -> (a -> f a)
  -> (f (a -> b) -> f a -> f b)
  -> ((a -> b) -> f (a -> b))

  -- interchange
  -> (f ((a -> b) -> b) -> f  (a -> b) -> f b)
  -> (  ((a -> b) -> b) -> f ((a -> b) -> b))

  -> QC Unit
checkApplicative' (==) (===) (====)

  idAPv pureId

  vAPw uAPv pureleftAPu pureleft _vAPw_ uAP_v

  pureb purea fAPpurea pureAB

  y_APu pure_X = do

  quickCheck identity
  quickCheck composition
  quickCheck homomorphism
  quickCheck interchange

  where

  identity :: f a -> Boolean
  identity v = (pureId id `idAPv` v) == (v :: f a)

  composition :: f (b -> c) -> f (a -> b) -> f a -> Boolean
  composition u v w = (pureleft (<<<) `pureleftAPu` u `uAPv` v `vAPw` w) ==== (u `uAP_v` (v `_vAPw_` w))

  homomorphism :: (a -> b) -> a -> Boolean
  homomorphism f x = (pureAB f `fAPpurea` purea x) === ((pureb (f x)) :: f b)

  interchange :: a -> f (a -> b) -> Boolean
  interchange y u = (u `fAPpurea` purea y) === (pure_X (\x -> x y) `y_APu` u)

-- undefined :: forall a. a
-- undefined = undefined' unit
--
--   where
--
--   undefined' :: Unit -> a
--   undefined' unit = undefined' unit

-- checkApplicativeInstance :: forall f a b c.
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
-- checkApplicativeInstance ta tb tc = undefined

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
