module Test.Monad where

import Test.Binary
import Test.QuickCheck
import Debug.Trace

-- psc no likes Fmap'
type Fmap  f a b =   (a -> b) -> f a -> f b
type Ap    f a b = f (a -> b) -> f a -> f b
type Pure  f a   = a -> f a
type Category f a b c = (f b c) -> (f a b) -> a -> c

checkFunctor' :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => CustomEq (f a) -> Fmap f a a -> QC Unit
checkFunctor' (==) (<$>) = do
  trace "Functor identity"
  quickCheck identity
  trace "Functor associativity"
  quickCheck associativity

  where

  identity :: f a -> Result
  identity f = id <$> f == id f
    <?> "oh no bro! functor identity isn't cool when"
    <> "\n f a = " <> show f
    <> "\n so..."
    <> "\n id <$> f = " <> show (id <$> f)
    <> "\n except"
    <> "\n id f = " <> show (id f)

  associativity :: f a -> (a -> a) -> (a -> a) -> Result
  associativity f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f
    <?> "no way, functor associativity won't work when"
    <> "\n f = " <> show f
    <> "\n cuz like"
    <> "\n (p <<< q) <$> f = " <> show ((p <<< q) <$> f)
    <> "\n but then"
    <> "\n ((<$>) p <<< (<$>) q) f = " <> show (((<$>) p <<< (<$>) q) f)

checkFunctor :: forall f a.
  ( Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Eq (f a)
  , Show (f a) )
  => Fmap f a a -> QC Unit
checkFunctor = checkFunctor' (==)

checkFunctorInstance' :: forall f a.
  ( Functor f
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => CustomEq (f a) -> QC Unit
checkFunctorInstance' (==) = checkFunctor' (==) (<$>)

checkFunctorInstance :: forall f a.
  ( Functor f
  , Eq (f a)
  , Arbitrary a
  , CoArbitrary a
  , Arbitrary (f a)
  , Show (f a) )
  => f a -> QC Unit
checkFunctorInstance _ = checkFunctorInstance' ((==) :: CustomEq (f a))

checkApply' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Show (f a)
  , Show (f b)
  , Show (f c) )
  => CustomEq (f c)
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply' (==) leftFMAPu vAPw uAPv _vAPw_ uAP_v =

  quickCheck composition

  where

  composition :: f (b -> c) -> f (a -> b) -> f a -> Result
  composition u v w = ((<<<) `leftFMAPu` u `uAPv` v `vAPw` w) == (u `uAP_v` (v `_vAPw_` w))
    <?> "Apply composition, it, did, not, hold, cuz like"
    <> "\n f a = " <> show w
    <> "\n and if you think about it"
    <> "\n (<<<) <$> u <*> v <*> w = " <> show (((<<<) `leftFMAPu` u `uAPv` v `vAPw` w))
    <> "\n but,"
    <> "\n u <*> (v <*> w) = " <> show (u `uAP_v` (v `_vAPw_` w))

checkApply :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Show (f a)
  , Show (f b)
  , Show (f c)
  , Eq (f c) )
  => Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply = checkApply' (==)

checkApplyInstance' :: forall f a b c.
  ( Applicative f
  , Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Show (f a)
  , Show (f b)
  , Show (f c) )
  => CustomEq (f c) -> f a -> f b -> QC Unit
checkApplyInstance' (==) _ _ = checkApply' (==)
  ((<$>) :: Fmap f (b -> c) ((a -> b) -> a -> c))
  ((<*>) :: Ap f a c)
  ((<*>) :: Ap f (a -> b) (a -> c))
  ((<*>) :: Ap f a b)
  ((<*>) :: Ap f b c)

checkApplyInstance :: forall f a b c.
  ( Applicative f
  , Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Show (f a)
  , Show (f b)
  , Show (f c)
  , Eq (f c) )
  => f a -> f b -> QC Unit
checkApplyInstance = checkApplyInstance' ((==) :: CustomEq (f c))

checkApplicative' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a
  , Show (f a)
  , Show (f b)
  , Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> Fmap f a a

  -- identity
  -> Ap f a a
  -> Pure f (a -> a)

  -- Composition
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c

  -- homomorphism
  -> Pure f b
  -> Pure f a
  -> Ap f a b
  -> Pure f (a -> b)

  -- interchange
  -> Ap f (a -> b) b
  -> Pure f ((a -> b) -> b)

  -> QC Unit
checkApplicative' (==) (===) (====) (<$>)
  idAPv pureId
  leftFMAPu vAPw uAPv _vAPw_ uAP_v
  pureb purea fAPpurea pureAB
  y_APu pure_X = do

  checkFunctor' (==) (<$>)
  checkApply' (====) leftFMAPu vAPw uAPv _vAPw_ uAP_v
  quickCheck identity
  quickCheck homomorphism
  quickCheck interchange

  where

  identity :: f a -> Boolean
  identity v = (pureId id `idAPv` v) == (v :: f a)

  homomorphism :: (a -> b) -> a -> Boolean
  homomorphism f x = (pureAB f `fAPpurea` purea x) === ((pureb (f x)) :: f b)

  interchange :: a -> f (a -> b) -> Boolean
  interchange y u = (u `fAPpurea` purea y) === (pure_X (\x -> x y) `y_APu` u)

checkApplicative :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a
  , Show (f a)
  , Show (f b)
  , Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) )
  => Fmap f a a
  -> Ap f a a -> Pure f (a -> a)
  -> Fmap f (b -> c) ((a -> b) -> a -> c) -> Ap f a c -> Ap f (a -> b) (a -> c) -> Ap f a b -> Ap f b c
  -> Pure f b -> Pure f a -> Ap f a b -> Pure f (a -> b)
  -> Ap f (a -> b) b -> Pure f ((a -> b) -> b) -> QC Unit
checkApplicative = checkApplicative' (==) (==) (==)

checkApplicativeInstance' :: forall f a b c fn ap.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a
  , Show (f a)
  , Show (f b)
  , Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> QC Unit
checkApplicativeInstance' (==) (===) (====) = checkApplicative' (==) (===) (====)
  (<$>) (<*>) pure (<$>) (<*>) (<*>) (<*>) (<*>) pure pure (<*>) pure (<*>) pure

checkApplicativeInstance :: forall f a b c.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , CoArbitrary a
  , Arbitrary b
  , Arbitrary a
  , Show (f a)
  , Show (f b)
  , Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) ) => f a -> f b -> f c -> QC Unit
checkApplicativeInstance _ _ _ = checkApplicativeInstance'
  ((==) :: CustomEq (f a)) ((==) :: CustomEq (f b)) ((==) :: CustomEq (f c))

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
