module Test.Monad where

import Test.Binary
import Test.QuickCheck
import Debug.Trace

-- psc no likes Fmap'
type Fmap  f a b =   (a -> b) -> f a -> f b
type Ap    f a b = f (a -> b) -> f a -> f b
type Pure  f a   = a -> f a
type Category f a b c = (f b c) -> (f a b) -> a -> c
type Bind m a b = m a -> (a -> m b) -> m b

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
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c) )
  => CustomEq (f c)
  -> Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply' (==) (<$>) leftFMAPu vAPw uAPv _vAPw_ uAP_v = do

  trace "Functor <= Apply"
  checkFunctor' (==) (<$>)
  trace "Apply composition"
  quickCheck composition

  where

  composition :: f (b -> c) -> f (a -> b) -> f a -> Result
  composition u v w = ((<<<) `leftFMAPu` u `uAPv` v `vAPw` w) == (u `uAP_v` (v `_vAPw_` w))
    <?> "Apply composition, it, did, not, hold, cuz like"
    <> "\n w = " <> show w
    <> "\n and if you think about it"
    <> "\n (<<<) <$> u <*> v <*> w = " <> show (((<<<) `leftFMAPu` u `uAPv` v `vAPw` w))
    <> "\n but,"
    <> "\n u <*> (v <*> w) = " <> show (u `uAP_v` (v `_vAPw_` w))

checkApply :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c)
  , Eq (f c) )
  => Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> QC Unit
checkApply = checkApply' (==)

checkApplyInstance' :: forall f a b c.
  ( Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c) )
  => CustomEq (f c) -> f a -> f b -> QC Unit
checkApplyInstance' (==) _ _ = checkApply' (==)
  ((<$>) :: Fmap f c c)
  ((<$>) :: Fmap f (b -> c) ((a -> b) -> a -> c))
  ((<*>) :: Ap f a c)
  ((<*>) :: Ap f (a -> b) (a -> c))
  ((<*>) :: Ap f a b)
  ((<*>) :: Ap f b c)

checkApplyInstance :: forall f a b c.
  ( Apply f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show (f a), Show (f b), Show (f c)
  , Eq (f c) )
  => f a -> f b -> QC Unit
checkApplyInstance = checkApplyInstance' ((==) :: CustomEq (f c))

checkApplicative' :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c)
  -> Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> Ap f a a
  -> Ap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a b
  -> Ap f (a -> b) b
  -> Pure f (a -> a)
  -> Pure f (Category (->) a b c)
  -> Pure f b
  -> Pure f a
  -> Pure f (a -> b)
  -> Pure f ((a -> b) -> b)
  -> QC Unit
checkApplicative' (==) (===) (====)
  (<$>) leftFMAPu
  vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
  pureId pureleft pureb purea pureAB pure_X = do

  trace "Applicative <= Apply"
  checkApply' (====) (<$>) leftFMAPu vAPw uAPv _vAPw_ uAP_v
  trace "Applicative composition"
  quickCheck composition
  trace "Applicative identity"
  quickCheck identity
  trace "Applicative homomorphism"
  quickCheck homomorphism
  trace "Applicative interchange"
  quickCheck interchange

  where

  composition :: f (b -> c) -> f (a -> b) -> f a -> Result
  composition u v w = (pureleft (<<<) `pureleftAPu` u `uAPv` v `vAPw` w) ==== (u `uAP_v` (v `_vAPw_` w))
    <?> "Applicative composition, it, did, not, hold, cuz like"
    <> "\n h = " <> show w
    <> "\n pure (<<<) <*> u <*> v <*> w) = "
    <> show (pureleft (<<<) `pureleftAPu` u `uAPv` v `vAPw` w)
    <> "\n but"
    <> "\n u <*> (v <*> w) = " <> show (u `uAP_v` (v `_vAPw_` w))

  identity :: f a -> Result
  identity v = (pureId id `idAPv` v) == (v :: f a)
    <?> "Applicative idenity, its just not true dude, when"
    <> "\n v = " <> show v
    <> "\n cuz"
    <> "\n pure id <*> v = " <> show (pureId id `idAPv` v)

  homomorphism :: (a -> b) -> a -> Result
  homomorphism f x = (pureAB f `fAPpurea` purea x) === ((pureb (f x)) :: f b)
    <?> "Dude, it not an Applicative Homomorphism when"
    <> "\n x = " <> show x
    <> "\n so..."
    <> "\n pure f <*> pure x = " <> show (pureAB f `fAPpurea` purea x)
    <> "\n but like"
    <> "\n pure (f x) = " <> show ((pureb (f x)) :: f b)
    <> "\n no homo"

  interchange :: a -> f (a -> b) -> Result
  interchange y u = (u `fAPpurea` purea y) === (pure_X (\x -> x y) `y_APu` u)
    <?> "Ugh, bro thats totally not an Applicative interchange."
    <> "\n y = " <> show y
    <> "\n and like"
    <> "\n u <*> (pure y) = " <> show (u `fAPpurea` purea y)
    <> "\n except"
    <> "\n (pure ($ y)) <*> u = " <> show (pure_X (\x -> x y) `y_APu` u)

checkApplicative :: forall f a b c.
  ( Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) )
  => Fmap f c c
  -> Fmap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a c
  -> Ap f (a -> b) (a -> c)
  -> Ap f a b
  -> Ap f b c
  -> Ap f a a
  -> Ap f (b -> c) ((a -> b) -> a -> c)
  -> Ap f a b
  -> Ap f (a -> b) b
  -> Pure f (a -> a)
  -> Pure f (Category (->) a b c)
  -> Pure f b
  -> Pure f a
  -> Pure f (a -> b)
  -> Pure f ((a -> b) -> b)
  -> QC Unit
checkApplicative = checkApplicative' (==) (==) (==)

checkApplicativeInstance' :: forall f a b c fn ap.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c) )
  => CustomEq (f a) -> CustomEq (f b) -> CustomEq (f c) -> QC Unit
checkApplicativeInstance' (==) (===) (====) = checkApplicative' (==) (===) (====)
  (<$>) (<$>)
  (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>)
  pure pure pure pure pure pure

checkApplicativeInstance :: forall f a b c.
  ( Applicative f
  , Arbitrary (f a)
  , Arbitrary (f (a -> b))
  , Arbitrary (f (b -> c))
  , Arbitrary a, CoArbitrary a
  , Arbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (f c)
  , Show a, Show (f a), Show (f b), Show (f c)
  , Eq (f a), Eq (f b), Eq (f c) ) => f a -> f b -> f c -> QC Unit
checkApplicativeInstance _ _ _ = checkApplicativeInstance'
  ((==) :: CustomEq (f a)) ((==) :: CustomEq (f b)) ((==) :: CustomEq (f c))

checkBind' :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Show (m a), Show (m c) )
  => CustomEq (m c)
  -> Bind m a b
  -> Bind m b c
  -> Bind m a c
  -> QC Unit
checkBind' (==) bind1 bind2 bind3 = do

  trace "Bind associativity"
  quickCheck associativity

  where
  associativity :: m a -> (a -> m b) -> (b -> m c) -> Result
  associativity x f g = ((x `bind1` f) `bind2` g) == (x `bind3` (\k -> f k `bind2` g))
    <?> "bind associativity, dude, it didn't pass. cuz when"
    <> "\n x = " <> show x
    <> "\n so..."
    <> "\n (x >>= f) >>= g = " <> show ((x `bind1` f) `bind2` g)
    <> "\n but then"
    <> "\n x >>= (λk -> f k >>= g) = " <> show (x `bind3` (\k -> f k `bind2` g))

checkBind :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Eq (m c)
  , Show (m a), Show (m c) )
  => Bind m a b
  -> Bind m b c
  -> Bind m a c
  -> QC Unit
checkBind = checkBind' (==)

checkBindInstance' :: forall m a b c.
  ( Bind m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Show (m a), Show (m c) )
  => CustomEq (m c) -> m a -> m b -> QC Unit
checkBindInstance' (==) _ _ = checkBind' (==)
  ((>>=) :: Bind m a b) ((>>=) :: Bind m b c) ((>>=) :: Bind m a c)

checkBindInstance :: forall m a b c.
  ( Bind m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , CoArbitrary b
  , Eq (m c)
  , Show (m a), Show (m c) )
  => m a -> m b -> m c -> QC Unit
checkBindInstance ma mb _ = checkBindInstance' ((==) :: CustomEq (m c)) ma mb

checkMonad' :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c) )
  => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c)
  -> Fmap m c c
  -> Fmap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a c
  -> Ap m (a -> b) (a -> c)
  -> Ap m a b
  -> Ap m b c
  -> Ap m a a
  -> Ap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a b
  -> Ap m (a -> b) b
  -> Pure m (a -> a)
  -> Pure m (Category (->) a b c)
  -> Pure m a
  -> Pure m b
  -> Pure m c
  -> Pure m (a -> b)
  -> Pure m ((a -> b) -> b)
  -> Bind m a a
  -> Bind m a b
  -> Bind m a c
  -> Bind m b c
  -> Bind m c c
  -> QC Unit
checkMonad'
  (==) (===) (====)
  (<$>) leftFMAPu
  vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
  pureId pureleft purea pureb purec pureAB pure_X
  bindaa bindab bindac bindbc bindcc = do

  trace "Monad <= Applicative"
  checkApplicative'
    (==) (===) (====)
    (<$>) leftFMAPu
    vAPw uAPv _vAPw_ uAP_v idAPv pureleftAPu fAPpurea y_APu
    pureId pureleft pureb purea pureAB pure_X

  trace "Monad <= Bind"
  checkBind' (====) bindab bindbc bindac

  quickCheck leftIdentity
  quickCheck rightIdentity

  where

  leftIdentity :: c -> (c -> m c) -> Result
  leftIdentity x f = (purec x `bindcc` f) ==== (f x)
    <?> "woah, no left id on Monad bro, when"
    <> "\n x = " <> show x
    <> "\n and like"
    <> "\n return x >>= f = " <> show (purec x `bindcc` f)
    <> "\n but totally"
    <> "\n f x = " <> show (f x)

  rightIdentity :: m a -> Result
  rightIdentity m = (m `bindaa` purea) == m
    <?> "woah, no right id on Monad bro, when"
    <> "\n m = " <> show m
    <> "\n but like"
    <> "\n m >>= return = " <> show (m `bindaa` purea)

checkMonad :: forall m a b c.
  ( Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c)
  , Eq (m a), Eq (m b), Eq (m c) )
  => Fmap m c c
  -> Fmap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a c
  -> Ap m (a -> b) (a -> c)
  -> Ap m a b
  -> Ap m b c
  -> Ap m a a
  -> Ap m (b -> c) ((a -> b) -> a -> c)
  -> Ap m a b
  -> Ap m (a -> b) b
  -> Pure m (a -> a)
  -> Pure m (Category (->) a b c)
  -> Pure m a
  -> Pure m b
  -> Pure m c
  -> Pure m (a -> b)
  -> Pure m ((a -> b) -> b)
  -> Bind m a a
  -> Bind m a b
  -> Bind m a c
  -> Bind m b c
  -> Bind m c c
  -> QC Unit
checkMonad = checkMonad' (==) (==) (==)

checkMonadInstance' :: forall m a b c.
  ( Monad m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c) )
  => CustomEq (m a) -> CustomEq (m b) -> CustomEq (m c) -> QC Unit
checkMonadInstance' (==) (===) (====) = checkMonad'
  (==) (===) (====)
  (<$>) (<$>)
  (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>) (<*>)
  pure pure pure pure pure pure pure
  (>>=) (>>=) (>>=) (>>=) (>>=)

checkMonadInstance :: forall m a b c.
  ( Monad m
  , Arbitrary (m a)
  , Arbitrary (m b)
  , Arbitrary (m c)
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary c, CoArbitrary c
  , Arbitrary (m (a -> b))
  , Arbitrary (m (b -> c))
  , Show a, Show c, Show (m a), Show (m b), Show (m c)
  , Eq (m a), Eq (m b), Eq (m c) )
  => m a -> m b -> m c -> QC Unit
checkMonadInstance _ _ _ = checkMonadInstance'
  ((==) :: CustomEq (m a))
  ((==) :: CustomEq (m b))
  ((==) :: CustomEq (m c))
