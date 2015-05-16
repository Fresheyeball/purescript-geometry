module Test.Classes where

import Data.Bifunctor
import Test.QuickCheck
import Debug.Trace

-- |
-- Functor
--

-- infix 4 =~=

-- Approximate equality to overcome precision issues
(=~=) :: Number -> Number -> Boolean
(=~=) x y = (y - x) <= epsilon && (y - x) >= (-epsilon)
  where
  epsilon = 0.00000001

checkFunctor :: forall f a. (Functor f, Arbitrary a, CoArbitrary a, Arbitrary (f a), Eq (f a)) => f a -> QC Unit
checkFunctor t = do
  trace "Functor identity"
  quickCheck $ identity t
  trace "Functor associativity"
  quickCheck $ associativity t

  where

  identity :: f a -> f a -> Boolean
  identity _ f = id <$> f == id f

  associativity :: f a -> f a -> (a -> a) -> (a -> a) -> Boolean
  associativity _ f p q = (p <<< q) <$> f == ((<$>) p <<< (<$>) q) f


checkBifunctor :: forall f a b.
  ( Bifunctor f
  , Arbitrary a, CoArbitrary a
  , Arbitrary b, CoArbitrary b
  , Arbitrary (f a b)
  , Show a
  , Eq        (f a b)) => f a b -> QC Unit
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

checkSemigroup' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -> (a -> a -> a) -> QC Unit
checkSemigroup' eq f = do
  trace "Semigroup associativity"
  quickCheck associativity

  where

  associativity ::  a -> a -> a -> Result
  associativity a b c = ((a `f` b) `f` c) `eq` (a `f` (b `f` c))
    <?> "its not associative bro, where"
    <>  "\n a = " <> show a
    <>  "\n b = " <> show b
    <>  "\n c = " <> show c
    <>  "\n (a * b) * c  = " <> show ( (a `f` b) `f` c  )
    <>  "\n  a * (b * c) = " <> show (  a `f` (b `f` c) )

checkSemigroup :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -> QC Unit
checkSemigroup = checkSemigroup' (==)

checkMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -> a -> QC Unit
checkMonoid f identity' = do
  trace "Monoid identity"
  quickCheck identity
  trace "Semigroup <= Monoid"
  --checkSemigroup f

  where

  identity :: a -> Result
  identity a = a == a         `f` identity'
            && a == identity' `f` a
    <?> "Identity, it totally didn't hold, where"
    <> "\n a = " <> show a
    <> "\n identity = " <> show identity'
    <> "\n a * identity = " <> show (a `f` identity')
    <> "\n identity * a = " <> show (identity' `f` a)

checkCommutative :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -> QC Unit
checkCommutative f = do
  trace "Commutative"
  quickCheck commutative

  where

  commutative :: a -> a -> Boolean
  commutative a b = a `f` b == b `f` a

checkCommutativeMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -> a -> QC Unit
checkCommutativeMonoid f identity = do
  trace "CommutativeMonoid <= Monoid"
  checkMonoid f identity
  trace "CommutativeMonoid <= Commutative"
  checkCommutative f

checkSemiring :: forall a.
  ( Semiring a
  , Arbitrary a
  , CoArbitrary a
  , Show a
  , Eq a )
  => a -> QC Unit
checkSemiring _ = do
  trace "Semiring <= CommutativeMonoid + 0"
  checkCommutativeMonoid (+) (zero :: a)
  trace "Semiring <= Monoid * 1"
  checkMonoid (*) (one :: a)
  trace "Semiring annihilate"
  quickCheck annihilate
  trace "Semiring distributive"
  quickCheck distributive

  where

  annihilate :: a -> Boolean
  annihilate a = zero == a * zero
              && zero == zero * a

  distributive :: a -> a -> a -> Boolean
  distributive a b c = a * (b + c) == a * b + a * c


-- |
-- Applicative
--

checkApplicative :: forall f a b c. (Applicative f, Arbitrary (f a), Arbitrary (f (a -> b)), Arbitrary (f (b -> c)), CoArbitrary a, Arbitrary b, Arbitrary a, Eq (f a), Eq (f b), Eq (f c)) => f a -> f b -> f c -> QC Unit
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

-- |
-- Monad
--

checkMonad :: forall m a. (Monad m, Arbitrary a, CoArbitrary a, Arbitrary (m a), Eq (m a)) => m a -> QC Unit
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
