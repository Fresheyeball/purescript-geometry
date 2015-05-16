module Test.Magma where

import Debug.Trace
import Test.QuickCheck
import Test.Binary

type Id a = a

checkSemigroup' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> QC Unit
checkSemigroup' (==) (*) = do
  trace "Semigroup associativity"
  quickCheck $ associative' (==) (*)

checkSemigroup :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> QC Unit
checkSemigroup = checkSemigroup' (==)

checkMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> Id a -> QC Unit
checkMonoid' (==) (*) identity' = do
  trace "Monoid identity"
  quickCheck identity
  trace "Semigroup <= Monoid"
  checkSemigroup' (==) (*)

  where

  identity :: a -> Result
  identity a = (a == (a         * identity'))
            && (a == (identity' * a))
    <?> "Identity, it totally didn't hold, when"
    <> "\n a = " <> show a
    <> "\n identity = " <> show identity'
    <> "\n so..."
    <> "\n a * identity = " <> show (a * identity')
    <> "\n but like"
    <> "\n identity * a = " <> show (identity' * a)

checkMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> Id a -> QC Unit
checkMonoid = checkMonoid' (==)

checkCommutativeMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> Binary a -> Id a -> QC Unit
checkCommutativeMonoid' (==) (+) identity = do
  trace "CommutativeMonoid <= Monoid"
  checkMonoid' (==) (+) identity
  trace "CommutativeMonoid <= Commutative"
  quickCheck $ commutative' (==) (+)

checkCommutativeMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => Binary a -> Id a -> QC Unit
checkCommutativeMonoid = checkCommutativeMonoid' (==)

checkSemiring' :: forall a.
  ( Show a
  , Semiring a
  , Arbitrary a
  , CoArbitrary a )
  => CustomEq a -> QC Unit
checkSemiring' (==) = do
  trace "Semiring <= CommutativeMonoid + 0"
  checkCommutativeMonoid' (==) (+) (zero :: a)
  trace "Semiring <= Monoid * 1"
  checkMonoid' (==) (*) (one :: a)
  trace "Semiring annihilate"
  quickCheck annihilate
  trace "Semiring distributive"
  quickCheck $ distributive' (==) (*) ((+) :: a -> a -> a)

  where

  annihilate :: a -> Result
  annihilate a = (zero == (a * zero))
              && (zero == (zero * a))
   <?> "It totally didn't annihilate, when"
   <> "\n a = " <> show a
   <> "\n zero = " <> show (zero :: a)
   <> "\n so..."
   <> "\n a * zero = " <> show (a * zero)
   <> "\n but like"
   <> "\n zero * a = " <> show (zero * a)

checkSemiring :: forall a.
  ( Semiring a
  , Arbitrary a
  , CoArbitrary a
  , Show a
  , Eq a )
  => a -- wildcard value for type lookup
  -> QC Unit
checkSemiring _ = checkSemiring' ((==) :: (a -> a -> Boolean))
