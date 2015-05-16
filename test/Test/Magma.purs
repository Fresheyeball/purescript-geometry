module Test.Magma where

import Debug.Trace
import Test.QuickCheck
import Test.Binary

checkSemigroup' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -- custom equality
  -> (a -> a -> a) -- binary operator in question
  -> QC Unit
checkSemigroup' (==) (*) = do
  trace "Semigroup associativity"
  quickCheck associativity

  where

  associativity ::  a -> a -> a -> Result
  associativity a b c = ((a * b) * c) == (a * (b * c))
    <?> "its not associative bro, where"
    <>  "\n a = " <> show a
    <>  "\n b = " <> show b
    <>  "\n c = " <> show c
    <>  "\n (a * b) * c  = " <> show ( (a * b) * c  )
    <>  "\n  a * (b * c) = " <> show (  a * (b * c) )

checkSemigroup :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -- binary operator in question
  -> QC Unit
checkSemigroup = checkSemigroup' (==)

checkMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -- custom equality
  -> (a -> a -> a) -- binary operator in question
  -> a -- identity value
  -> QC Unit
checkMonoid' (==) (*) identity' = do
  trace "Monoid identity"
  quickCheck identity
  trace "Semigroup <= Monoid"
  checkSemigroup' (==) (*)

  where

  identity :: a -> Result
  identity a = (a == (a         * identity'))
            && (a == (identity' * a))
    <?> "Identity, it totally didn't hold, where"
    <> "\n a = " <> show a
    <> "\n identity = " <> show identity'
    <> "\n a * identity = " <> show (a * identity')
    <> "\n identity * a = " <> show (identity' * a)

checkMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -- binary operator in question
  -> a -- identity value
  -> QC Unit
checkMonoid = checkMonoid' (==)

checkCommutativeMonoid' :: forall a.
  ( Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -- custom equality
  -> (a -> a -> a) -- binary operator in question
  -> a -- identity value
  -> QC Unit
checkCommutativeMonoid' (==) (+) identity = do
  trace "CommutativeMonoid <= Monoid"
  checkMonoid' (==) (+) identity
  trace "CommutativeMonoid <= Commutative"
  checkCommutative' (==) (+)

checkCommutativeMonoid :: forall a.
  ( Eq a
  , Show a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> a) -- binary operator in question
  -> a -- identity value
  -> QC Unit
checkCommutativeMonoid = checkCommutativeMonoid' (==)

checkSemiring' :: forall a.
  ( Show a
  , Semiring a
  , Arbitrary a
  , CoArbitrary a )
  => (a -> a -> Boolean) -- custom equality
  -> a -- type wildcard
  -> QC Unit
checkSemiring' (==) _ = do
  trace "Semiring <= CommutativeMonoid + 0"
  checkCommutativeMonoid' (==) (+) (zero :: a)
  trace "Semiring <= Monoid * 1"
  checkMonoid' (==) (*) (one :: a)
  trace "Semiring annihilate"
  quickCheck annihilate
  trace "Semiring distributive"
  quickCheck distributive

  where

  annihilate :: a -> Boolean
  annihilate a = (zero == (a * zero))
              && (zero == (zero * a))

  distributive :: a -> a -> a -> Boolean
  distributive a b c = (a * (b + c)) == ((a * b) + (a * c))

checkSemiring :: forall a.
  ( Semiring a
  , Arbitrary a
  , CoArbitrary a
  , Show a
  , Eq a )
  => a -> QC Unit
checkSemiring = checkSemiring' (==)
