module Test.Binary where

import Test.QuickCheck
import Debug.Trace

type CustomEq a = (a -> a -> Boolean)
type Binary a = (a -> a -> a)

commutative' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a -> a -> a -> Result
commutative' (==) (*) a b = (a * b) == (b * a)
  <?> "some things are just not commutative bro, when"
  <> "\n a = " <> show a
  <> "\n b = " <> show b
  <> "\n so..."
  <> "\n a * b = " <> show (a * b)
  <> "\n but like"
  <> "\n b * a = " <> show (b * a)

commutative :: forall a.
  ( Eq a
  , Show a )
  => Binary a -> a -> a -> Result
commutative = commutative' (==)

associative' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a -> a -> a -> a -> Result
associative' (==) (*) a b c = (((a * b) * c) == (a * (b * c)))
  <?> "its not associative bro, when"
  <>  "\n a = " <> show a
  <>  "\n b = " <> show b
  <>  "\n c = " <> show c
  <>  "\n so..."
  <>  "\n (a * b) * c  = " <> show ( (a * b) * c  )
  <>  "\n  a * (b * c) = " <> show (  a * (b * c) )

associative :: forall a.
  ( Show a
  , Eq a )
  => Binary a -> a -> a -> a -> Result
associative = associative' (==)

distributive' :: forall a.
  ( Show a )
  => CustomEq a -> Binary a -> Binary a -> a -> a -> a -> Result
distributive' (==) (*) (+) a b c = ( ( a * (b + c)) == ((a * b) + (a * c)) )
                                && ( ((a + b) * c ) == ((a * c) + (b * c)) )
  <?> "Dude, multiplication just won't distribute over addition, when"
  <> "\n a = " <> show a
  <> "\n b = " <> show b
  <> "\n c = " <> show c
  <> "\n so..."
  <> "\n  a * (b + c) = " <> show (a * (b + c))
  <> "\n but like"
  <> "\n (a * b) + (a * c) = " <> show ((a * b) + (a * c))
  <> "\n and so..."
  <> "\n (a + b) * c = " <> show ((a + b) * c)
  <> "\n but then like,"
  <> "\n (a * c) + (b * c) = " <> show ((a * c) + (b * c))

distributive :: forall a.
  ( Show a
  , Eq a )
  => Binary a -> Binary a -> a -> a -> a -> Result
distributive = distributive' (==)
