{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (concat)

import Data.Word

import Data.Hash.SL2
import Data.Hash.SL2.Internal (Hash)
import Data.Hash.SL2.Unsafe

import qualified Data.ByteString as B

import Foreign

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Property.Monoid

import Data.Monoid

instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary

instance Arbitrary Hash where
  arbitrary = fmap hash arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Properties"

  [ testGroup "equality"

    [ testProperty "true" $
        \a -> hash a == hash a

    , testProperty "false" $
        \a b -> (a /= b) ==> hash a /= hash b
    ]

  , testGroup "compare"

    [ testProperty "eq" $
        \a -> hash a == hash a

    , testProperty "gt" $
        \a b c -> (a > b && b > c) ==> (a :: Hash) > c

    , testProperty "ge" $
        \a b c -> (a >= b && b >= c) ==> (a :: Hash) >= c

    , testProperty "lt" $
        \a b c -> (a < b && b < c) ==> (a :: Hash) < c

    , testProperty "le" $
        \a b c -> (a <= b && b <= c) ==> (a :: Hash) <= c
    ]

  , testGroup "packing"

    [ testProperty "identity 8-bit" $
        \a -> Just a == pack8 (unpack8 a)

    , testProperty "identity 16-bit" $
        \a -> Just a == pack16 (unpack16 a)

    , testProperty "identity 32-bit" $
        \a -> Just a == pack32 (unpack32 a)

    , testProperty "identity 64-bit" $
        \a -> Just a == pack64 (unpack64 a)
    ]

  , testGroup "composition"

    [ testProperty "empty is valid" $
        valid (mempty :: Hash)

    , testProperty "hash is valid" $
        \a -> valid (hash a)

    , testProperty "append is valid" $
        \a b -> valid (hash a <> hash b)

    , testProperty "forms a monoid" $
        eq $ prop_Monoid (T :: T Hash)

    , testProperty "is distributive (mappend)" $
        \a b -> hash (a <> b) == hash a <> hash b

    , testProperty "is distributive (mconcat)" $
        \a b -> hash (a <> b) == mconcat (map hash [a, b])

    ]

  , testGroup "append"

    [ testGroup "single string"

      [ testProperty "equal to ((. hash) . concat)" $
          \a b -> ((. hash) . concat) a b == a `append` b
      ]

    , testGroup "multiple strings"

      [ testProperty "equal to (foldl append)" $
          \a (b :: [B.ByteString]) -> foldl append a b == a `foldAppend` b
      ]

    ]

  , testGroup "prepend"

    [ testGroup "single string"

      [ testProperty "equal to (concat . hash)" $
          \a b -> (concat . hash) a b == a `prepend` b
      ]

    , testGroup "multiple strings"

      [ testProperty "equal to (flip (foldr prepend)" $
          \(a :: [B.ByteString]) b -> foldr prepend b a == a `foldPrepend` b
      ]

    ]

  ]
