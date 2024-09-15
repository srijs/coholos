module Data.Hash.SL2.Chunk (Chunk(..), fromByteString) where

import Data.ByteString
import Data.Hash.SL2

data Chunk = Chunk
  { getChunkHash :: {-# unpack #-} !Hash
  , getChunkBytes :: {-# unpack #-} !ByteString
  }

instance Eq Chunk where
  a == b = getChunkHash a == getChunkHash b

instance Ord Chunk where
  compare a b = compare (getChunkHash a) (getChunkHash b)

instance Semigroup Chunk where
  a <> b = Chunk (getChunkHash a <> getChunkHash b) (getChunkBytes a <> getChunkBytes b)

instance Monoid Chunk where
  mempty = Chunk mempty mempty
  mconcat as = Chunk (mconcat $ fmap getChunkHash as) (mconcat $ fmap getChunkBytes as)

fromByteString :: ByteString -> Chunk
fromByteString b = Chunk (hash b) b
