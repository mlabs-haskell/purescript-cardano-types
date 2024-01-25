module Cardano.ToMetadata
  ( class ToMetadata
  , toMetadata
  ) where

import Prelude

import Cardano.Types.Int (Int) as Int
import Cardano.Types.TransactionMetadatum
  ( TransactionMetadatum(Map, List, Int, Bytes, Text)
  )
import Data.ByteArray (ByteArray)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Profunctor.Strong ((***))
import Data.Tuple (Tuple)

--------------------------------------------------------------------------------
-- ToMetadata
--------------------------------------------------------------------------------

class ToMetadata (a :: Type) where
  toMetadata :: a -> TransactionMetadatum

instance ToMetadata TransactionMetadatum where
  toMetadata = identity

instance (ToMetadata k, ToMetadata v) => ToMetadata (Map k v) where
  toMetadata mp =
    let
      entries = Map.toUnfoldable mp :: Array (Tuple k v)
    in
      Map <<< Map.fromFoldable $
        map (toMetadata *** toMetadata) entries

instance
  ( Ord k
  , ToMetadata k
  , ToMetadata v
  ) =>
  ToMetadata (Array (Tuple k v)) where
  toMetadata = toMetadata <<< Map.fromFoldable
else instance ToMetadata a => ToMetadata (Array a) where
  toMetadata = List <<< map toMetadata

instance ToMetadata Int.Int where
  toMetadata = Int

instance ToMetadata ByteArray where
  toMetadata = Bytes

instance ToMetadata String where
  toMetadata = Text
