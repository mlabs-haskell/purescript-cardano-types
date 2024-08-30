module Cardano.Types.Mint
  ( Mint(Mint)
  , empty
  , flatten
  , unflatten
  , singleton
  , toCsl
  , fromCsl
  , union
  , fromMultiAsset
  , toMultiAsset
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( unpackListContainer
  , unpackMapContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Serialization.Lib.Internal
  ( packMapContainerWithClone
  )
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.MultiAsset (MultiAsset)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (empty, filter, singleton, toUnfoldable, unionWith) as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.These (these)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

newtype Mint = Mint (Map ScriptHash (Array (Map AssetName Int.Int)))

derive instance Generic Mint _

instance Eq Mint where
  eq a b = eq (unwrap $ normalizeMint a) (unwrap $ normalizeMint b)

derive instance Newtype Mint _
-- no Ord instance to prevent confusion

instance Show Mint where
  show = genericShow

instance EncodeAeson Mint where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson Mint where
  decodeAeson = map fromCsl <<< decodeAeson

instance Semigroup Mint where
  append = union

instance Monoid Mint where
  mempty = empty

instance AsCbor Mint where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

empty :: Mint
empty = Mint Map.empty

toMultiAsset :: Mint -> Maybe MultiAsset
toMultiAsset mint = MultiAsset.unflatten =<<
  for (flatten mint) \(sh /\ an /\ i) -> do
    bi <- Int.asPositive i
    pure (sh /\ an /\ bi)

fromMultiAsset :: MultiAsset -> Mint
fromMultiAsset ma =
  unflatten $ MultiAsset.flatten ma <#>
    \(sh /\ an /\ bi) -> sh /\ an /\ Int.newPositive bi

singleton :: ScriptHash -> AssetName -> Int.Int -> Mint
singleton sh an n = Mint (Map.singleton sh [ Map.singleton an n ])

flatten :: Mint -> Array (ScriptHash /\ AssetName /\ Int.Int)
flatten (Mint mp) =
  Map.toUnfoldable mp >>= \(sh /\ arr) -> arr >>= \mp' -> do
    Map.toUnfoldable mp' >>= \(tn /\ amount) -> pure (sh /\ tn /\ amount)

unflatten :: Array (ScriptHash /\ AssetName /\ Int.Int) -> Mint
unflatten = normalizeMint <<< foldr (flip accumulate) empty
  where
  uncurry2 f (a /\ b /\ c) = f a b c
  accumulate ma = union ma <<< uncurry2 singleton

union :: Mint -> Mint -> Mint
union a b = Mint $ these identity identity append <$> MultiAsset.union (unwrap a) (unwrap b)

normalizeMint :: Mint -> Mint
normalizeMint = filterMint (notEq Int.zero)

filterMint :: (Int.Int -> Boolean) -> Mint -> Mint
filterMint p (Mint mp) =
  Mint $ Map.filter (not <<< Array.null) $ map (Map.filter p) <$> mp

toCsl :: Mint -> Csl.Mint
toCsl mint | Mint mp <- normalizeMint mint =
  do
    packMapContainerWithClone $
      -- TODO: why is a clone needed?
      -- because some values are not cloned.
      -- replace packMapContainerWithClone with
      -- packMapContainer when these problems are addressed:
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/672f3b394b6b8c4a8f7cccc8752c5cb8e9a09cd4/rust/src/lib.rs#L1556
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/672f3b394b6b8c4a8f7cccc8752c5cb8e9a09cd4/rust/src/lib.rs#L1544
      Map.toUnfoldable mp <#> \(scriptHash /\ mintsAssets) ->
        unwrap scriptHash /\
          packListContainer
            ( mintsAssets <#> \mintAssets ->
                ( packMapContainerWithClone -- TODO: why is a clone needed
                    ( Map.toUnfoldable mintAssets <#> \(assetName /\ quantity) -> do
                        unwrap assetName /\ unwrap quantity
                    )
                )
            )

fromCsl :: Csl.Mint -> Mint
fromCsl = unpackMapContainer
  >>> map (wrap *** unpackMintsAssets)
  >>> foldMints
  >>> wrap
  where

  foldMints :: Array (ScriptHash /\ Map AssetName Int.Int) -> Map ScriptHash (Array (Map AssetName Int.Int))
  foldMints = foldr
    ( \(scriptHash /\ mint) acc ->
        Map.unionWith append acc
          (Map.singleton scriptHash [ mint ])
    )
    Map.empty

  unpackMintsAssets :: Csl.MintsAssets -> Map AssetName Int.Int
  unpackMintsAssets =
    unpackListContainer
      >>> map (unpackMapContainerToMapWith (wrap :: _ -> AssetName) wrap)
      >>> foldr (Map.unionWith addTokenQuantities) Map.empty

  addTokenQuantities :: Int.Int -> Int.Int -> Int.Int
  addTokenQuantities x y =
    unsafePerformEffect $ maybe (throw "Mint.fromCsl: numeric overflow") pure $
      Int.add x y
