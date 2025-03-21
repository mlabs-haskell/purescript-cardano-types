module Cardano.Types.Mint
  ( Mint(Mint)
  , empty
  , flatten
  , unflatten
  , singleton
  , toCdl
  , fromCdl
  , union
  , fromMultiAsset
  , toMultiAsset
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( unpackListContainer
  , unpackMapContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Data.Lite as Cdl
import Cardano.Data.Lite.Internal (packMapContainerWithClone)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.MultiAsset (MultiAsset)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Array (foldM)
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (empty, filter, isEmpty, singleton, toUnfoldable, unionWith) as Map
import Data.Maybe (Maybe, fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

newtype Mint = Mint (Map ScriptHash (Map AssetName Int.Int))

derive instance Generic Mint _

instance Eq Mint where
  eq a b = eq (unwrap $ normalizeMint a) (unwrap $ normalizeMint b)

derive instance Newtype Mint _
-- no Ord instance to prevent confusion

instance Show Mint where
  show = genericShow

instance EncodeAeson Mint where
  encodeAeson = toCdl >>> encodeAeson

instance DecodeAeson Mint where
  decodeAeson = map fromCdl <<< decodeAeson

instance Partial => Semigroup Mint where
  append x y =
    unsafePerformEffect $ maybe (throw "Mint.append: numeric overflow") pure $
      unionWithNonAda Int.add x y

instance Partial => Monoid Mint where
  mempty = empty

instance AsCbor Mint where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

empty :: Mint
empty = Mint Map.empty

toMultiAsset :: Mint -> Maybe MultiAsset
toMultiAsset mint = MultiAsset.unflatten =<<
  for (flatten mint) \(sh /\ an /\ i) -> do
    bi <- Int.asPositive i
    pure (sh /\ an /\ bi)

fromMultiAsset :: MultiAsset -> Mint
fromMultiAsset ma =
  unsafePartial $ fromJust
    $ unflatten
    $ MultiAsset.flatten ma <#>
        \(sh /\ an /\ bi) -> sh /\ an /\ Int.newPositive bi

singleton :: ScriptHash -> AssetName -> Int.Int -> Mint
singleton sh an n = normalizeMint $ Mint (Map.singleton sh (Map.singleton an n))

flatten :: Mint -> Array (ScriptHash /\ AssetName /\ Int.Int)
flatten (Mint mp) =
  Map.toUnfoldable mp >>= \(sh /\ mp') -> do
    Map.toUnfoldable mp' >>= \(tn /\ amount) -> pure (sh /\ tn /\ amount)

unflatten :: Array (ScriptHash /\ AssetName /\ Int.Int) -> Maybe Mint
unflatten = map normalizeMint <<< foldM accumulate empty
  where
  uncurry2 f (a /\ b /\ c) = f a b c
  accumulate ma = union ma <<< uncurry2 singleton

union :: Mint -> Mint -> Maybe Mint
union = unionWithNonAda Int.add

unionWithNonAda
  :: (Int.Int -> Int.Int -> Maybe Int.Int)
  -> Mint
  -> Mint
  -> Maybe Mint
unionWithNonAda f ls rs =
  let
    combined :: Map ScriptHash (Map AssetName (These Int.Int Int.Int))
    combined = unionNonAda ls rs

    unBoth :: These Int.Int Int.Int -> Maybe Int.Int
    unBoth k' = case k' of
      This a -> f a Int.zero
      That b -> f Int.zero b
      Both a b -> f a b
  in
    normalizeMint <<< Mint <$> traverse (traverse unBoth) combined

normalizeMint :: Mint -> Mint
normalizeMint = filterMint (notEq Int.zero)

filterMint :: (Int.Int -> Boolean) -> Mint -> Mint
filterMint p (Mint mp) =
  Mint $ Map.filter (not Map.isEmpty) $ Map.filter p <$> mp

unionNonAda
  :: Mint
  -> Mint
  -> Map ScriptHash (Map AssetName (These Int.Int Int.Int))
unionNonAda (Mint l) (Mint r) =
  let
    combined
      :: Map ScriptHash
           (These (Map AssetName Int.Int) (Map AssetName Int.Int))
    combined = MultiAsset.union l r

    unBoth
      :: These (Map AssetName Int.Int) (Map AssetName Int.Int)
      -> Map AssetName (These Int.Int Int.Int)
    unBoth k = case k of
      This a -> This <$> a
      That b -> That <$> b
      Both a b -> MultiAsset.union a b
  in
    unBoth <$> combined

toCdl :: Mint -> Cdl.Mint
toCdl mint | Mint mp <- normalizeMint mint =
  do
    packMapContainerWithClone $
      -- TODO: why is a clone needed?
      -- because some values are not cloned.
      -- replace packMapContainerWithClone with
      -- packMapContainer when these problems are addressed:
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/672f3b394b6b8c4a8f7cccc8752c5cb8e9a09cd4/rust/src/lib.rs#L1556
      -- https://github.com/Emurgo/cardano-serialization-lib/blob/672f3b394b6b8c4a8f7cccc8752c5cb8e9a09cd4/rust/src/lib.rs#L1544
      Map.toUnfoldable mp <#> \(scriptHash /\ mintAssets) ->
        unwrap scriptHash /\
          coerceToMints
            ( packMapContainerWithClone -- TODO: why is a clone needed
                ( Map.toUnfoldable mintAssets <#> \(assetName /\ quantity) -> do
                    unwrap assetName /\ unwrap quantity
                )
            )
  where
  coerceToMints :: Cdl.MintAssets -> Cdl.MintsAssets
  coerceToMints = unsafeCoerce

-- NOTE: CSL.Mint can store multiple entries for the same policy id.
-- We should probably change the representation to match CSL
-- https://github.com/Emurgo/cardano-serialization-lib/blob/4a35ef11fd5c4931626c03025fe6f67743a6bdf9/rust/src/lib.rs#L3627
fromCdl :: Cdl.Mint -> Mint
fromCdl = unpackMapContainer
  >>> map (wrap *** unpackMintsAssets)
  >>> foldMints
  >>> wrap
  where

  foldMints :: Array (ScriptHash /\ Map AssetName Int.Int) -> Map ScriptHash (Map AssetName Int.Int)
  foldMints = foldr
    ( \(scriptHash /\ mint) acc ->
        Map.unionWith (Map.unionWith addTokenQuantities) acc
          (Map.singleton scriptHash mint)
    )
    Map.empty

  unpackMintsAssets :: Cdl.MintsAssets -> Map AssetName Int.Int
  unpackMintsAssets =
    unpackListContainer
      >>> map (unpackMapContainerToMapWith (wrap :: _ -> AssetName) wrap)
      >>> foldr (Map.unionWith addTokenQuantities) Map.empty

  addTokenQuantities :: Int.Int -> Int.Int -> Int.Int
  addTokenQuantities x y =
    unsafePerformEffect $ maybe (throw "Mint.fromCdl: numeric overflow") pure $
      Int.add x y
