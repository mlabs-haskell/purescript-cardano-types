module Cardano.Types.Value where

import Prelude hiding (add, join, one, zero)

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , encodeAeson
  , (.:)
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( value_coin
  , value_multiasset
  , value_newWithAssets
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Asset (Asset(Asset, AdaAsset))
import Cardano.Types.AssetClass (AssetClass(AssetClass))
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.Coin as Coin
import Cardano.Types.MultiAsset
  ( MultiAsset(MultiAsset)
  , pprintMultiAsset
  , unionNonAda
  , unionWithNonAda
  )
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Array (foldr)
import Data.Foldable (all)
import Data.Generic.Rep (class Generic)
import Data.Lattice
  ( class JoinSemilattice
  , class MeetSemilattice
  , join
  , meet
  )
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.These (These(Both, That, This))
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | In Plutus, Ada is is stored inside the map (with currency symbol and token
-- | name being empty bytestrings). cardano-serialization-lib makes semantic
-- | distinction between native tokens and Ada, and we follow this convention.
data Value = Value Coin MultiAsset

derive instance Generic Value _
derive instance Eq Value
-- no Ord instance to prevent confusion

instance Partial => Semigroup Value where
  append x y =
    unsafePerformEffect $ maybe (throw "Value.append: numeric overflow") pure $
      unionWith BigNum.add x y

instance Partial => Monoid Value where
  mempty = empty

instance Ord Value where
  compare a b = if a `lt` b then LT else if a `gt` b then GT else EQ

instance Arbitrary Value where
  arbitrary = Value <$> arbitrary <*> arbitrary

instance Show Value where
  show = genericShow

instance JoinSemilattice Value where
  join (Value c1 m1) (Value c2 m2) = Value (c1 `join` c2) (m1 `join` m2)

instance MeetSemilattice Value where
  meet (Value c1 m1) (Value c2 m2) = Value (c1 `meet` c2) (m1 `meet` m2)

instance EncodeAeson Value where
  encodeAeson (Value coin nonAdaAsset) = encodeAeson
    { coin
    , nonAdaAsset
    }

instance DecodeAeson Value where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    coin <- obj .: "coin" >>= decodeAeson
    nonAdaAsset <- obj .: "nonAdaAsset" >>= decodeAeson
    pure $ Value coin nonAdaAsset

instance AsCbor Value where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

empty :: Value
empty = Value Coin.zero MultiAsset.empty

add :: Value -> Value -> Maybe Value
add = unionWith BigNum.add

sum :: Array Value -> Maybe Value
sum = foldr (\v acc -> acc >>= add v) (pure empty)

-- for compatibility with older CTL
mkValue :: Coin -> MultiAsset -> Value
mkValue = Value

pprintValue :: Value -> TagSet
pprintValue value = TagSet.fromArray $
  [ "Lovelace" `tag` BigNum.toString (valueOf AdaAsset value) ]
    <>
      if nonAdaAssets /= MultiAsset.empty then
        [ "Assets" `tagSetTag` pprintMultiAsset nonAdaAssets ]
      else []
  where
  nonAdaAssets = getMultiAsset value

valueOf :: Asset -> Value -> BigNum
valueOf AdaAsset (Value coin _) = unwrap coin
valueOf (Asset sh tn) (Value _ (MultiAsset mp)) = fromMaybe BigNum.zero $
  Map.lookup sh mp >>= Map.lookup tn

getCoin :: Value -> Coin
getCoin (Value coin _) = coin

getMultiAsset :: Value -> MultiAsset
getMultiAsset (Value _ nonAdaAsset) = nonAdaAsset

singleton :: ScriptHash -> AssetName -> BigNum -> Value
singleton sh an n = Value Coin.zero $ MultiAsset.singleton sh an n

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#unionWith
-- | Combines `Value` with a binary function on `BigInt`s.
unionWith
  :: (BigNum -> BigNum -> Maybe BigNum)
  -> Value
  -> Value
  -> Maybe Value
unionWith f (Value (Coin c) na) (Value (Coin c') na') =
  Value <$> (Coin <$> f c c') <*> unionWithNonAda f na na'

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#geq
-- | Check whether one `Value` is greater than or equal to another. See `Value` for an explanation of how operations on `Value`s work.
geq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
geq = checkBinRel (>=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#gt
-- | Check whether one `Value` is strictly greater than another. See `Value` for an explanation of how operations on `Value`s work.
gt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
gt l r = not (isZero l && isZero r) && checkBinRel (>) l r

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#leq
-- | Check whether one `Value` is less than or equal to another. See `Value` for an explanation of how operations on `Value`s work.
leq :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true, but this is fine.
leq = checkBinRel (<=)

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#lt
-- | Check whether one `Value` is strictly less than another. See `Value` for an explanation of how operations on `Value`s work.
lt :: Value -> Value -> Boolean
-- If both are zero then checkBinRel will be vacuously true. So we have a special case.
lt l r = not (isZero l && isZero r) && checkBinRel (<) l r

-- From https://github.com/mlabs-haskell/bot-plutus-interface/blob/master/src/BotPlutusInterface/PreBalance.hs
-- "isValueNat" uses unsafeFlattenValue which guards against zeros, so non-strict
-- inequality is redundant. So we use strict equality instead.
-- | Checks if every asset has positive quantity
isPositive :: Value -> Boolean
isPositive val = (all (\(_ /\ _ /\ a) -> a > BigNum.zero) $ MultiAsset.flatten $ getMultiAsset val) &&
  valueOf AdaAsset val > BigNum.zero

-- From https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#isZero
-- | Check whether a `Value` is zero.
isZero :: Value -> Boolean
isZero (Value coin (MultiAsset nonAdaAsset)) =
  all (all ((==) BigNum.zero)) nonAdaAsset && coin == Coin.zero

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkPred
checkPred :: (These BigNum BigNum -> Boolean) -> Value -> Value -> Boolean
checkPred f (Value (Coin l) ls) (Value (Coin r) rs) =
  let
    inner :: Map AssetName (These BigNum BigNum) -> Boolean
    inner = all f -- this "all" may need to be checked?
  in
    f (Both l r) && all inner (unionNonAda ls rs) -- this "all" may need to be checked?

-- https://playground.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/src/Plutus.V1.Ledger.Value.html#checkBinRel
-- Check whether a binary relation holds for value pairs of two `Value` maps,
-- supplying 0 where a key is only present in one of them.
checkBinRel :: (BigNum -> BigNum -> Boolean) -> Value -> Value -> Boolean
checkBinRel f l r =
  let
    unThese :: These BigNum BigNum -> Boolean
    unThese k' = case k' of
      This a -> f a BigNum.zero
      That b -> f BigNum.zero b
      Both a b -> f a b
  in
    checkPred unThese l r

minus :: Value -> Value -> Maybe Value
minus = unionWith BigNum.sub

assetToValue :: AssetClass -> BigNum -> Value
assetToValue (AssetClass cs tn) quantity =
  Value Coin.zero (MultiAsset.singleton cs tn quantity)

getAssetQuantity :: AssetClass -> Value -> BigNum
getAssetQuantity (AssetClass cs tn) = valueOf (Asset cs tn)

valueAssets :: Value -> Array (AssetClass /\ BigNum)
valueAssets (Value _ assets) =
  MultiAsset.flatten assets
    <#> \(cs /\ tn /\ quantity) -> AssetClass cs tn /\ quantity

flatten :: Value -> Array (Asset /\ BigNum)
flatten (Value coin assets) = [ adaAsset ] <> nonAdaAssets
  where
  adaAsset = AdaAsset /\ unwrap coin
  nonAdaAssets = MultiAsset.flatten assets <#>
    \(sh /\ an /\ amount) -> Asset sh an /\ amount

unflatten :: Array (Asset /\ BigNum) -> Maybe Value
unflatten = foldr consume (pure empty)
  where
  consume (AdaAsset /\ amount) acc = do
    acc >>= add (Value (Coin amount) MultiAsset.empty)
  consume (Asset sh tn /\ amount) acc = do
    acc >>= add (Value Coin.zero (MultiAsset.singleton sh tn amount))

valueAssetClasses :: Value -> Array AssetClass
valueAssetClasses = map fst <<< valueAssets

-- coin coversion

-- | Convert a `BigInt` to Ada-only `Value`
lovelaceValueOf :: BigNum -> Value
lovelaceValueOf = flip (Value <<< Coin) MultiAsset.empty

-- | Create a `Value` containing only the given `Coin`.
coinToValue :: Coin -> Value
coinToValue (Coin i) = lovelaceValueOf i

-- | Get the `Coin` in the given `Value`.
valueToCoin :: Value -> Coin
valueToCoin = Coin <<< valueOf AdaAsset

fromCsl :: Csl.Value -> Value
fromCsl value = Value coin multiAsset
  where
  coin = Coin $ wrap $ value_coin value
  multiAsset = fromMaybe MultiAsset.empty $
    MultiAsset.fromCsl <$> toMaybe (value_multiasset value)

toCsl :: Value -> Csl.Value
toCsl (Value coin multiAsset) =
  value_newWithAssets (unwrap $ unwrap coin) (MultiAsset.toCsl multiAsset)
