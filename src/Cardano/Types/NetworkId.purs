module Cardano.Types.NetworkId where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue, AtKey, Named)
  , decodeAeson
  , fromString
  , toStringifiedNumbersJson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (fromBytes, networkId_kind, networkId_mainnet, networkId_testnet, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (encodeTagged')
import Data.Either (Either(Left))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(Nothing, Just), fromJust, fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt)

data NetworkId
  = TestnetId
  | MainnetId

instance EncodeAeson NetworkId where
  encodeAeson = case _ of
    TestnetId -> encodeTagged' "TestnetId" {}
    MainnetId -> encodeTagged' "MainnetId" {}

instance DecodeAeson NetworkId where
  decodeAeson json = do
    { tag } <- (decodeAeson json :: Either _ { tag :: String })
    case tag of
      "TestnetId" -> pure TestnetId
      "MainnetId" -> pure MainnetId
      tagValue -> Left
        $ Named "NetworkId"
        $ AtKey "tag"
        $ UnexpectedValue
        $ toStringifiedNumbersJson
        $ fromString tagValue

instance Ord NetworkId where
  compare = compare `on` toInt

toInt :: NetworkId -> Int
toInt = case _ of
  TestnetId -> 0
  MainnetId -> 1

derive instance Eq NetworkId
derive instance Generic NetworkId _

instance Show NetworkId where
  show = genericShow

instance Arbitrary NetworkId where
  arbitrary = fromMaybe MainnetId <<< fromInt <$> chooseInt 0 1

fromInt :: Int -> Maybe NetworkId
fromInt = case _ of
  0 -> Just TestnetId
  1 -> Just MainnetId
  _ -> Nothing

instance AsCbor NetworkId where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

toCsl :: NetworkId -> Csl.NetworkId
toCsl = case _ of
  TestnetId -> networkId_testnet
  MainnetId -> networkId_mainnet

fromCsl :: Csl.NetworkId -> NetworkId
fromCsl cslNetworkId = unsafePartial $ fromJust
  $ fromInt <=< Int.fromNumber
  $ networkId_kind cslNetworkId
