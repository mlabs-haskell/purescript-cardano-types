module Cardano.Types.NetworkId
  ( NetworkId(TestnetId, MainnetId)
  , fromCsl
  , fromInt
  , toCsl
  , toInt
  ) where

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
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (encodeTagged')
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data NetworkId
  = TestnetId
  | MainnetId

derive instance Generic NetworkId _
derive instance Eq NetworkId
derive instance Ord NetworkId

instance Show NetworkId where
  show = genericShow

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

instance AsCbor NetworkId where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

instance Arbitrary NetworkId where
  arbitrary = genericArbitrary

toInt :: NetworkId -> Int
toInt = case _ of
  TestnetId -> 0
  MainnetId -> 1

fromInt :: Int -> Maybe NetworkId
fromInt = case _ of
  0 -> Just TestnetId
  1 -> Just MainnetId
  _ -> Nothing

toCsl :: NetworkId -> Csl.NetworkId
toCsl = case _ of
  TestnetId -> Csl.networkId_testnet
  MainnetId -> Csl.networkId_mainnet

fromCsl :: Csl.NetworkId -> NetworkId
fromCsl cslNetworkId =
  case Csl.fromCslEnum (Csl.networkId_kind cslNetworkId) of
    Csl.NetworkIdKind_Testnet -> TestnetId
    Csl.NetworkIdKind_Mainnet -> MainnetId
