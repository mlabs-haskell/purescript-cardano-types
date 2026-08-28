module Cardano.Types.GovId
  ( GovId(GovCredential, GovAction)
  , GovIdType(CCHot, CCCold, DRep)
  , fromBech32
  , toBech32
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Credential (Credential(PubKeyHashCredential, ScriptHashCredential))
import Cardano.Types.GovernanceActionId (GovernanceActionId(GovernanceActionId))
import Cardano.Types.Internal.Helpers (decodeBech32, encodeBech32)
import Control.Alternative (guard)
import Data.Array (dropWhile, head, length, singleton) as Array
import Data.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe, byteArrayToIntArray, byteLength, subarray)
import Data.Either (hush)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (and, or, shl, shr) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Data.UInt (and, fromInt, or, shl, shr, toInt) as UInt
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

-- Reference: https://cips.cardano.org/cip/CIP-0129

data GovIdType
  = CCHot
  | CCCold
  | DRep

derive instance Generic GovIdType _
derive instance Eq GovIdType
derive instance Ord GovIdType

instance Show GovIdType where
  show = genericShow

instance Arbitrary GovIdType where
  arbitrary = genericArbitrary

encodeGovIdType :: GovIdType -> Int
encodeGovIdType govIdType = n `Int.shl` 4
  where
  n :: Int
  n =
    case govIdType of
      CCHot -> 0x0
      CCCold -> 0x1
      DRep -> 0x2

decodeGovIdType :: Int -> Maybe GovIdType
decodeGovIdType =
  case _ of
    0x0 -> Just CCHot
    0x1 -> Just CCCold
    0x2 -> Just DRep
    _ -> Nothing

bech32PrefixForGovIdType :: GovIdType -> String
bech32PrefixForGovIdType =
  case _ of
    CCHot -> "cc_hot"
    CCCold -> "cc_cold"
    DRep -> "drep"

data GovId
  = GovCredential
      { govIdType :: GovIdType
      , cred :: Credential
      }
  | GovAction GovernanceActionId

derive instance Generic GovId _
derive instance Eq GovId
derive instance Ord GovId

instance Show GovId where
  show = genericShow

instance Arbitrary GovId where
  arbitrary = genericArbitrary

encodeCred :: Credential -> Int
encodeCred =
  case _ of
    PubKeyHashCredential _ ->
      0x2
    ScriptHashCredential _ ->
      0x3

toBytes :: GovId -> ByteArray
toBytes =
  case _ of
    GovCredential { govIdType, cred } ->
      let
        header =
          byteArrayFromIntArrayUnsafe $ Array.singleton
            (encodeGovIdType govIdType `Int.or` encodeCred cred)
      in
        header <>
          unwrap case cred of
            PubKeyHashCredential pkh ->
              encodeCbor pkh
            ScriptHashCredential sh ->
              encodeCbor sh
    GovAction (GovernanceActionId { transactionId, index }) ->
      unwrap (encodeCbor transactionId)
        <> uintToBeBytes index

uintToBeBytes :: UInt -> ByteArray
uintToBeBytes n
  | n == zero = byteArrayFromIntArrayUnsafe $ Array.singleton 0x0
  | otherwise =
      byteArrayFromIntArrayUnsafe $ UInt.toInt <$> Array.dropWhile (eq zero)
        [ (n `UInt.shr` UInt.fromInt 24) `UInt.and` mask
        , (n `UInt.shr` UInt.fromInt 16) `UInt.and` mask
        , (n `UInt.shr` UInt.fromInt 8) `UInt.and` mask
        , n `UInt.and` mask
        ]
      where
      mask :: UInt
      mask = UInt.fromInt 0xff

beBytesToUInt :: ByteArray -> Maybe UInt
beBytesToUInt bytes = do
  let ints = byteArrayToIntArray bytes
  guard $ Array.length ints <= 4
  Just $ foldl f zero ints
  where
  f :: UInt -> Int -> UInt
  f acc byte =
    (acc `UInt.shl` UInt.fromInt 8) `UInt.or` UInt.fromInt byte

toBech32 :: GovId -> Bech32String
toBech32 govId = encodeBech32 prefix $ toBytes govId
  where
  prefix :: String
  prefix =
    case govId of
      GovCredential { govIdType } ->
        bech32PrefixForGovIdType govIdType
      GovAction _ ->
        "gov_action"

fromBech32 :: Bech32String -> Maybe GovId
fromBech32 str = do
  { prefix, bytes } <- hush $ decodeBech32 str
  let len = byteLength bytes
  case prefix of
    "gov_action" -> do
      guard $ len >= 33
      let txHashBytes = subarray 0 32 bytes
      txHash <- decodeCbor $ wrap txHashBytes
      let indexBytes = subarray 32 len bytes
      index <- beBytesToUInt indexBytes
      Just $ GovAction $ GovernanceActionId
        { transactionId: txHash
        , index
        }
    _ -> do
      header <- Array.head $ byteArrayToIntArray bytes
      govIdType <- decodeGovIdType $ header `Int.shr` 4
      guard $ prefix == bech32PrefixForGovIdType govIdType
      let
        credType = header `Int.and` 0x0f
        hashBytes = subarray 1 len bytes
      cred <-
        case credType of
          0x2 ->
            PubKeyHashCredential <$> decodeCbor (wrap hashBytes)
          0x3 ->
            ScriptHashCredential <$> decodeCbor (wrap hashBytes)
          _ ->
            Nothing
      Just $ GovCredential
        { govIdType
        , cred
        }
