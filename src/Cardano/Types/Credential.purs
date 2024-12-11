module Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  , asPubKeyHash
  , asScriptHash
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , (.:)
  )
import Cardano.AsCbor (class AsCbor, decodeCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.ScriptHash (ScriptHash)
import Control.Alt ((<|>))
import Data.Array (replicate)
import Data.Array.NonEmpty as NonEmptyArray
import Data.ByteArray (byteArrayFromIntArrayUnsafe)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (chooseInt, oneOf)

-- In CSL, this type is called StakeCredential. They reuse it for Payment Credentials as well.
data Credential = PubKeyHashCredential Ed25519KeyHash | ScriptHashCredential ScriptHash

asPubKeyHash :: Credential -> Maybe Ed25519KeyHash
asPubKeyHash (PubKeyHashCredential pk) = Just pk
asPubKeyHash _ = Nothing

asScriptHash :: Credential -> Maybe ScriptHash
asScriptHash (ScriptHashCredential sh) = Just sh
asScriptHash _ = Nothing

derive instance Generic Credential _
derive instance Eq Credential
derive instance Ord Credential

instance Show Credential where
  show = genericShow

instance Arbitrary Credential where
  arbitrary = oneOf $
    NonEmptyArray.cons' (PubKeyHashCredential <$> genKeyHash) [ ScriptHashCredential <$> genScriptHash ]
    where
    genKeyHash = do
      arr <- byteArrayFromIntArrayUnsafe <$> sequence
        ( replicate 28 (chooseInt 0 256)
        )
      pure $ unsafePartial $ fromJust
        $ decodeCbor
        $ wrap arr
    genScriptHash = do
      arr <- byteArrayFromIntArrayUnsafe <$> sequence
        ( replicate 28 (chooseInt 0 256)
        )
      pure $ unsafePartial $ fromJust
        $ decodeCbor
        $ wrap arr

instance EncodeAeson Credential where
  encodeAeson = case _ of
    PubKeyHashCredential kh -> encodeTagged' "PubKeyHashCredential" kh
    ScriptHashCredential sh -> encodeTagged' "ScriptHashCredential" sh

instance DecodeAeson Credential where
  decodeAeson = decodeAeson >=> \obj -> do
    tag <- obj .: "tag"
    let
      aesonContents
        :: forall (a :: Type). DecodeAeson a => Either JsonDecodeError a
      aesonContents = obj .: "content"
    case tag of
      "PubKeyHashCredential" -> PubKeyHashCredential <$> aesonContents
      "ScriptHashCredential" -> ScriptHashCredential <$> aesonContents
      _ -> Left $ TypeMismatch ("Unknown tag: " <> tag)

instance AsCbor Credential where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: Credential -> Csl.Credential
toCsl = case _ of
  PubKeyHashCredential kh ->
    Csl.credential_fromKeyhash (unwrap kh)
  ScriptHashCredential sh ->
    Csl.credential_fromScripthash (unwrap sh)

fromCsl :: Csl.Credential -> Credential
fromCsl sc = unsafePartial $ fromJust $
  (map (PubKeyHashCredential <<< wrap) $ toMaybe $ Csl.credential_toKeyhash sc) <|>
    (map (ScriptHashCredential <<< wrap) $ toMaybe $ Csl.credential_toScripthash sc)
