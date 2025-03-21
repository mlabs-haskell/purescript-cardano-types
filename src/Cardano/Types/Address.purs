module Cardano.Types.Address where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , decodeAeson
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( address_fromBech32
  , address_networkId
  , address_toBech32
  , baseAddress_fromAddress
  , baseAddress_toAddress
  , byronAddress_fromAddress
  , byronAddress_toAddress
  , enterpriseAddress_fromAddress
  , enterpriseAddress_toAddress
  , fromBytes
  , rewardAddress_fromAddress
  , rewardAddress_toAddress
  , toBytes
  )
import Cardano.Data.Lite as Cdl
import Cardano.Types.BaseAddress (BaseAddress, fromCdl, toCdl) as BA
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.ByronAddress (ByronAddress) as BA
import Cardano.Types.EnterpriseAddress as EA
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
import Cardano.Types.RewardAddress as RA
import Cardano.Types.StakeCredential (StakeCredential)
import Control.Alt ((<|>))
import Data.Array.NonEmpty as NonEmpty
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Literals.Undefined (undefined)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (oneOf)
import Unsafe.Coerce (unsafeCoerce)

data Address
  = BaseAddress BA.BaseAddress
  | ByronAddress BA.ByronAddress
  | EnterpriseAddress EA.EnterpriseAddress
  | RewardAddress RA.RewardAddress

derive instance Generic Address _
derive instance Eq Address
derive instance Ord Address

instance EncodeAeson Address where
  encodeAeson = encodeAeson <<< toBech32

instance DecodeAeson Address where
  decodeAeson = decodeAeson >=> fromBech32 >>> note (TypeMismatch "Address")

instance Show Address where
  show addr = "(Address.fromBech32Unsafe " <> show (toBech32 addr) <> ")"

instance AsCbor Address where
  encodeCbor = toCdl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCdl

instance Arbitrary Address where
  arbitrary = oneOf $ NonEmpty.cons' (BaseAddress <$> arbitrary)
    [ ByronAddress <$> arbitrary
    , EnterpriseAddress <$> arbitrary
    , RewardAddress <$> arbitrary
    ]

mkPaymentAddress
  :: NetworkId
  -> PaymentCredential
  -> Maybe StakeCredential
  -> Address
mkPaymentAddress networkId paymentCredential = case _ of
  Just stakeCredential ->
    BaseAddress { networkId, paymentCredential, stakeCredential }
  Nothing ->
    EnterpriseAddress { networkId, paymentCredential }

getNetworkId :: Address -> NetworkId
getNetworkId = unsafePartial $
  toCdl >>> address_networkId >>> Int.fromNumber >>> fromJust >>> NetworkId.fromInt >>> fromJust

getPaymentCredential :: Address -> Maybe PaymentCredential
getPaymentCredential = case _ of
  BaseAddress { paymentCredential } -> Just paymentCredential
  ByronAddress _ -> Nothing
  EnterpriseAddress { paymentCredential } -> Just paymentCredential
  RewardAddress _ -> Nothing

getStakeCredential :: Address -> Maybe StakeCredential
getStakeCredential = case _ of
  BaseAddress { stakeCredential } -> Just stakeCredential
  ByronAddress _ -> Nothing
  EnterpriseAddress _ -> Nothing
  RewardAddress { stakeCredential } -> Just stakeCredential

toBech32 :: Address -> Bech32String
toBech32 = toCdl >>> flip address_toBech32 (unsafeCoerce undefined)

fromBech32 :: Bech32String -> Maybe Address
fromBech32 = map fromCdl <<< toMaybe <<< address_fromBech32

fromBech32Unsafe :: Partial => Bech32String -> Address
fromBech32Unsafe = fromJust <<< fromBech32

toCdl :: Address -> Cdl.Address
toCdl = case _ of
  BaseAddress ba ->
    baseAddress_toAddress $ BA.toCdl ba
  ByronAddress ba ->
    byronAddress_toAddress $ unwrap ba
  EnterpriseAddress ea ->
    enterpriseAddress_toAddress $ EA.toCdl ea
  RewardAddress ra ->
    rewardAddress_toAddress $ RA.toCdl ra

fromCdl :: Cdl.Address -> Address
fromCdl addr =
  unsafePartial $ fromJust $
    asBaseAddress <|> asByronAddress <|> asEnterpriseAddress <|> asRewardAddress
  where
  asBaseAddress = toMaybe (baseAddress_fromAddress addr) <#>
    BaseAddress <<< BA.fromCdl
  asByronAddress = toMaybe (byronAddress_fromAddress addr) <#>
    ByronAddress <<< wrap
  asEnterpriseAddress = toMaybe (enterpriseAddress_fromAddress addr) <#>
    EnterpriseAddress <<< EA.fromCdl
  asRewardAddress = toMaybe (rewardAddress_fromAddress addr) <#>
    RewardAddress <<< RA.fromCdl
