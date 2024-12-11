module Cardano.Types.PoolParams where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( packListContainer
  , poolParams_cost
  , poolParams_margin
  , poolParams_new
  , poolParams_operator
  , poolParams_pledge
  , poolParams_poolMetadata
  , poolParams_poolOwners
  , poolParams_relays
  , poolParams_rewardAccount
  , poolParams_vrfKeyhash
  , unpackListContainer
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.PoolMetadata (PoolMetadata)
import Cardano.Types.PoolMetadata as PoolMetadata
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.Relay (Relay)
import Cardano.Types.Relay as Relay
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress as RewardAddress
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval as UnitInterval
import Cardano.Types.VRFKeyHash (VRFKeyHash)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Literals.Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

newtype PoolParams = PoolParams
  { operator :: PoolPubKeyHash -- cwitness (cert)
  , vrfKeyhash :: VRFKeyHash
  -- needed to prove that the pool won the lottery
  , pledge :: BigNum
  , cost :: BigNum -- >= pparams.minPoolCost
  , margin :: UnitInterval -- proportion that goes to the reward account
  , rewardAccount :: RewardAddress
  , poolOwners :: Array Ed25519KeyHash
  -- payment key hashes that contribute to pledge amount
  , relays :: Array Relay
  , poolMetadata :: Maybe PoolMetadata
  }

derive instance Newtype PoolParams _
derive instance Generic PoolParams _
derive instance Eq PoolParams
derive instance Ord PoolParams
derive newtype instance EncodeAeson PoolParams
derive newtype instance DecodeAeson PoolParams

instance Show PoolParams where
  show = genericShow

instance AsCbor PoolParams where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: PoolParams -> Csl.PoolParams
toCsl
  ( PoolParams
      { operator
      , vrfKeyhash
      , pledge
      , cost
      , margin
      , rewardAccount
      , poolOwners
      , relays
      , poolMetadata
      }
  ) = poolParams_new
  (unwrap $ unwrap operator)
  (unwrap vrfKeyhash)
  (unwrap pledge)
  (unwrap cost)
  (UnitInterval.toCsl margin)
  (RewardAddress.toCsl rewardAccount)
  (packListContainer $ map unwrap poolOwners)
  (packListContainer $ Relay.toCsl <$> relays)
  (fromMaybe (unsafeCoerce undefined) (poolMetadata <#> PoolMetadata.toCsl))

fromCsl :: Csl.PoolParams -> PoolParams
fromCsl csl = PoolParams
  { operator: wrap $ wrap $ poolParams_operator csl
  , vrfKeyhash: wrap $ poolParams_vrfKeyhash csl
  , pledge: wrap $ poolParams_pledge csl
  , cost: wrap $ poolParams_cost csl
  , margin: UnitInterval.fromCsl $ poolParams_margin csl
  , rewardAccount: RewardAddress.fromCsl $ poolParams_rewardAccount csl
  , poolOwners: map wrap $ unpackListContainer $ poolParams_poolOwners csl
  , relays: map Relay.fromCsl $ unpackListContainer $ poolParams_relays csl
  , poolMetadata: toMaybe (poolParams_poolMetadata csl) <#> PoolMetadata.fromCsl
  }
