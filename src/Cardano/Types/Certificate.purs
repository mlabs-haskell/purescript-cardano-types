module Cardano.Types.Certificate where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.Serialization.Lib
  ( certificate_asGenesisKeyDelegation
  , certificate_asMoveInstantaneousRewardsCert
  , certificate_asPoolRegistration
  , certificate_asPoolRetirement
  , certificate_asStakeDelegation
  , certificate_asStakeDeregistration
  , certificate_asStakeRegistration
  , certificate_newGenesisKeyDelegation
  , certificate_newMoveInstantaneousRewardsCert
  , certificate_newPoolRegistration
  , certificate_newPoolRetirement
  , certificate_newStakeDelegation
  , certificate_newStakeDeregistration
  , certificate_newStakeRegistration
  , genesisKeyDelegation_genesisDelegateHash
  , genesisKeyDelegation_genesishash
  , genesisKeyDelegation_new
  , genesisKeyDelegation_vrfKeyhash
  , moveInstantaneousRewardsCert_moveInstantaneousReward
  , moveInstantaneousRewardsCert_new
  , poolRegistration_new
  , poolRegistration_poolParams
  , poolRetirement_epoch
  , poolRetirement_new
  , poolRetirement_poolKeyhash
  , stakeDelegation_new
  , stakeDelegation_poolKeyhash
  , stakeDelegation_stakeCredential
  , stakeDeregistration_new
  , stakeDeregistration_stakeCredential
  , stakeRegistration_new
  , stakeRegistration_stakeCredential
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.Credential as Credential
import Cardano.Types.GenesisDelegateHash (GenesisDelegateHash)
import Cardano.Types.GenesisHash (GenesisHash)
import Cardano.Types.MoveInstantaneousReward (MoveInstantaneousReward)
import Cardano.Types.MoveInstantaneousReward as MoveInstantaneousReward
import Cardano.Types.MoveInstantaneousReward as MoveInstantaneousRewards
import Cardano.Types.PoolParams (PoolParams)
import Cardano.Types.PoolParams as PoolParams
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Cardano.Types.VRFKeyHash (VRFKeyHash)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt
import Partial.Unsafe (unsafePartial)

data Certificate
  = StakeRegistration StakeCredential
  | StakeDeregistration StakeCredential
  | StakeDelegation StakeCredential PoolPubKeyHash
  | PoolRegistration PoolParams
  | PoolRetirement
      { poolKeyHash :: PoolPubKeyHash
      , epoch :: Epoch
      }
  | GenesisKeyDelegation
      { genesisHash :: GenesisHash
      , genesisDelegateHash :: GenesisDelegateHash
      , vrfKeyhash :: VRFKeyHash
      }
  | MoveInstantaneousRewardsCert MoveInstantaneousReward

derive instance Eq Certificate
derive instance Ord Certificate
derive instance Generic Certificate _

instance EncodeAeson Certificate where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson Certificate where
  decodeAeson = decodeAeson >>> map fromCsl

instance Show Certificate where
  show = genericShow

instance AsCbor Certificate where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: Certificate -> Csl.Certificate
toCsl = case _ of
  StakeRegistration sc ->
    certificate_newStakeRegistration $ stakeRegistration_new
      $ Credential.toCsl
      $ unwrap sc
  StakeDeregistration sc ->
    certificate_newStakeDeregistration $ stakeDeregistration_new
      $ Credential.toCsl
      $ unwrap sc
  StakeDelegation sc pkh -> certificate_newStakeDelegation $ stakeDelegation_new
    (Credential.toCsl $ unwrap sc)
    (unwrap $ unwrap pkh)
  PoolRegistration prp -> certificate_newPoolRegistration
    $ poolRegistration_new
    $ PoolParams.toCsl prp
  PoolRetirement { poolKeyHash, epoch } -> certificate_newPoolRetirement $ poolRetirement_new
    (unwrap $ unwrap poolKeyHash)
    (UInt.toNumber $ unwrap epoch)
  GenesisKeyDelegation { genesisHash, genesisDelegateHash, vrfKeyhash } ->
    certificate_newGenesisKeyDelegation $
      genesisKeyDelegation_new (unwrap genesisHash) (unwrap genesisDelegateHash) (unwrap vrfKeyhash)
  MoveInstantaneousRewardsCert mir -> certificate_newMoveInstantaneousRewardsCert
    $ moveInstantaneousRewardsCert_new
    $ MoveInstantaneousReward.toCsl mir

fromCsl :: Csl.Certificate -> Certificate
fromCsl csl = unsafePartial $ fromJust $
  stakeRegistration
    <|> stakeDeregistration
    <|> stakeDelegation
    <|> poolRegistration
    <|> poolRetirement
    <|> genesisKeyDelegation
    <|> moveInstantaneousRewardsCert
  where
  stakeRegistration = toMaybe (certificate_asStakeRegistration csl) <#>
    stakeRegistration_stakeCredential >>> Credential.fromCsl
      >>> wrap
      >>> StakeRegistration
  stakeDeregistration = toMaybe (certificate_asStakeDeregistration csl) <#>
    stakeDeregistration_stakeCredential >>> Credential.fromCsl
      >>> wrap
      >>> StakeDeregistration
  stakeDelegation = toMaybe (certificate_asStakeDelegation csl) <#> \sd ->
    StakeDelegation
      (wrap $ Credential.fromCsl $ stakeDelegation_stakeCredential sd)
      (wrap $ wrap $ stakeDelegation_poolKeyhash sd)
  poolRegistration = toMaybe (certificate_asPoolRegistration csl) <#>
    poolRegistration_poolParams >>> PoolParams.fromCsl >>> PoolRegistration
  poolRetirement = toMaybe (certificate_asPoolRetirement csl) <#> \pr ->
    PoolRetirement
      { poolKeyHash: wrap $ wrap $ poolRetirement_poolKeyhash pr
      , epoch: wrap $ UInt.fromNumber $ poolRetirement_epoch pr
      }
  genesisKeyDelegation = toMaybe (certificate_asGenesisKeyDelegation csl) <#> \gkd ->
    GenesisKeyDelegation
      { genesisHash: wrap $ genesisKeyDelegation_genesishash gkd
      , genesisDelegateHash: wrap $ genesisKeyDelegation_genesisDelegateHash gkd
      , vrfKeyhash: wrap $ genesisKeyDelegation_vrfKeyhash gkd
      }
  moveInstantaneousRewardsCert = toMaybe (certificate_asMoveInstantaneousRewardsCert csl) <#>
    MoveInstantaneousRewardsCert <<< MoveInstantaneousRewards.fromCsl <<<
      moveInstantaneousRewardsCert_moveInstantaneousReward
