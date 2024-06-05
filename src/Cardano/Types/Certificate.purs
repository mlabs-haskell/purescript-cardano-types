module Cardano.Types.Certificate
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , VoteDelegCert
      , StakeVoteDelegCert
      , StakeRegDelegCert
      , VoteRegDelegCert
      , StakeVoteRegDelegCert
      , AuthCommitteeHotCert
      , ResignCommitteeColdCert
      , RegDrepCert
      , UnregDrepCert
      , UpdateDrepCert
      )
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCsl, toCsl) as Anchor
import Cardano.Types.Coin (Coin)
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential as Credential
import Cardano.Types.DRep (DRep)
import Cardano.Types.DRep (fromCsl, toCsl) as DRep
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.PoolParams (PoolParams)
import Cardano.Types.PoolParams as PoolParams
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
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
  | VoteDelegCert StakeCredential DRep
  | StakeVoteDelegCert StakeCredential PoolPubKeyHash DRep
  | StakeRegDelegCert StakeCredential PoolPubKeyHash Coin -- keyDeposit?
  | VoteRegDelegCert StakeCredential DRep Coin -- keyDeposit? 
  | StakeVoteRegDelegCert StakeCredential PoolPubKeyHash DRep Coin -- keyDeposit?
  | AuthCommitteeHotCert
      { coldCred :: Credential
      , hotCred :: Credential
      }
  | ResignCommitteeColdCert Credential (Maybe Anchor)
  | RegDrepCert Credential Coin (Maybe Anchor) -- separate type for DrepCredential?
  | UnregDrepCert Credential Coin -- returned deposit?
  | UpdateDrepCert Credential (Maybe Anchor)

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
  StakeRegistration stakeCred ->
    Csl.certificate_newStakeRegistration $
      Csl.stakeRegistration_new (Credential.toCsl $ unwrap stakeCred)

  StakeDeregistration stakeCred ->
    Csl.certificate_newStakeDeregistration $
      Csl.stakeDeregistration_new (Credential.toCsl $ unwrap stakeCred)

  StakeDelegation stakeCred pkh ->
    Csl.certificate_newStakeDelegation $
      Csl.stakeDelegation_new (Credential.toCsl $ unwrap stakeCred) (unwrap $ unwrap pkh)

  PoolRegistration poolParams ->
    Csl.certificate_newPoolRegistration $
      Csl.poolRegistration_new (PoolParams.toCsl poolParams)

  PoolRetirement { poolKeyHash, epoch } ->
    Csl.certificate_newPoolRetirement $
      Csl.poolRetirement_new (unwrap $ unwrap poolKeyHash) (UInt.toNumber $ unwrap epoch)

  VoteDelegCert stakeCred drep ->
    Csl.certificate_newVoteDelegation $
      Csl.voteDelegation_new (Credential.toCsl $ unwrap stakeCred) (DRep.toCsl drep)

  StakeVoteDelegCert stakeCred poolKeyHash drep ->
    Csl.certificate_newStakeAndVoteDelegation $
      Csl.stakeAndVoteDelegation_new (Credential.toCsl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (DRep.toCsl drep)

  StakeRegDelegCert stakeCred poolKeyHash deposit ->
    Csl.certificate_newStakeRegistrationAndDelegation $
      Csl.stakeRegistrationAndDelegation_new (Credential.toCsl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (unwrap $ unwrap deposit)

  VoteRegDelegCert stakeCred drep deposit ->
    Csl.certificate_newVoteRegistrationAndDelegation $
      Csl.voteRegistrationAndDelegation_new (Credential.toCsl $ unwrap stakeCred)
        (DRep.toCsl drep)
        (unwrap $ unwrap deposit)

  StakeVoteRegDelegCert stakeCred poolKeyHash drep deposit ->
    Csl.certificate_newStakeVoteRegistrationAndDelegation $
      Csl.stakeVoteRegistrationAndDelegation_new (Credential.toCsl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (DRep.toCsl drep)
        (unwrap $ unwrap deposit)

  AuthCommitteeHotCert { coldCred, hotCred } ->
    Csl.certificate_newCommitteeHotAuth $
      Csl.committeeHotAuth_new (Credential.toCsl coldCred) (Credential.toCsl hotCred)

  ResignCommitteeColdCert coldCred mbAnchor ->
    let
      coldCredCsl = Credential.toCsl coldCred
    in
      Csl.certificate_newCommitteeColdResign $
        maybe (Csl.committeeColdResign_new coldCredCsl)
          (Csl.committeeColdResign_newWithAnchor coldCredCsl <<< Anchor.toCsl)
          mbAnchor

  RegDrepCert drepCred deposit mbAnchor ->
    let
      drepCredCsl = Credential.toCsl drepCred
      depositCsl = unwrap $ unwrap deposit
    in
      Csl.certificate_newDrepRegistration $
        case mbAnchor of
          Nothing ->
            Csl.drepRegistration_new drepCredCsl depositCsl
          Just anchor ->
            Csl.drepRegistration_newWithAnchor drepCredCsl depositCsl (Anchor.toCsl anchor)

  UnregDrepCert drepCred coin ->
    Csl.certificate_newDrepDeregistration $
      Csl.drepDeregistration_new (Credential.toCsl drepCred) (unwrap $ unwrap coin)

  UpdateDrepCert drepCred mbAnchor ->
    let
      drepCredCsl = Credential.toCsl drepCred
    in
      Csl.certificate_newDrepUpdate $
        maybe (Csl.drepUpdate_new drepCredCsl)
          (Csl.drepUpdate_newWithAnchor drepCredCsl <<< Anchor.toCsl)
          mbAnchor

fromCsl :: Csl.Certificate -> Certificate
fromCsl csl = unsafePartial $ fromJust $
  stakeRegistration
    <|> stakeDeregistration
    <|> stakeDelegation
    <|> poolRegistration
    <|> poolRetirement
    <|> voteDelegCert
    <|> stakeVoteDelegCert
    <|> stakeRegDelegCert
    <|> voteRegDelegCert
    <|> stakeVoteRegDelegCert
    <|> authCommitteeHotCert
    <|> resignCommitteeColdCert
    <|> regDrepCert
    <|> unregDrepCert
    <|> updateDrepCert
  where
  stakeRegistration =
    toMaybe (Csl.certificate_asStakeRegistration csl) <#>
      Csl.stakeRegistration_stakeCredential >>> Credential.fromCsl
        >>> wrap
        >>> StakeRegistration

  stakeDeregistration =
    toMaybe (Csl.certificate_asStakeDeregistration csl) <#>
      Csl.stakeDeregistration_stakeCredential >>> Credential.fromCsl
        >>> wrap
        >>> StakeDeregistration

  stakeDelegation =
    toMaybe (Csl.certificate_asStakeDelegation csl) <#> \x ->
      StakeDelegation
        (wrap $ Credential.fromCsl $ Csl.stakeDelegation_stakeCredential x)
        (wrap $ wrap $ Csl.stakeDelegation_poolKeyhash x)

  poolRegistration =
    toMaybe (Csl.certificate_asPoolRegistration csl) <#>
      Csl.poolRegistration_poolParams >>> PoolParams.fromCsl >>> PoolRegistration

  poolRetirement =
    toMaybe (Csl.certificate_asPoolRetirement csl) <#> \x ->
      PoolRetirement
        { poolKeyHash: wrap $ wrap $ Csl.poolRetirement_poolKeyhash x
        , epoch: wrap $ UInt.fromNumber $ Csl.poolRetirement_epoch x
        }

  voteDelegCert =
    toMaybe (Csl.certificate_asVoteDelegation csl) <#> \x ->
      VoteDelegCert
        (wrap $ Credential.fromCsl $ Csl.voteDelegation_stakeCredential x)
        (DRep.fromCsl $ Csl.voteDelegation_drep x)

  stakeVoteDelegCert =
    toMaybe (Csl.certificate_asStakeAndVoteDelegation csl) <#> \x ->
      StakeVoteDelegCert
        (wrap $ Credential.fromCsl $ Csl.stakeAndVoteDelegation_stakeCredential x)
        (wrap $ wrap $ Csl.stakeAndVoteDelegation_poolKeyhash x)
        (DRep.fromCsl $ Csl.stakeAndVoteDelegation_drep x)

  stakeRegDelegCert =
    toMaybe (Csl.certificate_asStakeRegistrationAndDelegation csl) <#> \x ->
      StakeRegDelegCert
        (wrap $ Credential.fromCsl $ Csl.stakeRegistrationAndDelegation_stakeCredential x)
        (wrap $ wrap $ Csl.stakeRegistrationAndDelegation_poolKeyhash x)
        (wrap $ wrap $ Csl.stakeRegistrationAndDelegation_coin x)

  voteRegDelegCert =
    toMaybe (Csl.certificate_asVoteRegistrationAndDelegation csl) <#> \x ->
      VoteRegDelegCert
        (wrap $ Credential.fromCsl $ Csl.voteRegistrationAndDelegation_stakeCredential x)
        (DRep.fromCsl $ Csl.voteRegistrationAndDelegation_drep x)
        (wrap $ wrap $ Csl.voteRegistrationAndDelegation_coin x)

  stakeVoteRegDelegCert =
    toMaybe (Csl.certificate_asStakeVoteRegistrationAndDelegation csl) <#> \x ->
      StakeVoteRegDelegCert
        (wrap $ Credential.fromCsl $ Csl.stakeVoteRegistrationAndDelegation_stakeCredential x)
        (wrap $ wrap $ Csl.stakeVoteRegistrationAndDelegation_poolKeyhash x)
        (DRep.fromCsl $ Csl.stakeVoteRegistrationAndDelegation_drep x)
        (wrap $ wrap $ Csl.stakeVoteRegistrationAndDelegation_coin x)

  authCommitteeHotCert =
    toMaybe (Csl.certificate_asCommitteeHotAuth csl) <#> \x ->
      AuthCommitteeHotCert
        { coldCred: Credential.fromCsl $ Csl.committeeHotAuth_committeeColdKey x
        , hotCred: Credential.fromCsl $ Csl.committeeHotAuth_committeeHotKey x
        }

  resignCommitteeColdCert =
    toMaybe (Csl.certificate_asCommitteeColdResign csl) <#> \x ->
      ResignCommitteeColdCert
        (Credential.fromCsl $ Csl.committeeColdResign_committeeColdKey x)
        (Anchor.fromCsl <$> toMaybe (Csl.committeeColdResign_anchor x))

  regDrepCert =
    toMaybe (Csl.certificate_asDrepRegistration csl) <#> \x ->
      RegDrepCert
        (Credential.fromCsl $ Csl.drepRegistration_votingCredential x)
        (wrap $ wrap $ Csl.drepRegistration_coin x)
        (Anchor.fromCsl <$> toMaybe (Csl.drepRegistration_anchor x))

  unregDrepCert =
    toMaybe (Csl.certificate_asDrepDeregistration csl) <#> \x ->
      UnregDrepCert
        (Credential.fromCsl $ Csl.drepDeregistration_votingCredential x)
        (wrap $ wrap $ Csl.drepDeregistration_coin x)

  updateDrepCert =
    toMaybe (Csl.certificate_asDrepUpdate csl) <#> \x ->
      UpdateDrepCert
        (Credential.fromCsl $ Csl.drepUpdate_votingCredential x)
        (Anchor.fromCsl <$> toMaybe (Csl.drepUpdate_anchor x))
