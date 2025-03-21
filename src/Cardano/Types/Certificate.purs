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
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCdl, toCdl) as Anchor
import Cardano.Types.Coin (Coin)
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential as Credential
import Cardano.Types.DRep (DRep)
import Cardano.Types.DRep (fromCdl, toCdl) as DRep
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
  encodeAeson = toCdl >>> encodeAeson

instance DecodeAeson Certificate where
  decodeAeson = decodeAeson >>> map fromCdl

instance Show Certificate where
  show = genericShow

instance AsCbor Certificate where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

toCdl :: Certificate -> Cdl.Certificate
toCdl = case _ of
  StakeRegistration stakeCred ->
    Cdl.certificate_newStakeRegistration $
      Cdl.stakeRegistration_new (Credential.toCdl $ unwrap stakeCred)

  StakeDeregistration stakeCred ->
    Cdl.certificate_newStakeDeregistration $
      Cdl.stakeDeregistration_new (Credential.toCdl $ unwrap stakeCred)

  StakeDelegation stakeCred pkh ->
    Cdl.certificate_newStakeDelegation $
      Cdl.stakeDelegation_new (Credential.toCdl $ unwrap stakeCred) (unwrap $ unwrap pkh)

  PoolRegistration poolParams ->
    Cdl.certificate_newPoolRegistration $
      Cdl.poolRegistration_new (PoolParams.toCdl poolParams)

  PoolRetirement { poolKeyHash, epoch } ->
    Cdl.certificate_newPoolRetirement $
      Cdl.poolRetirement_new (unwrap $ unwrap poolKeyHash) (UInt.toNumber $ unwrap epoch)

  VoteDelegCert stakeCred drep ->
    Cdl.certificate_newVoteDelegation $
      Cdl.voteDelegation_new (Credential.toCdl $ unwrap stakeCred) (DRep.toCdl drep)

  StakeVoteDelegCert stakeCred poolKeyHash drep ->
    Cdl.certificate_newStakeAndVoteDelegation $
      Cdl.stakeAndVoteDelegation_new (Credential.toCdl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (DRep.toCdl drep)

  StakeRegDelegCert stakeCred poolKeyHash deposit ->
    Cdl.certificate_newStakeRegistrationAndDelegation $
      Cdl.stakeRegistrationAndDelegation_new (Credential.toCdl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (unwrap $ unwrap deposit)

  VoteRegDelegCert stakeCred drep deposit ->
    Cdl.certificate_newVoteRegistrationAndDelegation $
      Cdl.voteRegistrationAndDelegation_new (Credential.toCdl $ unwrap stakeCred)
        (DRep.toCdl drep)
        (unwrap $ unwrap deposit)

  StakeVoteRegDelegCert stakeCred poolKeyHash drep deposit ->
    Cdl.certificate_newStakeVoteRegistrationAndDelegation $
      Cdl.stakeVoteRegistrationAndDelegation_new (Credential.toCdl $ unwrap stakeCred)
        (unwrap $ unwrap poolKeyHash)
        (DRep.toCdl drep)
        (unwrap $ unwrap deposit)

  AuthCommitteeHotCert { coldCred, hotCred } ->
    Cdl.certificate_newCommitteeHotAuth $
      Cdl.committeeHotAuth_new (Credential.toCdl coldCred) (Credential.toCdl hotCred)

  ResignCommitteeColdCert coldCred mbAnchor ->
    let
      coldCredCdl = Credential.toCdl coldCred
    in
      Cdl.certificate_newCommitteeColdResign $
        maybe (Cdl.committeeColdResign_new coldCredCdl)
          (Cdl.committeeColdResign_newWithAnchor coldCredCdl <<< Anchor.toCdl)
          mbAnchor

  RegDrepCert drepCred deposit mbAnchor ->
    let
      drepCredCdl = Credential.toCdl drepCred
      depositCdl = unwrap $ unwrap deposit
    in
      Cdl.certificate_newDrepRegistration $
        case mbAnchor of
          Nothing ->
            Cdl.dRepRegistration_new drepCredCdl depositCdl
          Just anchor ->
            Cdl.dRepRegistration_newWithAnchor drepCredCdl depositCdl (Anchor.toCdl anchor)

  UnregDrepCert drepCred coin ->
    Cdl.certificate_newDrepDeregistration $
      Cdl.dRepDeregistration_new (Credential.toCdl drepCred) (unwrap $ unwrap coin)

  UpdateDrepCert drepCred mbAnchor ->
    let
      drepCredCdl = Credential.toCdl drepCred
    in
      Cdl.certificate_newDrepUpdate $
        maybe (Cdl.dRepUpdate_new drepCredCdl)
          (Cdl.dRepUpdate_newWithAnchor drepCredCdl <<< Anchor.toCdl)
          mbAnchor

fromCdl :: Cdl.Certificate -> Certificate
fromCdl csl = unsafePartial $ fromJust $
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
    toMaybe (Cdl.certificate_asStakeRegistration csl) <#>
      Cdl.stakeRegistration_stakeCredential >>> Credential.fromCdl
        >>> wrap
        >>> StakeRegistration

  stakeDeregistration =
    toMaybe (Cdl.certificate_asStakeDeregistration csl) <#>
      Cdl.stakeDeregistration_stakeCredential >>> Credential.fromCdl
        >>> wrap
        >>> StakeDeregistration

  stakeDelegation =
    toMaybe (Cdl.certificate_asStakeDelegation csl) <#> \x ->
      StakeDelegation
        (wrap $ Credential.fromCdl $ Cdl.stakeDelegation_stakeCredential x)
        (wrap $ wrap $ Cdl.stakeDelegation_poolKeyhash x)

  poolRegistration =
    toMaybe (Cdl.certificate_asPoolRegistration csl) <#>
      Cdl.poolRegistration_poolParams >>> PoolParams.fromCdl >>> PoolRegistration

  poolRetirement =
    toMaybe (Cdl.certificate_asPoolRetirement csl) <#> \x ->
      PoolRetirement
        { poolKeyHash: wrap $ wrap $ Cdl.poolRetirement_poolKeyhash x
        , epoch: wrap $ UInt.fromNumber $ Cdl.poolRetirement_epoch x
        }

  voteDelegCert =
    toMaybe (Cdl.certificate_asVoteDelegation csl) <#> \x ->
      VoteDelegCert
        (wrap $ Credential.fromCdl $ Cdl.voteDelegation_stakeCredential x)
        (DRep.fromCdl $ Cdl.voteDelegation_drep x)

  stakeVoteDelegCert =
    toMaybe (Cdl.certificate_asStakeAndVoteDelegation csl) <#> \x ->
      StakeVoteDelegCert
        (wrap $ Credential.fromCdl $ Cdl.stakeAndVoteDelegation_stakeCredential x)
        (wrap $ wrap $ Cdl.stakeAndVoteDelegation_poolKeyhash x)
        (DRep.fromCdl $ Cdl.stakeAndVoteDelegation_drep x)

  stakeRegDelegCert =
    toMaybe (Cdl.certificate_asStakeRegistrationAndDelegation csl) <#> \x ->
      StakeRegDelegCert
        (wrap $ Credential.fromCdl $ Cdl.stakeRegistrationAndDelegation_stakeCredential x)
        (wrap $ wrap $ Cdl.stakeRegistrationAndDelegation_poolKeyhash x)
        (wrap $ wrap $ Cdl.stakeRegistrationAndDelegation_coin x)

  voteRegDelegCert =
    toMaybe (Cdl.certificate_asVoteRegistrationAndDelegation csl) <#> \x ->
      VoteRegDelegCert
        (wrap $ Credential.fromCdl $ Cdl.voteRegistrationAndDelegation_stakeCredential x)
        (DRep.fromCdl $ Cdl.voteRegistrationAndDelegation_drep x)
        (wrap $ wrap $ Cdl.voteRegistrationAndDelegation_coin x)

  stakeVoteRegDelegCert =
    toMaybe (Cdl.certificate_asStakeVoteRegistrationAndDelegation csl) <#> \x ->
      StakeVoteRegDelegCert
        (wrap $ Credential.fromCdl $ Cdl.stakeVoteRegistrationAndDelegation_stakeCredential x)
        (wrap $ wrap $ Cdl.stakeVoteRegistrationAndDelegation_poolKeyhash x)
        (DRep.fromCdl $ Cdl.stakeVoteRegistrationAndDelegation_drep x)
        (wrap $ wrap $ Cdl.stakeVoteRegistrationAndDelegation_coin x)

  authCommitteeHotCert =
    toMaybe (Cdl.certificate_asCommitteeHotAuth csl) <#> \x ->
      AuthCommitteeHotCert
        { coldCred: Credential.fromCdl $ Cdl.committeeHotAuth_committeeColdCredential x
        , hotCred: Credential.fromCdl $ Cdl.committeeHotAuth_committeeHotCredential x
        }

  resignCommitteeColdCert =
    toMaybe (Cdl.certificate_asCommitteeColdResign csl) <#> \x ->
      ResignCommitteeColdCert
        (Credential.fromCdl $ Cdl.committeeColdResign_committeeColdCredential x)
        (Anchor.fromCdl <$> toMaybe (Cdl.committeeColdResign_anchor x))

  regDrepCert =
    toMaybe (Cdl.certificate_asDrepRegistration csl) <#> \x ->
      RegDrepCert
        (Credential.fromCdl $ Cdl.dRepRegistration_votingCredential x)
        (wrap $ wrap $ Cdl.dRepRegistration_coin x)
        (Anchor.fromCdl <$> toMaybe (Cdl.dRepRegistration_anchor x))

  unregDrepCert =
    toMaybe (Cdl.certificate_asDrepDeregistration csl) <#> \x ->
      UnregDrepCert
        (Credential.fromCdl $ Cdl.dRepDeregistration_drepCredential x)
        (wrap $ wrap $ Cdl.dRepDeregistration_coin x)

  updateDrepCert =
    toMaybe (Cdl.certificate_asDrepUpdate csl) <#> \x ->
      UpdateDrepCert
        (Credential.fromCdl $ Cdl.dRepUpdate_drepCredential x)
        (Anchor.fromCdl <$> toMaybe (Cdl.dRepUpdate_anchor x))
