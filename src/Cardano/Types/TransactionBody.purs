module Cardano.Types.TransactionBody where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib
  ( packListContainer
  , packMapContainer
  , transactionBody_auxiliaryDataHash
  , transactionBody_certs
  , transactionBody_collateral
  , transactionBody_collateralReturn
  , transactionBody_fee
  , transactionBody_inputs
  , transactionBody_mint
  , transactionBody_networkId
  , transactionBody_newTxBody
  , transactionBody_outputs
  , transactionBody_referenceInputs
  , transactionBody_requiredSigners
  , transactionBody_scriptDataHash
  , transactionBody_setAuxiliaryDataHash
  , transactionBody_setCerts
  , transactionBody_setCollateral
  , transactionBody_setCollateralReturn
  , transactionBody_setMint
  , transactionBody_setNetworkId
  , transactionBody_setReferenceInputs
  , transactionBody_setRequiredSigners
  , transactionBody_setScriptDataHash
  , transactionBody_setTotalCollateral
  , transactionBody_setTtl
  , transactionBody_setUpdate
  , transactionBody_setValidityStartIntervalBignum
  , transactionBody_setVotingProcedures
  , transactionBody_setVotingProposals
  , transactionBody_setWithdrawals
  , transactionBody_totalCollateral
  , transactionBody_ttlBignum
  , transactionBody_update
  , transactionBody_validityStartIntervalBignum
  , transactionBody_votingProcedures
  , transactionBody_votingProposals
  , transactionBody_withdrawals
  , unpackListContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AuxiliaryDataHash (AuxiliaryDataHash)
import Cardano.Types.Certificate (Certificate)
import Cardano.Types.Certificate as Certificate
import Cardano.Types.Coin (Coin)
import Cardano.Types.Coin as Coin
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.Internal.Helpers (clone, withNonEmptyArray)
import Cardano.Types.Mint (Mint)
import Cardano.Types.Mint as Mint
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress as RewardAddress
import Cardano.Types.ScriptDataHash (ScriptDataHash)
import Cardano.Types.Slot (Slot)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionOutput as TransactionOutput
import Cardano.Types.Update (Update)
import Cardano.Types.Update as Update
import Cardano.Types.VotingProcedures (VotingProcedures)
import Cardano.Types.VotingProcedures (empty, fromCsl, toCsl) as VotingProcedures
import Cardano.Types.VotingProposal (VotingProposal)
import Cardano.Types.VotingProposal as VotingProposal
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect.Unsafe (unsafePerformEffect)

newtype TransactionBody = TransactionBody
  { inputs :: Array TransactionInput
  , outputs :: Array TransactionOutput
  , fee :: Coin
  , ttl :: Maybe Slot
  , certs :: Array Certificate
  , withdrawals :: Map RewardAddress Coin
  , update :: Maybe Update
  , auxiliaryDataHash :: Maybe AuxiliaryDataHash
  , validityStartInterval :: Maybe Slot
  , mint :: Maybe Mint
  , scriptDataHash :: Maybe ScriptDataHash
  , collateral :: Array TransactionInput
  , requiredSigners :: Array Ed25519KeyHash
  , networkId :: Maybe NetworkId
  , collateralReturn :: Maybe TransactionOutput
  , totalCollateral :: Maybe Coin
  , referenceInputs :: Array TransactionInput
  , votingProposals :: Array VotingProposal
  , votingProcedures :: VotingProcedures
  }

derive instance Newtype TransactionBody _
derive instance Generic TransactionBody _
derive newtype instance Eq TransactionBody

empty :: TransactionBody
empty = TransactionBody
  { inputs: []
  , outputs: []
  , fee: Coin.zero
  , ttl: Nothing
  , certs: []
  , withdrawals: Map.empty
  , update: Nothing
  , auxiliaryDataHash: Nothing
  , validityStartInterval: Nothing
  , mint: Nothing
  , scriptDataHash: Nothing
  , collateral: []
  , requiredSigners: []
  , networkId: Nothing
  , collateralReturn: Nothing
  , totalCollateral: Nothing
  , referenceInputs: []
  , votingProposals: []
  , votingProcedures: VotingProcedures.empty
  }

instance Ord TransactionBody where
  compare = compare `on` repack
    where
    -- MultiAsset has no Ord instance on purpose
    repack (TransactionBody rec) = rec { mint = rec.mint <#> unwrap }

instance Show TransactionBody where
  show = genericShow

derive newtype instance EncodeAeson TransactionBody
derive newtype instance DecodeAeson TransactionBody

toCsl :: TransactionBody -> Csl.TransactionBody
toCsl
  ( TransactionBody
      { inputs
      , outputs
      , fee
      , ttl
      , certs
      , withdrawals
      , update
      , auxiliaryDataHash
      , validityStartInterval
      , mint
      , scriptDataHash
      , collateral
      , requiredSigners
      , networkId
      , collateralReturn
      , totalCollateral
      , referenceInputs
      , votingProposals
      , votingProcedures
      }
  ) = unsafePerformEffect do
  -- inputs, outputs, fee
  let
    tb = transactionBody_newTxBody
      (packListContainer $ TransactionInput.toCsl <$> inputs)
      (packListContainer $ TransactionOutput.toCsl <$> outputs)
      (unwrap $ unwrap fee)
  -- ttl
  for_ ttl $ transactionBody_setTtl tb <<< unwrap <<< unwrap
  -- certs
  withNonEmptyArray (Certificate.toCsl <$> certs) $ transactionBody_setCerts tb
  -- withdrawals
  transactionBody_setWithdrawals tb $ packMapContainer $
    (RewardAddress.toCsl *** unwrap <<< unwrap) <$> Map.toUnfoldable withdrawals
  -- update
  for_ update $ transactionBody_setUpdate tb <<< Update.toCsl
  -- auxiliaryDataHash
  for_ auxiliaryDataHash $ transactionBody_setAuxiliaryDataHash tb <<< unwrap
  -- validityStartInterval
  for_ validityStartInterval $ transactionBody_setValidityStartIntervalBignum tb
    <<< clone
    <<< unwrap
    <<< unwrap
  -- mint
  for_ mint $ transactionBody_setMint tb <<< Mint.toCsl
  -- scriptDataHash
  for_ scriptDataHash $ transactionBody_setScriptDataHash tb <<< unwrap
  -- collateral
  withNonEmptyArray (TransactionInput.toCsl <$> collateral) (transactionBody_setCollateral tb)
  -- requiredSigners
  withNonEmptyArray (unwrap <$> requiredSigners) (transactionBody_setRequiredSigners tb)
  -- networkId
  for_ networkId $ transactionBody_setNetworkId tb <<< NetworkId.toCsl
  -- collateralReturn
  for_ collateralReturn $ transactionBody_setCollateralReturn tb <<< TransactionOutput.toCsl
  -- totalCollateral
  for_ totalCollateral $ transactionBody_setTotalCollateral tb <<< unwrap <<< unwrap
  -- referenceInputs
  withNonEmptyArray (TransactionInput.toCsl <$> referenceInputs) $
    transactionBody_setReferenceInputs tb
  -- votingProposals
  withNonEmptyArray (VotingProposal.toCsl <$> votingProposals) $
    transactionBody_setVotingProposals tb
  -- votingProcedures
  when (votingProcedures /= VotingProcedures.empty) do
    transactionBody_setVotingProcedures tb $ VotingProcedures.toCsl votingProcedures
  pure tb

fromCsl :: Csl.TransactionBody -> TransactionBody
fromCsl tb =
  TransactionBody
    { inputs
    , outputs
    , fee
    , ttl
    , certs
    , withdrawals
    , update
    , auxiliaryDataHash
    , validityStartInterval
    , mint
    , scriptDataHash
    , collateral
    , requiredSigners
    , networkId
    , collateralReturn
    , totalCollateral
    , referenceInputs
    , votingProposals
    , votingProcedures
    }
  where
  inputs = map TransactionInput.fromCsl $ unpackListContainer $
    transactionBody_inputs tb
  outputs = map TransactionOutput.fromCsl $ unpackListContainer $
    transactionBody_outputs tb
  fee = wrap $ wrap $ transactionBody_fee tb
  ttl = wrap <<< wrap <$>
    toMaybe (transactionBody_ttlBignum tb)
  certs = Certificate.fromCsl <$> fromMaybe []
    (map unpackListContainer $ toMaybe $ transactionBody_certs tb)
  withdrawals = fromMaybe Map.empty
    $ map (unpackMapContainerToMapWith RewardAddress.fromCsl (wrap <<< wrap))
    $ toMaybe
    $ transactionBody_withdrawals tb
  update = Update.fromCsl <$>
    toMaybe (transactionBody_update tb)
  auxiliaryDataHash = wrap <$>
    toMaybe (transactionBody_auxiliaryDataHash tb)
  validityStartInterval = wrap <<< wrap <$>
    toMaybe (transactionBody_validityStartIntervalBignum tb)
  mint = Mint.fromCsl <$> toMaybe (transactionBody_mint tb)
  scriptDataHash = wrap <$> toMaybe (transactionBody_scriptDataHash tb)
  collateral = map TransactionInput.fromCsl $ fromMaybe []
    $ map unpackListContainer
    $ toMaybe (transactionBody_collateral tb)
  requiredSigners = map wrap $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_requiredSigners tb)
  networkId = NetworkId.fromCsl <$>
    toMaybe (transactionBody_networkId tb)
  collateralReturn = TransactionOutput.fromCsl <$>
    toMaybe (transactionBody_collateralReturn tb)
  totalCollateral = wrap <<< wrap <$>
    toMaybe (transactionBody_totalCollateral tb)
  referenceInputs = map TransactionInput.fromCsl $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_referenceInputs tb)
  votingProposals = map VotingProposal.fromCsl $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_votingProposals tb)
  votingProcedures =
    toMaybe (transactionBody_votingProcedures tb) #
      maybe VotingProcedures.empty VotingProcedures.fromCsl
