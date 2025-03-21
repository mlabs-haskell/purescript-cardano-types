module Cardano.Types.TransactionBody where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( packListContainer
  , packMapContainer
  , transactionBody_auxiliaryDataHash
  , transactionBody_certs
  , transactionBody_collateral
  , transactionBody_collateralReturn
  , transactionBody_currentTreasuryValue
  , transactionBody_donation
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
  , transactionBody_setCurrentTreasuryValue
  , transactionBody_setDonation
  , transactionBody_setMint
  , transactionBody_setNetworkId
  , transactionBody_setReferenceInputs
  , transactionBody_setRequiredSigners
  , transactionBody_setScriptDataHash
  , transactionBody_setTotalCollateral
  , transactionBody_setTtl
  , transactionBody_setValidityStartIntervalBignum
  , transactionBody_setVotingProcedures
  , transactionBody_setVotingProposals
  , transactionBody_setWithdrawals
  , transactionBody_totalCollateral
  , transactionBody_ttlBignum
  , transactionBody_validityStartIntervalBignum
  , transactionBody_votingProcedures
  , transactionBody_votingProposals
  , transactionBody_withdrawals
  , unpackListContainer
  , unpackMapContainerToMapWith
  )
import Cardano.Data.Lite as Cdl
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
import Cardano.Types.VotingProcedures (VotingProcedures)
import Cardano.Types.VotingProcedures (fromCdl, toCdl) as VotingProcedures
import Cardano.Types.VotingProposal (VotingProposal)
import Cardano.Types.VotingProposal as VotingProposal
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Nothing), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect.Unsafe (unsafePerformEffect)
import Type.Proxy (Proxy(Proxy))

newtype TransactionBody = TransactionBody
  { inputs :: Array TransactionInput
  , outputs :: Array TransactionOutput
  , fee :: Coin
  , ttl :: Maybe Slot
  , certs :: Array Certificate
  , withdrawals :: Map RewardAddress Coin
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
  , currentTreasuryValue :: Maybe Coin
  , donation :: Maybe Coin
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
  , votingProcedures: mempty
  , currentTreasuryValue: Nothing
  , donation: Nothing
  }

instance Ord TransactionBody where
  compare = compare `on` repack
    where
    -- MultiAsset has no Ord instance on purpose
    repack (TransactionBody rec) = rec { mint = rec.mint <#> unwrap }

instance Show TransactionBody where
  show = genericShow

instance AsCbor TransactionBody where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

derive newtype instance EncodeAeson TransactionBody
derive newtype instance DecodeAeson TransactionBody

toCdl :: TransactionBody -> Cdl.TransactionBody
toCdl
  ( TransactionBody
      { inputs
      , outputs
      , fee
      , ttl
      , certs
      , withdrawals
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
      , currentTreasuryValue
      , donation
      }
  ) = unsafePerformEffect do
  -- inputs, outputs, fee
  let
    tb = transactionBody_newTxBody
      (packListContainer $ TransactionInput.toCdl <$> inputs)
      (packListContainer $ TransactionOutput.toCdl <$> outputs)
      (unwrap $ unwrap fee)
  -- ttl
  for_ ttl $ transactionBody_setTtl tb <<< unwrap <<< unwrap
  -- certs
  withNonEmptyArray (Certificate.toCdl <$> certs) $ transactionBody_setCerts tb
  -- withdrawals
  unless (Map.isEmpty withdrawals)
    $ transactionBody_setWithdrawals tb
    $ packMapContainer
    $ map (RewardAddress.toCdl *** unwrap <<< unwrap)
    $ Map.toUnfoldable withdrawals
  -- auxiliaryDataHash
  for_ auxiliaryDataHash $ transactionBody_setAuxiliaryDataHash tb <<< unwrap
  -- validityStartInterval
  for_ validityStartInterval $ transactionBody_setValidityStartIntervalBignum tb
    <<< clone
    <<< unwrap
    <<< unwrap
  -- mint
  for_ mint $ transactionBody_setMint tb <<< Mint.toCdl
  -- scriptDataHash
  for_ scriptDataHash $ transactionBody_setScriptDataHash tb <<< unwrap
  -- collateral
  withNonEmptyArray (TransactionInput.toCdl <$> collateral) (transactionBody_setCollateral tb)
  -- requiredSigners
  withNonEmptyArray (unwrap <$> requiredSigners) (transactionBody_setRequiredSigners tb)
  -- networkId
  for_ networkId $ transactionBody_setNetworkId tb <<< NetworkId.toCdl
  -- collateralReturn
  for_ collateralReturn $ transactionBody_setCollateralReturn tb <<< TransactionOutput.toCdl
  -- totalCollateral
  for_ totalCollateral $ transactionBody_setTotalCollateral tb <<< unwrap <<< unwrap
  -- referenceInputs
  withNonEmptyArray (TransactionInput.toCdl <$> referenceInputs) $
    transactionBody_setReferenceInputs tb
  -- votingProposals
  withNonEmptyArray (VotingProposal.toCdl <$> votingProposals) $
    transactionBody_setVotingProposals tb
  -- votingProcedures
  when (votingProcedures /= mempty) do
    transactionBody_setVotingProcedures tb $ VotingProcedures.toCdl votingProcedures
  -- currentTreasuryValue
  for_ currentTreasuryValue $ transactionBody_setCurrentTreasuryValue tb <<< unwrap <<< unwrap
  -- donation
  for_ donation $ transactionBody_setDonation tb <<< unwrap <<< unwrap
  pure tb

fromCdl :: Cdl.TransactionBody -> TransactionBody
fromCdl tb =
  TransactionBody
    { inputs
    , outputs
    , fee
    , ttl
    , certs
    , withdrawals
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
    , currentTreasuryValue
    , donation
    }
  where
  inputs = map TransactionInput.fromCdl $ unpackListContainer $
    transactionBody_inputs tb
  outputs = map TransactionOutput.fromCdl $ unpackListContainer $
    transactionBody_outputs tb
  fee = wrap $ wrap $ transactionBody_fee tb
  ttl = wrap <<< wrap <$>
    toMaybe (transactionBody_ttlBignum tb)
  certs = Certificate.fromCdl <$> fromMaybe []
    (map unpackListContainer $ toMaybe $ transactionBody_certs tb)
  withdrawals = fromMaybe Map.empty
    $ map (unpackMapContainerToMapWith RewardAddress.fromCdl (wrap <<< wrap))
    $ toMaybe
    $ transactionBody_withdrawals tb
  auxiliaryDataHash = wrap <$>
    toMaybe (transactionBody_auxiliaryDataHash tb)
  validityStartInterval = wrap <<< wrap <$>
    toMaybe (transactionBody_validityStartIntervalBignum tb)
  mint = Mint.fromCdl <$> toMaybe (transactionBody_mint tb)
  scriptDataHash = wrap <$> toMaybe (transactionBody_scriptDataHash tb)
  collateral = map TransactionInput.fromCdl $ fromMaybe []
    $ map unpackListContainer
    $ toMaybe (transactionBody_collateral tb)
  requiredSigners = map wrap $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_requiredSigners tb)
  networkId = NetworkId.fromCdl <$>
    toMaybe (transactionBody_networkId tb)
  collateralReturn = TransactionOutput.fromCdl <$>
    toMaybe (transactionBody_collateralReturn tb)
  totalCollateral = wrap <<< wrap <$>
    toMaybe (transactionBody_totalCollateral tb)
  referenceInputs = map TransactionInput.fromCdl $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_referenceInputs tb)
  votingProposals = map VotingProposal.fromCdl $ fromMaybe [] $ unpackListContainer <$>
    toMaybe (transactionBody_votingProposals tb)
  votingProcedures = maybe mempty VotingProcedures.fromCdl $
    toMaybe (transactionBody_votingProcedures tb)
  currentTreasuryValue = wrap <<< wrap <$>
    toMaybe (transactionBody_currentTreasuryValue tb)
  donation = wrap <<< wrap <$>
    toMaybe (transactionBody_donation tb)

_inputs :: Lens' TransactionBody (Array TransactionInput)
_inputs = _Newtype <<< prop (Proxy :: Proxy "inputs")

_fee :: Lens' TransactionBody Coin
_fee = _Newtype <<< prop (Proxy :: Proxy "fee")

_outputs :: Lens' TransactionBody (Array TransactionOutput)
_outputs = _Newtype <<< prop (Proxy :: Proxy "outputs")

_certs :: Lens' TransactionBody (Array Certificate)
_certs = _Newtype <<< prop (Proxy :: Proxy "certs")

_networkId :: Lens' TransactionBody (Maybe NetworkId)
_networkId = _Newtype <<< prop (Proxy :: Proxy "networkId")

_scriptDataHash :: Lens' TransactionBody (Maybe ScriptDataHash)
_scriptDataHash = _Newtype <<< prop (Proxy :: Proxy "scriptDataHash")

_collateral :: Lens' TransactionBody (Array TransactionInput)
_collateral = _Newtype <<< prop (Proxy :: Proxy "collateral")

_collateralReturn :: Lens' TransactionBody (Maybe TransactionOutput)
_collateralReturn = _Newtype <<< prop (Proxy :: Proxy "collateralReturn")

_totalCollateral :: Lens' TransactionBody (Maybe Coin)
_totalCollateral = _Newtype <<< prop (Proxy :: Proxy "totalCollateral")

_referenceInputs :: Lens' TransactionBody (Array TransactionInput)
_referenceInputs = _Newtype <<< prop (Proxy :: Proxy "referenceInputs")

_requiredSigners :: Lens' TransactionBody (Array Ed25519KeyHash)
_requiredSigners = _Newtype <<< prop (Proxy :: Proxy "requiredSigners")

_withdrawals :: Lens' TransactionBody (Map RewardAddress Coin)
_withdrawals = _Newtype <<< prop (Proxy :: Proxy "withdrawals")

_mint :: Lens' TransactionBody (Maybe Mint)
_mint = _Newtype <<< prop (Proxy :: Proxy "mint")

_auxiliaryDataHash :: Lens' TransactionBody (Maybe AuxiliaryDataHash)
_auxiliaryDataHash = _Newtype <<< prop (Proxy :: Proxy "auxiliaryDataHash")

_ttl :: Lens' TransactionBody (Maybe Slot)
_ttl = _Newtype <<< prop (Proxy :: Proxy "ttl")

_validityStartInterval :: Lens' TransactionBody (Maybe Slot)
_validityStartInterval = _Newtype <<< prop
  (Proxy :: Proxy "validityStartInterval")

_votingProposals :: Lens' TransactionBody (Array VotingProposal)
_votingProposals = _Newtype <<< prop (Proxy :: Proxy "votingProposals")

_votingProcedures :: Lens' TransactionBody (VotingProcedures)
_votingProcedures = _Newtype <<< prop (Proxy :: Proxy "votingProcedures")

_currentTreasuryValue :: Lens' TransactionBody (Maybe Coin)
_currentTreasuryValue = _Newtype <<< prop (Proxy :: Proxy "currentTreasuryValue")

_donation :: Lens' TransactionBody (Maybe Coin)
_donation = _Newtype <<< prop (Proxy :: Proxy "donation")
