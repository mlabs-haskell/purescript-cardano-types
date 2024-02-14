module Cardano.Types.TransactionBody where

import Prelude

import Cardano.Serialization.Lib
  ( packListContainer
  , packMapContainer
  , transactionBody_newTxBody
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
  , transactionBody_setWithdrawals
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AuxiliaryDataHash (AuxiliaryDataHash)
import Cardano.Types.Certificate (Certificate)
import Cardano.Types.Certificate as Certificate
import Cardano.Types.Coin (Coin)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.Internal.Helpers (withNonEmptyArray)
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
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
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
  }

derive instance Newtype TransactionBody _
derive instance Generic TransactionBody _
derive newtype instance Eq TransactionBody

instance Ord TransactionBody where
  compare = compare `on` repack
    where
    -- MultiAsset has no Ord instance on purpose
    repack (TransactionBody rec) = rec { mint = rec.mint <#> unwrap }

instance Show TransactionBody where
  show = genericShow

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
      }
  ) = unsafePerformEffect do
  -- inputs, outputs, fee
  let tb = transactionBody_newTxBody (packListContainer $ TransactionInput.toCsl <$> inputs) (packListContainer $ TransactionOutput.toCsl <$> outputs) (unwrap $ unwrap fee)
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
  for_ validityStartInterval $ transactionBody_setValidityStartIntervalBignum tb <<< unwrap <<< unwrap
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
  pure tb
