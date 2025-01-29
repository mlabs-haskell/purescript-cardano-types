module Cardano.Types.TxEvaluation
  ( OgmiosTxIn
  , OgmiosTxOutRef
  , OgmiosTxOut
  , OgmiosAddress
  , RedeemerPointer
  , ExecutionUnits
  , OgmiosDatum
  , OgmiosScript
  , OgmiosTxId
  , showRedeemerPointer
  , TxEvaluationResult(TxEvaluationResult)
  , TxEvaluationR(TxEvaluationR)
  , TxEvaluationFailure(UnparsedError, AdditionalUtxoOverlap, ScriptFailures)
  , ScriptFailure
      ( ExtraRedeemers
      , MissingRequiredDatums
      , MissingRequiredScripts
      , ValidatorFailed
      , UnknownInputReferencedByRedeemer
      , NonScriptInputReferencedByRedeemer
      , NoCostModelForLanguage
      , InternalLedgerTypeConversionError
      , IllFormedExecutionBudget
      )
  ) where

import Prelude

import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.RedeemerTag (RedeemerTag)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptRef (ScriptRef)
import Cardano.Types.Value (Value)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)

type OgmiosTxIn =
  { txId :: OgmiosTxId
  , index :: Prim.Int
  }

type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt
  }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

type OgmiosAddress = Bech32String

type RedeemerPointer = { redeemerTag :: RedeemerTag, redeemerIndex :: UInt }

type ExecutionUnits = { memory :: BigNum, steps :: BigNum }

showRedeemerPointer :: RedeemerPointer -> String
showRedeemerPointer ptr = show ptr.redeemerTag <> ":" <> show ptr.redeemerIndex

type OgmiosDatum = String
type OgmiosScript = String
type OgmiosTxId = String

newtype TxEvaluationResult = TxEvaluationResult
  (Map RedeemerPointer ExecutionUnits)

derive instance Newtype TxEvaluationResult _
derive instance Generic TxEvaluationResult _

instance Show TxEvaluationResult where
  show = genericShow

newtype TxEvaluationR = TxEvaluationR
  (Either TxEvaluationFailure TxEvaluationResult)

derive instance Newtype TxEvaluationR _
derive instance Generic TxEvaluationR _

instance Show TxEvaluationR where
  show = genericShow

-- The following cases are fine to fall through into unparsed error:
-- IncompatibleEra
-- NotEnoughSynced
-- CannotCreateEvaluationContext
data TxEvaluationFailure
  = UnparsedError String
  | AdditionalUtxoOverlap (Array OgmiosTxOutRef)
  | ScriptFailures (Map RedeemerPointer (Array ScriptFailure))

derive instance Generic TxEvaluationFailure _

instance Show TxEvaluationFailure where
  show = genericShow

-- | Reason a script failed.
--
-- The type definition is a least common denominator between Ogmios v6 format used by ogmios backend
-- and ogmios v5.6 format used by blockfrost backend
data ScriptFailure
  = ExtraRedeemers (Array RedeemerPointer)
  | MissingRequiredDatums
      { missing :: (Array OgmiosDatum)
      , provided :: Maybe (Array OgmiosDatum)
      }
  | MissingRequiredScripts
      { missing :: Array RedeemerPointer
      , resolved :: Maybe (Map RedeemerPointer ScriptHash)
      }
  | ValidatorFailed { error :: String, traces :: Array String }
  | UnknownInputReferencedByRedeemer (Array OgmiosTxIn)
  | NonScriptInputReferencedByRedeemer OgmiosTxIn
  | NoCostModelForLanguage (Array String)
  | InternalLedgerTypeConversionError String
  | IllFormedExecutionBudget (Maybe ExecutionUnits)

derive instance Generic ScriptFailure _

instance Show ScriptFailure where
  show = genericShow