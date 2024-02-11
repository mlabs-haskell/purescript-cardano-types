module Cardano.Types.PlutusScriptSource
  ( PlutusScriptSource(PlutusScriptSourceScript, PlutusScriptSourceRefInput)
  , toCsl
  ) where

import Prelude

import Cardano.Serialization.Lib as Csl
import Cardano.Types.Language (Language)
import Cardano.Types.Language as Language
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)

data PlutusScriptSource 
    = PlutusScriptSourceScript PlutusScript
    | PlutusScriptSourceRefInput ScriptHash TransactionInput Language

derive instance Generic PlutusScriptSource _
derive instance Eq PlutusScriptSource
derive instance Ord PlutusScriptSource

instance Show PlutusScriptSource where
  show = genericShow


toCsl :: PlutusScriptSource -> Csl.PlutusScriptSource
toCsl = case _ of
  PlutusScriptSourceScript script -> Csl.plutusScriptSource_new (PlutusScript.toCsl script)
  PlutusScriptSourceRefInput scriptHash transactionInput language -> Csl.plutusScriptSource_newRefInputWithLangVer (unwrap scriptHash) (TransactionInput.toCsl transactionInput) (Language.toCsl language)
