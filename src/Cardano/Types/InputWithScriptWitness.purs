module Cardano.Types.InputWithScriptWitness
  ( InputWithScriptWitness(InputWithScriptWitness)
  , toCsl
  ) where

import Prelude

import Cardano.Serialization.Lib as Csl
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusWitness (PlutusWitness)
import Cardano.Types.PlutusWitness as PlutusWitness
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionInput as TransactionInput
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype InputWithScriptWitness = InputWithScriptWitness
  { input :: TransactionInput
  , witness :: Either NativeScript PlutusWitness
  }

derive instance Newtype InputWithScriptWitness _
derive instance Generic InputWithScriptWitness _
derive instance Eq InputWithScriptWitness
derive instance Ord InputWithScriptWitness


instance Show InputWithScriptWitness where
  show = genericShow


toCsl :: InputWithScriptWitness -> Csl.InputWithScriptWitness
toCsl (InputWithScriptWitness { input, witness }) = case witness of
  Left native -> Csl.inputWithScriptWitness_newWithNativeScriptWitness (TransactionInput.toCsl input) (NativeScript.toCsl native)
  Right witness' -> Csl.inputWithScriptWitness_newWithPlutusWitness (TransactionInput.toCsl input) (PlutusWitness.toCsl witness')
