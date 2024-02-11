module Cardano.Types.MintWitness where

import Prelude

import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScriptSource (PlutusScriptSource)
import Cardano.Types.PlutusScriptSource as PlutusScriptSource
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- In CSL, this type is called StakeMintWitness. They reuse it for Payment MintWitness as well.
data MintWitness = MintWitnessNativeScript NativeScript | MintWitnessPlutusScript PlutusScriptSource Redeemer

derive instance Generic MintWitness _
derive instance Eq MintWitness
derive instance Ord MintWitness

instance Show MintWitness where
  show = genericShow


toCsl :: MintWitness -> Csl.MintWitness
toCsl = case _ of
  MintWitnessNativeScript x -> Csl.mintWitness_newNativeScript (NativeScript.toCsl x)
  MintWitnessPlutusScript x y -> Csl.mintWitness_newPlutusScript (PlutusScriptSource.toCsl x) (Redeemer.toCsl y)

