module Cardano.Types.PlutusWitness where


import Prelude

import Cardano.Serialization.Lib as Csl
import Cardano.Types.DatumSource (DatumSource)
import Cardano.Types.DatumSource as DatumSource
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PlutusScriptSource (PlutusScriptSource)
import Cardano.Types.PlutusScriptSource as PlutusScriptSource
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

data PlutusWitness 
    = PlutusWitnessScript PlutusScript (Maybe PlutusData) Redeemer
    | PlutusWitnessRef PlutusScriptSource (Maybe DatumSource) Redeemer

derive instance Generic PlutusWitness _
derive instance Eq PlutusWitness
derive instance Ord PlutusWitness

instance Show PlutusWitness where
  show = genericShow


fromCsl :: Csl.PlutusWitness -> Maybe PlutusWitness
fromCsl input = let
  script = toMaybe $ Csl.plutusWitness_script input
  datum = toMaybe $ Csl.plutusWitness_datum input
  redeemer = Csl.plutusWitness_redeemer input
  in
   case script /\ datum of
        Just script' /\ Just datum' -> Just $ PlutusWitnessScript (PlutusScript.fromCsl script') (Just (PlutusData.fromCsl datum')) (Redeemer.fromCsl redeemer)
        _ -> Nothing


toCsl :: PlutusWitness -> Csl.PlutusWitness
toCsl = case _ of
  PlutusWitnessScript script data' redeemer ->
    case data' of
      Nothing ->
        Csl.plutusWitness_newWithoutDatum
        (PlutusScript.toCsl script)
        (Redeemer.toCsl redeemer)

      Just d ->
        Csl.plutusWitness_new
            (PlutusScript.toCsl script)
            (PlutusData.toCsl d)
            (Redeemer.toCsl redeemer)

  PlutusWitnessRef script datum redeemer ->
    case datum of
    Nothing ->
        Csl.plutusWitness_newWithRefWithoutDatum
        (PlutusScriptSource.toCsl script)
        (Redeemer.toCsl redeemer)

    Just d ->
        Csl.plutusWitness_newWithRef
        (PlutusScriptSource.toCsl script)
        (DatumSource.toCsl d)
        (Redeemer.toCsl redeemer)
