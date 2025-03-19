module Cardano.Types.Committee
  ( Committee(Committee)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (unpackListContainer)
import Cardano.Data.Lite as Csl
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCsl, toCsl) as Credential
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCsl, toCsl) as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromNumber, toNumber) as UInt
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

newtype Committee = Committee
  { quorumThreshold :: UnitInterval
  , members :: Array (Credential /\ Epoch)
  }

derive instance Generic Committee _
derive instance Newtype Committee _
derive instance Eq Committee
derive instance Ord Committee
derive newtype instance EncodeAeson Committee
derive newtype instance DecodeAeson Committee

instance Show Committee where
  show = genericShow

instance AsCbor Committee where
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: Committee -> Csl.Committee
toCsl (Committee rec) =
  unsafePerformEffect do
    committee <- Csl.committee_new $ UnitInterval.toCsl rec.quorumThreshold
    for_ rec.members \(cred /\ memberEpoch) ->
      Csl.committee_addMember committee (Credential.toCsl cred)
        (UInt.toNumber $ unwrap memberEpoch)
    pure committee

fromCsl :: Csl.Committee -> Committee
fromCsl committee =
  Committee
    { quorumThreshold:
        UnitInterval.fromCsl $
          Csl.committee_quorumThreshold committee
    , members:
        (Credential.fromCsl *** (wrap <<< UInt.fromNumber)) <$>
          map
            ( \cred ->
                Tuple cred $ unsafePartial fromJust $
                  toMaybe (Csl.committee_getMemberEpoch committee cred)
            )
            ( unpackListContainer $
                Csl.committee_membersKeys committee
            )
    }
