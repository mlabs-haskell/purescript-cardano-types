module Cardano.Types.Committee
  ( Committee(Committee)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (unpackListContainer)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Credential (Credential)
import Cardano.Types.Credential (fromCdl, toCdl) as Credential
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCdl, toCdl) as UnitInterval
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
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: Committee -> Cdl.Committee
toCdl (Committee rec) =
  unsafePerformEffect do
    committee <- Cdl.committee_new $ UnitInterval.toCdl rec.quorumThreshold
    for_ rec.members \(cred /\ memberEpoch) ->
      Cdl.committee_addMember committee (Credential.toCdl cred)
        (UInt.toNumber $ unwrap memberEpoch)
    pure committee

fromCdl :: Cdl.Committee -> Committee
fromCdl committee =
  Committee
    { quorumThreshold:
        UnitInterval.fromCdl $
          Cdl.committee_quorumThreshold committee
    , members:
        (Credential.fromCdl *** (wrap <<< UInt.fromNumber)) <$>
          map
            ( \cred ->
                Tuple cred $ unsafePartial fromJust $
                  toMaybe (Cdl.committee_getMemberEpoch committee cred)
            )
            ( unpackListContainer $
                Cdl.committee_membersKeys committee
            )
    }
