module Cardano.Types.VotingProcedure
  ( VotingProcedure(VotingProcedure)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCdl, toCdl) as Anchor
import Cardano.Types.Vote (Vote)
import Cardano.Types.Vote (fromCdl, toCdl) as Vote
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype VotingProcedure = VotingProcedure
  { vote :: Vote
  , anchor :: Maybe Anchor
  }

derive instance Generic VotingProcedure _
derive instance Newtype VotingProcedure _
derive instance Eq VotingProcedure
derive instance Ord VotingProcedure
derive newtype instance EncodeAeson VotingProcedure
derive newtype instance DecodeAeson VotingProcedure

instance Show VotingProcedure where
  show = genericShow

instance AsCbor VotingProcedure where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: VotingProcedure -> Cdl.VotingProcedure
toCdl (VotingProcedure rec) =
  maybe (Cdl.votingProcedure_new vote)
    (Cdl.votingProcedure_newWithAnchor vote <<< Anchor.toCdl)
    rec.anchor
  where
  vote = Vote.toCdl rec.vote

fromCdl :: Cdl.VotingProcedure -> VotingProcedure
fromCdl votingProcedure =
  VotingProcedure
    { vote: Vote.fromCdl $ Cdl.votingProcedure_voteKind votingProcedure
    , anchor: Anchor.fromCdl <$> toMaybe (Cdl.votingProcedure_anchor votingProcedure)
    }
