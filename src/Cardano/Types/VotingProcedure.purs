module Cardano.Types.VotingProcedure
  ( VotingProcedure(VotingProcedure)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCsl, toCsl) as Anchor
import Cardano.Types.Vote (Vote)
import Cardano.Types.Vote (fromCsl, toCsl) as Vote
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
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

toCsl :: VotingProcedure -> Csl.VotingProcedure
toCsl (VotingProcedure rec) =
  maybe (Csl.votingProcedure_new vote)
    (Csl.votingProcedure_newWithAnchor vote <<< Anchor.toCsl)
    rec.anchor
  where
  vote = Vote.toCsl rec.vote

fromCsl :: Csl.VotingProcedure -> VotingProcedure
fromCsl votingProcedure =
  VotingProcedure
    { vote: Vote.fromCsl $ Csl.votingProcedure_voteKind votingProcedure
    , anchor: Anchor.fromCsl <$> toMaybe (Csl.votingProcedure_anchor votingProcedure)
    }
