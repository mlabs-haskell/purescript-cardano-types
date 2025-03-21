module Cardano.Types.VotingProcedures
  ( VotingProcedures(VotingProcedures)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (unpackListContainer)
import Cardano.Data.Lite as Cdl
import Cardano.Types.GovernanceActionId (GovernanceActionId)
import Cardano.Types.GovernanceActionId (fromCdl, toCdl) as GovernanceActionId
import Cardano.Types.Internal.Helpers (decodeMap, encodeMap)
import Cardano.Types.Voter (Voter)
import Cardano.Types.Voter (fromCdl, toCdl) as Voter
import Cardano.Types.VotingProcedure (VotingProcedure)
import Cardano.Types.VotingProcedure (fromCdl, toCdl) as VotingProcedure
import Data.Array (concat) as Array
import Data.Foldable (foldM)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (empty, insertWith, singleton, union, unionWith) as Map
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)

newtype VotingProcedures =
  VotingProcedures (Map Voter (Map GovernanceActionId VotingProcedure))

derive instance Generic VotingProcedures _
derive instance Newtype VotingProcedures _
derive instance Eq VotingProcedures
derive instance Ord VotingProcedures

instance Show VotingProcedures where
  show = genericShow

instance EncodeAeson VotingProcedures where
  encodeAeson = encodeMap <<< map encodeMap <<< unwrap

instance DecodeAeson VotingProcedures where
  decodeAeson = map wrap <<< traverse decodeMap <=< decodeMap

instance AsCbor VotingProcedures where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

instance Semigroup VotingProcedures where
  append = over2 wrap (Map.unionWith (Map.unionWith (\_ rhs -> rhs)))

instance Monoid VotingProcedures where
  mempty = wrap Map.empty

toCdl :: VotingProcedures -> Cdl.VotingProcedures
toCdl (VotingProcedures mp) =
  unsafePerformEffect do
    procedures <- Cdl.votingProcedures_new
    void $ traverseWithIndex
      ( \voter votes ->
          traverseWithIndex
            ( \govActionId vote ->
                Cdl.votingProcedures_insert procedures (Voter.toCdl voter)
                  (GovernanceActionId.toCdl govActionId)
                  (VotingProcedure.toCdl vote)
            )
            votes
      )
      mp
    pure procedures

fromCdl :: Cdl.VotingProcedures -> VotingProcedures
fromCdl procedures =
  unsafePerformEffect do
    voters <- unpackListContainer <$> Cdl.votingProcedures_getVoters procedures
    votersActions <-
      Array.concat <$>
        for voters \voter ->
          map (Tuple voter) <<< unpackListContainer <$>
            Cdl.votingProcedures_getGovernanceActionIdsByVoter procedures voter
    wrap <$> foldM
      ( \mp (voter /\ govActionId) ->
          Cdl.votingProcedures_get procedures voter govActionId <#> \mVote ->
            toMaybe mVote # maybe mp \vote ->
              Map.insertWith Map.union
                (Voter.fromCdl voter)
                ( Map.singleton (GovernanceActionId.fromCdl govActionId)
                    (VotingProcedure.fromCdl vote)
                )
                mp
      )
      Map.empty
      votersActions
