module Cardano.Types.RedeemerTag
  ( RedeemerTag
      ( Spend
      , Mint
      , Cert
      , Reward
      , Vote
      , Propose
      )
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)

data RedeemerTag
  = Spend
  | Mint
  | Cert
  | Reward
  | Vote
  | Propose

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag

instance Show RedeemerTag where
  show = genericShow

instance EncodeAeson RedeemerTag where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson RedeemerTag where
  decodeAeson = map fromCsl <<< decodeAeson

instance AsCbor RedeemerTag where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.RedeemerTag -> RedeemerTag
fromCsl redTag =
  case Csl.fromCslEnum (Csl.redeemerTag_kind redTag) of
    Csl.RedeemerTagKind_Spend -> Spend
    Csl.RedeemerTagKind_Mint -> Mint
    Csl.RedeemerTagKind_Cert -> Cert
    Csl.RedeemerTagKind_Reward -> Reward
    Csl.RedeemerTagKind_Vote -> Vote
    Csl.RedeemerTagKind_VotingProposal -> Propose

toCsl :: RedeemerTag -> Csl.RedeemerTag
toCsl = case _ of
  Spend -> Csl.redeemerTag_newSpend
  Mint -> Csl.redeemerTag_newMint
  Cert -> Csl.redeemerTag_newCert
  Reward -> Csl.redeemerTag_newReward
  Vote -> Csl.redeemerTag_newVote
  Propose -> Csl.redeemerTag_newVotingProposal
