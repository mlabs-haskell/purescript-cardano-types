module Cardano.Types.RedeemerTag
  ( RedeemerTag
      ( Spend
      , Mint
      , Cert
      , Reward
      , Vote
      , Propose
      )
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
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
  encodeAeson = toCdl >>> encodeAeson

instance DecodeAeson RedeemerTag where
  decodeAeson = map fromCdl <<< decodeAeson

instance AsCbor RedeemerTag where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.RedeemerTag -> RedeemerTag
fromCdl redTag =
  case Cdl.fromCslEnum (Cdl.redeemerTag_kind redTag) of
    Cdl.RedeemerTagKind_Spend -> Spend
    Cdl.RedeemerTagKind_Mint -> Mint
    Cdl.RedeemerTagKind_Cert -> Cert
    Cdl.RedeemerTagKind_Reward -> Reward
    Cdl.RedeemerTagKind_Vote -> Vote
    Cdl.RedeemerTagKind_VotingProposal -> Propose

toCdl :: RedeemerTag -> Cdl.RedeemerTag
toCdl = case _ of
  Spend -> Cdl.redeemerTag_newSpend
  Mint -> Cdl.redeemerTag_newMint
  Cert -> Cdl.redeemerTag_newCert
  Reward -> Cdl.redeemerTag_newReward
  Vote -> Cdl.redeemerTag_newVote
  Propose -> Cdl.redeemerTag_newVotingProposal
