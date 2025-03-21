module Cardano.Types.Vote
  ( Vote(VoteNo, VoteYes, VoteAbstain)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , decodeAeson
  , encodeAeson
  , fromString
  , toStringifiedNumbersJson
  )
import Cardano.Data.Lite as Cdl
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

data Vote
  = VoteNo
  | VoteYes
  | VoteAbstain

derive instance Generic Vote _
derive instance Eq Vote
derive instance Ord Vote

instance Show Vote where
  show = genericShow

instance EncodeAeson Vote where
  encodeAeson =
    encodeAeson <<< case _ of
      VoteNo -> "VoteNo"
      VoteYes -> "VoteYes"
      VoteAbstain -> "VoteAbstain"

instance DecodeAeson Vote where
  decodeAeson =
    decodeAeson >=> case _ of
      "VoteNo" -> pure VoteNo
      "VoteYes" -> pure VoteYes
      "VoteAbstain" -> pure VoteAbstain
      other -> Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
        other

instance Arbitrary Vote where
  arbitrary = genericArbitrary

toCdl :: Vote -> Cdl.VoteKind
toCdl =
  Cdl.toCslEnum <<< case _ of
    VoteNo -> Cdl.VoteKind_No
    VoteYes -> Cdl.VoteKind_Yes
    VoteAbstain -> Cdl.VoteKind_Abstain

fromCdl :: Cdl.VoteKind -> Vote
fromCdl =
  Cdl.fromCslEnum >>> case _ of
    Cdl.VoteKind_No -> VoteNo
    Cdl.VoteKind_Yes -> VoteYes
    Cdl.VoteKind_Abstain -> VoteAbstain
