module Cardano.Types.Vote
  ( Vote(VoteNo, VoteYes, VoteAbstain)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(UnexpectedValue), decodeAeson, encodeAeson, fromString, toStringifiedNumbersJson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
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

toCsl :: Vote -> Csl.VoteKind
toCsl =
  Csl.toCslEnum <<< case _ of
    VoteNo -> Csl.VoteKind_No
    VoteYes -> Csl.VoteKind_Yes
    VoteAbstain -> Csl.VoteKind_Abstain

fromCsl :: Csl.VoteKind -> Vote
fromCsl =
  Csl.fromCslEnum >>> case _ of
    Csl.VoteKind_No -> VoteNo
    Csl.VoteKind_Yes -> VoteYes
    Csl.VoteKind_Abstain -> VoteAbstain
