module Cardano.Types.RedeemerTag
  ( RedeemerTag(..)
  , fromCsl
  , toCsl
  , redeemerTagFromNumber
  , redeemerTagToNumber
  )
  where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Int (floor)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

data RedeemerTag
    = RedeemerTagNewSpend
    | RedeemerTagNewMint
    | RedeemerTagNewCert
    | RedeemerTagNewReward

derive instance Generic RedeemerTag _
derive instance Eq RedeemerTag
derive instance Ord RedeemerTag


instance Show RedeemerTag where
  show = genericShow


instance AsCbor RedeemerTag where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

redeemerTagToNumber :: RedeemerTag -> Int
redeemerTagToNumber = case _ of
  RedeemerTagNewSpend -> 0
  RedeemerTagNewMint -> 1
  RedeemerTagNewCert -> 2
  RedeemerTagNewReward -> 3

redeemerTagFromNumber :: Int -> Maybe RedeemerTag
redeemerTagFromNumber = case _ of
  0 -> Just RedeemerTagNewSpend
  1 -> Just RedeemerTagNewMint
  2 -> Just RedeemerTagNewCert
  3 -> Just RedeemerTagNewReward
  _ -> Nothing

fromCsl :: Csl.RedeemerTag -> RedeemerTag
fromCsl rt = case Csl.redeemerTag_kind rt # floor of
  0 -> RedeemerTagNewSpend
  1 -> RedeemerTagNewMint
  2 -> RedeemerTagNewCert
  3 -> RedeemerTagNewReward
  _ -> unsafePerformEffect $ throw "Invalid redeemer tag"

toCsl :: RedeemerTag -> Csl.RedeemerTag
toCsl = case _ of
  RedeemerTagNewSpend -> Csl.redeemerTag_newSpend
  RedeemerTagNewMint -> Csl.redeemerTag_newMint
  RedeemerTagNewCert -> Csl.redeemerTag_newCert
  RedeemerTagNewReward -> Csl.redeemerTag_newReward
