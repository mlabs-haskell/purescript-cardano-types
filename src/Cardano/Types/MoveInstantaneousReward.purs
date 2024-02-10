module Cardano.Types.MoveInstantaneousReward where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, finiteNumber)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (moveInstantaneousReward_asToOtherPot, moveInstantaneousReward_asToStakeCreds, moveInstantaneousReward_newToOtherPot, moveInstantaneousReward_newToStakeCreds, moveInstantaneousReward_pot)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.MIRToStakeCredentials (MIRToStakeCredentials)
import Cardano.Types.MIRToStakeCredentials as MIRToStakeCredentials
import Cardano.Types.BigNum (BigNum(..))
import Cardano.Types.Coin (Coin(..))
import Cardano.Types.MIRPot (MIRPot)
import Cardano.Types.MIRPot as MIRPot
import Cardano.Types.MIRPot as Pot
import Cardano.Types.Internal.Helpers (encodeMap, encodeTagged')
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)

data MoveInstantaneousReward
  = ToOtherPot
      { pot :: MIRPot
      , amount :: Coin
      }
  | ToStakeCreds
      { pot :: MIRPot
      , amounts :: MIRToStakeCredentials
      }

derive instance Eq MoveInstantaneousReward
derive instance Ord MoveInstantaneousReward
derive instance Generic MoveInstantaneousReward _

instance Show MoveInstantaneousReward where
  show = genericShow

instance EncodeAeson MoveInstantaneousReward where
  encodeAeson = case _ of
    ToOtherPot r -> encodeTagged' "ToOtherPot" r
      -- We assume the numbers are finite
      { pot = MIRPot.toInt r.pot }
    ToStakeCreds r -> encodeTagged' "ToStakeCreds" r
      -- We assume the numbers are finite
      { pot = MIRPot.toInt r.pot }

--  TODO
-- instance DecodeAeson MoveInstantaneousReward where

instance AsCbor MoveInstantaneousReward where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: MoveInstantaneousReward -> Csl.MoveInstantaneousReward
toCsl = case _ of
  ToOtherPot { pot, amount } ->
    moveInstantaneousReward_newToOtherPot (Int.toNumber $ Pot.toInt pot) (unwrap $ unwrap amount)
  ToStakeCreds { pot, amounts } ->
    moveInstantaneousReward_newToStakeCreds (Int.toNumber $ Pot.toInt pot) (MIRToStakeCredentials.toCsl amounts)

fromCsl :: Csl.MoveInstantaneousReward -> MoveInstantaneousReward
fromCsl csl = unsafePartial $ fromJust $
  toOtherPot <|> toStakeCreds
  where
  pot =
    unsafePartial $ fromJust $
      MIRPot.fromInt =<< Int.fromNumber (moveInstantaneousReward_pot csl)
  toOtherPot = toMaybe (moveInstantaneousReward_asToOtherPot csl) <#> \amount ->
    ToOtherPot { amount: Coin $ BigNum amount, pot }
  toStakeCreds = toMaybe (moveInstantaneousReward_asToStakeCreds csl) <#> \amounts ->
    ToStakeCreds { amounts: MIRToStakeCredentials.fromCsl amounts, pot }
