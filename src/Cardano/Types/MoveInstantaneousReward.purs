module Cardano.Types.MoveInstantaneousReward
  ( MoveInstantaneousReward(ToOtherPot, ToStakeCreds)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum(BigNum))
import Cardano.Types.Coin (Coin(Coin))
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.MIRPot (MIRPot)
import Cardano.Types.MIRPot as MIRPot
import Cardano.Types.MIRToStakeCredentials (MIRToStakeCredentials)
import Cardano.Types.MIRToStakeCredentials as MIRToStakeCredentials
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
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

derive instance Generic MoveInstantaneousReward _
derive instance Eq MoveInstantaneousReward
derive instance Ord MoveInstantaneousReward

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

-- TODO
-- instance DecodeAeson MoveInstantaneousReward where

instance AsCbor MoveInstantaneousReward where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: MoveInstantaneousReward -> Csl.MoveInstantaneousReward
toCsl = case _ of
  ToOtherPot { pot, amount } ->
    Csl.moveInstantaneousReward_newToOtherPot
      (MIRPot.toCsl pot)
      (unwrap $ unwrap amount)
  ToStakeCreds { pot, amounts } ->
    Csl.moveInstantaneousReward_newToStakeCreds
      (MIRPot.toCsl pot)
      (MIRToStakeCredentials.toCsl amounts)

fromCsl :: Csl.MoveInstantaneousReward -> MoveInstantaneousReward
fromCsl csl = unsafePartial $ fromJust $
  toOtherPot <|> toStakeCreds
  where
  pot = MIRPot.fromCsl $ Csl.moveInstantaneousReward_pot csl

  toOtherPot =
    toMaybe (Csl.moveInstantaneousReward_asToOtherPot csl) <#> \amount ->
      ToOtherPot
        { amount: Coin $ BigNum amount
        , pot
        }

  toStakeCreds =
    toMaybe (Csl.moveInstantaneousReward_asToStakeCreds csl) <#> \amounts ->
      ToStakeCreds
        { amounts: MIRToStakeCredentials.fromCsl amounts
        , pot
        }
