module Cardano.Types.TreasuryWithdrawalsAction
  ( TreasuryWithdrawalsAction(TreasuryWithdrawalsAction)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Coin (Coin)
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress (fromCsl, toCsl) as RewardAddress
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)

newtype TreasuryWithdrawalsAction = TreasuryWithdrawalsAction
  { withdrawals :: Map RewardAddress Coin
  , policyHash :: Maybe ScriptHash
  }

derive instance Generic TreasuryWithdrawalsAction _
derive instance Newtype TreasuryWithdrawalsAction _
derive instance Eq TreasuryWithdrawalsAction
derive instance Ord TreasuryWithdrawalsAction
derive newtype instance EncodeAeson TreasuryWithdrawalsAction
derive newtype instance DecodeAeson TreasuryWithdrawalsAction

instance Show TreasuryWithdrawalsAction where
  show = genericShow

toCsl :: TreasuryWithdrawalsAction -> Csl.TreasuryWithdrawalsAction
toCsl (TreasuryWithdrawalsAction rec) =
  case rec.policyHash of
    Nothing ->
      Csl.treasuryWithdrawalsAction_new withdrawals
    Just policyHash ->
      Csl.treasuryWithdrawalsAction_newWithPolicyHash withdrawals
        (unwrap policyHash)
  where
  withdrawals =
    Csl.packMapContainer $
      (RewardAddress.toCsl *** unwrap <<< unwrap) <$> Map.toUnfoldable rec.withdrawals

fromCsl :: Csl.TreasuryWithdrawalsAction -> TreasuryWithdrawalsAction
fromCsl action =
  TreasuryWithdrawalsAction
    { withdrawals:
        Csl.treasuryWithdrawalsAction_withdrawals action #
          Csl.unpackMapContainerToMapWith RewardAddress.fromCsl (wrap <<< wrap)
    , policyHash:
        wrap <$> toMaybe (Csl.treasuryWithdrawalsAction_policyHash action)
    }
