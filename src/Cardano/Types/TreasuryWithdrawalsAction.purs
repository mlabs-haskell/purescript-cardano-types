module Cardano.Types.TreasuryWithdrawalsAction
  ( TreasuryWithdrawalsAction(TreasuryWithdrawalsAction)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Coin (Coin)
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress (fromCdl, toCdl) as RewardAddress
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

instance AsCbor TreasuryWithdrawalsAction where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: TreasuryWithdrawalsAction -> Cdl.TreasuryWithdrawalsAction
toCdl (TreasuryWithdrawalsAction rec) =
  case rec.policyHash of
    Nothing ->
      Cdl.treasuryWithdrawalsAction_new withdrawals
    Just policyHash ->
      Cdl.treasuryWithdrawalsAction_newWithPolicyHash withdrawals
        (unwrap policyHash)
  where
  withdrawals =
    Cdl.packMapContainer $
      (RewardAddress.toCdl *** unwrap <<< unwrap) <$> Map.toUnfoldable rec.withdrawals

fromCdl :: Cdl.TreasuryWithdrawalsAction -> TreasuryWithdrawalsAction
fromCdl action =
  TreasuryWithdrawalsAction
    { withdrawals:
        Cdl.treasuryWithdrawalsAction_withdrawals action #
          Cdl.unpackMapContainerToMapWith RewardAddress.fromCdl (wrap <<< wrap)
    , policyHash:
        wrap <$> toMaybe (Cdl.treasuryWithdrawalsAction_policyHash action)
    }
