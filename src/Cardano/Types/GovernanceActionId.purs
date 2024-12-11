module Cardano.Types.GovernanceActionId
  ( GovernanceActionId(GovernanceActionId)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.TransactionHash (TransactionHash)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.UInt (UInt)
import Data.UInt (fromNumber, toInt, toNumber) as UInt

newtype GovernanceActionId = GovernanceActionId
  { transactionId :: TransactionHash
  , index :: UInt -- index identifying a proposal withing a transaction
  }

derive instance Newtype GovernanceActionId _
derive instance Generic GovernanceActionId _
derive instance Eq GovernanceActionId
derive instance Ord GovernanceActionId
derive newtype instance EncodeAeson GovernanceActionId
derive newtype instance DecodeAeson GovernanceActionId

instance Show GovernanceActionId where
  show (GovernanceActionId rec) = -- fixup unlawful UInt instance

    "(GovernanceActionId { transactionId: "
      <> show rec.transactionId
      <> ", index: UInt.fromInt "
      <> show (UInt.toInt rec.index)
      <> " })"

instance AsCbor GovernanceActionId where
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: GovernanceActionId -> Csl.GovernanceActionId
toCsl (GovernanceActionId rec) =
  Csl.governanceActionId_new (unwrap rec.transactionId) (UInt.toNumber rec.index)

fromCsl :: Csl.GovernanceActionId -> GovernanceActionId
fromCsl actionId =
  let
    transactionId = wrap $ Csl.governanceActionId_transactionId actionId
    index = UInt.fromNumber $ Csl.governanceActionId_index actionId
  in
    GovernanceActionId
      { transactionId
      , index
      }
