module Cardano.Types.GovernanceActionId
  ( GovernanceActionId(GovernanceActionId)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
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
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: GovernanceActionId -> Cdl.GovernanceActionId
toCdl (GovernanceActionId rec) =
  Cdl.governanceActionId_new (unwrap rec.transactionId) (UInt.toNumber rec.index)

fromCdl :: Cdl.GovernanceActionId -> GovernanceActionId
fromCdl actionId =
  let
    transactionId = wrap $ Cdl.governanceActionId_transactionId actionId
    index = UInt.fromNumber $ Cdl.governanceActionId_index actionId
  in
    GovernanceActionId
      { transactionId
      , index
      }
