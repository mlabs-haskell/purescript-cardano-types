module Cardano.Types.Update
  ( Update(Update)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( update_epoch
  , update_new
  , update_proposedProtocolParameterUpdates
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.ProposedProtocolParameterUpdates
  ( ProposedProtocolParameterUpdates
  )
import Cardano.Types.ProposedProtocolParameterUpdates as ProposedProtocolParameterUpdates
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt

newtype Update = Update
  { proposedProtocolParameterUpdates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

derive newtype instance Eq Update
derive newtype instance Ord Update
derive newtype instance EncodeAeson Update
derive newtype instance DecodeAeson Update
derive instance Newtype Update _
derive instance Generic Update _

instance AsCbor Update where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

instance Show Update where
  show = genericShow

toCsl :: Update -> Csl.Update
toCsl (Update { proposedProtocolParameterUpdates, epoch }) =
  update_new
    (ProposedProtocolParameterUpdates.toCsl proposedProtocolParameterUpdates)
    (UInt.toNumber $ unwrap epoch)

fromCsl :: Csl.Update -> Update
fromCsl update =
  Update
    { proposedProtocolParameterUpdates:
        ProposedProtocolParameterUpdates.fromCsl $ update_proposedProtocolParameterUpdates update
    , epoch: wrap $ UInt.fromNumber $ update_epoch update
    }
