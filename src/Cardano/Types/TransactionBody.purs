module Cardano.Types.TransactionBody where

import Prelude

import Cardano.Serialization.Lib as Csl
import Cardano.Types.AuxiliaryDataHash (AuxiliaryDataHash)
import Cardano.Types.Certificate (Certificate)
import Cardano.Types.Coin (Coin)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash)
import Cardano.Types.MultiAsset (MultiAsset)
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.ScriptDataHash (ScriptDataHash)
import Cardano.Types.Slot (Slot)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.Update (Update)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

newtype TransactionBody = TransactionBody
  { inputs :: Array TransactionInput
  , outputs :: Array TransactionOutput
  , fee :: Coin
  , ttl :: Maybe Slot
  , certs :: Maybe (Array Certificate)
  , withdrawals :: Maybe (Map RewardAddress Coin)
  , update :: Maybe Update
  , auxiliaryDataHash :: Maybe AuxiliaryDataHash
  , validityStartInterval :: Maybe Slot
  , mint :: Maybe MultiAsset
  , scriptDataHash :: Maybe ScriptDataHash
  , collateral :: Maybe (Array TransactionInput)
  , requiredSigners :: Maybe (Array Ed25519KeyHash)
  , networkId :: Maybe NetworkId
  , collateralReturn :: Maybe TransactionOutput
  , totalCollateral :: Maybe Coin
  , referenceInputs :: Array TransactionInput
  }

derive instance Newtype TransactionBody _
derive instance Generic TransactionBody _
derive newtype instance Eq TransactionBody

instance Ord TransactionBody where
  compare = compare `on` repack
    where
    -- MultiAsset has no Ord instance on purpose
    repack (TransactionBody rec) = rec { mint = rec.mint <#> unwrap }

instance Show TransactionBody where
  show = genericShow
