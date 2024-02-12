module Cardano.Types.ProtocolParamUpdate where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Type.Epoch (Epoch)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.ExUnitPrices (ExUnitPrices)
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.Language (Language)
import Cardano.Types.ProtocolVersion (ProtocolVersion)
import Cardano.Types.UnitInterval (UnitInterval)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)

newtype ProtocolParamUpdate = ProtocolParamUpdate
  { minfeeA :: Maybe Coin
  , minfeeB :: Maybe Coin
  , maxBlockBodySize :: Maybe UInt
  , maxTxSize :: Maybe UInt
  , maxBlockHeaderSize :: Maybe UInt
  , keyDeposit :: Maybe Coin
  , poolDeposit :: Maybe Coin
  , maxEpoch :: Maybe Epoch
  , nOpt :: Maybe UInt
  , poolPledgeInfluence :: Maybe UnitInterval
  , expansionRate :: Maybe UnitInterval
  , treasuryGrowthRate :: Maybe UnitInterval
  , protocolVersion :: Maybe ProtocolVersion
  , minPoolCost :: Maybe BigNum
  , adaPerUtxoByte :: Maybe BigNum
  , costModels :: Maybe (Map Language CostModel)
  , executionCosts :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe UInt
  , collateralPercentage :: Maybe UInt
  , maxCollateralInputs :: Maybe UInt
  }

derive instance Newtype ProtocolParamUpdate _

derive newtype instance Eq ProtocolParamUpdate
derive newtype instance Ord ProtocolParamUpdate
derive newtype instance EncodeAeson ProtocolParamUpdate

derive instance Generic ProtocolParamUpdate _

instance Show ProtocolParamUpdate where
  show = genericShow
