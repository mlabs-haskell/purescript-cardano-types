module Cardano.Types.ProtocolParamUpdate where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib
  ( packMapContainer
  , protocolParamUpdate_new
  , protocolParamUpdate_setAdaPerUtxoByte
  , protocolParamUpdate_setCollateralPercentage
  , protocolParamUpdate_setCostModels
  , protocolParamUpdate_setExecutionCosts
  , protocolParamUpdate_setExpansionRate
  , protocolParamUpdate_setKeyDeposit
  , protocolParamUpdate_setMaxBlockBodySize
  , protocolParamUpdate_setMaxBlockExUnits
  , protocolParamUpdate_setMaxBlockHeaderSize
  , protocolParamUpdate_setMaxCollateralInputs
  , protocolParamUpdate_setMaxEpoch
  , protocolParamUpdate_setMaxTxExUnits
  , protocolParamUpdate_setMaxTxSize
  , protocolParamUpdate_setMaxValueSize
  , protocolParamUpdate_setMinPoolCost
  , protocolParamUpdate_setMinfeeA
  , protocolParamUpdate_setMinfeeB
  , protocolParamUpdate_setNOpt
  , protocolParamUpdate_setPoolDeposit
  , protocolParamUpdate_setPoolPledgeInfluence
  , protocolParamUpdate_setProtocolVersion
  , protocolParamUpdate_setTreasuryGrowthRate
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Type.Epoch (Epoch)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.CostModel as CostModels
import Cardano.Types.ExUnitPrices (ExUnitPrices)
import Cardano.Types.ExUnitPrices as ExUnitPrices
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.Language (Language)
import Cardano.Types.Language as Language
import Cardano.Types.ProtocolVersion (ProtocolVersion)
import Cardano.Types.ProtocolVersion as ProtocolVersion
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)

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

toCsl :: ProtocolParamUpdate -> Csl.ProtocolParamUpdate
toCsl
  ( ProtocolParamUpdate
      { minfeeA
      , minfeeB
      , maxBlockBodySize
      , maxTxSize
      , maxBlockHeaderSize
      , keyDeposit
      , poolDeposit
      , maxEpoch
      , nOpt
      , poolPledgeInfluence
      , expansionRate
      , treasuryGrowthRate
      , protocolVersion
      , minPoolCost
      , adaPerUtxoByte
      , costModels
      , executionCosts
      , maxTxExUnits
      , maxBlockExUnits
      , maxValueSize
      , collateralPercentage
      , maxCollateralInputs
      }
  ) = unsafePartial $ unsafePerformEffect do
  let pp = protocolParamUpdate_new
  for_ minfeeA $ protocolParamUpdate_setMinfeeA pp <<< unwrap <<< unwrap
  for_ minfeeB $ protocolParamUpdate_setMinfeeB pp <<< unwrap <<< unwrap
  for_ maxBlockBodySize $ protocolParamUpdate_setMaxBlockBodySize pp <<< UInt.toNumber
  for_ maxTxSize $ protocolParamUpdate_setMaxTxSize pp <<< UInt.toNumber
  for_ maxBlockHeaderSize $ protocolParamUpdate_setMaxBlockHeaderSize pp <<< UInt.toNumber
  for_ keyDeposit $ protocolParamUpdate_setKeyDeposit pp <<< unwrap <<< unwrap
  for_ poolDeposit $ protocolParamUpdate_setPoolDeposit pp <<< unwrap <<< unwrap
  for_ maxEpoch $ protocolParamUpdate_setMaxEpoch pp <<< UInt.toNumber <<< unwrap
  for_ nOpt $ protocolParamUpdate_setNOpt pp <<< UInt.toNumber
  for_ poolPledgeInfluence $ protocolParamUpdate_setPoolPledgeInfluence pp <<< UnitInterval.toCsl
  for_ expansionRate $ protocolParamUpdate_setExpansionRate pp <<< UnitInterval.toCsl
  for_ treasuryGrowthRate $ protocolParamUpdate_setTreasuryGrowthRate pp <<< UnitInterval.toCsl
  for_ protocolVersion $ protocolParamUpdate_setProtocolVersion pp <<< ProtocolVersion.toCsl
  for_ minPoolCost $ protocolParamUpdate_setMinPoolCost pp <<< unwrap
  for_ adaPerUtxoByte $ protocolParamUpdate_setAdaPerUtxoByte pp <<< unwrap
  for_ costModels $ protocolParamUpdate_setCostModels pp
    <<< packMapContainer
    <<< map (Language.toCsl *** CostModels.toCsl)
    <<< Map.toUnfoldable
  for_ executionCosts $ protocolParamUpdate_setExecutionCosts pp <<< ExUnitPrices.toCsl
  for_ maxTxExUnits $ protocolParamUpdate_setMaxTxExUnits pp <<< ExUnits.toCsl
  for_ maxBlockExUnits $ protocolParamUpdate_setMaxBlockExUnits pp <<< ExUnits.toCsl
  for_ maxValueSize $ protocolParamUpdate_setMaxValueSize pp <<< UInt.toNumber
  for_ collateralPercentage $ protocolParamUpdate_setCollateralPercentage pp <<< UInt.toNumber
  for_ maxCollateralInputs $ protocolParamUpdate_setMaxCollateralInputs pp <<< UInt.toNumber
  pure pp
