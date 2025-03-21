module Cardano.Types.ProtocolParamUpdate
  ( ProtocolParamUpdate(ProtocolParamUpdate)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( packMapContainer
  , protocolParamUpdate_adaPerUtxoByte
  , protocolParamUpdate_collateralPercentage
  , protocolParamUpdate_committeeTermLimit
  , protocolParamUpdate_costModels
  , protocolParamUpdate_drepDeposit
  , protocolParamUpdate_drepInactivityPeriod
  , protocolParamUpdate_drepVotingThresholds
  , protocolParamUpdate_executionCosts
  , protocolParamUpdate_expansionRate
  , protocolParamUpdate_governanceActionDeposit
  , protocolParamUpdate_governanceActionValidityPeriod
  , protocolParamUpdate_keyDeposit
  , protocolParamUpdate_maxBlockBodySize
  , protocolParamUpdate_maxBlockExUnits
  , protocolParamUpdate_maxBlockHeaderSize
  , protocolParamUpdate_maxCollateralInputs
  , protocolParamUpdate_maxEpoch
  , protocolParamUpdate_maxTxExUnits
  , protocolParamUpdate_maxTxSize
  , protocolParamUpdate_maxValueSize
  , protocolParamUpdate_minCommitteeSize
  , protocolParamUpdate_minPoolCost
  , protocolParamUpdate_minfeeA
  , protocolParamUpdate_minfeeB
  , protocolParamUpdate_nOpt
  , protocolParamUpdate_new
  , protocolParamUpdate_poolDeposit
  , protocolParamUpdate_poolPledgeInfluence
  , protocolParamUpdate_poolVotingThresholds
  , protocolParamUpdate_refScriptCoinsPerByte
  , protocolParamUpdate_setAdaPerUtxoByte
  , protocolParamUpdate_setCollateralPercentage
  , protocolParamUpdate_setCommitteeTermLimit
  , protocolParamUpdate_setCostModels
  , protocolParamUpdate_setDrepDeposit
  , protocolParamUpdate_setDrepInactivityPeriod
  , protocolParamUpdate_setDrepVotingThresholds
  , protocolParamUpdate_setExecutionCosts
  , protocolParamUpdate_setExpansionRate
  , protocolParamUpdate_setGovernanceActionDeposit
  , protocolParamUpdate_setGovernanceActionValidityPeriod
  , protocolParamUpdate_setKeyDeposit
  , protocolParamUpdate_setMaxBlockBodySize
  , protocolParamUpdate_setMaxBlockExUnits
  , protocolParamUpdate_setMaxBlockHeaderSize
  , protocolParamUpdate_setMaxCollateralInputs
  , protocolParamUpdate_setMaxEpoch
  , protocolParamUpdate_setMaxTxExUnits
  , protocolParamUpdate_setMaxTxSize
  , protocolParamUpdate_setMaxValueSize
  , protocolParamUpdate_setMinCommitteeSize
  , protocolParamUpdate_setMinPoolCost
  , protocolParamUpdate_setMinfeeA
  , protocolParamUpdate_setMinfeeB
  , protocolParamUpdate_setNOpt
  , protocolParamUpdate_setPoolDeposit
  , protocolParamUpdate_setPoolPledgeInfluence
  , protocolParamUpdate_setPoolVotingThresholds
  , protocolParamUpdate_setRefScriptCoinsPerByte
  , protocolParamUpdate_setTreasuryGrowthRate
  , protocolParamUpdate_treasuryGrowthRate
  , unpackMapContainerToMapWith
  )
import Cardano.Data.Lite as Cdl
import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.CostModel as CostModel
import Cardano.Types.CostModel as CostModels
import Cardano.Types.DRepVotingThresholds (DRepVotingThresholds)
import Cardano.Types.DRepVotingThresholds (fromCdl, toCdl) as DRepVotingThresholds
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.ExUnitPrices (ExUnitPrices)
import Cardano.Types.ExUnitPrices as ExUnitPrices
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.Language (Language)
import Cardano.Types.Language as Language
import Cardano.Types.PoolVotingThresholds (PoolVotingThresholds)
import Cardano.Types.PoolVotingThresholds (fromCdl, toCdl) as PoolVotingThresholds
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (Nullable, toMaybe)
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
  , minPoolCost :: Maybe Coin
  , adaPerUtxoByte :: Maybe Coin
  , costModels :: Maybe (Map Language CostModel)
  , executionCosts :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe UInt
  , collateralPercentage :: Maybe UInt
  , maxCollateralInputs :: Maybe UInt
  , poolVotingThresholds :: Maybe PoolVotingThresholds
  , drepVotingThresholds :: Maybe DRepVotingThresholds
  , minCommitteeSize :: Maybe UInt
  , committeeTermLimit :: Maybe Epoch
  , govActionValidityPeriod :: Maybe Epoch
  , govActionDeposit :: Maybe Coin
  , drepDeposit :: Maybe Coin
  , drepInactivityPeriod :: Maybe Epoch
  , refScriptCoinsPerByte :: Maybe UnitInterval
  }

derive instance Newtype ProtocolParamUpdate _

derive newtype instance Eq ProtocolParamUpdate
derive newtype instance Ord ProtocolParamUpdate
derive newtype instance EncodeAeson ProtocolParamUpdate
derive newtype instance DecodeAeson ProtocolParamUpdate

derive instance Generic ProtocolParamUpdate _

instance Show ProtocolParamUpdate where
  show = genericShow

instance AsCbor ProtocolParamUpdate where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: ProtocolParamUpdate -> Cdl.ProtocolParamUpdate
toCdl
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
      , minPoolCost
      , adaPerUtxoByte
      , costModels
      , executionCosts
      , maxTxExUnits
      , maxBlockExUnits
      , maxValueSize
      , collateralPercentage
      , maxCollateralInputs
      , poolVotingThresholds
      , drepVotingThresholds
      , minCommitteeSize
      , committeeTermLimit
      , govActionValidityPeriod
      , govActionDeposit
      , drepDeposit
      , drepInactivityPeriod
      , refScriptCoinsPerByte
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
  for_ poolPledgeInfluence $ protocolParamUpdate_setPoolPledgeInfluence pp <<< UnitInterval.toCdl
  for_ expansionRate $ protocolParamUpdate_setExpansionRate pp <<< UnitInterval.toCdl
  for_ treasuryGrowthRate $ protocolParamUpdate_setTreasuryGrowthRate pp <<< UnitInterval.toCdl
  for_ minPoolCost $ protocolParamUpdate_setMinPoolCost pp <<< unwrap <<< unwrap
  for_ adaPerUtxoByte $ protocolParamUpdate_setAdaPerUtxoByte pp <<< unwrap <<< unwrap
  for_ costModels $ protocolParamUpdate_setCostModels pp
    <<< packMapContainer
    <<< map (Language.toCdl *** CostModels.toCdl)
    <<< Map.toUnfoldable
  for_ executionCosts $ protocolParamUpdate_setExecutionCosts pp <<< ExUnitPrices.toCdl
  for_ maxTxExUnits $ protocolParamUpdate_setMaxTxExUnits pp <<< ExUnits.toCdl
  for_ maxBlockExUnits $ protocolParamUpdate_setMaxBlockExUnits pp <<< ExUnits.toCdl
  for_ maxValueSize $ protocolParamUpdate_setMaxValueSize pp <<< UInt.toNumber
  for_ collateralPercentage $ protocolParamUpdate_setCollateralPercentage pp <<< UInt.toNumber
  for_ maxCollateralInputs $ protocolParamUpdate_setMaxCollateralInputs pp <<< UInt.toNumber
  for_ poolVotingThresholds $ protocolParamUpdate_setPoolVotingThresholds pp <<< PoolVotingThresholds.toCdl
  for_ drepVotingThresholds $ protocolParamUpdate_setDrepVotingThresholds pp <<< DRepVotingThresholds.toCdl
  for_ minCommitteeSize $ protocolParamUpdate_setMinCommitteeSize pp <<< UInt.toNumber
  for_ committeeTermLimit $ protocolParamUpdate_setCommitteeTermLimit pp <<< UInt.toNumber <<< unwrap
  for_ govActionValidityPeriod $ protocolParamUpdate_setGovernanceActionValidityPeriod pp <<< UInt.toNumber <<< unwrap
  for_ govActionDeposit $ protocolParamUpdate_setGovernanceActionDeposit pp <<< unwrap <<< unwrap
  for_ drepDeposit $ protocolParamUpdate_setDrepDeposit pp <<< unwrap <<< unwrap
  for_ drepInactivityPeriod $ protocolParamUpdate_setDrepInactivityPeriod pp <<< UInt.toNumber <<< unwrap
  for_ refScriptCoinsPerByte $ protocolParamUpdate_setRefScriptCoinsPerByte pp <<< UnitInterval.toCdl
  pure pp

fromCdl :: Cdl.ProtocolParamUpdate -> ProtocolParamUpdate
fromCdl pp =
  ProtocolParamUpdate
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
    , minPoolCost
    , adaPerUtxoByte
    , costModels
    , executionCosts
    , maxTxExUnits
    , maxBlockExUnits
    , maxValueSize
    , collateralPercentage
    , maxCollateralInputs
    , poolVotingThresholds
    , drepVotingThresholds
    , minCommitteeSize
    , committeeTermLimit
    , govActionValidityPeriod
    , govActionDeposit
    , drepDeposit
    , drepInactivityPeriod
    , refScriptCoinsPerByte
    }
  where
  use :: forall b. (Cdl.ProtocolParamUpdate -> Nullable b) -> Maybe b
  use f = toMaybe (f pp)
  minfeeA = wrap <<< wrap <$> use protocolParamUpdate_minfeeA
  minfeeB = wrap <<< wrap <$> use protocolParamUpdate_minfeeB
  maxBlockBodySize = UInt.fromNumber <$>
    use protocolParamUpdate_maxBlockBodySize
  maxTxSize = UInt.fromNumber <$>
    use protocolParamUpdate_maxTxSize
  maxBlockHeaderSize = UInt.fromNumber <$>
    use protocolParamUpdate_maxBlockHeaderSize
  keyDeposit = wrap <<< wrap <$> use protocolParamUpdate_keyDeposit
  poolDeposit = wrap <<< wrap <$> use protocolParamUpdate_poolDeposit
  maxEpoch = wrap <<< UInt.fromNumber <$> use protocolParamUpdate_maxEpoch
  nOpt = UInt.fromNumber <$> use protocolParamUpdate_nOpt
  poolPledgeInfluence = UnitInterval.fromCdl <$> use protocolParamUpdate_poolPledgeInfluence
  expansionRate = UnitInterval.fromCdl <$> use protocolParamUpdate_expansionRate
  treasuryGrowthRate = UnitInterval.fromCdl <$> use protocolParamUpdate_treasuryGrowthRate
  minPoolCost = wrap <<< wrap <$> use protocolParamUpdate_minPoolCost
  adaPerUtxoByte = wrap <<< wrap <$> use protocolParamUpdate_adaPerUtxoByte
  costModels = costModelsFromCdl <$> use protocolParamUpdate_costModels
  costModelsFromCdl = unpackMapContainerToMapWith Language.fromCdl CostModel.fromCdl
  executionCosts = ExUnitPrices.fromCdl <$> use protocolParamUpdate_executionCosts
  maxTxExUnits = ExUnits.fromCdl <$> use protocolParamUpdate_maxTxExUnits
  maxBlockExUnits = ExUnits.fromCdl <$> use protocolParamUpdate_maxBlockExUnits
  maxValueSize = UInt.fromNumber <$> use protocolParamUpdate_maxValueSize
  collateralPercentage = UInt.fromNumber <$> use protocolParamUpdate_collateralPercentage
  maxCollateralInputs = UInt.fromNumber <$> use protocolParamUpdate_maxCollateralInputs
  poolVotingThresholds = PoolVotingThresholds.fromCdl <$> use protocolParamUpdate_poolVotingThresholds
  drepVotingThresholds = DRepVotingThresholds.fromCdl <$> use protocolParamUpdate_drepVotingThresholds
  minCommitteeSize = UInt.fromNumber <$> use protocolParamUpdate_minCommitteeSize
  committeeTermLimit = wrap <<< UInt.fromNumber <$> use protocolParamUpdate_committeeTermLimit
  govActionValidityPeriod = wrap <<< UInt.fromNumber <$> use protocolParamUpdate_governanceActionValidityPeriod
  govActionDeposit = wrap <<< wrap <$> use protocolParamUpdate_governanceActionDeposit
  drepDeposit = wrap <<< wrap <$> use protocolParamUpdate_drepDeposit
  drepInactivityPeriod = wrap <<< UInt.fromNumber <$> use protocolParamUpdate_drepInactivityPeriod
  refScriptCoinsPerByte = UnitInterval.fromCdl <$> use protocolParamUpdate_refScriptCoinsPerByte
