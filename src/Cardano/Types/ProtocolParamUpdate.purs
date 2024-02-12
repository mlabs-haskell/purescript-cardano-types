module Cardano.Types.ProtocolParamUpdate
  ( ProtocolParamUpdate (..)
  , def
  ) where

import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.ExUnitPrices (ExUnitPrices)
import Cardano.Types.ExUnits (ExUnits)
import Cardano.Types.Nonce (Nonce)
import Cardano.Types.ProtocolVersion (ProtocolVersion)
import Cardano.Types.UnitInterval (UnitInterval)
import Control.Bind (bind, discard, pure)
import Data.Eq (class Eq)
import Data.Foldable (traverse_)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect (Effect)

-- TODO
-- some fields seem to be readonly, also all of them are optional
-- certainly this is not a usual purescript record, maybe it should remain a wrapper
-- over the csl type?

newtype ProtocolParamUpdate = ProtocolParamUpdate
  { minfeeA :: Maybe BigNum
  , minfeeB :: Maybe BigNum
  , maxBlockBodySize :: Maybe Number
  , maxTxSize :: Maybe Number
  , maxBlockHeaderSize :: Maybe Number
  , keyDeposit :: Maybe BigNum
  , poolDeposit :: Maybe BigNum
  , maxEpoch :: Maybe Number
  , nOpt :: Maybe Number
  , poolPledgeInfluence :: Maybe UnitInterval
  , expansionRate :: Maybe UnitInterval
  , treasuryGrowthRate :: Maybe UnitInterval
  , d :: Maybe UnitInterval
  , extraEntropy :: Maybe Nonce
  , protocolVersion :: Maybe ProtocolVersion
  , minPoolCost :: Maybe BigNum
  , adaPerUtxoByte :: Maybe BigNum
  , costModels :: Maybe (Array CostModel)
  , executionCosts :: Maybe ExUnitPrices
  , maxTxExUnits :: Maybe ExUnits
  , maxBlockExUnits :: Maybe ExUnits
  , maxValueSize :: Maybe Number
  , collateralPercentage :: Maybe Number
  , maxCollateralInputs :: Maybe Number
  }

derive instance Newtype ProtocolParamUpdate _
derive instance Eq ProtocolParamUpdate
derive instance Generic ProtocolParamUpdate _

instance Show ProtocolParamUpdate where
    show = genericShow

def :: ProtocolParamUpdate
def = wrap
  { minfeeA: Nothing
  , minfeeB: Nothing
  , maxBlockBodySize: Nothing
  , maxTxSize: Nothing
  , maxBlockHeaderSize: Nothing
  , keyDeposit: Nothing
  , poolDeposit: Nothing
  , maxEpoch: Nothing
  , nOpt: Nothing
  , poolPledgeInfluence: Nothing
  , expansionRate: Nothing
  , treasuryGrowthRate: Nothing
  , d: Nothing
  , extraEntropy: Nothing
  , protocolVersion: Nothing
  , minPoolCost: Nothing
  , adaPerUtxoByte: Nothing
  , costModels: Nothing
  , executionCosts: Nothing
  , maxTxExUnits: Nothing
  , maxBlockExUnits: Nothing
  , maxValueSize: Nothing
  , collateralPercentage: Nothing
  , maxCollateralInputs: Nothing
  }

-- toCsl :: ProtocolParamUpdate -> Effect Csl.ProtocolParamUpdate
-- toCsl (ProtocolParamUpdate x) = do
--   let ppu = Csl.protocolParamUpdate_new
--   for_ x.minfeeA (unwrap >>> Csl.protocolParamUpdate_setMinfeeA ppu)
--   for_ x.minfeeB (unwrap >>> Csl.protocolParamUpdate_setMinfeeB ppu)
--   for_ x.maxBlockBodySize (Csl.protocolParamUpdate_setMaxBlockBodySize ppu)
--   for_ x.maxTxSize (Csl.protocolParamUpdate_setMaxTxSize ppu)
--   for_ x.maxBlockHeaderSize (Csl.protocolParamUpdate_setMaxBlockHeaderSize ppu)
--   for_ x.keyDeposit (unwrap >>> Csl.protocolParamUpdate_setKeyDeposit ppu)
--   for_ x.poolDeposit (unwrap >>> Csl.protocolParamUpdate_setPoolDeposit ppu)
--   for_ x.maxEpoch (Csl.protocolParamUpdate_setMaxEpoch ppu)
--   for_ x.nOpt (Csl.protocolParamUpdate_setNOpt ppu)
--   for_ x.poolPledgeInfluence (?unwrap >>> Csl.protocolParamUpdate_setPoolPledgeInfluence ppu)
--   for_ x.expansionRate (?unwrap2 >>> Csl.protocolParamUpdate_setExpansionRate ppu)
--   for_ x.treasuryGrowthRate (?unwrap3 >>> Csl.protocolParamUpdate_setTreasuryGrowthRate ppu)
--   for_ x.protocolVersion (?unwrap4 >>> Csl.protocolParamUpdate_setProtocolVersion ppu)
--   for_ x.minPoolCost (unwrap >>> Csl.protocolParamUpdate_setMinPoolCost ppu)
--   for_ x.adaPerUtxoByte (unwrap >>> Csl.protocolParamUpdate_setAdaPerUtxoByte ppu)
--   for_ x.costModels (unwrap >>> (Csl.protocolParamUpdate_setCostModels ppu))
--   for_ x.executionCosts (?unwrap5 >>> Csl.protocolParamUpdate_setExecutionCosts ppu)
--   for_ x.maxTxExUnits (?unwrap6 >>> Csl.protocolParamUpdate_setMaxTxExUnits ppu)
--   for_ x.maxBlockExUnits (?unwrap7 >>> Csl.protocolParamUpdate_setMaxBlockExUnits ppu)
--   for_ x.maxValueSize (unwrap >>> Csl.protocolParamUpdate_setMaxValueSize ppu)
--   for_ x.collateralPercentage (unwrap >>> Csl.protocolParamUpdate_setCollateralPercentage ppu)
--   for_ x.maxCollateralInputs (unwrap >>> Csl.protocolParamUpdate_setMaxCollateralInputs ppu)
--   pure ppu


