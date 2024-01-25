module Cardano.Types.AssetClass where

import Prelude hiding (join)

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, arbitrary)

--------------------------------------------------------------------------------
-- AssetClass
--------------------------------------------------------------------------------

data AssetClass = AssetClass ScriptHash AssetName

derive instance Generic AssetClass _
derive instance Eq AssetClass
derive instance Ord AssetClass

instance Arbitrary AssetClass where
  arbitrary = AssetClass <$> arbitrary <*> arbitrary

instance Show AssetClass where
  show = genericShow
