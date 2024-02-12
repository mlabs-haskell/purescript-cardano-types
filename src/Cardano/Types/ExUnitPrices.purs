module Cardano.Types.ExUnitPrices where

import Cardano.Types.UnitInterval (UnitInterval)

type ExUnitPrices =
  { memPrice :: UnitInterval -- SubCoin in CSL
  , stepPrice :: UnitInterval
  }
