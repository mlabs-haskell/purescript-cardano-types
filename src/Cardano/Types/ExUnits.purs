module Cardano.Types.ExUnits where

import Cardano.Types.BigNum (BigNum)

type ExUnits =
  { mem :: BigNum
  , steps :: BigNum
  }
