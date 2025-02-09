module Cardano.Types.SystemStart
  ( SystemStart(SystemStart)
  , sysStartUnixTime
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import JS.BigInt (BigInt)
import JS.BigInt (fromNumber) as BigInt

newtype SystemStart = SystemStart DateTime

derive instance Generic SystemStart _
derive instance Newtype SystemStart _
derive newtype instance Eq SystemStart

instance Show SystemStart where
  show = genericShow

-- | Returns system start Unix time in milliseconds as `BigInt`.
sysStartUnixTime :: SystemStart -> Maybe BigInt
sysStartUnixTime (SystemStart dateTime) =
  BigInt.fromNumber $ unwrap $ unInstant $ fromDateTime dateTime
