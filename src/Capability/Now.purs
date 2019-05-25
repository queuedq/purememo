module Purememo.Capability.Now where

import Prelude

import Data.Date (Date)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (Instant)
import Data.Time (Time)
import Halogen (HalogenM, lift)


class Monad m <= Now m where
  now :: m Instant
  nowDate :: m Date
  nowTime :: m Time
  nowDateTime :: m DateTime

instance nowHalogenM :: Now m => Now (HalogenM s f g p o m) where
  now = lift now
  nowDate = lift nowDate
  nowTime = lift nowTime
  nowDateTime = lift nowDateTime
