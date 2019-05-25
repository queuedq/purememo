module Purememo.AppM where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as Now
import Purememo.Api.LocalStorage as L
import Purememo.Capability.Now (class Now)
import Purememo.Capability.Resource.Memo (class ManageMemo)
import Type.Equality (class TypeEquals, from)


type Env =
  { logLevel :: LogLevel
  }

data LogLevel = Dev | Prod

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel


newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance manageMemoAppM :: ManageMemo AppM where
  getMemo = L.getMemo
  getMemos = L.getMemos
  createMemo = L.createMemo
  updateMemo = L.updateMemo
  deleteMemo = L.deleteMemo
  pinMemo = L.pinMemo
  unpinMemo = L.unpinMemo
