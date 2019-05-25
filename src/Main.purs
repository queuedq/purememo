module Main where

import Prelude

import Data.Char.Unicode (toLower)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)
import Effect (Effect)
import Effect.Console (log)
import Halogen (liftEffect)
-- import Halogen as H
import Halogen.Aff as HA
-- import Halogen.HTML as HH
import Purememo.Api.Utils.LocalStorage (filterItemsByKey, filterItemsByValue, getAllItems)
import Purememo.AppM (LogLevel(..), Env)


main :: Effect Unit
main = HA.runHalogenAff do
  -- | Playing around with local storage API
  body <- HA.awaitBody

  let logLevel = Dev

  let
    environment :: Env
    environment = { logLevel }

  allMemos <- liftEffect getAllItems
  liftEffect $ log (show allMemos)

  let
    startsWithF :: String -> Boolean
    startsWithF s = case charAt 0 s of
      Nothing -> false
      Just c -> toLower c == 'f'

  filtered <- liftEffect $ filterItemsByKey startsWithF
  liftEffect $ log (show filtered)

  filteredByValue <- liftEffect $ filterItemsByValue startsWithF
  liftEffect $ log (show filteredByValue)

  pure unit
