{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides battery widgets using the UPower system
-- service.
--
-- Currently it reports only the first battery it finds.  If it does
-- not find a batterym it just returns an obnoxious widget with
-- warning text in it.  Battery hotplugging is not supported.  These
-- more advanced features could be supported if there is interest.
module System.Taffybar.Battery (
  batteryBarNew,
  textBatteryNew,
  defaultBatteryConfig
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import qualified Control.Exception.Enclosed as E
import Data.Int ( Int64 )
import Data.IORef
import Data.Map
import Data.Maybe
import Graphics.UI.Gtk
import qualified System.IO as IO
import Text.Printf ( printf )
import Text.StringTemplate

import System.Information.Battery
import System.Information.UPower
import System.Taffybar.Widgets.BatteryBar
import System.Taffybar.Widgets.PollingLabel

safeGetBatteryInfo :: IORef BatteryContext -> IO (Maybe BatteryInfo)
safeGetBatteryInfo mv = do
  ctxt <- readIORef mv
  E.catchAny (getBatteryInfo ctxt) $ \_ -> reconnect
  where
    reconnect = do
      mctxt <- batteryContextNew
      case mctxt of
        Nothing -> IO.hPutStrLn IO.stderr "Could not reconnect to UPower"
        Just ctxt -> writeIORef mv ctxt
      return Nothing

battInfo :: IORef BatteryContext -> String -> IO String
battInfo r fmt = do
  minfo <- safeGetBatteryInfo r
  case minfo of
    Nothing -> return ""
    Just info -> do
      let battPctNum :: Int
          battPctNum = floor (batteryPercentage info)
          formatTime :: Int64 -> String
          formatTime seconds =
            let minutes = seconds `div` 60
                hours = minutes `div` 60
                minutes' = minutes `mod` 60
            in printf "%02d:%02d" hours minutes'

          battTime :: String
          battTime = case (batteryState info) of
            BatteryStateCharging -> (formatTime $ batteryTimeToFull info)
            BatteryStateDischarging -> (formatTime $ batteryTimeToEmpty info)
            _ -> "-"

          tpl = newSTMP fmt
          tpl' = setManyAttrib [ ("percentage", show battPctNum)
                               , ("time", battTime)
                               ] tpl
      return $ render tpl'

-- | A simple textual battery widget that auto-updates once every
-- polling period (specified in seconds).  The displayed format is
-- specified format string where $percentage$ is replaced with the
-- percentage of battery remaining and $time$ is replaced with the
-- time until the battery is fully charged/discharged.
textBatteryNew :: String    -- ^ Display format
                  -> Double -- ^ Poll period in seconds
                  -> IO Widget
textBatteryNew fmt pollSeconds = do
  battCtxt <- batteryContextNew
  case battCtxt of
    Nothing -> do
      let lbl :: Maybe String
          lbl = Just "No battery"
      labelNew lbl >>= return . toWidget
    Just ctxt -> do
      r <- newIORef ctxt
      l <- pollingLabelNew "" pollSeconds (battInfo r fmt)
      widgetShowAll l
      return l

-- | Returns the current battery percent as a double in the range [0,
-- 1]
battPct :: IORef BatteryContext -> IO Double
battPct r = do
  minfo <- safeGetBatteryInfo r
  case minfo of
    Nothing -> return 0
    Just info -> return (batteryPercentage info / 100)

defaultBatteryConfig :: BarConfig
defaultBatteryConfig =
  defaultBarConfig colorFunc
  where
    colorFunc pct
      | pct < 0.1 = (1, 0, 0)
      | otherwise = (0.3, 0.8, 1)

frontend s battCfg height = UPowerFrontend { upowerAdd = batteryAdd s battCfg height
                                           , upowerRemove = batteryRemove s
                                           , upowerUpdate = batteryUpdate s
                                           }

batteryAdd s battCfg height path object = do
  let widgets = snd s
  a <- readTVarIO widgets
  unless (member path a) $
    when (upowerType object == UPowerTypeBattery) $ do
      (bar, handle) <- batteryWidgetNew height battCfg
      batteryWidgetSet handle object
      boxPackEnd (fst s) bar PackNatural 0
      let a' = insert path (bar, handle) a
      atomically . writeTVar widgets $ a'

batteryRemove s path = do
  let widgets = snd s
  a <- readTVarIO widgets
  let widget = fst <$> Data.Map.lookup path a
  when (isJust widget) $ do
    containerRemove (fst s) (fromJust widget)
    atomically . writeTVar widgets $ delete path a

batteryUpdate s path object = do
  let widgets = snd s
  a <- readTVarIO widgets
  let widget = snd <$> Data.Map.lookup path a
  when (isJust widget) $ batteryWidgetSet (fromJust widget) object

batteryBarNew :: Int -- ^ Height of bar
                 -> BarConfig -- ^ Configuration options for the bar display
                 -> IO Widget
batteryBarNew height battCfg = do
  widgets <- newTVarIO (Data.Map.empty :: Map UPowerId (Widget, BatteryBarHandle))
  b <- hBoxNew False 1
  upowerWatcher $ frontend (b, widgets) battCfg height
  widgetShowAll b
  return (toWidget b)
