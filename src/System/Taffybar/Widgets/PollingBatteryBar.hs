-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widgets.PollingBatteryBar (
  BarConfig(..),
  -- * Constructors and accessors
  pollingBatteryBarNew,
  defaultBarConfig
  ) where

import Control.Concurrent
import Control.Monad ( forever )
import Graphics.UI.Gtk

import System.Taffybar.Widgets.BatteryBar
import System.Information.Battery

pollingBatteryBarNew :: Int -> BarConfig -> Double -> IO BatteryInfo -> IO Widget
pollingBatteryBarNew height cfg pollSeconds action = do
  (drawArea, h) <- batteryBarNew height cfg

  _ <- on drawArea realize $ do
    _ <- forkIO $ forever $ do
      sample <- action
      batteryBarSet h sample
      threadDelay $ floor (pollSeconds * 1000000)
    return ()

  return drawArea
