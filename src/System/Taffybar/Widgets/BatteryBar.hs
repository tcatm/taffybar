-- | A battery bar that can plot data in the range [0, 1].  The
-- colors are configurable.
module System.Taffybar.Widgets.BatteryBar (
  -- * Types
  BatteryBarHandle,
  BarConfig(..),
  -- * Accessors/Constructors
  batteryBarNew,
  batteryBarSet,
  defaultBarConfig
  ) where


import Data.Maybe
import Control.Concurrent
import Control.Monad
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk

import System.Information.Battery

newtype BatteryBarHandle = BBH (MVar BatteryBarState)
data BatteryBarState =
  BatteryBarState { barIsBootstrapped :: Bool
                   , barInfo :: Maybe BatteryInfo
                   , barCanvas :: DrawingArea
                   , barConfig :: BarConfig
                   }

data BarConfig =
  BarConfig { barBorderColor :: (Double, Double, Double) -- ^ Color of the border drawn around the widget
            , barBackgroundColor :: Double -> (Double, Double, Double) -- ^ The background color of the widget
            , barColor :: Double -> (Double, Double, Double) -- ^ A function to determine the color of the widget for the current data point
            , barFlashColor :: (Double, Double, Double) -- ^ Color of the charging indicator
            , barAlertColor :: (Double, Double, Double) -- ^ Color of the alert (empty) indicator
            , barPlugColor :: (Double, Double, Double) -- ^ Color of the plug (fully charged) indicator
            , barPadding :: Int -- ^ Number of pixels of padding around the widget
            }

-- | A default bar configuration.  The color of the active portion of
-- the bar must be specified.
defaultBarConfig :: (Double -> (Double, Double, Double)) -> BarConfig
defaultBarConfig c = BarConfig { barBorderColor = (0.4, 0.4, 0.4)
                               , barBackgroundColor = const (0, 0, 0)
                               , barFlashColor = (0.15, 0.15, 0.15)
                               , barAlertColor = (1, 0, 0)
                               , barPlugColor = (0.15, 0.15, 0.15)
                               , barColor = c
                               , barPadding = 3
                               }

batteryBarSet :: BatteryBarHandle -> BatteryInfo -> IO ()
batteryBarSet (BBH mv) info = do
  s <- readMVar mv
  let drawArea = barCanvas s
  modifyMVar_ mv (\s' -> return s' { barInfo = Just info })
  case barIsBootstrapped s of
    False -> return ()
    True -> postGUIAsync $ widgetQueueDraw drawArea

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

renderCell :: Render ()
renderCell = do
  save
  scale (1/16) (1/16)
  moveTo 0 16
  lineTo 10 16
  lineTo 10 2
  lineTo 7 2
  lineTo 7 0
  lineTo 3 0
  lineTo 3 2
  lineTo 0 2
  closePath
  restore

renderBackground cfg = do
  let (frameR, frameG, frameB) = barBorderColor cfg
  setSourceRGB frameR frameG frameB
  renderCell
  fill

renderForeground cfg pct = do
  let (frameR, frameG, frameB) = barColor cfg pct
  setSourceRGB frameR frameG frameB
  rectangle 0 (1-pct) 1 1
  save
  clip
  renderCell
  fill
  restore

renderFlash cfg = do
  let (frameR, frameG, frameB) = barFlashColor cfg
  setSourceRGB frameR frameG frameB
  save
  scale (1/16) (1/16)
  translate (1/3) 0
  moveTo 3 4
  lineTo 2 9
  lineTo 4 9
  lineTo 2 14
  lineTo 8 8
  lineTo 5 8
  lineTo 7 4
  fill
  restore

renderPlug cfg = do
  let (frameR, frameG, frameB) = barPlugColor cfg
  setSourceRGB frameR frameG frameB
  save
  scale (1/16) (1/16)
  moveTo 4 6
  lineTo 4 8.1875
  curveTo 2.8416522 8.6030105 2 9.6986791 2 11
  lineTo 3 11
  lineTo 3 13
  lineTo 4 13
  lineTo 4 11
  lineTo 6 11
  lineTo 6 13
  lineTo 7 13
  lineTo 7 11
  lineTo 8 11
  curveTo 8 9.6986791 7.1583478 8.6030105 6 8.1875
  lineTo 6 6
  lineTo 4 6
  fill
  restore

renderAlert cfg = do
  let (frameR, frameG, frameB) = barAlertColor cfg
  setSourceRGB frameR frameG frameB
  save
  scale (1/16) (1/16)
  moveTo 4 4
  lineTo 4 10
  lineTo 6 10
  lineTo 6 4
  fill
  moveTo 4 12
  lineTo 4 14
  lineTo 6 14
  lineTo 6 12
  fill
  restore

renderBar :: BatteryInfo -> BarConfig -> Int -> Int -> Render ()
renderBar info cfg width height = do
  let pad = barPadding cfg
  let pct = clamp 0 1 $ ((batteryPercentage info) / 100)

  translate (fromIntegral pad) (fromIntegral pad)
  let s = fromIntegral (height - 2 * pad)
  scale s s

  renderBackground cfg
  renderForeground cfg pct

  let charging = batteryState info == BatteryStateCharging
  when charging $ renderFlash cfg

  let empty = batteryState info == BatteryStateEmpty
  when empty $ renderAlert cfg

  let full = batteryState info == BatteryStateFullyCharged
  when full $ renderPlug cfg

drawBar :: MVar BatteryBarState -> DrawingArea -> IO ()
drawBar mv drawArea = do
  (w, h) <- widgetGetSize drawArea
  drawWin <- widgetGetDrawWindow drawArea
  s <- readMVar mv
  let info = fromJust $ barInfo s
  modifyMVar_ mv (\s' -> return s' { barIsBootstrapped = True })
  renderWithDrawable drawWin (renderBar info (barConfig s) w h)

batteryBarNew :: Int -> BarConfig -> IO (Widget, BatteryBarHandle)
batteryBarNew height cfg = do
  drawArea <- drawingAreaNew

  mv <- newMVar BatteryBarState { barIsBootstrapped = False
                                 , barInfo = Nothing
                                 , barCanvas = drawArea
                                 , barConfig = cfg
                                 }


  let width = (round $ (fromIntegral height) * 30 / 46) + 2 * (barPadding cfg)

  widgetSetSizeRequest drawArea width (-1)

  _ <- on drawArea exposeEvent $ tryEvent $ liftIO (drawBar mv drawArea)

  box <- hBoxNew False 1
  boxPackStart box drawArea PackGrow 0
  widgetShowAll box

  return (toWidget box, BBH mv)
