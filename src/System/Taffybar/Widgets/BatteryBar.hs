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
            , barPadding :: Int -- ^ Number of pixels of padding around the widget
            }

-- | A default bar configuration.  The color of the active portion of
-- the bar must be specified.
defaultBarConfig :: (Double -> (Double, Double, Double)) -> BarConfig
defaultBarConfig c = BarConfig { barBorderColor = (0.4, 0.4, 0.4)
                               , barBackgroundColor = const (0, 0, 0)
                               , barFlashColor = (0.15, 0.15, 0.15)
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
  scale (1/46) (1/46)
  moveTo 0 46
  lineTo 30 46
  lineTo 30 5
  lineTo 22 5
  lineTo 22 0
  lineTo 8 0
  lineTo 8 5
  lineTo 0 5
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
  scale (1/46) (1/46)
  moveTo 8 11
  lineTo 6 25
  lineTo 12 25
  lineTo 6 41
  lineTo 24 23
  lineTo 14 23
  lineTo 22 11
  fill
  restore

renderBar :: BatteryInfo -> BarConfig -> Int -> Int -> Render ()
renderBar info cfg width height = do
  let pad = barPadding cfg
  let pct = clamp 0 1 $ ((batteryPercentage info) / 100)
  let charging = batteryState info == BatteryStateCharging

  translate (fromIntegral pad) (fromIntegral pad)
  let s = fromIntegral (height - 2 * pad)
  scale s s

  renderBackground cfg
  renderForeground cfg pct

  case charging of
    False -> return()
    True -> renderFlash cfg

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
