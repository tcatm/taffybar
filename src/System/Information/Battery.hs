{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple library to query the Linux UPower daemon (via
-- DBus) for battery information.  Currently, it only retrieves
-- information for the first battery it finds.
module System.Information.Battery (
  -- * Types
  BatteryContext,
  BatteryInfo(..),
  BatteryState(..),
  BatteryTechnology(..),
  BatteryType(..),
  -- * Accessors
  batteryContextNew,
  getBatteryInfo
  ) where

import Control.Applicative
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Data.Word
import Data.Int
import DBus
import DBus.Client
import Data.List ( find, isInfixOf )
import Safe ( atMay )

-- | An opaque wrapper around some internal library state
data BatteryContext = BC Client ObjectPath

data BatteryType = BatteryTypeUnknown
                 | BatteryTypeLinePower
                 | BatteryTypeBatteryType
                 | BatteryTypeUps
                 | BatteryTypeMonitor
                 | BatteryTypeMouse
                 | BatteryTypeKeyboard
                 | BatteryTypePda
                 | BatteryTypePhone
                 deriving (Show, Ord, Eq, Enum)

data BatteryState = BatteryStateUnknown
                  | BatteryStateCharging
                  | BatteryStateDischarging
                  | BatteryStateEmpty
                  | BatteryStateFullyCharged
                  | BatteryStatePendingCharge
                  | BatteryStatePendingDischarge
                  deriving (Show, Ord, Eq, Enum)

data BatteryTechnology = BatteryTechnologyUnknown
                       | BatteryTechnologyLithiumIon
                       | BatteryTechnologyLithiumPolymer
                       | BatteryTechnologyLithiumIronPhosphate
                       | BatteryTechnologyLeadAcid
                       | BatteryTechnologyNickelCadmium
                       | BatteryTechnologyNickelMetalHydride
                       deriving (Show, Ord, Eq, Enum)

-- | There are a few fields supported by UPower that aren't exposed
-- here.. could be easily.
data BatteryInfo = BatteryInfo { batteryNativePath :: String
                               , batteryVendor :: String
                               , batteryModel :: String
                               , batterySerial :: String
                               -- , batteryUpdateTime :: Time
                               , batteryType :: BatteryType
                               , batteryPowerSupply :: Bool
                               , batteryHasHistory :: Bool
                               , batteryHasStatistics :: Bool
                               , batteryOnline :: Bool
                               , batteryEnergy :: Double
                               , batteryEnergyEmpty :: Double
                               , batteryEnergyFull :: Double
                               , batteryEnergyFullDesign :: Double
                               , batteryEnergyRate :: Double
                               , batteryVoltage :: Double
                               , batteryTimeToEmpty :: Int64
                               , batteryTimeToFull :: Int64
                               , batteryPercentage :: Double
                               , batteryIsPresent :: Bool
                               , batteryState :: BatteryState
                               , batteryIsRechargable :: Bool
                               , batteryCapacity :: Double
                               , batteryTechnology :: BatteryTechnology
{-                               , batteryRecallNotice :: Bool
                               , batteryRecallVendor :: String
                               , batteryRecallUr :: String
-}
                               }

-- | Find the first power source that is a battery in the list.  The
-- simple heuristic is a substring search on 'BAT'
firstBattery :: [ObjectPath] -> Maybe ObjectPath
firstBattery = find (isInfixOf "BAT" . formatObjectPath)

-- | The name of the power daemon bus
powerBusName :: BusName
powerBusName = "org.freedesktop.UPower"

-- | The base object path
powerBaseObjectPath :: ObjectPath
powerBaseObjectPath = "/org/freedesktop/UPower"

-- | A helper to read the variant contents of a dict with a default
-- value.
readDict :: (IsVariant a) => Map String Variant -> String -> a -> a
readDict dict key dflt = fromMaybe dflt $ do
  variant <- M.lookup key dict
  fromVariant variant

-- | Read the variant contents of a dict which is of an unknown integral type.
readDictIntegral :: Map String Variant -> String -> Int32 -> Int
readDictIntegral dict key dflt = fromMaybe (fromIntegral dflt) $ do
  v <- M.lookup key dict
  case variantType v of
    TypeWord8   -> return $ fromIntegral (f v :: Word8)
    TypeWord16  -> return $ fromIntegral (f v :: Word16)
    TypeWord32  -> return $ fromIntegral (f v :: Word32)
    TypeWord64  -> return $ fromIntegral (f v :: Word64)
    TypeInt16   -> return $ fromIntegral (f v :: Int16)
    TypeInt32   -> return $ fromIntegral (f v :: Int32)
    TypeInt64   -> return $ fromIntegral (f v :: Int64)
    _           -> Nothing
  where
    f :: (Num a, IsVariant a) => Variant -> a
    f = fromMaybe (fromIntegral dflt) . fromVariant

-- | Query the UPower daemon about information on a specific battery.
-- If some fields are not actually present, they may have bogus values
-- here.  Don't bet anything critical on it.
getBatteryInfo :: BatteryContext -> IO (Maybe BatteryInfo)
getBatteryInfo (BC systemConn battPath) = do
  -- Grab all of the properties of the battery each call with one
  -- message.
  reply <- call_ systemConn (methodCall battPath "org.freedesktop.DBus.Properties" "GetAll")
                             { methodCallDestination = Just "org.freedesktop.UPower"
                             , methodCallBody = [toVariant ("org.freedesktop.UPower.Device" :: String)]
                             }

  return $ do
    body <- methodReturnBody reply `atMay` 0
    dict <- fromVariant body
    return BatteryInfo { batteryNativePath = readDict dict "NativePath" ""
                       , batteryVendor = readDict dict "Vendor" ""
                       , batteryModel = readDict dict "Model" ""
                       , batterySerial = readDict dict "Serial" ""
                       , batteryType = toEnum $ fromIntegral $ readDictIntegral dict "Type" 0
                       , batteryPowerSupply = readDict dict "PowerSupply" False
                       , batteryHasHistory = readDict dict "HasHistory" False
                       , batteryHasStatistics = readDict dict "HasStatistics" False
                       , batteryOnline = readDict dict "Online" False
                       , batteryEnergy = readDict dict "Energy" 0.0
                       , batteryEnergyEmpty = readDict dict "EnergyEmpty" 0.0
                       , batteryEnergyFull = readDict dict "EnergyFull" 0.0
                       , batteryEnergyFullDesign = readDict dict "EnergyFullDesign" 0.0
                       , batteryEnergyRate = readDict dict "EnergyRate" 0.0
                       , batteryVoltage = readDict dict "Voltage" 0.0
                       , batteryTimeToEmpty = readDict dict "TimeToEmpty" 0
                       , batteryTimeToFull = readDict dict "TimeToFull" 0
                       , batteryPercentage = readDict dict "Percentage" 0.0
                       , batteryIsPresent = readDict dict "IsPresent" False
                       , batteryState = toEnum $ readDictIntegral dict "State" 0
                       , batteryIsRechargable = readDict dict "IsRechargable" True
                       , batteryCapacity = readDict dict "Capacity" 0.0
                       , batteryTechnology =
                         toEnum $ fromIntegral $ readDictIntegral dict "Technology" 0
                       }

-- | Construct a battery context if possible.  This could fail if the
-- UPower daemon is not running.  The context can be used to get
-- actual battery state with 'getBatteryInfo'.
batteryContextNew :: IO (Maybe BatteryContext)
batteryContextNew = do
  systemConn <- connectSystem

  -- First, get the list of devices.  For now, we just get the stats
  -- for the first battery
  reply <- call_ systemConn (methodCall powerBaseObjectPath "org.freedesktop.UPower" "EnumerateDevices")
        { methodCallDestination = Just powerBusName
        }
  let powerDevices = fromJust . fromVariant . head $ methodReturnBody reply

  return $ BC systemConn <$> firstBattery powerDevices
