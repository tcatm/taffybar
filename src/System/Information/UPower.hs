{-# LANGUAGE OverloadedStrings #-}
-- | This is a simple library to query the Linux UPower daemon (via
-- DBus) for upower information.  Currently, it only retrieves
-- information for the first upower it finds.
module System.Information.UPower (
  -- * Types
  UPowerContext,
  UPowerId,
  UPowerInfo(..),
  UPowerState(..),
  UPowerTechnology(..),
  UPowerType(..),
  UPowerFrontend(..),
  -- * Accessors
  upowerWatcher,
  getUPowerInfo
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Data.Int
import DBus
import DBus.Client
import Data.List ( find, isInfixOf )

-- | An opaque wrapper around some internal library state
data UPowerContext = BC Client ObjectPath

data UPowerType = UPowerTypeUnknown
                | UPowerTypeLinePower
                | UPowerTypeBattery
                | UPowerTypeUps
                | UPowerTypeMonitor
                | UPowerTypeMouse
                | UPowerTypeKeyboard
                | UPowerTypePda
                | UPowerTypePhone
                deriving (Show, Ord, Eq, Enum)

data UPowerState = UPowerStateUnknown
                 | UPowerStateCharging
                 | UPowerStateDischarging
                 | UPowerStateEmpty
                 | UPowerStateFullyCharged
                 | UPowerStatePendingCharge
                 | UPowerStatePendingDischarge
                 deriving (Show, Ord, Eq, Enum)

data UPowerTechnology = UPowerTechnologyUnknown
                      | UPowerTechnologyLithiumIon
                      | UPowerTechnologyLithiumPolymer
                      | UPowerTechnologyLithiumIronPhosphate
                      | UPowerTechnologyLeadAcid
                      | UPowerTechnologyNickelCadmium
                      | UPowerTechnologyNickelMetalHydride
                      deriving (Show, Ord, Eq, Enum)

-- | There are a few fields supported by UPower that aren't exposed
-- here.. could be easily.
data UPowerInfo = UPowerInfo { upowerNativePath :: String
                             , upowerVendor :: String
                             , upowerModel :: String
                             , upowerSerial :: String
                             -- , upowerUpdateTime :: Time
                             , upowerType :: UPowerType
                             , upowerPowerSupply :: Bool
                             , upowerHasHistory :: Bool
                             , upowerHasStatistics :: Bool
                             , upowerOnline :: Bool
                             , upowerEnergy :: Double
                             , upowerEnergyEmpty :: Double
                             , upowerEnergyFull :: Double
                             , upowerEnergyFullDesign :: Double
                             , upowerEnergyRate :: Double
                             , upowerVoltage :: Double
                             , upowerTimeToEmpty :: Int64
                             , upowerTimeToFull :: Int64
                             , upowerPercentage :: Double
                             , upowerIsPresent :: Bool
                             , upowerState :: UPowerState
                             , upowerIsRechargable :: Bool
                             , upowerCapacity :: Double
                             , upowerTechnology :: UPowerTechnology
{-                             , upowerRecallNotice :: Bool
                             , upowerRecallVendor :: String
                             , upowerRecallUr :: String
-}
                             } deriving (Show)

-- | Find the first power source that is a upower in the list.  The
-- simple heuristic is a substring search on 'BAT'
firstUPower :: [ObjectPath] -> Maybe ObjectPath
firstUPower = find (isInfixOf "BAT" . formatObjectPath)

-- | The name of the power daemon bus
powerBusName :: BusName
powerBusName = "org.freedesktop.UPower"

-- | The base object path
powerBaseObjectPath :: ObjectPath
powerBaseObjectPath = "/org/freedesktop/UPower"

-- | A helper to read the variant contents of a dict with a default
-- value.
readDict :: (IsVariant a) => Map String Variant -> String -> a -> a
readDict dict key dflt = val
  where
    Just val = fromVariant variant
    variant = M.findWithDefault (toVariant dflt) key dict

-- | Read the variant contents of a dict which is of an unknown integral type.
readDictIntegral :: Map String Variant -> String -> Int32 -> Int
readDictIntegral dict key dflt = case variantType variant of
    TypeWord8   -> fromIntegral (f variant :: Word8)
    TypeWord16  -> fromIntegral (f variant :: Word16)
    TypeWord32  -> fromIntegral (f variant :: Word32)
    TypeWord64  -> fromIntegral (f variant :: Word64)
    TypeInt16   -> fromIntegral (f variant :: Int16)
    TypeInt32   -> fromIntegral (f variant :: Int32)
    TypeInt64   -> fromIntegral (f variant :: Int64)
    t           -> error $ "readDictIntegral " ++ show key ++ ": got type " ++ show t
  where
    variant = M.findWithDefault (toVariant dflt) key dict
    f :: IsVariant a => Variant -> a
    f = fromJust . fromVariant

-- | Query the UPower daemon about information on a specific upower.
-- If some fields are not actually present, they may have bogus values
-- here.  Don't bet anything critical on it.
getUPowerInfo :: UPowerContext -> IO UPowerInfo
getUPowerInfo (BC systemConn battPath) = do
  -- Grab all of the properties of the upower each call with one
  -- message.
  reply <- call_ systemConn (methodCall battPath "org.freedesktop.DBus.Properties" "GetAll")
                             { methodCallDestination = Just "org.freedesktop.UPower"
                             , methodCallBody = [toVariant ("org.freedesktop.UPower.Device" :: String)]
                             }

  let dict :: Map String Variant
      Just dict = fromVariant (methodReturnBody reply !! 0)
  return UPowerInfo { upowerNativePath = readDict dict "NativePath" ""
                     , upowerVendor = readDict dict "Vendor" ""
                     , upowerModel = readDict dict "Model" ""
                     , upowerSerial = readDict dict "Serial" ""
                     , upowerType = toEnum $ fromIntegral $ readDictIntegral dict "Type" 0
                     , upowerPowerSupply = readDict dict "PowerSupply" False
                     , upowerHasHistory = readDict dict "HasHistory" False
                     , upowerHasStatistics = readDict dict "HasStatistics" False
                     , upowerOnline = readDict dict "Online" False
                     , upowerEnergy = readDict dict "Energy" 0.0
                     , upowerEnergyEmpty = readDict dict "EnergyEmpty" 0.0
                     , upowerEnergyFull = readDict dict "EnergyFull" 0.0
                     , upowerEnergyFullDesign = readDict dict "EnergyFullDesign" 0.0
                     , upowerEnergyRate = readDict dict "EnergyRate" 0.0
                     , upowerVoltage = readDict dict "Voltage" 0.0
                     , upowerTimeToEmpty = readDict dict "TimeToEmpty" 0
                     , upowerTimeToFull = readDict dict "TimeToFull" 0
                     , upowerPercentage = readDict dict "Percentage" 0.0
                     , upowerIsPresent = readDict dict "IsPresent" False
                     , upowerState = toEnum $ readDictIntegral dict "State" 0
                     , upowerIsRechargable = readDict dict "IsRechargable" True
                     , upowerCapacity = readDict dict "Capacity" 0.0
                     , upowerTechnology =
                       toEnum $ fromIntegral $ readDictIntegral dict "Technology" 0
                     }

newtype UPowerId = UPowerId String deriving (Show, Eq, Ord)

data UPowerFrontend = UPowerFrontend { upowerAdd :: UPowerId -> UPowerInfo -> IO ()
                                     , upowerRemove :: UPowerId -> IO ()
                                     , upowerUpdate :: UPowerId -> UPowerInfo -> IO ()
                                     }

deviceAdd :: MVar (Map ObjectPath SignalHandler) -> Client -> UPowerFrontend -> ObjectPath -> IO ()
deviceAdd handlers client frontend path = do
  hs <- takeMVar handlers
  let handler = M.lookup path hs
  a' <- case handler of
         Just _ -> return hs
         Nothing -> do
           info <- getUPowerInfo $ BC client path
           upowerAdd frontend (UPowerId . formatObjectPath $ path) info
           let match = matchAny { matchPath = Just path, matchMember = Just "PropertiesChanged" }
           h <- addMatch client match $ deviceChange client frontend

           return $ M.insert path h hs

  putMVar handlers a'

deviceRemove :: MVar (Map ObjectPath SignalHandler) -> UPowerFrontend -> ObjectPath -> IO ()
deviceRemove handlers frontend path = do
  upowerRemove frontend $ UPowerId . formatObjectPath $ path
  hs <- takeMVar handlers
  putMVar handlers $ M.delete path hs

deviceChange :: Client -> UPowerFrontend -> Signal -> IO ()
deviceChange client frontend signal = do
  let path = signalPath signal
  info <- getUPowerInfo $ BC client path
  upowerUpdate frontend (UPowerId . formatObjectPath $ path) info

pathFromSignal :: Signal -> ObjectPath
pathFromSignal = fromJust . fromVariant . head . signalBody

upowerWatcher :: UPowerFrontend -> IO ()
upowerWatcher frontend = do
  systemConn <- connectSystem
  reply <- call_ systemConn (methodCall powerBaseObjectPath "org.freedesktop.UPower" "EnumerateDevices")
        { methodCallDestination = Just powerBusName
        }

  handlers <- newMVar (M.empty :: Map ObjectPath SignalHandler)

  let devices :: [ObjectPath]
      devices = fromJust . fromVariant . head . methodReturnBody $ reply

  mapM (deviceAdd handlers systemConn frontend) devices

  let matchDeviceAdded = matchAny { matchPath = Just powerBaseObjectPath, matchMember = Just "DeviceAdded" }
  let matchDeviceRemoved = matchAny { matchPath = Just powerBaseObjectPath, matchMember = Just "DeviceRemoved" }
  let matchDeviceChanged = matchAny { matchPath = Just powerBaseObjectPath, matchMember = Just "PropertiesChanged" }

  addMatch systemConn matchDeviceAdded $ deviceAdd handlers systemConn frontend . pathFromSignal
  addMatch systemConn matchDeviceRemoved $ deviceRemove handlers frontend . pathFromSignal

  return ()
