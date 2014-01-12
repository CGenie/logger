module Logger.Geo.Base where

data Geo = Geo {
    latitude :: Double,
    longitude :: Double
} deriving (Show)

data Device = Device {
    deviceId :: String
} deriving (Show)

constructGeo :: Double -> Double -> Geo
constructGeo lat lng = Geo {
    latitude = lat,
    longitude = lng
}

constructDevice :: String -> Device
constructDevice deviceId = Device { deviceId = deviceId }
