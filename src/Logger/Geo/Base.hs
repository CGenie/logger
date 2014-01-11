module Logger.Geo.Base where

data Geo = Geo {
    latitude :: Double,
    longitude :: Double
} deriving (Show)

data Device = Device {
    name :: String
} deriving (Show)

constructGeo :: Double -> Double -> Geo
constructGeo lat lng = Geo {
    latitude = lat,
    longitude = lng
}

constructDevice :: String -> Device
constructDevice name = Device { name = name }
