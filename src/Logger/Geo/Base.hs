module Logger.Geo.Base where

data Geo = Geo {
    latitude :: Double,
    longitude :: Double
} deriving (Show)

constructGeo :: Double -> Double -> Geo
constructGeo lat lng = Geo {
    latitude = lat,
    longitude = lng
}
