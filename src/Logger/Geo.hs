module Logger.Geo (
    Geo,
    latitude,
    longitude,
    constructGeo,

    createTableIfMissing,
    insertGeo
) where


import Logger.Geo.Base (
        Geo,
        latitude,
        longitude,
        constructGeo
    )
import Logger.Geo.Database(
        createTableIfMissing,
        insertGeo
    )
