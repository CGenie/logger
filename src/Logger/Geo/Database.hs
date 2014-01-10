module Logger.Geo.Database where

import Logger.Geo.Base (
        Geo,
        latitude,
        longitude,
        constructGeo)

import Data.Time.Clock (getCurrentTime)

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)

import Database.HDBC (getTables, handleSqlError, run, commit, toSql, withTransaction,
                      SqlValue ( SqlDouble ) )
import Database.HDBC.Types (IConnection)
import Database.HDBC.PostgreSQL


createTableIfMissing :: (IConnection a) => a -> IO ()
createTableIfMissing db = do
    tables <- handleSqlError $ getTables db
    unless ("geo" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE geo (id SERIAL, "
            ++ "created TIMESTAMP, "
            ++ "latitude NUMERIC(21, 15), longitude NUMERIC(21, 15))") []
        commit db

insertGeo :: (IConnection a, MonadIO m) => a -> Geo -> m Integer
insertGeo db geo = do
    now <- liftIO $ getCurrentTime
    liftIO $ withTransaction db $
        \d -> run d "INSERT INTO geo (created, latitude, longitude) VALUES (?, ?, ?)"
                   [toSql $ now,
                    toSql $ latitude geo,
                    toSql $ longitude geo]
