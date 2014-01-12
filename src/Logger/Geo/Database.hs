module Logger.Geo.Database where

import Logger.Geo.Base (
        Geo,
        latitude,
        longitude,
        constructGeo,

        Device,
        constructDevice)

import Data.Time.Clock (getCurrentTime)

import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)

import Database.HDBC (getTables, handleSqlError, run, commit, toSql, fromSql,
                      withTransaction, prepare, execute, fetchRow, SqlValue ( SqlDouble ) )
import Database.HDBC.Types (IConnection)
import Database.HDBC.PostgreSQL


data GeoDB = GeoDB {
    geo_id :: Integer,
    geo :: Geo
} deriving (Show)

deviceFromSql :: [SqlValue] -> Device
deviceFromSql vals = constructDevice device_id
    where
        device_id = fromSql $ vals !! 0


createTableIfMissing :: (IConnection a) => a -> IO ()
createTableIfMissing db = do
    tables <- handleSqlError $ getTables db
    unless ("devices" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE devices ("
                ++ "device_id TEXT PRIMARY KEY"
            ++ ");") []
        commit db
    unless ("geo" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE geo ("
                ++ "geo_id SERIAL PRIMARY KEY, "
                ++ "created TIMESTAMP, "
                ++ "latitude NUMERIC(21, 15), longitude NUMERIC(21, 15), "
                ++ "device_id TEXT NOT NULL, "
                ++ "FOREIGN KEY (device_id) REFERENCES devices(device_id)"
            ++ ");") []
        commit db

insertGeo :: (IConnection a, MonadIO m) => a -> Geo -> String -> m Integer
insertGeo db geo device_id = do
    now <- liftIO $ getCurrentTime
    liftIO $ withTransaction db $
        \d -> run d "INSERT INTO geo (created, latitude, longitude, device_id) VALUES (?, ?, ?, ?)"
                   [toSql $ now,
                    toSql $ latitude geo,
                    toSql $ longitude geo,
                    toSql $ device_id]

getDeviceById :: (IConnection a, MonadIO m) => a -> String -> m (Maybe Device)
getDeviceById db device_id = do
    stmt <- liftIO $ prepare db "SELECT device_id FROM devices WHERE device_id = ?"
    v <- liftIO $ execute stmt [toSql device_id]
    row <- liftIO $ fetchRow stmt
    case row of
        Nothing -> return Nothing
        Just vals -> return $ Just (deviceFromSql vals)

createDeviceIfNotExists :: (IConnection a, MonadIO m) => a -> String -> m Device
createDeviceIfNotExists db device_id = do
    device <- getDeviceById db device_id
    case device of
        Just device -> return device
        Nothing -> do
            liftIO $ withTransaction db $
                \d -> run d "INSERT INTO devices (device_id) VALUES (?)"
                        [toSql device_id]
            device <- getDeviceById db device_id
            case device of
                Just device -> return device
                -- Nothing should never occur
                Nothing -> error "I've inserted data but it's not there!"
