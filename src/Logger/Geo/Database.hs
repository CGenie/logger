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

data DeviceDB = DeviceDB {
    device_id :: Integer,
    device :: Device
} deriving (Show)

deviceDBFromSql :: [SqlValue] -> DeviceDB
deviceDBFromSql vals = DeviceDB { device_id = dev_id, device = (constructDevice name) }
    where
        dev_id = fromSql $ vals !! 0
        name = fromSql $ vals !! 1


createTableIfMissing :: (IConnection a) => a -> IO ()
createTableIfMissing db = do
    tables <- handleSqlError $ getTables db
    unless ("devices" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE devices (device_id SERIAL PRIMARY KEY, "
            ++ "name VARCHAR(100));") []
        commit db
    unless ("geo" `elem` tables) $ handleSqlError $ do
        run db ("CREATE TABLE geo ("
                ++ "geo_id SERIAL PRIMARY KEY, "
                ++ "created TIMESTAMP, "
                ++ "latitude NUMERIC(21, 15), longitude NUMERIC(21, 15), "
                ++ "device_id INTEGER NOT NULL, "
                ++ "FOREIGN KEY (device_id) REFERENCES devices(device_id)"
            ++ ");") []
        commit db

insertGeo :: (IConnection a, MonadIO m) => a -> Geo -> Integer -> m Integer
insertGeo db geo device_id = do
    now <- liftIO $ getCurrentTime
    liftIO $ withTransaction db $
        \d -> run d "INSERT INTO geo (created, latitude, longitude, device_id) VALUES (?, ?, ?, ?)"
                   [toSql $ now,
                    toSql $ latitude geo,
                    toSql $ longitude geo,
                    toSql $ device_id]

getDeviceById :: (IConnection a, MonadIO m) => a -> Integer -> m (Maybe DeviceDB)
getDeviceById db device_id = do
    stmt <- liftIO $ prepare db "SELECT device_id, name FROM devices WHERE device_id = ?"
    v <- liftIO $ execute stmt [toSql $ device_id]
    row <- liftIO $ fetchRow stmt
    case row of
        Nothing -> return Nothing
        Just vals -> return $ Just (deviceDBFromSql vals)

getDeviceByName :: (IConnection a, MonadIO m) => a -> String -> m (Maybe DeviceDB)
getDeviceByName db name = do
    stmt <- liftIO $ prepare db "SELECT device_id, name FROM devices WHERE name = ?"
    v <- liftIO $ execute stmt [toSql $ name]
    row <- liftIO $ fetchRow stmt
    case row of
        Nothing -> return Nothing
        Just vals -> return $ Just (deviceDBFromSql vals)

createDeviceIfNotExists :: (IConnection a, MonadIO m) => a -> String -> m DeviceDB
createDeviceIfNotExists db name = do
    device <- getDeviceByName db name
    case device of
        Just device -> return device
        Nothing -> do
            liftIO $ withTransaction db $
                \d -> run d "INSERT INTO devices (name) VALUES (?)"
                        [toSql name]
            device <- getDeviceByName db name
            case device of
                Just device -> return device
                -- Nothing should never occur
