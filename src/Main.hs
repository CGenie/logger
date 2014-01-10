{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Happstack.Server.Response (badRequest)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Happstack.Server.RqData as RQD

import qualified Logger.Geo as Geo

import Database.HDBC.Types (IConnection, disconnect)
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import Data.Configurator (load, lookupDefault, Worth( Required ))


main :: IO ()
main = do
    config   <- load [Required "$(HOME)/.logger"]
    host     <- lookupDefault "localhost" config "host"
    dbname   <- lookupDefault "logger"    config "dbname"
    user     <- lookupDefault "logger"    config "user"
    password <- lookupDefault "logger"    config "password"
    db <- connectPostgreSQL ("host=" ++ host ++ " dbname=" ++ dbname ++ " user=" ++ user ++" password=" ++ password)
    Geo.createTableIfMissing db
    serve (Just defaultServerConfig { port = 10000 }) (myApp db)
    disconnect db


myApp :: (IConnection a) => a -> ServerPart Response
myApp db = msum
  [ dir "echo"    $ echo,
    dir "geo"     $ (geo db)
  ]


template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"


echo :: ServerPart Response
echo =
    path $ \(msg :: String) ->
        ok $ template "echo" $ do
          p $ "echo says: " >> toHtml msg
          p "Change the url to echo something else."


double :: String -> RQD.RqData Double
double name = RQD.lookRead name


getLatLng :: RQD.RqData Geo.Geo
getLatLng = do
    (lat, lng) <- (,) <$> double "latitude" <*> double "longitude"
    return $ Geo.constructGeo lat lng


geo :: (IConnection a) => a -> ServerPart Response
geo db = msum [insertData]
    where
        insertData = do
            method POST
            geo <- RQD.getDataFn getLatLng
            case geo of
                Left e -> badRequest $ toResponse $ unlines e
                Right g -> do
                    gId <- Geo.insertGeo db g
                    ok $ toResponse $ "geo = " ++ (show g)
