{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module AgdaSearch.Web(serve) where

import AgdaSearch.Web.Settings
import AgdaSearch.Uri
import Data.Pool
import Control.Monad
import qualified Data.Text as Text
import Database.SQLite.Simple
import AgdaSearch.Schema
import Yesod.Core
import qualified Data.Default as Default
import Yesod.Default.Util
import Text.Hamlet (hamletFile)
import Data.Text(Text)
import Yesod.Form.Types
import Yesod.Form.Functions
import Yesod.Form.Fields
import AgdaSearch.Query

data RoutedApp = MkRoutedApp { appSqlite :: Pool Connection }

instance RenderMessage RoutedApp FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesod "RoutedApp" [parseRoutes|
/                HomeR      GET
/file/#Text/#Int FileR      GET
|]

searchForm :: Html -> MForm (Handler) (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) ("") Nothing

appLayout :: Widget -> Handler Html
appLayout widget = do
  pc <- widgetToPageContent $ widget
  withUrlRenderer $(hamletFile "templates/layout.hamlet")

homeWidget :: Widget -> [Text] -> [(Identifier, Maybe ModRef)] -> Handler Html
homeWidget searchWidget failures identifiers =
  appLayout $(widgetFileNoReload Default.def "home")

getFileR :: Text -> Int -> Handler Html
getFileR name offset =
  redirect $ _1labFile name offset

getHomeR :: Handler Html
getHomeR = do
  ((result, searchWidget), _) <- runFormGet searchForm
  case result of
      FormMissing -> do
          $(logInfo) "No form was send with the request, displaying default"
          homeWidget searchWidget [] []
      FormFailure failures -> do
          homeWidget searchWidget failures []
      FormSuccess search -> do
          pool <- appSqlite <$> getYesod
          $(logInfo) "runnign sql"

          identifiers <-
            withRunInIO $ \runYesod -> do
            withResource pool $ \conn -> do
              runYesod $ $(logInfo) "getting identifiers"
              identfiers <- getIdentifiers conn (Text.unpack search)
              forM identfiers $ \identifier -> do
                runYesod $ $(logInfo) $ "getting modref for " <> Text.pack (show identifier)
                ref <- getModRef conn (fileref identifier)
                -- TOOD warn on nothing
                pure (identifier, ref)
          homeWidget searchWidget [] identifiers

-- wtf?
instance Yesod RoutedApp

makeFoundation :: FilePath -> IO RoutedApp
makeFoundation sqlitePath =
  MkRoutedApp <$> createPool (open sqlitePath) close 1 5 10

serve :: Settings -> IO ()
serve settings@MkSettings{..} = do
  print ("running with" :: String, settings)
  x <- makeFoundation dbPath
  warp port x
