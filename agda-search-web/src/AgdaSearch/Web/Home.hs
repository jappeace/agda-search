{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module AgdaSearch.Web.Home(
  getHomeR
  ) where


import AgdaSearch.Web.Layout
import AgdaSearch.Web.Routes
import Text.Regex.Base()
import Data.Pool
import Control.Monad
import qualified Data.Text as Text
import AgdaSearch.Schema
import Yesod.Core
import qualified Data.Default as Default
import Yesod.Default.Util
import Data.Text(Text)
import Yesod.Form.Types
import Yesod.Form.Functions
import Yesod.Form.Fields
import AgdaSearch.Query

searchForm :: Html -> MForm (Handler) (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField True) ("") Nothing

homeWidget :: Widget -> [Text] -> [(Identifier, Maybe ModRef)] -> Handler Html
homeWidget searchWidget failures identifiers =
  appLayout $(widgetFileNoReload Default.def "home")

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
