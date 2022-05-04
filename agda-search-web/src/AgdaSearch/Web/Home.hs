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

data SearchForm = MkSearchForm
  { sfQuery :: Text
  , sfDb    :: Database
  } deriving Show

searchForm :: Html -> MForm (Handler) (FormResult SearchForm, Widget)
searchForm = renderDivs $
  MkSearchForm
  <$> areq (searchField True) ("") Nothing
  <*> areq (selectFieldList [("1lab" :: Text.Text, DB1Lab), ("stdlib", DBStdLib)]) "package" (Just DBStdLib)

homeWidget :: Database -> Widget -> [Text] -> [(Identifier, Maybe ModRef)] -> Handler Html
homeWidget database searchWidget failures identifiers =
  appLayout $(widgetFileNoReload Default.def "home")

getHomeR :: Handler Html
getHomeR = do
  ((result, searchWidget), _) <- runFormGet searchForm
  case result of
      FormMissing -> do
          $(logInfo) "No form was send with the request, displaying default"
          homeWidget DBStdLib searchWidget [] []
      FormFailure failures -> do
          homeWidget DBStdLib searchWidget failures []
      FormSuccess form -> do
          $(logInfo) $ "searching for " <> Text.pack (show form)
          let search = sfQuery form
              database = sfDb form
              poolSelect =  case database of
                    DB1Lab -> appSqlite1Lab
                    DBStdLib -> appSqliteStdLib
          pool <- poolSelect <$> getYesod
          $(logInfo) "runnign sql"

          identifiers <-
            withRunInIO $ \runYesod -> do
            withResource pool $ \conn -> do
              identfiers <- getIdentifiers conn (Text.unpack search)
              forM identfiers $ \identifier -> do
                ref <- getModRef conn (fileref identifier)
                case ref of
                  Nothing -> runYesod $ $logWarn $ "Nothing result for modref on " <> Text.pack (show identifier)
                  _ -> pure ()

                -- TOOD warn on nothing
                pure (identifier, ref)
          homeWidget database searchWidget [] identifiers
