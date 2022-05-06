{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module AgdaSearch.Web.Home(
  getHomeR
  , getHomeStdLibR
  , getHome1LabR
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

searchForm :: Database -> Html -> MForm (Handler) (FormResult SearchForm, Widget)
searchForm defDb = renderDivs $
  MkSearchForm
  <$> areq (searchField True) ("") Nothing
  <*> areq (selectFieldList [("1lab" :: Text.Text, DB1Lab), ("stdlib", DBStdLib)]) "" (Just defDb)

homeWidget :: Database -> Widget -> [Text] -> [(Identifier, Maybe ModRef)] -> Handler Html
homeWidget database searchWidget failures ids =
  appLayout $ do
    identifiers <- traverse (\x -> (,x) <$> newIdent) ids

    $(widgetFileNoReload Default.def "home")

getHome1LabR :: Handler Html
getHome1LabR = genericHome DB1Lab showResults

getHomeStdLibR :: Handler Html
getHomeStdLibR = genericHome DBStdLib showResults

getHomeR :: Handler Html
getHomeR = genericHome DBStdLib $ \_ form -> do
  params <- reqGetParams <$> getRequest

  -- this weird hack allows us to do agdasearch.com/1lab and have it
  -- set to 1lab by default with no state
  case sfDb form of
    DB1Lab   -> redirect (Home1LabR, params)
    DBStdLib -> redirect (HomeStdLibR, params)

showResults :: Widget -> SearchForm -> Handler Html
showResults searchWidget form =  do
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

genericHome :: Database -> (Widget -> SearchForm -> Handler Html) -> Handler Html
genericHome defDb onSucces = do
  ((result, searchWidget), _) <- runFormGet $ searchForm defDb
  case result of
      FormMissing -> do
          $(logInfo) "No form was send with the request, displaying default"
          homeWidget defDb searchWidget [] []
      FormFailure failures -> do
          homeWidget defDb searchWidget failures []
      FormSuccess form -> do
        onSucces searchWidget form
