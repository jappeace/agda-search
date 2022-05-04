{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module AgdaSearch.Web.Routes where

import AgdaSearch.Web.Settings
import Data.Pool
import Database.SQLite.Simple
import Yesod.Core
import Data.Text(Text)
import Yesod.Form.Types
import Yesod.Form.Fields

data App = MkApp
  { appSqliteStdLib :: Pool Connection
  , appSqlite1Lab :: Pool Connection
  , appSettings :: Settings
  }

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

mkYesodData "App" [parseRoutes|
/                HomeR      GET
/file/#Text      FileR      GET
|]

instance Yesod App
