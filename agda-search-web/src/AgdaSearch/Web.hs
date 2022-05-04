{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module AgdaSearch.Web(
  serve
  ) where

import Text.Regex.Base()
import AgdaSearch.Web.Settings
import Data.Pool
import Database.SQLite.Simple
import Yesod.Core
import AgdaSearch.Web.File
import AgdaSearch.Web.Home
import AgdaSearch.Web.Routes

mkYesodDispatch "App" resourcesApp

makeFoundation :: Settings -> IO App
makeFoundation settings =
  MkApp <$> createPool (open (dbPathStdLib settings)) close 1 5 10
              <*> pure settings

serve :: Settings -> IO ()
serve settings@MkSettings{..} = do
  print ("running with" :: String, settings)
  x <- makeFoundation settings
  warp port x
