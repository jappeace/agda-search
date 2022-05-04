{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module AgdaSearch.Web.Layout(
  appLayout
  ) where

import AgdaSearch.Web.Routes
import Text.Regex.Base()
import Yesod.Core
import Text.Hamlet (hamletFile)

appLayout :: Widget -> Handler Html
appLayout widget = do
  pc <- widgetToPageContent $ widget
  withUrlRenderer $(hamletFile "templates/layout.hamlet")
