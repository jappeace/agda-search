{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

--
module AgdaSearch.Web.Refer(
  getRefer1labR
  ) where


import AgdaSearch.Web.Routes
import Yesod.Core
import AgdaSearch.Uri
import Data.Text(Text)

getRefer1labR :: Text -> Int -> Handler Html
getRefer1labR txt int =
  redirect $ _1labFile txt int
