{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

--
module AgdaSearch.Web.File(
  getFileR
  ) where


import AgdaSearch.Web.Routes
import AgdaSearch.Web.Layout
import System.FilePath
import Text.Regex.Base()
import AgdaSearch.Web.Settings
import qualified Data.Text as Text
import Yesod.Core
import Data.Text(Text)
import qualified Text.Regex.Quote as Regex
import Text.Regex.Posix (Regex)
import Text.Regex.Lens
import Control.Lens
import qualified Data.Text.IO as Text
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as BA
import Data.String (fromString)

allDots :: Regex
allDots = [Regex.r|\.|]

getFileR :: Text -> Handler Html
getFileR name = do
  $(logInfo) $ "looking up file: " <> name
  app <- getYesod
  let sources = agdaSources $ appSettings app
      -- it doesn't work with text :S
      slashed :: String -- Algebra.Relations.agda -> Algebra/Relations/agda
      slashed = (regex allDots . matchedString .~ "/") $ Text.unpack name

      postfixed :: String -- Algebra/Relations/agda ->  Algebra/Relations.agda
      postfixed = slashed <> ".agda"

  $(logInfo) $ "fixed up the module path: " <> Text.pack (show (slashed, postfixed))

  let fullPath = sources </> postfixed

  $(logInfo) $ "opening " <> Text.pack fullPath

  res <- liftIO $ Text.readFile $ sources </> postfixed

  let lines' :: [(Char,Int)]
      lines' = zip (Text.unpack res) [0..]

      html :: Html
      html = B.pre $
            B.code $
            foldMap (\(txt,count) -> (B.span B.! BA.id (fromString ("char-" <> show count))) (B.toHtml txt)) lines'

  appLayout $ toWidget html
