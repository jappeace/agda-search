{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module AgdaSearch.Uri where

import qualified Data.Text as Text

_1labFile :: Text.Text -> Int -> Text.Text
_1labFile modref offset = "https://1lab.dev/" <> modref <> ".html#" <> Text.pack (show offset)
