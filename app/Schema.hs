{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Data.Text (Text)
import Database.SQLite.Simple

createDb :: Connection -> IO ()
createDb conn =
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS identifiers (          \
    \   identid    INTEGER PRIMARY KEY AUTOINCREMENT,  \
    \   name       TEXT NOT NULL,                      \
    \   typestr    TEXT NOT NULL,                      \
    \   modname    TEXT NOT NULL,                      \
    \   byteoffset INTEGER NOT NULL,                   \
    \   fileref    TEXT                                \
    \ )"

data Identifier = Identifier
  { identid :: Int,
    name :: Text,
    typestr :: Text,
    filename :: FilePath,
    byteoffset :: Int,
    fileref :: Maybe Text
  }
  deriving (Show)

instance FromRow Identifier where
  fromRow = Identifier <$> field <*> field <*> field <*> field <*> field <*> field
