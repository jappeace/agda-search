{-# LANGUAGE OverloadedStrings #-}

module AgdaSearch.Cli where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Database.SQLite.Simple
import AgdaSearch.Populate
import AgdaSearch.Schema
import System.Environment

-- | cabal run agda-search -- ../agda ../agda/src/main/Main.hs x.db
--
--  to create the all.agda file:
--  download std-lib
--  run:
--
--  @
-- std-lib/src on  HEAD (b85c5d1)
-- ❯ fd ".*agda" . > All.agda
--  @
--
--  then open in vim.
--  :%s/[a-z]/import &/g
--  :%s/\.agda//g
--  :%s/\///g
main :: IO ()
main = do
  [basepath, main', database] <- getArgs

  withConnection database $ \conn -> do
    createDb conn
    [Only count] <- query_ conn "select count(*) from identifiers"

    when (count == (0 :: Int)) $ do
      putStrLn "Loading types..."
      runAgda basepath $ do
        iss <- findInScopeSet main'

        liftIO . putStrLn $ "Type checked! Populating database..."
        traverse_ (insertIdentifier conn) iss

    putStrLn "Loaded types!"
    putStr "\27[2J\27[H"
    forever $ do
      putStr "> "
      queryStr <- getLine

      sl <-
        query
          conn
          "select * from identifiers where name like ?"
          (Only (Text.singleton '%' <> Text.pack queryStr <> Text.singleton '%'))

      let showit Identifier {name = name', typestr = typestr', fileref = fileref', byteoffset = bo} = do
            [Only modname'] <-
              query conn "select modname from filemod where fileid = ?" (Only fileref')
            let url = "https://1lab.dev/" <> modname' <> ".html#" <> Text.pack (show bo)
            pure $ name' <> " : " <> typestr' <> "\nDefined at: " <> url

      traverse_ (Text.putStrLn <=< showit) sl
