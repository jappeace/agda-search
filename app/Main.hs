{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.Text as Text
import qualified Data.Text.AhoCorasick.Automaton as Aho
import Data.Text.AhoCorasick.Searcher (automaton, needles)
import qualified Data.Text.IO as Text
import Database.SQLite.Simple
import Populate
import Schema
import System.Environment

main :: IO ()
main = withConnection "test.db" $ \conn -> do
  createDb conn
  [Only count] <- query_ conn "select count(*) from identifiers"

  when (count == (0 :: Int)) $ do
    putStrLn "Loading types..."
    runAgda "/home/me/default/Projects/1lab.dev/src/" $ do
      iss <- findInScopeSet "/home/me/default/Projects/1lab.dev/_build/all-pages.agda"
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

    let showit Identifier {name = name, typestr = typestr, fileref = fileref} =
          case fileref of
            Just fl -> name <> " : " <> typestr <> "\nDefined at: " <> fl
            Nothing -> name <> " : " <> typestr

    traverse_ (Text.putStrLn . showit) sl
