module Main where

import qualified AgdaSearch.Web as Web
import AgdaSearch.Web.Settings

main :: IO ()
main = parseSettings >>= Web.serve
