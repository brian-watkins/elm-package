{-# LANGUAGE OverloadedStrings #-}
module Elmer (packageName, catalogHost) where

import qualified Elm.Package as Package

packageName :: Package.Name
packageName =
  Package.Name "brian-watkins" "elmer"

catalogHost :: String
catalogHost =
  "http://elmer-test.cfapps.io"
