{-# LANGUAGE OverloadedStrings #-}
module Main where

import TW.Ast
import TW.Loader
import TW.Check
import qualified TW.CodeGen.Haskell as HS

import Control.Monad
import qualified Data.Text as T

main :: IO ()
main =
    do allModules <- loadModules ["samples"] [ModuleName ["Basic"]]
       case allModules of
         Left err -> fail err
         Right ok ->
             case checkModules ok of
               Left err -> fail err
               Right readyModules ->
                   forM_ readyModules (putStrLn . T.unpack . HS.makeModule)
