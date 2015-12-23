{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TW.CodeGen.HaskellTest where

import TW.Ast
import TW.Check
import TW.Loader
import TW.Types
import qualified TW.CodeGen.Haskell as HS

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid
import Test.Framework
import System.IO.Temp
import System.Directory
import System.FilePath
import System.Process
import System.Exit

mainTemplate :: T.Text
mainTemplate =
  T.unlines
  [ "module Main where"
  , "import Basic"
  , "import Data.Aeson"
  , "main :: IO ()"
  , "main = "
  , "  do putStrLn \"Lauching self-check ...\""
  , "     let b = Bar True 42"
  , "         bEnc = encode b"
  , "         bDec = decode' bEnc"
  , "     if (Just b /= bDec) then fail \"Bar enc/dec check ... failed\" else putStrLn \"Bar enc/dec check ... ok\""
  ]

stackTemplate :: T.Text
stackTemplate =
  T.unlines
  [ "resolver: lts-3.10"
  , "packages:"
  , "- '.'"
  , "extra-deps:"
  , "- Spock-0.10.0.1"
  , "- " <> li_name HS.libraryInfo <> "-" <> li_version HS.libraryInfo
  ]

cabalTemplate :: T.Text
cabalTemplate =
  T.unlines
  [ "name: typed-wire-haskell-test"
  , "version:             0.1.0.0"
  , "synopsis:            Just Testing..."
  , "description:         Please see README.md"
  , "homepage:            http://github.com/typed-wire/typed-wire#readme"
  , "license:             MIT"
  , "author:              Alexander Thiemann <mail@athiemann.net>"
  , "maintainer:          Alexander Thiemann <mail@athiemann.net>"
  , "copyright:           (c) 2015 Alexander Thiemann <mail@athiemann.net>"
  , "category:            Web"
  , "build-type:          Simple"
  , "cabal-version:       >=1.10"
  , "tested-with:         GHC==7.10.2"
  , "executable test"
  , "  hs-source-dirs:      src"
  , "  main-is:             Main.hs"
  , "  other-modules:"
  , "                Basic"
  , "              , Other"
  , "  ghc-options:         -threaded -rtsopts -with-rtsopts=-N"
  , "  build-depends:"
  , "                base >= 4.7 && < 5"
  , "              , aeson"
  , "              , time"
  , "              , text"
  , "              , vector"
  , "              , mtl"
  , "              , Spock"
  , "              , " <> li_name HS.libraryInfo <> " == " <> li_version HS.libraryInfo
  , "  default-language:    Haskell2010"
  ]

test_haskellCodeGen :: IO ()
test_haskellCodeGen =
  withSystemTempDirectory "haskellCodeGenX" $ \dir ->
  do let srcDir = dir </> "src"
     loaded <- loadModules ["samples"] [ModuleName ["Basic"]]
     allModules <- assertRight loaded
     checkedModules <- assertRight $ checkModules allModules
     mapM_ (runner srcDir HS.makeModule HS.makeFileName) checkedModules
     T.writeFile (dir </> "typed-wire-haskell-test.cabal") cabalTemplate
     T.writeFile (dir </> "stack.yaml") stackTemplate
     T.writeFile (srcDir </> "Main.hs") mainTemplate
     let p =
           (shell "stack build --pedantic")
           { cwd = Just dir
           }
     (_, _, _, handle) <- createProcess p
     ec <- waitForProcess handle
     assertEqual ExitSuccess ec
     let pRun =
           (shell "stack exec test")
           { cwd = Just dir
           }
     (_, _, _, hRun) <- createProcess pRun
     ecRun <- waitForProcess hRun
     assertEqual ExitSuccess ecRun

runner :: FilePath -> (Module -> T.Text) -> (ModuleName -> FilePath) -> Module -> IO ()
runner baseDir mkModule mkFilename m =
    let moduleSrc = mkModule m
        moduleFp = baseDir </> mkFilename (m_name m)
    in do createDirectoryIfMissing True (takeDirectory moduleFp)
          T.writeFile moduleFp moduleSrc
