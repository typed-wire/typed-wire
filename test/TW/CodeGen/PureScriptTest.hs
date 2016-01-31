{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module TW.CodeGen.PureScriptTest where

import TW.Ast
import TW.Check
import TW.Loader
import TW.Types
import qualified TW.CodeGen.PureScript as PS

import Data.Aeson ((.=))
import System.Directory
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Process
import Test.Framework
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

mainTemplate :: T.Text
mainTemplate =
  T.unlines
  [ "module Main where"
  , "import Basic"
  , "import Data.Either"
  , "import Data.TypedWire.Prelude"
  , "import Control.Monad.Eff.Console"
  , "import Control.Monad.Eff"
  , ""
  , "main :: forall a. Eff (console :: CONSOLE | a) Unit"
  , "main = "
  , "  do log \"Lauching self-check ...\""
  , "     let b = Bar { someField: true, moreFields: 42 }"
  , "         bEnc = encodeJson b"
  , "         bDec = decodeJson bEnc"
  , "     if (Right b /= bDec) then error \"Bar enc/dec check ... failed\" else log \"Bar enc/dec check ... ok\""
  ]

bowerTemplate :: T.Text
bowerTemplate =
  T.decodeUtf8 $
  BSL.toStrict $
  A.encode $
  A.object
  [ "name" .= T.pack "purescript-typed-wire-test"
  , "version" .= T.pack "0.1.0"
  , "moduleType" .= [T.pack "node"]
  , "ignore" .= map T.pack ["**/.*", "node_modules", "bower_components", "output"]
  , "dependencies" .=
      A.object
      [ (li_name PS.libraryInfo) .= (li_version PS.libraryInfo)
      , "purescript-console" .= T.pack "0.1.1"
      ]
  , "repository" .=
       A.object
       [ "type" .= T.pack "git"
       , "url" .= T.pack "git://github.com/typed-wire/purescript-typed-wire"
       ]
  ]

test_pureScriptCodeGen :: IO ()
test_pureScriptCodeGen =
  withSystemTempDirectory "purescriptCodeGenX" $ \dir ->
  do putStrLn dir
     let srcDir = dir </> "src"
     loaded <- loadModules ["samples"] [ModuleName ["Basic"]]
     allModules <- assertRight loaded
     checkedModules <- assertRight $ checkModules allModules
     mapM_ (runner srcDir PS.makeModule PS.makeFileName) checkedModules
     T.writeFile (dir </> "bower.json") bowerTemplate
     T.writeFile (srcDir </> "Main.purs") mainTemplate
     let pDeps =
           (shell "pulp dep install")
           { cwd = Just dir
           }
     (_, _, _, hDeps) <- createProcess pDeps
     ecDeps <- waitForProcess hDeps
     assertEqual ExitSuccess ecDeps
     let p =
           (shell "pulp build")
           { cwd = Just dir
           }
     (_, _, _, handle) <- createProcess p
     ec <- waitForProcess handle
     assertEqual ExitSuccess ec
     let pRun =
           (shell "pulp run")
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
