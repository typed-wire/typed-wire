{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main where

import TW.Ast
import TW.Check
import qualified TW.CodeGen.Elm as Elm
import qualified TW.CodeGen.Haskell as HS
import qualified TW.CodeGen.PureScript as PS
import TW.Loader
import TW.Parser
import TW.Types

import qualified Paths_typed_wire as Meta

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Version as Vers
import Development.GitRev
import Options.Applicative
import System.Directory
import System.FilePath

data Options
   = Options
   { o_showVersion :: Bool
   , o_sourceDirs  :: [FilePath]
   , o_entryPoints :: [ModuleName]
   , o_hsOutDir    :: Maybe FilePath
   , o_elmOutDir   :: Maybe FilePath
   , o_elmVersionInfo :: Maybe ElmVersion
   , o_psOutDir    :: Maybe FilePath
   }

optParser :: Parser Options
optParser =
    Options
    <$> switch (long "version" <> help "Show version and exit")
    <*> sourceDirsP
    <*> entryPointsP
    <*> hsOutP
    <*> elmOutP
    <*> elmVersionP
    <*> psOutP

sourceDirsP :: Parser [FilePath]
sourceDirsP =
    many $ strOption $
    long "include-dir" <> short 'i' <> metavar "DIR" <> help "Directory to search for modules"

entryPointsP :: Parser [ModuleName]
entryPointsP =
    many $ option (str >>= mkMod) $
    long "entrypoint" <> short 'e' <> metavar "MODULE-NAME" <> help "Entrypoint for compiler"
    where
      mkMod t =
          case makeModuleName (T.pack t) of
            Left _ -> fail $ "Can not parse " ++ t ++ " as module"
            Right x -> return x

hsOutP :: Parser (Maybe FilePath)
hsOutP =
    optional $ strOption $
    long "hs-out" <> metavar "DIR" <> help "Generate Haskell bindings to specified dir"

elmVersionP :: Parser (Maybe ElmVersion)
elmVersionP =
    mkElmVersion <$>
    (optional $ strOption $
    long "elm-version" <> metavar "VERSION" <> help "Generate Elm bindings for a specific elm version")
    where
      mkElmVersion (Just s) =
        case makeElmVersion (T.pack s) of
          Left _ -> return Elm0p16
          Right x -> return x
      mkElmVersion Nothing = return Elm0p16

elmOutP :: Parser (Maybe FilePath)
elmOutP =
    optional $ strOption $
    long "elm-out" <> metavar "DIR" <> help "Generate Elm bindings to specified dir"

psOutP :: Parser (Maybe FilePath)
psOutP =
    optional $ strOption $
    long "purescript-out" <> metavar "DIR" <> help "Generate PureScript bindings to specified dir"

main :: IO ()
main =
    execParser opts >>= run
    where
      opts =
          info (helper <*> optParser)
          ( fullDesc
          <> progDesc "Language-independent type-safe communication"
          <> header "Generate bindings using typed-wire for different languages"
          )

showVersion :: IO ()
showVersion =
    T.putStrLn versionMessage
    where
      versionMessage =
          T.unlines
          [ "Version " <> T.pack (Vers.showVersion Meta.version)
          , "Git: " <> $(gitBranch) <> "@" <> $(gitHash)
          , " (" <> $(gitCommitCount) <> " commits in HEAD)"
          , if $(gitDirty) then " (includes uncommited changes)" else ""
          ]

run :: Options -> IO ()
run opts =
    if o_showVersion opts
    then showVersion
    else run' opts

run' :: Options -> IO ()
run' opts =
    do allModules <- loadModules (o_sourceDirs opts) (o_entryPoints opts)
       putStrLn "All modules loaded"
       case allModules of
         Left err -> fail err
         Right ok ->
             case checkModules ok of
               Left err -> fail err
               Right readyModules ->
                 do
                   forM_ (o_hsOutDir opts) $ \dir ->
                     do printLibraryInfo HS.libraryInfo
                        mapM_ (runner dir HS.makeModule HS.makeFileName) readyModules
                   forM_ (o_elmOutDir opts) $ \dir ->
                     do printLibraryInfo Elm.libraryInfo
                        mapM_ (runner dir (Elm.makeModule (o_elmVersionInfo opts)) Elm.makeFileName) readyModules
                   forM_ (o_psOutDir opts) $ \dir ->
                     do printLibraryInfo PS.libraryInfo
                        mapM_ (runner dir PS.makeModule PS.makeFileName) readyModules
                   return ()

printLibraryInfo :: LibraryInfo -> IO ()
printLibraryInfo li = T.putStrLn $
    "Required " <> li_type li <> " " <>
    li_name li <> " version " <> li_version li

runner :: FilePath -> (Module -> T.Text) -> (ModuleName -> FilePath) -> Module -> IO ()
runner baseDir mkModule mkFilename m =
    let moduleSrc = mkModule m
        moduleFp = baseDir </> mkFilename (m_name m)
    in do createDirectoryIfMissing True (takeDirectory moduleFp)
          putStrLn $ "Writing " ++ moduleFp ++ " ..."
          T.writeFile moduleFp moduleSrc
