{-# LANGUAGE OverloadedStrings #-}
module Main where

import TW.Ast
import TW.Loader
import TW.Check
import TW.Parser
import qualified TW.CodeGen.Haskell as HS
import qualified TW.CodeGen.Elm as Elm

import System.FilePath
import Control.Monad
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Options
   = Options
   { o_sourceDirs :: [FilePath]
   , o_entryPoints :: [ModuleName]
   , o_hsOutDir :: Maybe FilePath
   , o_elmOutDir :: Maybe FilePath
   }

optParser :: Parser Options
optParser =
    Options
    <$> sourceDirsP
    <*> entryPointsP
    <*> hsOutP
    <*> elmOutP

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

elmOutP :: Parser (Maybe FilePath)
elmOutP =
    optional $ strOption $
    long "elm-out" <> metavar "DIR" <> help "Generate Elm bindings to specified dir"

main :: IO ()
main =
    execParser opts >>= run
    where
      opts =
          info (helper <*> optParser)
          ( fullDesc
          <> progDesc "Language idependent type-safe communication"
          <> header "Generate bindings using typed-wire for different languages"
          )

run :: Options -> IO ()
run opts =
    do allModules <- loadModules (o_sourceDirs opts) (o_entryPoints opts)
       case allModules of
         Left err -> fail err
         Right ok ->
             case checkModules ok of
               Left err -> fail err
               Right readyModules ->
                   forM_ readyModules $ \m ->
                   do case o_hsOutDir opts of
                        Just dir ->
                            let moduleSrc = HS.makeModule m
                                moduleFp = dir </> HS.makeFileName (m_name m)
                            in do putStrLn $ "Writing " ++ moduleFp ++ "..."
                                  T.writeFile moduleFp moduleSrc
                        Nothing -> return ()
                      case o_elmOutDir opts of
                        Just dir ->
                            let moduleSrc = Elm.makeModule m
                                moduleFp = dir </> Elm.makeFileName (m_name m)
                            in do putStrLn $ "Writing " ++ moduleFp ++ "..."
                                  T.writeFile moduleFp moduleSrc
                        Nothing -> return ()

{-
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
-}
