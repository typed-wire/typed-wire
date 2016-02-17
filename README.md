typed-wire
=====

[![Build Status](https://travis-ci.org/typed-wire/typed-wire.svg)](https://travis-ci.org/typed-wire/typed-wire)
[![Hackage](https://img.shields.io/hackage/v/typed-wire.svg)](http://hackage.haskell.org/package/typed-wire)

## Intro

Hackage: [typed-wire](http://hackage.haskell.org/package/typed-wire)
Stackage: [typed-wire](https://www.stackage.org/package/typed-wire)

Language idependent type-safe communication

## Motivation / WIP Notice

Here are some details on the library:

* I wrote this because I have more and more micro-service architectures. The simplest form is a Haskell (REST-Api) Backend talking with an Elm/Purescript/... frontend. I got really tired of defining all data types in Haskell, Elm, PureScript, ... and writing JSON parsing/serialization functions for them in all languages. typed-wire attempts to solve this/my problem: It generates type definitions and matching JSON parsing/serialization for all target languages from a definition file. It can also generate API definitions, but this feature is not finished yet.
* It's not ready yet - it is still missing some planned features, testing and documentation
* It's used in production at one of my projects to ensure DRY type-safe communication between an Elm client and a Haskell server (www.bahn-buddy.de , german)
* If anyone thinks this is cool, needs it too and would like to help out please shoot me an email :-)

## Cli Usage: twirec

```sh
$ twirec --help
Generate bindings using typed-wire for different languages

Usage: twirec [--version] [-i|--include-dir DIR] [-e|--entrypoint MODULE-NAME]
              [--hs-out DIR] [--elm-out DIR] [--purescript-out DIR]
  Language idependent type-safe communication

Available options:
  -h,--help                Show this help text
  --version                Show version and exit
  -i,--include-dir DIR     Directory to search for modules
  -e,--entrypoint MODULE-NAME
                           Entrypoint for compiler
  --hs-out DIR             Generate Haskell bindings to specified dir
  --elm-out DIR            Generate Elm bindings to specified dir
  --purescript-out DIR     Generate PureScript bindings to specified dir

```

## Install

* Using cabal: `cabal install typed-wire`
* Using Stack: `stack install typed-wire`
* From Source (cabal): `git clone https://github.com/typed-wire/typed-wire.git && cd typed-wire && cabal install`
* From Source (stack): `git clone https://github.com/typed-wire/typed-wire.git && cd typed-wire && stack build`


## Misc

### Supported GHC Versions

* 7.10.2

### License

Released under the MIT license.
(c) 2015 - 2016 Alexander Thiemann <mail@athiemann.net>
