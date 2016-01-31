{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} TW.CodeGen.HaskellTest
import {-@ HTF_TESTS @-} TW.CodeGen.PureScriptTest

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
