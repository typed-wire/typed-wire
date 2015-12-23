{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} TW.CodeGen.HaskellTest

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
