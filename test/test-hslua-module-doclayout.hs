{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires language extensions ForeignFunctionInterface,
              OverloadedStrings.

Tests for the `doclayout` Lua module.
-}
module Main (main) where

import Control.Monad (void)
import Foreign.Lua (Lua)
import Foreign.Lua.Module.DocLayout (preloadModule, pushModule)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.Lua (translateResultsFromFile)

import qualified Foreign.Lua as Lua

main :: IO ()
main = do
  luaTestResults <- Lua.run $ do
    Lua.openlibs
    Lua.requirehs "doclayout" (void pushModule)
    translateResultsFromFile "test/test-doclayout.lua"
  defaultMain $ testGroup "hslua-module-doclayout" [tests, luaTestResults]

-- | HSpec tests for the Lua 'system' module
tests :: TestTree
tests = testGroup "HsLua doclayout module"
  [ testCase "module can be pushed to the stack" $
      Lua.run (void pushModule)

  , testCase "module can be added to the preloader" . Lua.run $ do
      Lua.openlibs
      preloadModule "doclayout"
      assertEqual' "function not added to preloader" Lua.TypeFunction =<< do
        Lua.getglobal' "package.preload.doclayout"
        Lua.ltype (-1)

  , testCase "module can be loaded via preloader" . Lua.run $ do
      Lua.openlibs
      preloadModule "doclayout"
      assertEqual' "loading the module fails " Lua.OK =<<
        Lua.dostring "require 'doclayout'"
  ]

assertEqual' :: (Show a, Eq a) => String -> a -> a -> Lua ()
assertEqual' msg expected = Lua.liftIO . assertEqual msg expected
