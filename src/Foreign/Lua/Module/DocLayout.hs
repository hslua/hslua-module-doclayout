{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Foreign.Lua.Module.DocLayout
Copyright   : © 2020 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+hslua@zeitkraut.de>
Stability   : alpha
Portability : Requires GHC 8 or later.

Provides a Lua module which wraps @'Text.DocLayout'@. The @Doc'
type is specialized to @'Text'@.

This module defines orphan instances for @Doc Text@.
-}
module Foreign.Lua.Module.DocLayout (
  -- * Module
    pushModule
  , preloadModule
  )
where

import Data.Text (Text)
import Foreign.Lua (Lua, NumResults (..), Pushable)
import Text.DocLayout (Doc, empty)

import qualified Foreign.Lua as Lua

--
-- Module
--

-- | Pushes the @doclayout@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  Lua.addfield "empty_doc" (empty :: Doc Text)
  return 1

-- | Add the @doclayout@ module under the given name to the table
-- of preloaded packages.
preloadModule :: String -> Lua ()
preloadModule = flip Lua.preloadhs pushModule

instance Pushable (Doc Text) where
  push = Lua.pushAny
