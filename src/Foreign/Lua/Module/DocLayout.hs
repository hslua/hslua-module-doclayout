{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

  -- * Functions
  , render

  -- * Marshaling
  , peekDoc
  , pushDoc
  )
where

import Data.Text (Text)
import Foreign.Lua (Lua, NumResults (..), Optional,
                    Peekable, Pushable, StackIndex)
import Text.DocLayout (Doc)

import qualified Foreign.Lua as Lua
import qualified Text.DocLayout as Doc

--
-- Module
--

-- | Pushes the @doclayout@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  Lua.addfield "empty_doc" (Doc.empty :: Doc Text)
  Lua.addfunction "render" render
  return 1

-- | Add the @doclayout@ module under the given name to the table
-- of preloaded packages.
preloadModule :: String -> Lua ()
preloadModule = flip Lua.preloadhs pushModule

-- | Render a @'Doc'@. The text is reflowed on breakable spaces
-- to match the given line length. Text is not reflowed if the
-- line length parameter is omitted or nil.
render :: Doc Text -> Optional Int -> Lua Text
render doc optLength = return $ Doc.render (Lua.fromOptional optLength) doc

--
-- Marshaling
--

-- | Retrieve a @Doc Text@ value from the Lua stack. Strings are
-- converted to plain @'Doc'@ values.
peekDoc :: StackIndex -> Lua (Doc Text)
peekDoc idx = Lua.ltype idx >>= \case
    Lua.TypeString   -> Doc.literal <$> Lua.peek idx
    _                -> Lua.peekAny idx

instance Peekable (Doc Text) where
  peek = peekDoc

-- | Push a @Doc Text@ value to the Lua stack.
pushDoc :: Doc Text -> Lua ()
pushDoc = Lua.pushAny

instance Pushable (Doc Text) where
  push = pushDoc
