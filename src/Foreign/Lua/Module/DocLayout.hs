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

  -- * Doc constructors and combinators
  , after_break
  , before_non_blank
  , blankline
  , blanklines
  , braces
  , brackets
  , cblock
  , chomp
  , cr
  , double_quotes
  , empty
  , flush
  , hang
  , inside
  , lblock
  , literal
  , nest
  , nestle
  , nowrap
  , parens
  , prefixed
  , quotes
  , rblock
  , space
  , vfill

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
import Text.DocLayout ((<+>), Doc)

import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Types.Peekable as Lua
import qualified Foreign.Lua.Userdata as Lua
import qualified Text.DocLayout as Doc

--
-- Module
--

-- | Pushes the @doclayout@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  -- constructors
  Lua.addfield "empty"     empty
  Lua.addfield "blankline" blankline
  Lua.addfield "cr"        cr
  Lua.addfield "space"     space
  Lua.addfunction "after_break" after_break
  Lua.addfunction "before_non_blank" before_non_blank
  Lua.addfunction "blanklines" blanklines
  Lua.addfunction "braces"     braces
  Lua.addfunction "brackets"   brackets
  Lua.addfunction "cblock"     cblock
  Lua.addfunction "chomp"      chomp
  Lua.addfunction "double_quotes" double_quotes
  Lua.addfunction "flush"      flush
  Lua.addfunction "hang"       hang
  Lua.addfunction "inside"     inside
  Lua.addfunction "lblock"     lblock
  Lua.addfunction "literal"    literal
  Lua.addfunction "nest"       nest
  Lua.addfunction "nestle"     nestle
  Lua.addfunction "nowrap"     nowrap
  Lua.addfunction "parens"     parens
  Lua.addfunction "quotes"     quotes
  Lua.addfunction "prefixed"   prefixed
  Lua.addfunction "rblock"     rblock
  Lua.addfunction "vfill"      vfill
  -- renderign
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
-- Constructors
--

-- | Creates a @'Doc'@ which is conditionally included only if it
-- comes at the beginning of a line.
after_break :: Text -> Lua (Doc Text)
after_break = return . Doc.afterBreak

-- | Conditionally includes the given @'Doc'@ unless it is
-- followed by a blank space.
before_non_blank :: Doc Text -> Lua (Doc Text)
before_non_blank = return . Doc.beforeNonBlank

-- | Inserts a blank line unless one exists already.
blankline :: Doc Text
blankline = Doc.blankline

-- | Insert blank lines unless they exist already.
blanklines :: Int -> Lua (Doc Text)
blanklines = return . Doc.blanklines

-- | Puts a @'Doc'@ in curly braces.
braces :: Doc Text -> Lua (Doc Text)
braces = return . Doc.braces

-- | Puts a @'Doc'@ in square brackets.
brackets :: Doc Text -> Lua (Doc Text)
brackets = return . Doc.brackets

-- | Like @'lblock'@ but aligned centered.
cblock :: Int -> Doc Text -> Lua (Doc Text)
cblock width = return . Doc.cblock width

-- | Chomps trailing blank space off of a @'Doc'@.
chomp :: Doc Text -> Lua (Doc Text)
chomp = return . Doc.chomp

-- | A carriage return. Does nothing if we're at the beginning of
-- a line; otherwise inserts a newline.
cr :: Doc Text
cr = Doc.cr

-- | Wraps a @'Doc'@ in double quotes
double_quotes :: Doc Text -> Lua (Doc Text)
double_quotes = return . Doc.doubleQuotes

-- | The empty document.
empty :: Doc Text
empty = Doc.empty

-- | Makes a @'Doc'@ flush against the left margin.
flush :: Doc Text -> Lua (Doc Text)
flush = return . Doc.flush

-- | Creates a hanging indent.
hang :: Int -> Doc Text -> Doc Text -> Lua (Doc Text)
hang ind start doc = return $ Doc.hang ind start doc

-- | Encloses a @'Doc'@ inside a start and end @'Doc'@.
inside :: Doc Text -> Doc Text -> Doc Text -> Lua (Doc Text)
inside start end contents = return $ Doc.inside start end contents

-- | Creates a block with the given width and content, aligned to the left.
lblock :: Int -> Doc Text -> Lua (Doc Text)
lblock width = return . Doc.lblock width

-- | Creates a @'Doc'@ from a string.
literal :: Text -> Lua (Doc Text)
literal = return . Doc.literal

-- | Indents a @'Doc' by the specified number of spaces.
nest :: Int -> Doc Text -> Lua (Doc Text)
nest ind = return . Doc.nest ind

-- | Removes leading blank lines from a @'Doc'@.
nestle :: Doc Text -> Lua (Doc Text)
nestle = return . Doc.nestle

-- | Makes a @'Doc'@ non-reflowable.
nowrap :: Doc Text -> Lua (Doc Text)
nowrap = return . Doc.nowrap

-- | Puts a @'Doc'@ in parentheses.
parens :: Doc Text -> Lua (Doc Text)
parens = return . Doc.parens

-- | Uses the specified string as a prefix for every line of the
-- inside document (except the first, if not at the beginning of
-- the line).
prefixed :: Text -> Doc Text -> Lua (Doc Text)
prefixed prefix = return . Doc.prefixed (T.unpack prefix)

-- | Wraps a @'Doc'@ in single quotes.
quotes :: Doc Text -> Lua (Doc Text)
quotes = return . Doc.quotes

-- | Like @'lblock'@ but aligned to the right.
rblock :: Int -> Doc Text -> Lua (Doc Text)
rblock ind = return . Doc.rblock ind

-- | A breaking (reflowable) space.
space :: Doc Text
space = Doc.space

vfill :: Text -> Lua (Doc Text)
vfill = return . Doc.vfill


--
-- Marshaling
--

-- | Name used for the @Doc@ Lua userdata values.
docTypeName :: String
docTypeName = "HsLua DocLayout.Doc"

-- | Retrieve a @Doc Text@ value from the Lua stack. Strings are
-- converted to plain @'Doc'@ values.
peekDoc :: StackIndex -> Lua (Doc Text)
peekDoc idx = Lua.ltype idx >>= \case
  Lua.TypeString   -> Doc.literal <$> Lua.peek idx
  _                -> Lua.reportValueOnFailure docTypeName
                        (`Lua.toAnyWithName` docTypeName)
                        idx

instance Peekable (Doc Text) where
  peek = peekDoc

-- | Push a @Doc Text@ value to the Lua stack.
pushDoc :: Doc Text -> Lua ()
pushDoc = Lua.pushAnyWithMetatable pushDocMT
  where
    pushDocMT = Lua.ensureUserdataMetatable docTypeName $ do
      Lua.addfunction "__add"      __add
      Lua.addfunction "__concat"   __concat
      Lua.addfunction "__eq"       __eq
      Lua.addfunction "__tostring" __tostring

instance Pushable (Doc Text) where
  push = pushDoc

-- | Concatenate two @'Doc'@s, putting breakable spaces between them.
__add :: Doc Text -> Doc Text -> Lua (Doc Text)
__add a b = return (a <+> b)

-- | Concatenate two @'Doc'@.
__concat :: Doc Text -> Doc Text -> Lua (Doc Text)
__concat a b = return (a <> b)

-- | Test @'Doc'@ equality.
__eq :: Doc Text -> Doc Text -> Lua Bool
__eq a b = return (a == b)

-- | Convert to string by rendering without reflowing.
__tostring :: Doc Text -> Lua Text
__tostring d = return $ Doc.render Nothing d
