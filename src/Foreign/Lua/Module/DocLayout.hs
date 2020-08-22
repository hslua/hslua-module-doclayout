{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
  , description
  , fields
  , functions

  -- * Doc constructors and combinators
  , after_break
  , before_non_blank
  , blankline
  , blanklines
  , braces
  , brackets
  , cblock
  , chomp
  , concat
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

  -- * Rendering
  , render

  -- * Document Querying
  , is_empty
  , height
  , min_offset
  , offset
  , real_length
  , update_column

  -- * Marshaling
  , peekDoc
  , pushDoc
  )
where

import Prelude hiding (concat)
import Control.Monad (forM_)
import Data.List (intersperse)
import Data.Text (Text)
import Foreign.Lua (Lua, NumResults (..), Peekable, Pushable, StackIndex)
import Foreign.Lua.Call hiding (render)
import Foreign.Lua.Module hiding (preloadModule, pushModule, render)
import Foreign.Lua.Peek
  (Peeker, peekIntegral, peekList, peekString, peekText, toPeeker)
import Foreign.Lua.Push (Pusher, pushBool, pushIntegral, pushText)
import Text.DocLayout (Doc, (<+>), ($$), ($+$))

import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module as Module
import qualified Foreign.Lua.Types.Peekable as Lua
import qualified Foreign.Lua.Userdata as Lua
import qualified Text.DocLayout as Doc

#if ! MIN_VERSION_base(4, 11, 0)
import Data.Monoid ((<>))
#endif

--
-- Module
--

-- | Textual description of the "doclayout" module.
description :: Text
description = "Plain-text document layouting."

-- | Self-documenting module.
documentedModule :: Module
documentedModule = Module
  { moduleName = "doclayout"
  , moduleFields = fields
  , moduleDescription = description
  , moduleFunctions = functions
  }

-- | Pushes the @doclayout@ module to the Lua stack.
pushModule :: Lua NumResults
pushModule = 1 <$ pushModule' documentedModule

pushModule' :: Module -> Lua ()
pushModule' mdl = do
  Module.pushModule mdl
  forM_ (moduleFields mdl) $ \field -> do
    pushText (fieldName field)
    fieldPushValue field
    Lua.rawset (Lua.nth 3)

-- | Add the @doclayout@ module under the given name to the table
-- of preloaded packages.
preloadModule :: String -> Lua ()
preloadModule name = Module.preloadModule $
  documentedModule { moduleName = T.pack name }

--
-- Fields
--

-- | Exposed fields.
fields :: [Field]
fields =
  [ blankline
  , cr
  , empty
  , space
  ]

-- | Wrapped and documented 'Doc.blankline' value.
blankline :: Field
blankline = Field
  { fieldName = "blankline"
  , fieldDescription = "Inserts a blank line unless one exists already."
  , fieldPushValue = pushDoc Doc.blankline
  }

-- | Wrapped and documented 'Doc.cr' value.
cr :: Field
cr = Field
  { fieldName = "cr"
  , fieldDescription = "A carriage return. Does nothing if we're at " <>
                       "the beginning of a line; " <>
                       "otherwise inserts a newline."
  , fieldPushValue = pushDoc Doc.cr
  }

-- | Wrapped and documented 'Doc.empty' value.
empty :: Field
empty = Field
  { fieldName = "empty"
  , fieldDescription = "The empty document."
  , fieldPushValue = pushDoc Doc.empty
  }

-- | Wrapped and documented 'Doc.space' value.
space :: Field
space = Field
  { fieldName = "space"
  , fieldDescription = "A breaking (reflowable) space."
  , fieldPushValue = pushDoc Doc.space
  }

--
-- Functions
--

-- | Exposed module functions.
functions :: [(Text, HaskellFunction)]
functions =
  [ -- Constructors
    ("after_break", after_break)
  , ("before_non_blank", before_non_blank)
  , ("blanklines", blanklines)
  , ("braces", braces)
  , ("brackets", brackets)
  , ("cblock", cblock)
  , ("chomp", chomp)
  , ("concat", concat)
  , ("double_quotes", double_quotes)
  , ("flush", flush)
  , ("hang", hang)
  , ("inside", inside)
  , ("lblock", lblock)
  , ("literal", literal)
  , ("nest", nest)
  , ("nestle", nestle)
  , ("nowrap", nowrap)
  , ("parens", parens)
  , ("prefixed", prefixed)
  , ("quotes", quotes)
  , ("rblock", rblock)
  , ("vfill", vfill)
    -- rendering
  , ("render", render)
    -- querying
  , ("is_empty", is_empty)
  , ("height", height)
  , ("min_offset", min_offset)
  , ("offset", offset)
  , ("real_length", real_length)
  , ("update_column", update_column)
  ]


-- | Render a @'Doc'@. The text is reflowed on breakable spaces
-- to match the given line length. Text is not reflowed if the
-- line length parameter is omitted or nil.
render :: HaskellFunction
render = toHsFnPrecursor (flip Doc.render)
  <#> docParam "doc"
  <#> optionalParameter (peekIntegral @Int) "integer" "colwidth" ""
  =#> functionResult pushText "Doc" "rendered doc"
  #? ("Render a @'Doc'@. The text is reflowed on breakable spaces" <>
      "to match the given line length. Text is not reflowed if the" <>
      "line length parameter is omitted or nil.")

--
-- Querying
--

-- | @True@ iff the document is empty.
is_empty :: HaskellFunction
is_empty = toHsFnPrecursor Doc.isEmpty
  <#> docParam "doc"
  =#> booleanResult "`true` iff `doc` is the empty document, `false` otherwise."
  #? "Checks whether a doc is empty."

-- | Returns the width of a @'Doc'@.
offset :: HaskellFunction
offset = toHsFnPrecursor Doc.offset
  <#> docParam "doc"
  =#> intResult "doc width"
  #? "Returns the width of a `Doc` as number of characters."

-- | Returns the minimal width of a @'Doc'@ when reflowed at
-- breakable spaces.
min_offset :: HaskellFunction
min_offset = toHsFnPrecursor Doc.minOffset
  <#> docParam "doc"
  =#> intResult "minimal possible width"
  #? ("Returns the minimal width of a `Doc` when reflowed at " <>
      "breakable spaces.")

-- | Returns the column that would be occupied by the last laid
-- out character.
update_column :: HaskellFunction
update_column = toHsFnPrecursor Doc.updateColumn
  <#> docParam "doc"
  <#> intParam "i"
  =#> intResult "column number"
  #? ("Returns the column that would be occupied by the last " <>
      "laid out character.")

-- | Returns the height of a block or other Doc.
height :: HaskellFunction
height = toHsFnPrecursor Doc.height
  <#> docParam "doc"
  =#> intResult "doc height"
  #? "Returns the height of a block or other Doc."


-- | Returns the real length of a string in a monospace font: 0
-- for a combining character, 1, for a regular character, 2 for
-- an East Asian wide character.
real_length :: HaskellFunction
real_length = toHsFnPrecursor Doc.realLength
  <#> textParam "str"
  =#> intResult "text length"
  #? ("Returns the real length of a string in a monospace font: " <>
      "0 for a combining chaeracter, 1 for a regular character, " <>
      "2 for an East Asian wide character.")

--
-- Constructors
--

-- | Creates a @'Doc'@ which is conditionally included only if it
-- comes at the beginning of a line.
after_break :: HaskellFunction
after_break = toHsFnPrecursor Doc.afterBreak
  <#> textParam "text"
  =#> docResult "new doc"
  #? ("Creates a `Doc` which is conditionally included only if it" <>
      "comes at the beginning of a line.")

-- | Conditionally includes the given @'Doc'@ unless it is
-- followed by a blank space.
before_non_blank :: HaskellFunction
before_non_blank = toHsFnPrecursor Doc.beforeNonBlank
  <#> docParam "doc"
  =#> docResult "conditional doc"
  #? ("Conditionally includes the given `doc` unless it is " <>
      "followed by a blank space.")

-- | Insert blank lines unless they exist already.
blanklines :: HaskellFunction
blanklines = toHsFnPrecursor Doc.blanklines
  <#> intParam "n"
  =#> docResult "conditional blank lines"
  #? "Inserts blank lines unless they exist already."

-- | Puts a @'Doc'@ in curly braces.
braces :: HaskellFunction
braces = toHsFnPrecursor Doc.braces
  <#> docParam "doc"
  =#> docResult "doc enclosed by {}."
  #? "Puts the `doc` in curly braces."

-- | Puts a @'Doc'@ in square brackets.
brackets :: HaskellFunction -- Doc Text -> Lua (Doc Text)
brackets = toHsFnPrecursor Doc.brackets
  <#> docParam "doc"
  =#> docResult "doc enclosed by []."
  #? "Puts the `doc` in square brackets"

-- | Like @'lblock'@ but aligned centered.
cblock :: HaskellFunction
cblock = toHsFnPrecursor Doc.cblock
  <#> parameter peekIntegral "integer" "width" "block width in chars"
  <#> docParam "doc"
  =#> docResult ("doc, aligned centered in a block with max" <>
                 "`width` chars per line.")
  #? ("Creates a block with the given width and content, " <>
      "aligned centered.")

-- | Chomps trailing blank space off of a @'Doc'@.
chomp :: HaskellFunction
chomp = toHsFnPrecursor Doc.chomp
  <#> docParam "doc"
  =#> docResult "`doc` without trailing blanks"
  #? "Chomps trailing blank space off of the `doc`."

-- | Concatenates a list of @'Doc'@s.
concat :: HaskellFunction
concat = toHsFnPrecursor (\docs optSep -> mconcat $
                           maybe docs (`intersperse` docs) optSep)
  <#> parameter (peekList peekDoc) "`{Doc,...}`" "docs" "list of Docs"
  <#> optionalParameter peekDoc "Doc" "sep" "separator"
  =#> docResult "concatenated doc"
  #? "Concatenates a list of `Doc`s."

-- | Wraps a @'Doc'@ in double quotes
double_quotes :: HaskellFunction
double_quotes = toHsFnPrecursor Doc.doubleQuotes
  <#> docParam "doc"
  =#> docResult "`doc` enclosed by `\"` chars"
  #? "Wraps a `Doc` in double quotes."

-- | Makes a @'Doc'@ flush against the left margin.
flush :: HaskellFunction
flush = toHsFnPrecursor Doc.flush
  <#> docParam "doc"
  =#> docResult "flushed `doc`"
  #? "Makes a `Doc` flush against the left margin."

-- | Creates a hanging indent.
hang :: HaskellFunction
hang = toHsFnPrecursor Doc.hang
  <#> parameter peekIntegral "integer" "ind" "indentation width"
  <#> docParam "start"
  <#> docParam "doc"
  =#> docResult ("`doc` prefixed by `start` on the first line, " <>
                 "subsequent lines indented by `ind` spaces.")
  #? "Creates a hanging indent."

-- | Encloses a @'Doc'@ inside a start and end @'Doc'@.
inside :: HaskellFunction
inside = toHsFnPrecursor Doc.inside
  <#> docParam "start"
  <#> docParam "end"
  <#> docParam "contents"
  =#> docResult "enclosed contents"
  #? "Encloses a `Doc` inside a start and end `Doc`."

-- | Creates a block with the given width and content, aligned to
-- the left.
lblock :: HaskellFunction
lblock = toHsFnPrecursor Doc.lblock
  <#> parameter peekIntegral "integer" "width" "block width in chars"
  <#> docParam "doc"
  =#> docResult "doc put into block with max `width` chars per line."
  #? ("Creates a block with the given width and content, " <>
      "aligned to the left.")

-- | Creates a @'Doc'@ from a string.
literal :: HaskellFunction
literal = toHsFnPrecursor Doc.literal
  <#> textParam "string"
  =#> docResult "doc contatining just the literal string"
  #? "Creates a `Doc` from a string."

-- | Indents a @'Doc'@ by the specified number of spaces.
nest :: HaskellFunction
nest = toHsFnPrecursor Doc.nest
  <#> parameter peekIntegral "integer" "ind" "indentation size"
  <#> docParam "doc"
  =#> docResult "`doc` indented by `ind` spaces"
  #? "Indents a `Doc` by the specified number of spaces."

-- | Removes leading blank lines from a @'Doc'@.
nestle :: HaskellFunction
nestle = toHsFnPrecursor Doc.nestle
  <#> docParam "doc"
  =#> docResult "`doc` with leading blanks removed"
  #? "Removes leading blank lines from a `Doc`."

-- | Makes a @'Doc'@ non-reflowable.
nowrap :: HaskellFunction
nowrap = toHsFnPrecursor Doc.nowrap
  <#> docParam "doc"
  =#> docResult "same as input, but non-reflowable"
  #? "Makes a `Doc` non-reflowable."

-- | Puts a @'Doc'@ in parentheses.
parens :: HaskellFunction
parens = toHsFnPrecursor Doc.parens
  <#> docParam "doc"
  =#> docResult "doc enclosed by ()."
  #? "Puts the `doc` in parentheses."


-- | Uses the specified string as a prefix for every line of the
-- inside document (except the first, if not at the beginning of
-- the line).
prefixed :: HaskellFunction
prefixed = toHsFnPrecursor Doc.prefixed
  <#> parameter peekString "string" "prefix" "prefix for each line"
  <#> docParam "doc"
  =#> docResult "prefixed `doc`"
  #? ("Uses the specified string as a prefix for every line of " <>
      "the inside document (except the first, if not at the " <>
      "beginning of the line).")

-- | Wraps a @'Doc'@ in single quotes.
quotes :: HaskellFunction
quotes = toHsFnPrecursor Doc.quotes
  <#> docParam "doc"
  =#> docResult "doc enclosed in `'`."
  #? "Wraps a `Doc` in single quotes."

-- | Like @'rblock'@ but aligned to the right.
rblock :: HaskellFunction
rblock = toHsFnPrecursor Doc.rblock
  <#> parameter peekIntegral "integer" "width" "block width in chars"
  <#> docParam "doc"
  =#> docResult ("doc, right aligned in a block with max" <>
                 "`width` chars per line.")
  #? ("Creates a block with the given width and content, " <>
      "aligned to the right.")

-- | An expandable border that, when placed next to a box,
-- expands to the height of the box.  Strings cycle through the
-- list provided.
vfill :: HaskellFunction
vfill = toHsFnPrecursor Doc.vfill
  <#> textParam "border"
  =#> docResult "automatically expanding border Doc"
  #? ("An expandable border that, when placed next to a box, " <>
      "expands to the height of the box.  Strings cycle through the " <>
      "list provided.")

--
-- Marshaling
--

-- | Name used for the @Doc@ Lua userdata values.
docTypeName :: String
docTypeName = "HsLua DocLayout.Doc"

-- | Retrieve a @Doc Text@ value from the Lua stack. Strings are
-- converted to plain @'Doc'@ values.
peekDoc' :: StackIndex -> Lua (Doc Text)
peekDoc' idx = Lua.ltype idx >>= \case
  Lua.TypeString   -> let stringToDoc s = if T.null s
                                          then Doc.empty
                                          else Doc.literal s
                      in stringToDoc <$> Lua.peek idx
  Lua.TypeNumber   -> Doc.literal <$> Lua.peek idx
  _                -> Lua.reportValueOnFailure docTypeName
                        (`Lua.toAnyWithName` docTypeName)
                        idx

peekDoc :: Peeker (Doc Text)
peekDoc = toPeeker peekDoc'

instance Peekable (Doc Text) where
  peek = peekDoc'

-- | Push a @Doc Text@ value to the Lua stack.
pushDoc :: Pusher (Doc Text)
pushDoc = Lua.pushAnyWithMetatable pushDocMT
  where
    pushDocMT = Lua.ensureUserdataMetatable docTypeName $ do
      Lua.addfunction "__add"      __add
      Lua.addfunction "__concat"   __concat
      Lua.addfunction "__div"      __div
      Lua.addfunction "__eq"       __eq
      Lua.addfunction "__idiv"     __idiv
      Lua.addfunction "__tostring" __tostring

instance Pushable (Doc Text) where
  push = pushDoc

-- | Concatenate two @'Doc'@s, putting breakable spaces between them.
__add :: Doc Text -> Doc Text -> Lua (Doc Text)
__add a b = return (a <+> b)

-- | Concatenate two @'Doc'@.
__concat :: Doc Text -> Doc Text -> Lua (Doc Text)
__concat a b = return (a <> b)

-- | @a / b@ puts @a@ above @b@.
__div :: Doc Text -> Doc Text -> Lua (Doc Text)
__div a b = return (a $$ b)

-- | Test @'Doc'@ equality.
__eq :: Doc Text -> Doc Text -> Lua Bool
__eq a b = return (a == b)

-- | @a // b@ puts @a@ above @b@.
__idiv :: Doc Text -> Doc Text -> Lua (Doc Text)
__idiv a b = return (a $+$ b)

-- | Convert to string by rendering without reflowing.
__tostring :: Doc Text -> Lua Text
__tostring d = return $ Doc.render Nothing d

--
-- Parameters
--

-- | @Doc@ typed function parameter.
docParam :: Text -> Parameter (Doc Text)
docParam name = parameter peekDoc "Doc" name ""

-- | @Int@ typed function parameter.
intParam :: Text -> Parameter Int
intParam name = parameter (peekIntegral @Int) "integer "name ""

-- | @Text@ typed function parameter.
textParam :: Text -> Parameter Text
textParam name = parameter peekText "string" name ""

--
-- Results
--

-- | Boolean function result.
booleanResult :: Text -- ^ Description
              -> FunctionResults Bool
booleanResult = functionResult pushBool "boolean"

-- | Function result of type @'Doc'@.
docResult :: Text -- ^ Description
          -> FunctionResults (Doc Text)
docResult = functionResult pushDoc "Doc"

-- | Function result of type @'Int'@.
intResult :: Text -- ^ Description
          -> FunctionResults Int
intResult = functionResult (pushIntegral @Int) "integer"

