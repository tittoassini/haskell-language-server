-- |Parse a Section, a group of zero or more tests defined in a multiline comment or a sequence of one line comments.
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Ide.Plugin.Eval.Parse.Section
  ( allSections,
    validSections,
    Section (..),
  )
where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Control.Monad.Combinators                (many, optional, some,
                                                           (<|>))
import qualified Data.List.NonEmpty                       as NE
import           Data.Maybe                               (catMaybes, fromMaybe)
import           Ide.Plugin.Eval.Parse.Parser             (Parser, runParser,
                                                           satisfy)
import           Ide.Plugin.Eval.Parse.Token              (Token (BlockOpen, blockFormat, blockLanguage, blockName),
                                                           TokenS, isBlockClose,
                                                           isBlockOpen,
                                                           isCodeLine,
                                                           isPropLine,
                                                           isStatement,
                                                           isTextLine,
                                                           unsafeContent)
import           Ide.Plugin.Eval.Types                    (Format (SingleLine),
                                                           Loc,
                                                           Located (Located, located, location),
                                                           Section (..),
                                                           Test (Example, Property),
                                                           hasTests, unLoc)

type Tk = Loc TokenS


validSections :: [Tk] -> Either String [Section]
validSections = (filter hasTests <$>) . allSections

allSections :: [Tk] -> Either String [Section]
allSections = runParser sections

{-|
>>> import  System.IO.Extra(readFileUTF8')
>>> testSource fp = runParser sections . tokensFrom <$> readFileUTF8' fp

>>> testSource "test/testdata/eval/T11.hs"
Right [Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 5, located = Example {testLines = " :set -XTupleSections -XFlexibleInstances" :| [" (\"a\",) \"b\""], testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 10, located = Example {testLines = " (\"a\",) \"b\"" :| [], testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 15, located = Example {testLines = " :set -XWrong" :| [], testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine}]

>>> testSource "test/testdata/eval/T12.hs"
Right [Section {sectionName = "setup", sectionTests = [Located {location = 3, located = Example {testLines = " let a = 11" :| [" let z = 33"], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "setup", sectionTests = [Located {location = 9, located = Example {testLines = " let x=11" :| [" let y=22"], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 12, located = Example {testLines = " x+y+z" :| [], testOutput = []}}], sectionLanguage = Haddock, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 14, located = Example {testLines = " \"A\"" :| [], testOutput = ["\"A\""]}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 18, located = Example {testLines = " x=33" :| [" y=18"," x+y"], testOutput = ["51"]}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 25, located = Example {testLines = " let x=11" :| [" y = 22"], testOutput = []}},Located {location = 28, located = Example {testLines = " x+y" :| [" x-y"], testOutput = []}},Located {location = 31, located = Example {testLines = " x+1+m" :| [], testOutput = ["Variable not in scope: m :: Integer"]}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 35, located = Example {testLines = " \"" :| [], testOutput = ["lexical error in string/character literal at end of input"]}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 39, located = Example {testLines = " \"abc\"" :| [], testOutput = ["\"abc\""]}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 46, located = Example {testLines = " print \"ABC\"" :| [], testOutput = ["()"]}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 55, located = Example {testLines = " import System.IO" :| [" import GHC.IO.Handle"," hSetEncoding stdout utf8 >> hSetEncoding stderr utf8"], testOutput = ["()"]}},Located {location = 64, located = Example {testLines = " import Data.ByteString" :| [" Data.ByteString.pack \"\20908\29916\""], testOutput = ["Couldn't match type \8216Char\8217 with \8216Word8\8217","Expected type: [Word8]","  Actual type: [Char]"]}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 73, located = Example {testLines = " :set -XFlexibleInstances" :| [], testOutput = []}},Located {location = 75, located = Example {testLines = " class Print f where asPrint :: f -> IO String" :| [" instance Show a => Print (IO a) where asPrint io = io >>= return . show"," instance Show a => Print a where asPrint a = return (show a)"," asPrint (print \"GG\")"," asPrint \"GG\""], testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine}]

-}
sections :: Parser Tk [Section]
sections =
  catMaybes <$> many (const Nothing <$> some code <|> Just <$> section)


section :: Parser Tk Section
section = sectionBody >>= sectionEnd

sectionBody :: Parser Tk Section
sectionBody =
  do
    ( \(unLoc -> BlockOpen {..}) ts ->
        Section (fromMaybe "" blockName) (catMaybes ts) blockLanguage blockFormat
      )
    <$> open <*> many (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)

sectionEnd :: Section -> Parser Tk Section
sectionEnd s
  | sectionFormat s == SingleLine = optional code *> return s
  | otherwise = close *> return s

-- section = do
--   s <-
--     maybe
--       (Section "" [] Plain SingleLine)
--       ( \(Located _ BlockOpen {..}) ->
--           Section (fromMaybe "" blockName) [] blockLanguage blockFormat
--       )
--       <$> optional open
--   ts <- many (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)
--   optional close
--   return $ s {sectionTests = catMaybes ts}

-- singleSection :: Parser Tk Section
-- singleSection = (\ts -> Section "" (catMaybes ts) Plain SingleLine) <$> tests

-- tests :: Parser Tk [Maybe (Loc Test)]
-- tests = some (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)

doc :: Parser Tk [Tk]
doc = some text

example, property :: Parser Tk (Loc Test)
property =
  ( \(Located l p) rs ->
      Located l (Property (unsafeContent p) (unsafeContent . located <$> rs))
  )
    <$> prop
    <*> many nonEmptyText
example =
  ( \es rs ->
      Located
        (location (NE.head es))
        (Example (unsafeContent . located <$> es) (unsafeContent . located <$> rs))
  )
    <$> NE.some statement
    <*> many nonEmptyText

open, close, statement, nonEmptyText, text, prop, code :: Parser Tk Tk
statement = is isStatement
text = is isTextLine
prop = is isPropLine
open = is isBlockOpen
close = is isBlockClose
code = is isCodeLine
nonEmptyText = is (\l -> isTextLine l && not (null (unsafeContent l)))

is :: (b -> Bool) -> Parser (Loc b) (Loc b)
is p = satisfy (p . unLoc)
