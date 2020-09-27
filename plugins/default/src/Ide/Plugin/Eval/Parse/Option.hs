-- |GHC language options parser
{-# OPTIONS_GHC -Wwarn #-}

module Ide.Plugin.Eval.Parse.Option
  ( langOptions
  )
where
import           Control.Monad.Combinators
import           Ide.Plugin.Eval.Parse.Parser

{- |
>>> langOptions ":set   -XBinaryLiterals  -XOverloadedStrings "
Right ["BinaryLiterals","OverloadedStrings"]

>>> langOptions ":set"
Right []

>>> langOptions ""
Left "No match"
-}
langOptions :: [Char] -> Either String [[Char]]
langOptions = runParser (many space *> languageOpts <* many space)

-- >>> runParser languageOpts ":set -XBinaryLiterals -XOverloadedStrings"
-- Right ["BinaryLiterals","OverloadedStrings"]
languageOpts :: Parser Char [[Char]]
languageOpts =
  string ":set" *> many (many space *> string "-X" *> (many letterChar))

-- type Parser = Parsec Void String
