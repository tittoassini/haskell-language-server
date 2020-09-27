-- Tests can be in either plain or Haddock comments in both single line or multi line format.
-- Comments and tests that do not start on the first column or the first line of a multi-line comment are ignored.
module AllComments where

-- Single line plain comment
-- >>> "a"++"b"

{- Multi line plain comment
>>> "b"++"c"
-}

-- | Single line Haddock comment
-- >>> "a"++"b"

{- Multi line Haddock comment
>>> "b"++"c"
-}

twice :: [a] -> [a]
twice a = a ++ a
-- ^ Also in backward Haddock comments
-- >>> twice "ABC"

   -- Ignored as it doesn't start on the first column.
   -- >>> IGNORED

{- | >>> 2+5

     >>> IGNORED
-}
