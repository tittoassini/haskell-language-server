module Ide.Plugin.Eval.Tutorial where

{-
The "Eval" plugin evaluates code in comment prompts and splice the results right below.

This is mainly useful to:
* quickly evaluate small code fragments
* test and document functions

A quick calculation:
>>> 2**4.5/pi
7.202530529256849

A little test for the following function:

>>> import Data.List(intercalate)
>>> intercalate " " example
"This is an example of interactive evaluation"
-}
example :: [String]
example = ["This is an example", "of", "interactive", "evaluation"]

{-
The basic unit that gets evaluated is a test, that's to say a sequence of:
* imports
* statements
* directives
* properties
* expressions
in no particular order, with every line introduced by `>>>` (or `prop>` in the case of properties).

For example:

>>> import Data.Maybe(fromJust)
>>> just2 = fromJust . fromJust
>>> :type just2
just2 :: forall c. Maybe (Maybe c) -> c

>>> just2 (Just (Just True))
True

Tests can appear in either plain comments like this one or in Haddock comments.

You execute a test by clicking on `Evaluate` or `Refresh`.
-}

{-
A test can contain multiple expressions:

>>> "AB" ++ "CD"
>>> "CD" ++ "AB"
"ABCD"
"CDAB"
-}

{- |
All tests in the same comment sections are executed together.

This is very convenient to execute multiple tests on the same function, as in this example:

>>> double 0

>>> double 11

>>> double 22
-}
double :: Num a => a -> a
double n = n*2

-- A section prefixed with '$setup' has a special meaning, its code is executed before any other test.

-- $setup
-- >>> x = 11
-- >>> y = 22

-- 'x' and 'y' are available in any test:
-- >>> (x,y)
-- (11,22)


{- |
Haddock comments, like this one, constitute the external module's documentation.

Theirs tests are part of the module functions' definitions and their results are not supposed to change.

So, whenever tests in Haddock comments are refreshed their current result is compared with the previous one and differences are displayed.

If by mistake we change the definition of 'evens', we get a warning:

>>> evens [1..7]
WAS [2,4,6]
NOW [1,3,5,7]

On the contrary, the result of tests in plain comments are simply updated.
-}
evens :: [Integer] -> [Integer]
evens = filter odd

{-
Let's see in more detail the components of a test.

Language Extensions:

>>> :set -XScopedTypeVariables -XStandaloneDeriving -XDataKinds -XTypeOperators -XExplicitNamespaces

Imports, from any package in scope:

>>> import Data.List
>>> import GHC.TypeNats

or from modules in the same source directory:

>>> import Ide.Plugin.Eval.CodeLens

Statements/declarations (function declaration can optionally be introduced by 'let'):

>>> let tuple x = (x,x)
>>> triple x = (x,x,x)
>>> data TertiumDatur = Truly | Falsely | Other deriving Show
>>> class Display a where display :: a -> String
>>> instance Display TertiumDatur where display = show

Type and Kind directives (as in ghci):

>>> :type Truly
Truly :: TertiumDatur

>>> :kind TertiumDatur
TertiumDatur :: *

>>> :type 3
3 :: forall p. Num p => p

>>> :type +d 3
3 :: Integer

>>> type N = 1
>>> type M = 40
>>> :kind! N + M + 1
N + M + 1 :: Nat
= 42

Properties:

prop> \(l::[Int]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.

prop> \(l::[Bool]) -> reverse l == l
*** Failed! Falsified (after 5 tests and 2 shrinks):
[True,False]

And finally expressions:

>>> tuple 2
>>> triple 3
>>> display Other
(2,2)
(3,3,3)
"Other"
-}

-- Though the Eval plugin functionality is quite similar to that of <https://hackage.haskell.org/package/doctest Doctest>, some features are not supported.

{-
Capturing stdout:

>>> print "foo"
()
-}

{-
Matching arbitrary output

-- >>> "foo bar baz"
-- foo ... baz
-}

{-
Omitting lambda abstractions in property tests:

prop> reverse (reverse l) == (l::[Int])
Variable not in scope: l :: [Int]
Variable not in scope: l :: [Int]

This works:

prop> \(l::[Int]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.
-}

{-
Multiline comments like the following:
>>> :{
 let
   x = 1
   y = 2
 in x + y + multiline
:}
-}

{- Tip: Pretty printing.

Say that you want to pretty print a value.

You could, for example, use the pretty-simple package:

>>> import Text.Pretty.Simple
>>> pShowNoColor [1..3]
"[ 1\n, 2\n, 3\n] "

But what we get is just a String.

We could try to print it, but stdout is not captured:

>>> print $ pShowNoColor [1..7]
()

To 'print' it properly, we can exploit the fact that the output of an error is displayed as a multi-line text:

>>> import qualified Data.Text.Lazy as TL
>>> import Text.Pretty.Simple
>>> prettyPrint v = error (TL.unpack $ pShowNoColor v) :: IO String
>>> prettyPrint [1..3]
[ 1
, 2
, 3
]
-}

{-
Tip: prop> can be customised.

propEvaluation is the function used to evaluate a property, it can be redefined (e.g. to use something different than QuickCheck):

prop> \(l::[Bool]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.

>>> propEvaluation p = return "Your property is a joke and it miserably failed!"

prop> \(l::[Bool]) -> reverse (reverse l) == l
"Your property is a joke and it miserably failed!"
-}
