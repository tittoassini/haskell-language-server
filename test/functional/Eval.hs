{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval (
    tests,
) where

import Control.Applicative.Combinators (
    skipManyTill,
 )
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Test (
    anyMessage,
    documentContents,
    executeCommand,
    fullCaps,
    getCodeLenses,
    message,
    openDoc,
    runSession,
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditRequest,
    CodeLens (
        CodeLens,
        _command,
        _range
    ),
    Command (_title),
    Position (..),
    Range (..),
 )
import System.Directory (doesFileExist)
import System.FilePath (
    (<.>),
    (</>),
 )
import Test.Hls.Util (hlsCommand)
import Test.Tasty (
    TestTree,
    testGroup,
 )
import Test.Tasty.ExpectedFailure (
    expectFailBecause,
    ignoreTestBecause,
 )
import Test.Tasty.HUnit (
    testCase,
    (@?=),
 )

tests :: TestTree
tests =
    testGroup
        "eval"
        [ testCase "Produces Evaluate code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Evaluate..."]
        , testCase "Produces Refresh code lenses" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getCodeLenses doc
                liftIO $ map (fmap _title . _command) lenses @?= [Just "Refresh..."]
        , testCase "Code lenses have ranges" $
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T1.hs" "haskell"
                lenses <- getCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Multi-line expressions have a multi-line range" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T3.hs" "haskell"
                lenses <- getCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 3 0) (Position 5 0)]
        , testCase "Executed expressions range covers only the expression" $ do
            runSession hlsCommand fullCaps evalPath $ do
                doc <- openDoc "T2.hs" "haskell"
                lenses <- getCodeLenses doc
                liftIO $ map _range lenses @?= [Range (Position 4 0) (Position 5 0)]
        , testCase "Evaluation of expressions" $ goldenTest "T1.hs"
        , testCase "Reevaluation of expressions" $ goldenTest "T2.hs"
        , testCase "Evaluation of expressions w/ imports" $ goldenTest "T3.hs"
        , testCase "Evaluation of expressions w/ lets" $ goldenTest "T4.hs"
        , testCase "Refresh an evaluation" $ goldenTest "T5.hs"
        , testCase "Refresh an evaluation w/ lets" $ goldenTest "T6.hs"
        , testCase "Refresh a multiline evaluation" $ goldenTest "T7.hs"
        , testCase "Evaluate incorrect expressions" $ goldenTest "T8.hs"
        , testCase "Applies file LANGUAGE extensions" $ goldenTest "T9.hs"
        , testCase "Evaluate a type with :kind!" $ goldenTest "T10.hs"
        , testCase "Reports an error for an incorrect type with :kind!" $
            goldenTest "T11.hs"
        , testCase "Shows a kind with :kind" $ goldenTest "T12.hs"
        , testCase "Reports an error for an incorrect type with :kind" $
            goldenTest "T13.hs"
        , testCase "Returns a fully-instantiated type for :type" $
            goldenTest "T14.hs"
        , testCase "Returns an uninstantiated type for :type +v, admitting multiple whitespaces around arguments" $
            goldenTest "T15.hs"
        , testCase "Returns defaulted type for :type +d, admitting multiple whitespaces around arguments" $
            goldenTest "T16.hs"
        , testCase ":type reports an error when given with unknown +x option" $
            goldenTest "T17.hs"
        , testCase "Reports an error when given with unknown command" $
            goldenTest "T18.hs"
        , testCase "Returns defaulted type for :type +d reflecting the default declaration specified in the >>> prompt" $
            goldenTest "T19.hs"
        , expectFailBecause "known issue - see a note in P.R. #361" $
            testCase ":type +d reflects the `default' declaration of the module" $
                goldenTest "T20.hs"
        , testCase ":type handles a multilined result properly" $
            goldenTest "T21.hs"
        , testCase ":t behaves exactly the same as :type" $
            goldenTest "T22.hs"
        , testCase ":type does \"dovetails\" for short identifiers" $
            goldenTest "T23.hs"
        , testCase ":kind! treats a multilined result properly" $
            goldenTest "T24.hs"
        , testCase ":kind treats a multilined result properly" $
            goldenTest "T25.hs"
        , testCase "local imports" $
            goldenTest "T26.hs"
        , testCase
            "Evaluate expressions in both Plain and Haddock comments in both single line and multi line format"
            $ goldenTest "TAllComments.hs"
        , testCase "Compare results (for Haddock tests only)" $
            goldenTest "TCompare.hs"
        , testCase "Local Modules imports are accessible in a test" $
            goldenTest "TLocalImport.hs"
        , -- , testCase "Local Modules can be imported in a test" $ goldenTest "TLocalImportInTest.hs"
          ignoreTestBecause "Unexplained but minor issue" $
            testCase "Setting language option TupleSections" $
                goldenTest "TLanguageOptionsTupleSections.hs"
        , testCase "IO expressions are supported, stdout/stderr output is ignored" $
            goldenTest "TIO.hs"
        , testCase "Property checking" $ goldenTest "TProperty.hs"
        , testCase
            "Prelude has no special treatment, it is imported as stated in the module."
            $ goldenTest "TPrelude.hs"
#if __GLASGOW_HASKELL__ >= 808
  , testCase "CPP support" $ goldenTest "TCPP.hs"
  , testCase "Literate Haskell Bird Style" $ goldenTest "TLHS.lhs"
#endif
            -- , testCase "Literate Haskell LaTeX Style" $ goldenTest "TLHSLateX.lhs"
        ]

goldenTest :: FilePath -> IO ()
goldenTest = goldenTestBy id

{- |Execute all CodeLens accepted by 'filter'
 Compare results with the contents of corresponding '.expected' file (and creates it, if missing)
-}
goldenTestBy :: ([CodeLens] -> [CodeLens]) -> FilePath -> IO ()
goldenTestBy filter input = runSession hlsCommand fullCaps evalPath $ do
    doc <- openDoc input "haskell"

    -- Execute lenses backwards, to avoid affecting their position in the source file
    codeLenses <- reverse . filter <$> getCodeLenses doc
    mapM_ executeCommand $ [c | CodeLens{_command = Just c} <- codeLenses]
    _resp :: ApplyWorkspaceEditRequest <- skipManyTill anyMessage message
    edited <- replaceUnicodeQuotes <$> documentContents doc

    let expectedFile = evalPath </> input <.> "expected"

    liftIO $ do
        -- Write expected file if missing
        missingExpected <- not <$> doesFileExist expectedFile
        when missingExpected $ T.writeFile expectedFile edited

    expected <- liftIO $ T.readFile expectedFile
    liftIO $ edited @?= expected

replaceUnicodeQuotes :: T.Text -> T.Text
replaceUnicodeQuotes = T.replace "‘" "'" . T.replace "’" "'"

evalPath :: FilePath
evalPath = "test/testdata/eval"
