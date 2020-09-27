{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-type-defaults #-}

{- |
A plugin inspired by the REPLoid feature of <https://github.com/jyp/dante Dante>, <https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744 Haddock>'s Examples and Properties and <https://hackage.haskell.org/package/doctest Doctest>.

For a full example see the "Ide.Plugin.Eval.Example" module.
-}
module Ide.Plugin.Eval.CodeLens
  ( codeLens,
    evalCommand
  )
where
import           Control.Applicative
import           Control.Arrow                  (second)
import qualified Control.Exception              as E
import           Control.Monad                  (void, when)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Except     (ExceptT (..), runExceptT,
                                                 throwE)
import           Data.Aeson                     (FromJSON, ToJSON, Value (Null),
                                                 toJSON)
import           Data.Bifunctor                 (first)
import           Data.Char                      (isSpace)
import           Data.Either                    (isRight)
import qualified Data.HashMap.Strict            as Map
import           Data.List                      (dropWhileEnd)
import           Data.Maybe                     (catMaybes, fromMaybe)
import           Data.String                    (IsString (fromString))
import           Data.Text                      (Text)
import qualified Data.Text                      as T
-- import           Data.Text.Encoding                (decodeUtf8)
import           Data.Time                      (getCurrentTime)
import           Data.Time.Clock                (diffUTCTime)
import           Data.Typeable                  (Typeable)
-- import           Development.IDE.Core.Preprocessor (preprocessor)
import           Data.List                      (find)
import           Development.IDE.Core.Rules     (runAction)
import           Development.IDE.Core.RuleTypes (GetModSummary (..),
                                                 GhcSession (..))
import           Development.IDE.Core.Shake     (use_)
import           Development.IDE.GHC.Util       (HscEnvEq, evalGhcEnv, hscEnv,
                                                 moduleImportPath,
                                                 textToStringBuffer)
import           Development.IDE.Types.Location (toNormalizedFilePath',
                                                 uriToFilePath')
import           DynamicLoading                 (initializePlugins)
import           DynFlags                       (targetPlatform)
import           GHC                            (DynFlags (ghcLink, ghcMode, hscTarget, importPaths, packageFlags, pkgDatabase, pkgState, useColor, ways),
                                                 ExecOptions (execLineNumber, execSourceFile),
                                                 ExecResult (ExecBreak, ExecComplete),
                                                 GeneralFlag (Opt_IgnoreHpcChanges, Opt_IgnoreOptimChanges, Opt_ImplicitImportQualified),
                                                 Ghc, GhcLink (LinkInMemory),
                                                 GhcMode (CompManager),
                                                 GhcMonad (getSession),
                                                 HscTarget (HscInterpreted),
                                                 LoadHowMuch (LoadAllTargets),
                                                 ModSummary (ms_hspp_opts),
                                                 Module (moduleName),
                                                 SuccessFlag (Failed, Succeeded),
                                                 TcRnExprMode (..), execOptions,
                                                 execStmt, exprType,
                                                 getInteractiveDynFlags,
                                                 getSessionDynFlags, isImport,
                                                 isStmt, load, runDecls,
                                                 setContext,
                                                 setInteractiveDynFlags,
                                                 setLogAction,
                                                 setSessionDynFlags, setTargets,
                                                 typeKind)
import           GHC.Generics                   (Generic)
import qualified GHC.LanguageExtensions.Type    as LangExt
import           GhcPlugins                     (GeneralFlag (Opt_DiagnosticsShowCaret),
                                                 defaultLogActionHPutStrDoc,
                                                 gopt_set, gopt_unset,
                                                 interpWays, updateWays,
                                                 wayGeneralFlags,
                                                 wayUnsetGeneralFlags, xopt_set)
import           HscTypes                       (HscEnv,
                                                 InteractiveImport (IIModule),
                                                 ModSummary (ms_mod),
                                                 Target (Target),
                                                 TargetId (TargetFile))
import           HscTypes                       (HscEnv (hsc_IC))
import           HscTypes                       (InteractiveContext (ic_dflags))
import           Ide.Plugin                     (mkLspCommand)
import           Ide.Plugin.Eval.Code           (Statement, asStatements,
                                                 evalExpr, evalExtensions,
                                                 evalSetup, propSetup,
                                                 resultRange, testCheck,
                                                 testRanges)
import           Ide.Plugin.Eval.Debug          (asS, dbg, dbg_)
import           Ide.Plugin.Eval.GHC            (addExtension, addImport,
                                                 addPackages, gStrictTry,
                                                 hasPackage, isExpr)
import           Ide.Plugin.Eval.Parse.Option   (langOptions)
import           Ide.Plugin.Eval.Parse.Section  (Section (sectionFormat, sectionTests),
                                                 allSections)
import           Ide.Plugin.Eval.Parse.Token    (tokensFrom)
import           Ide.Plugin.Eval.Types          (Format (SingleLine), Loc,
                                                 Located (Located), Test,
                                                 hasTests, isProperty,
                                                 splitSections, unLoc)
import           Ide.Types                      (CodeLensProvider,
                                                 CommandFunction, CommandId,
                                                 PluginCommand (PluginCommand))
import           Language.Haskell.LSP.Core      (LspFuncs (getVirtualFileFunc, withIndefiniteProgress),
                                                 ProgressCancellable (Cancellable))
import           Language.Haskell.LSP.Types     (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                 CodeLens (CodeLens),
                                                 CodeLensParams (CodeLensParams, _textDocument),
                                                 Command (_arguments, _title),
                                                 ErrorCode (InternalError),
                                                 List (List), Range (Range),
                                                 ResponseError (ResponseError),
                                                 ServerMethod (WorkspaceApplyEdit),
                                                 TextDocumentIdentifier (..),
                                                 TextEdit (TextEdit),
                                                 WorkspaceEdit (WorkspaceEdit),
                                                 toNormalizedUri)
import           Language.Haskell.LSP.VFS       (virtualFileText)
import           Outputable                     (nest, ppr, showSDoc, text,
                                                 ($$), (<+>))
import           System.FilePath                (takeFileName)
import           System.IO                      (hClose)
import           System.IO.Temp                 (withSystemTempFile)
import           Text.Read                      (readMaybe)
import           Util                           (OverridingBool (Never))

-- | Code Lens provider
-- NOTE: Invoked every time the document is modified, not just when the document is saved.
codeLens :: CodeLensProvider
codeLens lsp _state plId CodeLensParams {_textDocument} = response $ do
  now <- liftIO getCurrentTime
  let TextDocumentIdentifier uri = _textDocument
  contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri uri
  let Just text = virtualFileText <$> contents

  -- Trying to use GerParsedModule to normalise CPP/LHS files, but all comments are removed.
  -- pr <-
  --   liftIO $
  --     runAction "runEvalCmd.GetParsedModule" state $
  --       use_ GetParsedModule nfp
  -- liftIO $ dbg "PREPROCESSED !" $ asS . pm_parsed_source $ pr

  -- Use the preprocessor to normalise CPP/LHS files, but we still get an error.
  -- fp <- handleMaybe "uri" $ uriToFilePath' uri
  -- let nfp = toNormalizedFilePath' fp
  -- session :: HscEnvEq <-
  --   liftIO $
  --     runAction "runEvalCmd.ghcSession" state $
  --       use_ GhcSession nfp
  -- Right (ppContent, dflags) <- liftIO $ evalGhcEnv (hscEnv session) $ runExceptT $ preprocessor fp (Just $ textToStringBuffer content)
  -- let text = decodeUtf8 $ stringBufferToByteString ppContent
  -- liftIO $ dbg "PREPROCESSED CONTENT" text

  -- Parse test sections
  -- CHECK: add a Shake Action to return the result of parsing tests out of the current file?
  let Right (setups, nonSetups) =
        (splitSections . filter hasTests <$>)
          . allSections
          . tokensFrom
          . T.unpack
          $ text -- fp' (T.unpack text)
  let tests = testsBySection nonSetups

  cmd <- liftIO $ mkLspCommand plId evalCommandName "Evaluate=..." (Just [])
  let lenses =
        [ CodeLens testRange (Just cmd') Nothing
          | (section, test) <- tests,
            let (testRange, resultRange) = testRanges test
                args = EvalParams (setups ++ [section]) _textDocument
                cmd' =
                  (cmd :: Command)
                    { _arguments = Just (List [toJSON args]),
                      _title =
                        if trivial resultRange
                          then "Evaluate..."
                          else "Refresh..."
                    }
        ]

  dbg "SETUPS" setups

  dbg_ $
    unwords
      [ show (length tests),
        "tests in",
        show (length nonSetups),
        "sections",
        show (length setups),
        "setups",
        show (length lenses),
        "lenses."
      ]

  now' <- liftIO getCurrentTime
  dbg "TIME" (diffUTCTime now' now)

  -- dbg "LENSES" lenses
  return $ List lenses
  where
    trivial (Range p p') = p == p'

testsBySection :: [Section] -> [(Section, Loc Test)]
testsBySection sections =
  [(section, test) | section <- sections, test <- sectionTests section]

evalCommandName :: CommandId
evalCommandName = "evalCommand"

evalCommand :: PluginCommand
evalCommand = PluginCommand evalCommandName "evaluate" runEvalCmd

data EvalParams = EvalParams
  { sections :: [Section],
    module_  :: !TextDocumentIdentifier
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- TODO: report errors that take place during
runEvalCmd :: CommandFunction EvalParams
runEvalCmd lsp state EvalParams {..} = withIndefiniteProgress lsp "Evaluating" Cancellable $ response' $ do
  now <- liftIO getCurrentTime

  let TextDocumentIdentifier {_uri} = module_
  fp <- handleMaybe "uri" $ uriToFilePath' _uri
  let nfp = toNormalizedFilePath' fp
  contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri _uri
  text <- handleMaybe "contents" $ virtualFileText <$> contents

  session :: HscEnvEq <-
    liftIO $
      runAction "runEvalCmd.ghcSession" state $
        use_ GhcSession nfp

  ms <-
    liftIO $
      runAction "runEvalCmd.getModSummary" state $
        use_ GetModSummary nfp

  -- pr <-
  --   liftIO $
  --     runAction "runEvalCmd.GetParsedModule" state $
  --       use_ GetParsedModule nfp
  -- liftIO $ dbg "PREPROCESSED" $ asS . pm_parsed_source $ pr

  -- >>> 2+2

  let tests = testsBySection sections

  let modName = moduleName $ ms_mod ms

  let Just impPath = moduleImportPath nfp modName
  liftIO $ dbg "IMPORT PATH" impPath

  -- Setup environment for evaluation
  hscEnv' <- withSystemTempFile (takeFileName fp) $ \logFilename logHandle -> ExceptT $ (either Left id <$>) . gStrictTry $
  -- Right hscEnv' <- liftIO $ withSystemTempFile (takeFileName fp) $ \logFilename logHandle ->  (either Left id <$>) . gStrictTry $
      evalGhcEnv (hscEnv session) $ do
        env <- getSession
        dbg "ENV SESSION DFLAGS".  ic_dflags . hsc_IC  $ env

        -- Get options and language flags from module source
        df0 <- liftIO $ setupDynFlagsForGHCiLike env $ ms_hspp_opts ms

        -- We add back the local importPath, so that we can find local dependencies
        let df = df0 {importPaths=[impPath]}
        _lp <- setSessionDynFlags df
        dbg "df" df

        -- property tests need QuickCheck
        when (needsQuickCheck tests) $ void $ addPackages df ["QuickCheck"]
        dbg "QUICKCHECK NEEDS, HAS" (needsQuickCheck tests,hasQuickCheck df)

        -- copy the package state to the interactive DynFlags
        idflags <- getInteractiveDynFlags
        dbg "idflags" idflags

        setInteractiveDynFlags $
          (foldl xopt_set idflags evalExtensions)
            { pkgState = pkgState df,
              pkgDatabase = pkgDatabase df,
              packageFlags = packageFlags df,
              useColor = Never -- BUG: GHC still using color in output?
            }

        -- set up a custom log action
        setLogAction $ \_df _wr _sev _span _style _doc ->
          defaultLogActionHPutStrDoc _df logHandle _doc _style

        -- Load the module with its current content (as the saved module might not be up to date)
        -- BUG: this fails for files that use CPP
        -- CPP is not allowed in combination with targetContents, see https://gitlab.haskell.org/ghc/ghc/-/issues/17066
        -- SOL: If this fails, save the file if necessary (detect if in-memory image is up to date) and load it from there.
        eSetTarget <- gStrictTry $ setTargets [Target
              (TargetFile fp Nothing)
              False
              (Just (textToStringBuffer text, now))]
        dbg "setTarget" eSetTarget

        -- load the module in the interactive environmentp
        -- this is the slow bit
        loadResult <- load LoadAllTargets
        liftIO $ dbg "LOAD RESULT" (asS loadResult)
        case loadResult of
          Failed -> liftIO $ do
            hClose logHandle
            err <- readFile logFilename
            dbg "load ERR" err
            return $ Left err
          Succeeded -> do
            setContext
              --[IIDecl (simpleImportDecl $ moduleName pRELUDE), IIModule modName]
              [IIModule modName]
            Right <$> getSession

  edits <- liftIO $ evalGhcEnv hscEnv' $ runTests fp tests

  let workspaceEditsMap = Map.fromList [(_uri, List edits)]
  let workspaceEdits = WorkspaceEdit (Just workspaceEditsMap) Nothing

  now' <- liftIO getCurrentTime
  dbg "TIME" (diffUTCTime now' now)

  return (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdits)

runTests :: String -> [(Section, Loc Test)] -> Ghc [TextEdit]
runTests fp tests =  do
            -- df <- getSessionDynFlags
            df <- getInteractiveDynFlags
            evalSetup
            when (hasQuickCheck df && needsQuickCheck tests) $ void $ evals fp df propSetup
            mapM (processTest fp df) tests
            -- dbg "evals RES" res
  where
  processTest :: String -> DynFlags  -> (Section, Loc Test) -> Ghc TextEdit
  processTest fp df (section, test) = do
          let pad = pad_ $ padPrefix (sectionFormat section)

          rs <- runTest fp df test
          dbg "TEST RESULTS" rs

          let checkedResult = testCheck (section, unLoc test) rs

          let edit =
                TextEdit
                  (resultRange test)
                  (T.unlines . map pad $ checkedResult)
          dbg "TEST EDIT" edit
          return edit

  runTest :: String -> DynFlags -> Loc Test -> Ghc [Text]
  runTest _ df test
          | not (hasQuickCheck df) && (isProperty . unLoc $ test) =
            return $
              singleLine
                "Add QuickCheck to your cabal dependencies to run this test."
  runTest fp df test = evals fp df (asStatements test)

evals :: String -> DynFlags -> [Statement] -> Ghc [Text]
evals fp df stmts = do
          er <- gStrictTry $ mapM (eval fp df) stmts
          return $ case er of
            Left err -> errorLines err
            Right rs -> concat . catMaybes $ rs

eval :: String -> DynFlags -> Statement -> Ghc (Maybe [Text])
eval fp df (Located l stmt)
          | -- A :set -XLanguageOption directive
            isRight (langOptions stmt) =
            either
              (return . Just . errorLines)
              (\es -> do
                dbg ":SET" es
                ndf <- getInteractiveDynFlags
                dbg "pre set" ndf
                mapM_ addExtension es
                ndf <- getInteractiveDynFlags
                dbg "post set" ndf
                return Nothing)
              $ ghcOptions stmt

          | -- A type/kind command
            Just (cmd, arg) <- parseGhciLikeCmd $ T.pack stmt
            = evalGhciLikeCmd cmd arg

          | -- An expression
            isExpr df stmt =
            do
              dbg "{EXPR" stmt
              -- NOTE: catches type/syntax errors but does not catch `error`
              eres <- gStrictTry $ evalExpr stmt
              dbg "RES ->" eres
              let res = case eres of
                    Left err -> errorLines err
                    Right rs -> [T.pack rs]
              dbg "EXPR} ->" res
              return . Just $ res
          | -- A statement
            isStmt df stmt =
            do
              dbg "{STMT " stmt
              res <- exec stmt l
              r <- case res of
                ExecComplete (Left err) _ ->
                  return . Just . errorLines . show $ err
                ExecComplete (Right _) _ -> return Nothing
                ExecBreak {} ->
                  return . Just . singleLine $ "breakpoints are not supported"
              dbg "STMT} -> " r
              return r
          | -- An import
            isImport df stmt =
            do
              dbg "{IMPORT " stmt
              _ <- addImport stmt
              return Nothing
          | -- A declaration
            otherwise =
            do
              dbg "{DECL " stmt
              void $ runDecls stmt
              return Nothing
  where
        exec stmt l =
          let opts = execOptions {execSourceFile = fp, execLineNumber = l}
           in execStmt stmt opts


needsQuickCheck :: [(Section, Loc Test)] -> Bool
needsQuickCheck = any (isProperty . unLoc .snd)

hasQuickCheck :: DynFlags -> Bool
hasQuickCheck df = hasPackage df "QuickCheck"

singleLine :: String -> [Text]
singleLine s = [T.pack s]

-- Errors might be multilines
errorLines :: String -> [Text]
errorLines err =
  map
    ( \e ->
        fromMaybe
          e
          (T.stripSuffix "arising from a use of ‘asPrint’" e)
    )
    . dropWhileEnd T.null
    . takeWhile (not . ("CallStack" `T.isPrefixOf`))
    . T.lines
    . T.pack
    $ err

-- | Resulting @Text@ MUST NOT prefix each line with @--@
--   Such comment-related post-process will be taken place
--   solely in 'evalGhciLikeCmd'.
type GHCiLikeCmd = DynFlags -> Text -> Ghc (Maybe Text)

-- Should we use some sort of trie here?
ghciLikeCommands :: [(Text, GHCiLikeCmd)]
ghciLikeCommands =
  [ ("kind", doKindCmd False)
  , ("kind!", doKindCmd True)
  , ("type", doTypeCmd)
  ]

evalGhciLikeCmd :: Text -> Text -> Ghc (Maybe [Text])
evalGhciLikeCmd cmd arg = do
  df <- getSessionDynFlags
  case lookup cmd ghciLikeCommands
    <|> snd <$> find (T.isPrefixOf cmd . fst) ghciLikeCommands of
    Just hndler ->
      fmap
      --   -- (T.unlines . map ("-- " <>) . T.lines)
      T.lines
      <$>
      hndler df arg
    _           -> E.throw $ GhciLikeCmdNotImplemented cmd arg

doKindCmd :: Bool -> DynFlags -> Text -> Ghc (Maybe Text)
doKindCmd False df arg = do
  let input = T.strip arg
  (_, kind) <- typeKind False $ T.unpack input
  let kindText = text (T.unpack input) <+> "::" <+> ppr kind
  pure $ Just $ T.pack (showSDoc df kindText)
doKindCmd True df arg = do
  let input = T.strip arg
  (ty, kind) <- typeKind True $ T.unpack input
  let kindDoc = text (T.unpack input) <+> "::" <+> ppr kind
      tyDoc = "=" <+> ppr ty
  pure $ Just $ T.pack (showSDoc df $ kindDoc $$ tyDoc)

doTypeCmd :: DynFlags -> Text -> Ghc (Maybe Text)
doTypeCmd dflags arg = do
  let (emod, expr) = parseExprMode arg
  ty <- exprType emod $ T.unpack expr
  let rawType = T.strip $ T.pack $ showSDoc dflags $ ppr ty
      broken = T.any (\c -> c == '\r' || c == '\n') rawType
  pure $ Just $
    if broken
    then T.pack
      $ showSDoc dflags
      $ text (T.unpack expr) $$
          (nest 2 $
            "::" <+> ppr ty
          )
    else expr <> " :: " <> rawType <> "\n"

-- evalGhciLikeCmd :: GhcMonad m => Text -> Text -> m (Maybe [Text])
-- evalGhciLikeCmd cmd arg = do
--   df <- getSessionDynFlags
--   let tppr = T.pack . showSDoc df . ppr
--   case cmd of
--     "kind" -> do
--       let input = T.strip arg
--       (_, kind) <- typeKind False $ T.unpack input
--       pure $ Just [input <> " :: " <> tppr kind]
--     "kind!" -> do
--       let input = T.strip arg
--       (ty, kind) <- typeKind True $ T.unpack input
--       pure
--         $ Just
--         [ input <> " :: " <> tppr kind
--         , "= " <> tppr ty
--         ]
--     "type" -> do
--       let (emod, expr) = parseExprMode arg
--       ty <- exprType emod $ T.unpack expr
--       pure $ Just [expr <> " :: " <> tppr ty]
--     _ -> E.throw $ GhciLikeCmdNotImplemented cmd arg

parseExprMode :: Text -> (TcRnExprMode, T.Text)
parseExprMode rawArg =
  case T.break isSpace rawArg of
    ("+v", rest) -> (TM_NoInst, T.strip rest)
    ("+d", rest) -> (TM_Default, T.strip rest)
    _            -> (TM_Inst, rawArg)

data GhciLikeCmdException =
    GhciLikeCmdNotImplemented
      { ghciCmdName :: Text
      , ghciCmdArg  :: Text
      }
  deriving (Typeable)

instance Show GhciLikeCmdException where
  showsPrec _ GhciLikeCmdNotImplemented{..} =
    showString "unknown command '" .
    showString (T.unpack ghciCmdName) . showChar '\''

instance E.Exception GhciLikeCmdException

-- instance E.Exception [Char]

-- >>> parseGhciLikeCmd (T.pack ":kind! N + M + 1")
-- Just ("kind!","N + M + 1")
-- >>> parseGhciLikeCmd (T.pack ":kind a")
-- Just ("kind","a")
parseGhciLikeCmd :: Text -> Maybe (Text, Text)
parseGhciLikeCmd input = do
  (':', rest) <- T.uncons $ T.stripStart input
  pure $ second T.strip $ T.break isSpace rest


{-
>>> ghcOptions ":set -XLambdaCase"
Right [LambdaCase]
>>> ghcOptions ":set -XLambdaCase -XNotRight"
Left "Unknown extension: \"NotRight\""
-}
ghcOptions :: [Char] -> Either String [LangExt.Extension]
ghcOptions = either Left (mapM chk) . langOptions
  where
    chk o =
      maybe
        (Left $ unwords ["Unknown extension:", show o])
        Right
        (readMaybe o :: Maybe LangExt.Extension)


padPrefix :: IsString p => Format -> p
padPrefix SingleLine = "-- "
padPrefix _          = ""

-- |
-- >>> map (pad_ (T.pack "--")) (map T.pack ["2+2",""])
-- ["--2+2","--<BLANKLINE>"]
pad_ :: Text -> Text -> Text
-- pad_ prefix = T.unlines . map ((prefix `T.append`) . convertBlank) . T.lines
pad_ prefix = (prefix `T.append`) . convertBlank

convertBlank :: Text -> Text
convertBlank x
  | T.null x = "<BLANKLINE>"
  | otherwise = x

-------------------------------------------------------------------------------

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

-- handleEither :: Monad m => Either e v -> ExceptT e m v
-- handleEither = either throwE return

-- handleEitherM :: Monad m => m (Either e b) -> ExceptT e m b
-- handleEitherM act = either throwE return  =<< lift act

-- handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
-- handleMaybeM msg act = maybe (throwE msg) return =<< lift act

response :: ExceptT String IO a -> IO (Either ResponseError a)
response =
  fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
    . runExceptT

response' :: ExceptT String IO a -> IO (Either ResponseError Value, Maybe a)
response' act = do
  res <- runExceptT act
  case res of
    Left e ->
      return
        (Left (ResponseError InternalError (fromString e) Nothing), Nothing)
    Right a -> return (Right Null, Just a)

setupDynFlagsForGHCiLike :: HscEnv -> DynFlags -> IO DynFlags
setupDynFlagsForGHCiLike env dflags = do
  let dflags3 =
        dflags
          { hscTarget = HscInterpreted,
            ghcMode = CompManager,
            ghcLink = LinkInMemory
          }
      platform = targetPlatform dflags3
      dflags3a = updateWays $ dflags3 {ways = interpWays}
      dflags3b =
        foldl gopt_set dflags3a $
          concatMap (wayGeneralFlags platform) interpWays
      dflags3c =
        foldl gopt_unset dflags3b $
          concatMap (wayUnsetGeneralFlags platform) interpWays
      dflags4 =
        dflags3c
          `gopt_set` Opt_ImplicitImportQualified
          `gopt_set` Opt_IgnoreOptimChanges
          `gopt_set` Opt_IgnoreHpcChanges
          `gopt_unset` Opt_DiagnosticsShowCaret
  initializePlugins env dflags4

{-
The result of evaluating a test line can be:
* a value
* nothing
* a (possibly multiline) error message

A value is returned for a correct expression, either a pure value:

>>> 'h' :"askell"
"haskell"

or an 'IO a' (output on stdout/stderr is ignored):
>>> print "OK" >> return "ABC"
"ABC"

Nothing is returned for a correct directive:

>>>:set -XFlexibleInstances
>>> import Data.Maybe

Nothing is returned for a correct declaration (let..,x=, data, class)

>>> let x = 11
>>> y = 22
>>> data B = T | F
>>> class C a

Nothing is returned for an empty line:

>>>

A, possibly multi line, error is returned for a wrong declaration, directive or value or an exception thrown by the evaluated code:

>>>:set -XNonExistent
Unknown extension: "NonExistent"

>>> cls C
Variable not in scope: cls :: t0 -> f0
Data constructor not in scope: C

>>> "A
lexical error in string/character literal at end of input

>>> 3 `div` 0
divide by zero

>>> error "Something went wrong\nbad times" :: E.SomeException
Not in scope: type constructor or class ‘E.SomeException’
No module named ‘E’ is imported.

Or for a value that does not have a Show instance:
>>> data V = V
>>> V

No instance for (Show V)
-}

