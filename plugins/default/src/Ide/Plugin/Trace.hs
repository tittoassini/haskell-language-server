{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-unused-imports #-}

{- | In-Source Tracing

https://hackage.haskell.org/packages/search?terms=eventlog

https://hackage.haskell.org/package/ekg
https://hackage.haskell.org/package/opentelemetry

& http://hackage.haskell.org/package/base-4.14.0.0/docs/Debug-Trace.html#g:2

* https://hackage.haskell.org/package/tracing
http://hackage.haskell.org/package/tracing-control
    An OpenTracing-compliant, simple, and extensible distributed tracing library.

PROB: when text is modified the traces do not fall in the right place (should be adapted tracking changes or simply hidden if file is modified).
PROB: displayed only after first change to file.
-}
module Ide.Plugin.Trace (
    descriptor,
) where

import Control.Monad (join, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe ()
import Data.Aeson (
    ToJSON (toJSON),
    Value (Null),
 )
import qualified Data.HashMap.Strict as Map
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, modifyIORef', newIORef, readIORef)
import Data.List
import Data.List (isPrefixOf)
import Data.List.Extra (replace)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.String (IsString)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Debug.Trace as DT
import Development.IDE (
    GetParsedModule (
        GetParsedModule
    ),
    GhcSession (GhcSession),
    HscEnvEq,
    IdeState,
    List (..),
    NormalizedFilePath,
    Position (Position),
    Range (Range),
    evalGhcEnv,
    hscEnvWithImportPaths,
    realSrcSpanToRange,
    runAction,
    toNormalizedUri,
    uriToFilePath',
    use,
    use_,
 )
import Development.IDE.Core.Shake
import Development.IDE.Plugin (getPid)
import Development.IDE.Types.Logger
import GHC (
    DynFlags (importPaths),
    GenLocated (L),
    HsModule (hsmodName),
    ParsedModule (pm_parsed_source),
    SrcSpan (RealSrcSpan),
    getSessionDynFlags,
    unLoc,
 )
import GHC.Base (when)
import GHC.Exts (IsList, Item, toList)
import GHC.Stack
import Ide.Plugin (mkLspCmdId)
import Ide.Plugin.Eval.Types (Located (Located))
import Ide.Types (
    CommandFunction,
    PluginCommand (..),
    PluginDescriptor (..),
    PluginId (..),
    defaultPluginDescriptor,
 )
import Language.Haskell.LSP.Core (
    LspFuncs,
    getVirtualFileFunc,
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditParams (..),
    CAResult (CACodeAction),
    CodeAction (CodeAction),
    CodeActionKind (
        CodeActionQuickFix
    ),
    CodeLens (CodeLens),
    CodeLensParams (CodeLensParams),
    Command (Command),
    ServerMethod (..),
    TextDocumentIdentifier (
        TextDocumentIdentifier
    ),
    TextEdit (TextEdit),
    Uri,
    WorkspaceEdit (..),
    uriToNormalizedFilePath,
 )
import System.IO.Unsafe (unsafePerformIO)

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginId = plId
        , pluginCodeLensProvider = Just codeLens
        , pluginCommands = [PluginCommand editCommandName editCommandName editCmd]
        }

-- | Generate code lenses
codeLens ::
    LspFuncs c ->
    IdeState ->
    PluginId ->
    CodeLensParams ->
    IO (Either a2 (List CodeLens))
codeLens lsp state pluginId (CodeLensParams (TextDocumentIdentifier uri) _) =
    do
        pid <- getPid
        actions (asCodeLens (mkLspCmdId pid pluginId editCommandName)) lsp state uri

editCommandName :: IsString p => p
editCommandName = "edit"

-- | Generic command to apply a group of edits
editCmd :: CommandFunction WorkspaceEdit
editCmd _lf _ide workspaceEdits =
    return
        ( Right Null
        , Just $ (WorkspaceApplyEdit, ApplyWorkspaceEditParams workspaceEdits)
        )

-- | Required actions that can be converted to either CodeLenses or CodeActions
actions ::
    (Action -> a1) ->
    LspFuncs c ->
    IdeState ->
    Uri ->
    IO (Either a2 (List a1))
actions convert _lsp state uri = do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri

    Just (_, mdlName) <- codeModuleName state nfp

    ls <- logsPerModule $ traceShowId mdlName

    let act = Action uri
    let actions =
            [ convert $
                act
                    (asRange l)
                    val
                    ( T.unwords
                        ["--", val]
                    )
            | Located l val <- ls
            ]

    traceIO (show $ length actions)

    pure . Right . List $ traceShow (length actions) actions

logsPerModule :: ModuleName -> IO [Log]
logsPerModule mdlName = do
    ls <- readIORef logs
    let found = maybe [] M.elems (M.lookup mdlName ls)
    print $ unwords ["logsPerModule", show mdlName, show found]
    return found

type Logs = M.Map ModuleName (M.Map ModuleLine Log)

type ModuleName = T.Text

type ModuleLine = Int

logs :: IORef Logs
{-# NOINLINE logs #-}
logs = unsafePerformIO (newIORef M.empty)

traceIO_ :: (IsList l, Item l ~ (a, SrcLoc)) => l -> String -> IO ()
traceIO_ stack msg = do
    let stk = toList stack
    unless (null stk) $ do
        let l = snd . head $ stk
            log = Located l (T.pack msg)
        print $ unwords ["tracing", show log]
        atomicModifyIORef' logs (\ls -> (M.insertWith M.union (T.pack $ srcLocModule l) (M.singleton (srcLocStartLine l) log) ls, ()))

traceIO :: HasCallStack => String -> IO ()
traceIO = traceIO_ callStack

trace_ :: (IsList l, Item l ~ (a1, SrcLoc)) => l -> String -> a2 -> a2
trace_ callStack string expr = unsafePerformIO $ do
    traceIO_ callStack string
    return expr

trace :: HasCallStack => String -> a -> a
trace = trace_ callStack

traceId :: HasCallStack => String -> String
traceId a = trace_ callStack a a

traceShow :: HasCallStack => Show a => a -> b -> b
traceShow = trace_ callStack . show

traceShowId :: HasCallStack => Show a => a -> a
traceShowId a = trace_ callStack (show a) a

traceM :: HasCallStack => Applicative f => String -> f ()
traceM string = trace_ callStack string $ pure ()

-- traceShowM :: (Show a, Applicative f) => a -> f ()
-- traceShowM = traceM . show

-- traceStack :: String -> a -> a
-- traceStack str expr = unsafePerformIO $ do
--     traceIO str
--     stack <- currentCallStack
--     unless (null stack) $ traceIO (renderStack stack)
--     return expr

asT :: Show a => a -> Text
asT = T.pack . show

type Log = Located SrcLoc Text -- String -- (T.Text, T.Text)

asRange :: SrcLoc -> Range
asRange l = Range (Position (srcLocStartLine l -1) (srcLocStartCol l -1)) (Position (srcLocEndLine l -1) (srcLocEndCol l -1))

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, Text))
codeModuleName state nfp =
    ((\(L (RealSrcSpan l) m) -> (realSrcSpanToRange l, T.pack . show $ m)) <$>)
        . join
        . (hsmodName . unLoc . pm_parsed_source <$>)
        <$> runAction "ModuleName.GetParsedModule" state (use GetParsedModule nfp)

-- | A source code change
data Action = Action {aUri :: Uri, aRange :: Range, aTitle :: Text, aCode :: Text} deriving (Show)

-- | Convert an Action to a CodeLens
asCodeLens :: Text -> Action -> CodeLens
asCodeLens cid act@Action{..} =
    CodeLens
        aRange
        (Just $ Command aTitle cid (Just (List [toJSON $ asEdit act])))
        Nothing

-- | Convert an Action to a CodeAction
asCodeAction :: Action -> CAResult
asCodeAction act@Action{..} =
    CACodeAction $
        CodeAction
            aTitle
            (Just CodeActionQuickFix)
            (Just $ List [])
            (Just $ asEdit act)
            Nothing

asEdit :: Action -> WorkspaceEdit
asEdit act@Action{..} =
    WorkspaceEdit (Just $ Map.singleton aUri $ List (asTextEdits act)) Nothing

asTextEdits :: Action -> [TextEdit]
asTextEdits Action{..} = [TextEdit aRange aCode]
