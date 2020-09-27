{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wwarn #-}

-- |GHC API utilities
module Ide.Plugin.Eval.GHC
  ( isExpr,
    addExtension,
    addImport,
    hasPackage,
    addPackages,
    gStrictTry,
  )
where
import           Control.Exception           (SomeException)
import           Data.List                   (isPrefixOf)
import           DynFlags                    (DynFlags (importPaths, includePaths))
import qualified EnumSet
import           Exception                   (ExceptionMonad)
import           GHC                         (DynFlags (extensionFlags, extensions, packageFlags),
                                              Ghc, GhcMonad,
                                              InteractiveImport (IIDecl),
                                              gcatch, getContext,
                                              getSessionDynFlags,
                                              parseImportDecl, setContext,
                                              setSessionDynFlags)
import           GHC                         (DynFlags (packageEnv), Module,
                                              ModuleName)
import           GHC.LanguageExtensions.Type (Extension (..))
import           GhcMonad                    (modifySession)
import           GhcPlugins                  (DefUnitId (DefUnitId),
                                              HscEnv (hsc_IC),
                                              InstalledUnitId (InstalledUnitId),
                                              ModRenaming (ModRenaming),
                                              PackageArg (PackageArg, UnitIdArg),
                                              PackageFlag (ExposePackage),
                                              fsLit, xopt_set)
import           GhcPlugins                  (ComponentId, PackageName,
                                              SourcePackageId)
import           HscTypes                    (InteractiveContext (ic_dflags))
import           Ide.Plugin.Eval.Debug       (dbgO, asS)
import qualified Lexer                       as Lexer
import           Module                      (UnitId (DefiniteUnitId))
import           Outputable                  (Outputable (ppr), SDoc,
                                              showSDocUnsafe, text, vcat, (<+>))
import qualified Parser                      as Parser
import           SrcLoc                      (mkRealSrcLoc)
import           StringBuffer                (stringToStringBuffer)

-- "/Users/titto/.stack/programs/x86_64-osx/ghc-8.6.5/lib/ghc-8.6.5"
-- "/Users/titto/.stack/snapshots/x86_64-osx/65b7f0730eb8d5b5606f86e169ee82d145a1a9bf9b50f15c61c3fb503534bb9c/8.6.5/"

-- $setup
-- >>> import GHC
-- >>> -- libdir = "/usr/local/lib/ghc-8.6.5"
-- >>> -- libdir = "/Users/titto/.stack/snapshots/x86_64-osx/65b7f0730eb8d5b5606f86e169ee82d145a1a9bf9b50f15c61c3fb503534bb9c/8.6.5"
-- >>> -- libdir = "/Users/titto/.stack/snapshots/x86_64-osx/006ed83b78b71386c9343a9748834df5f31b7b2dbd0ee5c5e0f22a740c48781f/8.6.5/lib/x86_64-osx-ghc-8.6.5/"
-- >>> import GHC.Paths
-- >>> run act = runGhc (Just libdir) (getSessionDynFlags >>= act)
-- >>> libdir
-- "/Users/titto/.stack/programs/x86_64-osx/ghc-8.6.5/lib/ghc-8.6.5"

{-| Returns true if string is an expression

>>> isExprTst e df = return (isExpr df e)
>>> run $ isExprTst "3"
True

>>> run $ isExprTst "(x+y)"
True

>>> run $ isExprTst "import Data.Maybe"
False

>>> run $ isExprTst "three=3"
False
-}
isExpr :: DynFlags -> String -> Bool
isExpr df stmt = case parseThing Parser.parseExpression df stmt of
  Lexer.POk _ _    -> True
  Lexer.PFailed {} -> False

parseThing :: Lexer.P thing -> DynFlags -> String -> Lexer.ParseResult thing
parseThing parser dflags stmt = do
  let buf = stringToStringBuffer stmt
      loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

  Lexer.unP parser (Lexer.mkPState dflags buf loc)

{- | True if specified package is present in DynFlags
-- >>> hasPackageTst pkg = run $ \df -> return (hasPackage df pkg)
>>> hasPackageTst pkg = run $ \df -> addPackages df [pkg] >>= return . either Left (\df -> Right (hasPackage df pkg))

>>> hasPackageTst "base"
Right True

>>> hasPackageTst "ghc"
Right True

>>> hasPackageTst "QuickCheck"
Left "<command line>: cannot satisfy -package QuickCheck\n    (use -v for more information)"
-}
hasPackage :: DynFlags -> String -> Bool
hasPackage df name = any (\pkg -> case pkg of
  ExposePackage _ (PackageArg n) _ | name `isPrefixOf` n -> True
  ExposePackage _ (UnitIdArg (DefiniteUnitId (DefUnitId (InstalledUnitId n)))) _ | name `isPrefixOf` asS n  -> True
  _                                                      -> False) $ packageFlags df

{- | Expose a list of packages
>>> addPackagesTest pkgs = run (\df -> (packageFlags <$>) <$> addPackages df pkgs)

>>> addPackagesTest []
Right []

>>> addPackagesTest ["base","array"]
Right [-package base{package base True ([])},-package array{package array True ([])}]

>>> addPackagesTest ["QuickCheck"]
Left "<command line>: cannot satisfy -package QuickCheck\n    (use -v for more information)"

>>> addPackagesTest ["notThere"]
Left "<command line>: cannot satisfy -package notThere\n    (use -v for more information)"
-}
addPackages :: DynFlags -> [String] -> Ghc (Either String DynFlags)
addPackages df pkgNames = gStrictTry $ do
    _ <- setSessionDynFlags $ df
      { packageFlags = map expose pkgNames ++ packageFlags df
         }
    dflags <- getSessionDynFlags
    --(dflags,_preloadedUnits) <- liftIO $ initPackages dflags
    return dflags

  where
    expose name = ExposePackage ("-package "++name) (PackageArg name) (ModRenaming True []) -- -package-id filepath-1.4.2.1

{- Add import to evaluation context

>>> run $ \_ -> addImport "import Data.Maybe"
Could not find module ‘Data.Maybe’
Use -v to see a list of the files searched for.

>>> run $ \df -> addPackages df ["base"] >> addImport "import Data.Maybe"
[import Data.Maybe]

>>> run $ \df -> addPackages df ["base"] >> addImport "import qualified Data.Maybe as M"
[import qualified Data.Maybe as M]

-}
addImport :: GhcMonad m => String -> m [InteractiveImport]
addImport i = do
  ctx <- getContext
  dbgO "CONTEXT" ctx
  idecl <- parseImportDecl i
  setContext $ IIDecl idecl : ctx
  -- ctx' <- getContext
  -- dbg "CONTEXT'" ctx'
  getContext

-- TODO: not working for source files in the same project as the module being tested
-- addImport :: GhcMonad m => String -> m ()
-- addImport i = do
--   ctx <- getContext
--   dbgO "CONTEXT" ctx
--   idecl <- parseImportDecl i
--   let L _  moduleName = ideclName idecl
--   -- let mdlName = moduleNameString moduleName
--   -- dbg "MDLNAME" mdlName
--   isLoaded moduleName >>= dbgO "ISLOADED"
--   -- dbg "MDLNAME2" mdlName
--   -- getModSummary moduleName >>= dbgO "SUMMARY"

--   -- mdl <- lookupModule moduleName Nothing
--   -- dbgO "MDL" mdl
--   -- isInterpreted <- moduleIsInterpreted mdl
--   -- dbg "INTERPRETED" isInterpreted

--   -- when isInterpreted $ do
--   --   target <- guessTarget mdlName Nothing
--   --   addTarget target
--   --   loaded <- load LoadAllTargets
--   --   dbg "LOADED" (asS loaded)
--   --   -- case r of
--   --   --    GHC.Failed -> error $ "Failed compilation of " ++ mdlName
--   --   --    GHC.Succeeded -> dbg "LOADED" $ "Compiled " ++ mdlName
--   --           -- let m = mkModuleName m
--   dbgO "ADD IMPORT" idecl
--   setContext $ IIDecl idecl : ctx
--   ctx' <- getContext
--   dbgO "CONTEXT'" ctx'

{- Add extension to interactive evaluation session
>>> import GHC.LanguageExtensions.Type(Extension(..))
>>> run $ \_ -> addExtension DeriveGeneric
()
-}

addExtension :: GhcMonad m => Extension -> m ()
addExtension ext =
  modifySession $ \hsc -> hsc {hsc_IC = setExtension (hsc_IC hsc) ext}

setExtension :: InteractiveContext -> Extension -> InteractiveContext
setExtension ic ext = ic {ic_dflags = xopt_set (ic_dflags ic) ext}

deriving instance Read Extension

-- Partial display of DynFlags contents, for testing purposes
instance Show DynFlags where
    show df = showSDocUnsafe . vcat . map (\(n,d) -> text (n ++ ": ") <+> d) $ [
      ("extensions",ppr . extensions $ df)
      ,("extensionFlags",ppr . EnumSet.toList . extensionFlags $ df)
      ,("importPaths", vList $ importPaths df)
      ,("includePaths",text . show $ includePaths df)
      ,("packageEnv",ppr $ packageEnv df)
      ,("packageFlags",vcat . map ppr $ packageFlags df)
      -- ,("pkgDatabase",(map) (ppr . installedPackageId) . pkgDatabase $ df)
      -- ,("pkgDatabase",text . show <$> pkgDatabase $ df)
      ]

  -- where
  --    showInstalledPkgInfo dfs =
--       Out.showSDoc dfs . Out.ppr . installedPackageId

instance Show PackageFlag where show = asS

instance Show InteractiveImport where show = asS

instance Show InstalledUnitId  where show = asS

instance Show ComponentId  where show = asS

instance Show PackageName  where show = asS

instance Show ModuleName  where show = asS

instance Show Module  where show = asS

instance Show SourcePackageId  where show = asS

-- hList :: [String] -> SDoc
-- hList = hsep . map text

vList :: [String] -> SDoc
vList = vcat . map text

gStrictTry :: ExceptionMonad m => m b -> m (Either String b)
gStrictTry op =
  gcatch
    -- gStrictTry op = MC.catch
    (op >>= \v -> return $! Right $! v)
    (\(err :: SomeException) -> return $! Left $! show $! err)

-- strictTry :: NFData b => IO b -> IO (Either String b)
-- strictTry op = E.catch
--   (op >>= \v -> return $! Right $! deepseq v v)
--   (\(err :: E.SomeException) -> return $! Left $ show err)
