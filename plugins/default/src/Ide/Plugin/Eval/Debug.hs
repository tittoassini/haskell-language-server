-- |Debug utilities
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ide.Plugin.Eval.Debug
  ( pPrint,
    pShowNoColor,
    dbg,
    dbgO,
    dbg_,
    asS,
  )
where

-- import           GHC.Hs.Doc              (hsDocStringToByteString)
-- import           GHC.Hs.Decls             (DocDecl (..))
--import           HsSyn              (DocDecl (..), hsDocStringToByteString)
import           MonadUtils         (MonadIO)
import           Outputable         (Outputable, ppr, showSDocUnsafe)
import           Text.Pretty.Simple (pPrint,
                                     pShowNoColor,pPrintNoColor)

-- instance Show DocDecl where
--   show (DocCommentNext hsDocStr) =
--     unwords ["DocCommentNext", show $ hsDocStringToByteString hsDocStr]
--   show (DocCommentPrev hsDocStr) =
--     unwords ["DocCommentPrev", show $ hsDocStringToByteString hsDocStr]
--   show (DocCommentNamed name hsDocStr) =
--     unwords ["DocCommentNamed", name, show $ hsDocStringToByteString hsDocStr]
--   show (DocGroup n hsDocStr) =
--     unwords ["DocGroup", show n, show $ hsDocStringToByteString hsDocStr]

dbgO :: (MonadIO m, Show a1, Outputable a) => a1 -> a -> m ()
dbgO s o = dbg s (asS o)

dbg :: (MonadIO m, Show a1, Show a2) => a1 -> a2 -> m ()
dbg s o = dbg_ s >> dbg_ o

dbg_ :: (MonadIO m, Show a) => a -> m ()
-- dbg_ a | dbgOn = pPrintOpt defaultOutputOptionsNoColor a
dbg_ a | dbgOn = pPrintNoColor a
       | otherwise = return ()

dbgOn :: Bool
dbgOn = False

asS :: Outputable a => a -> String
asS = showSDocUnsafe . ppr

