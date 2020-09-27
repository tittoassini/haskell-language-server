{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{- |
A plugin inspired by:

* the REPLoid feature of <https://github.com/jyp/dante Dante>

* <https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744 Haddock>'s Examples and Properties

* <https://hackage.haskell.org/package/doctest Doctest>

See the "Ide.Plugin.Eval.Tutorial" module for a full introduction to the plugin functionality.
-}
module Ide.Plugin.Eval
  ( descriptor
  )
where

import qualified Ide.Plugin.Eval.CodeLens as CL
import           Ide.Types                (PluginDescriptor (pluginCodeLensProvider, pluginCommands, pluginId),
                                           PluginId, defaultPluginDescriptor)

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginId = plId,
       pluginCodeLensProvider = Just CL.codeLens,
       pluginCommands = [CL.evalCommand]
    }
