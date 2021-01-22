module Squeal.PostgreSQL.Plugin
  ( plugin
  ) where

-- import GhcPlugins
import Class (Class(className, classTyCon))
import FastString (mkFastString)
import GHC.TcPluginM.Extra (lookupModule, lookupName)
import MkCore (mkCoreConApps)
import Module (mkModuleName)
import Name (Name, mkTcOcc)
import Plugins (Plugin(pluginRecompile, tcPlugin), defaultPlugin, purePlugin)
import TcEvidence (EvTerm(EvExpr))
import TcPluginM (TcPluginM)
import TcRnTypes (Ct(CDictCan), TcPlugin(TcPlugin, tcPluginInit, tcPluginSolve, tcPluginStop) , TcPluginResult(TcPluginOk)
  )
import TyCon (tyConDataCons_maybe)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just $ TcPlugin
    { tcPluginInit  = pluginInit
    , tcPluginSolve = pluginSolve
    , tcPluginStop  = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }

pluginInit :: TcPluginM Name
pluginInit = do
  aliasModule <- lookupModule
    (mkModuleName "Squeal.PostgreSQL.Type.Alias")
    (mkFastString "squeal-postgresql")
  lookupName aliasModule (mkTcOcc "HasErr")

pluginSolve :: Name -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve hasErrName _ _ = go
  where
    go :: [Ct] -> TcPluginM TcPluginResult
    go [] = do
      pure $ TcPluginOk [] []
    go (ct@(CDictCan _ class_ _ _) : _) | className class_ == hasErrName = do
      let hasErrClass = class_
          dictTyCon = classTyCon hasErrClass
          Just [dictDataCon] = tyConDataCons_maybe dictTyCon
      pure $ TcPluginOk [(EvExpr $ mkCoreConApps dictDataCon [], ct)] []
    go (_ : cts) = do
      go cts
