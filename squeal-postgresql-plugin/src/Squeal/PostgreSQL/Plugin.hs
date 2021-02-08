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
import Outputable (showSDocUnsafe, ppr)
import Plugins (Plugin(pluginRecompile, tcPlugin), defaultPlugin, purePlugin)
import TcEvidence (EvTerm(EvExpr))
import TcPluginM (TcPluginM, tcPluginIO)
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
    (mkModuleName "Squeal.PostgreSQL.Has")
    (mkFastString "squeal-postgresql-plugin")
  lookupName aliasModule (mkTcOcc "Has")

-- printf-debugging:
--import TcPluginM (tcPluginIO)
--import Outputable
----tcPluginIO $ print ("foo", showSDocUnsafe $ ppr foo)

pluginSolve :: Name -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
pluginSolve hasName _ _ = go
  where
    go :: [Ct] -> TcPluginM TcPluginResult
    go [] = do
      pure $ TcPluginOk [] []
    go (ct@(CDictCan _ class_ tyArgs _) : _) | className class_ == hasName = do
      let hasErrClass = class_
          dictTyCon = classTyCon hasErrClass
          Just [dictDataCon] = tyConDataCons_maybe dictTyCon
      tcPluginIO $ print ("tyArgs", showSDocUnsafe $ ppr tyArgs)
      pure $ TcPluginOk [(EvExpr $ mkCoreConApps dictDataCon [], ct)] []
    go (_ : cts) = do
      go cts
