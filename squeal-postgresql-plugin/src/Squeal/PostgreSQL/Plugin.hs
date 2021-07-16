{-# OPTIONS_GHC -Wwarn #-}
{-# Language ViewPatterns #-}
module Squeal.PostgreSQL.Plugin
  ( plugin
  ) where

import GhcPlugins (Type, eqType, splitTyConApp_maybe)
import Data.Bifunctor (first)
import Control.Monad (guard)
import Data.Function (on)
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
import TcRnTypes (TcPlugin(TcPlugin, tcPluginInit, tcPluginSolve, tcPluginStop) , TcPluginResult(TcPluginOk)
  )
import TysWiredIn (promotedConsDataCon, promotedNilDataCon, promotedTupleDataCon)
import Constraint (Ct(CDictCan))
import TyCon (tyConDataCons_maybe)
import BasicTypes (Boxity(Boxed))

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
          [_keyKind, _valueKind, needle, haystackT, _result] = tyArgs
          returnType = do
            list <- typeToMList haystackT
            listTuples <- traverse typeToMTuple list
            findType needle listTuples
      tcPluginIO $ print ("tyArgs", showSDocUnsafe $ ppr tyArgs)
      tcPluginIO $ print ("haystack list args", showSDocUnsafe . ppr $ typeToMList haystackT)
      tcPluginIO $ print ("haystack list arg tuples", showSDocUnsafe . ppr $ (typeToMList haystackT >>= traverse typeToMTuple))
      tcPluginIO $ print ("our x", showSDocUnsafe . ppr $ returnType)
      -- next steps:
      --   only return non-empty list to TcPluginOk if we have a Just for returnType
      --   and if we do, also return a second list showing that result ~ returnType
      pure $ TcPluginOk [(EvExpr $ mkCoreConApps dictDataCon [], ct)] []
    go (_ : cts) = do
      go cts
    typeToMList :: Type -> Maybe [Type]
    typeToMList ty = do
      (con, args) <- splitTyConApp_maybe ty
      case args of
        [_kind, h, t] | con == promotedConsDataCon ->
          (h :) <$> typeToMList t
        [_kind] | con == promotedNilDataCon ->
          pure []
        _ -> Nothing
    typeToMTuple :: Type -> Maybe (Type, Type)
    typeToMTuple ty = do
      (con, [_kindl, _kindr, l, r]) <- splitTyConApp_maybe ty
      guard (con == promotedTupleDataCon Boxed 2)
      pure (l, r)
    findType :: Type -> [(Type, Type)] -> Maybe Type
    findType (TypeEq -> needle) (fmap (first TypeEq) -> haystack) =
      lookup needle haystack

newtype TypeEq = TypeEq
  { unTypeEq :: Type
  }

instance Eq TypeEq where
  (==) = eqType `on` unTypeEq
