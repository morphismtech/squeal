{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DerivingVia
  , FlexibleInstances
  , OverloadedLabels
  , PolyKinds
  , StandaloneDeriving
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Catalog.Query where

import Data.Foldable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.TH.Syntax hiding (Inline)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

data ObjSchemum = ObjSchemum
  { nspname :: String
  , objname :: String
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjSchemum
deriving via VarArray [Composite ObjSchemum]
  instance IsPG [ObjSchemum]
deriving via VarArray [Composite ObjSchemum]
  instance Inline [ObjSchemum]

getEnums :: [ObjSchemum] -> Statement DB () (ObjSchemum, Type)
getEnums objs =
  let
    cond = case objs of
      [] -> true
      _ -> arrAny #obj (.==) (inline objs)
    sql
      = from (subquery (selectEnums `as` #enums))
      & where_ cond
      & select Star
    enc = nilParams
    dec = do
      obj <- #obj
      VarArray enumlabels <- #enumlabels
      let
        consLabel ty str = AppT
          (AppT PromotedConsT (LitT (StrTyLit str))) ty
        labels = foldl' consLabel PromotedNilT (enumlabels :: [String])
        enumType = AppT
          (PromotedT (mkName "Typedef"))
          (AppT (PromotedT (mkName "PGenum")) labels)
      return (obj, enumType)
  in
    Query enc dec sql

selectEnums :: Query lat with DB params '[
  "obj" ::: NullPG ObjSchemum,
  "enumlabels" ::: 'NotNull ('PGvararray ('NotNull 'PGtext))]
selectEnums =
  from
    ( table (#pg_catalog ! #pg_namespace `as` #nsp)
      & innerJoin (table (#pg_catalog ! #pg_type `as` #typ))
        (#typ ! #typnamespace .== #nsp ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_enum `as` #enum))
        (#enum ! #enumtypid .== #typ ! #oid) )
  & groupBy (#nsp ! #nspname :* #typ ! #typname)
  & select_
    ( row
        ( mapAliased (cast text) (#nsp ! #nspname) :*
          cast text (#typ ! #typname) `as` #objname
        ) `as` #obj :*
      fromNull (array0 text)
        ( arrayAgg
          ( All (cast text (#enum ! #enumlabel))
            & orderBy [Asc (#enum ! #enumsortorder)]
          ) ) `as` #enumlabels )

selectRelations :: Query lat with DB params '[
  "obj" ::: 'NotNull (PG ObjSchemum),
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "typname" ::: 'NotNull 'PGtext,
    "typtype" ::: 'NotNull ('PGchar 1)])))]
selectRelations =
  from
    ( table (#pg_catalog ! #pg_namespace `as` #nsp)
      & innerJoin (table (#pg_catalog ! #pg_class `as` #rel))
        (#rel ! #relnamespace .== #nsp ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_attribute `as` #att))
        (#att ! #attrelid .== #rel ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_type `as` #atttyp))
        (#atttyp ! #oid .== #att ! #atttypid)
      & innerJoin (table (#pg_catalog ! #pg_namespace `as` #attnsp))
        (#attnsp ! #oid .== #atttyp ! #typnamespace) )
  & groupBy (#nsp ! #nspname :* #rel ! #relname :* #rel ! #relkind)
  & select_
    ( row
        ( mapAliased (cast text) (#nsp ! #nspname) :*
          cast text (#rel ! #relname) `as` #objname
        ) `as` #obj :*
      #rel ! #relkind :*
      fromNull (array0 record)
        ( arrayAgg
          ( All (row (
              mapAliased (cast text) (#att ! #attname) :*
              #att ! #atthasdef :*
              mapAliased (cast text) (#atttyp ! #typname) :*
              #atttyp ! #typtype ))
            & orderBy [Asc (#att ! #attnum)]
          ) ) `as` #attributes )

selectTableConstraints :: Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "relname" ::: 'NotNull 'PGtext,
  "conname" ::: 'NotNull 'PGtext,
  "contype" ::: 'NotNull ('PGchar 1),
  "conkey_columns" ::: 'NotNull ('PGvararray ('NotNull 'PGtext)),
  "confrel_schema" ::: 'Null 'PGtext,
  "confrel_name" ::: 'Null 'PGtext,
  "confkey_columns" ::: 'Null ('PGvararray ('NotNull 'PGtext))]
selectTableConstraints =
  from
    ( table (#pg_catalog ! #pg_namespace `as` #nsp)
      & innerJoin (table (#pg_catalog ! #pg_class `as` #rel))
        (#rel ! #relnamespace .== #nsp ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_constraint `as` #con))
        (#con ! #conrelid .== #rel ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_attribute `as` #att))
        ( #att ! #attrelid .== #rel ! #oid .&&
          arrAny (#att ! #attnum) (.==) (#con ! #conkey) )
      & leftOuterJoin (table (#pg_catalog ! #pg_class `as` #confrel))
        (#confrel ! #oid .== #con ! #confrelid)
      & leftOuterJoin (table (#pg_catalog ! #pg_namespace `as` #confrel_nsp))
        (#confrel_nsp ! #oid .== #confrel ! #relnamespace)
      & leftOuterJoin (table (#pg_catalog ! #pg_attribute `as` #confatt))
        ( #confatt ! #attrelid .== #confrel ! #oid .&&
          arrAny (#confatt ! #attnum) (.==) (#con ! #confkey) ) )
  & groupBy
     ( #nsp ! #nspname :*
       #rel ! #relname :*
       #con ! #conname :*
       #con ! #contype :*
       #confrel_nsp ! #nspname :*
       #confrel ! #relname )
  & select_
    ( mapAliased (cast text) (#nsp ! #nspname) :*
      mapAliased (cast text) (#rel ! #relname) :*
      mapAliased (cast text) (#con ! #conname) :*
      #con ! #contype :*
      fromNull (array0 text)
        (arrayAgg (All (cast text (#att ! #attname))))
        `as` #conkey_columns :*

      cast text (#confrel_nsp ! #nspname) `as` #confrel_schema :*
      cast text (#confrel ! #relname) `as` #confrel_name :*
      arrayAgg (All (cast text (#confatt ! #attname)))
        `as` #confkey_columns )
