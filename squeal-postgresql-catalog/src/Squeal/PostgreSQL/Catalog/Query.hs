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

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
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

data ObjType = ObjType
  { typobj :: ObjSchemum
  , typtype :: Char
  , typarray :: ObjArrayType
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjType

data ObjArrayType = ObjArrayType
  { arrtypnspname :: Maybe String
  , arrtypname :: Maybe String
  , arrtyptype :: Maybe Char
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjArrayType

data ObjAtt = ObjAtt
  { attname :: String
  , atthasdef :: Bool
  , typ :: ObjType
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjAtt

data ObjEnum = ObjEnum
  { enumobj :: ObjSchemum
  , enumlabels :: VarArray [String]
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjEnum

data ObjCols = ObjCols
  { colstbl :: ObjSchemum
  , attributes :: VarArray [ObjAtt]
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjCols

data ObjCon = ObjCon
  { conname :: String
  , contype :: Char
  , conkey_columns :: VarArray [String]
  , confrel_schema :: Maybe String
  , confrel_name :: Maybe String
  , confkey_columns :: Maybe (VarArray [String])
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjCon
deriving via VarArray [Composite ObjCon]
  instance IsPG [ObjCon]
deriving via VarArray [Composite ObjCon]
  instance Inline [ObjCon]
deriving via VarArray [Composite ObjCon]
  instance FromPG [ObjCon]

data ObjCons = ObjCons
  { constbl :: ObjSchemum
  , constraints :: [ObjCon]
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjCons

getEnums :: Maybe [ObjSchemum] -> Statement DB () ObjEnum
getEnums objsMaybe =
  let
    cond = case objsMaybe of
      Nothing -> true
      Just objs -> arrAny #enumobj (.==) (inline objs)
    sql
      = from (subquery (selectEnums `as` #enums))
      & where_ cond
      & select Star
  in
    query sql

selectEnums :: Query lat with DB params '[
  "enumobj" ::: NullPG ObjSchemum,
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
        ) `as` #enumobj :*
      fromNull (array0 text)
        ( arrayAgg
          ( All (cast text (#enum ! #enumlabel))
            & orderBy [Asc (#enum ! #enumsortorder)]
          ) ) `as` #enumlabels )

getColumns
  :: Maybe [ObjSchemum]
  -> Statement DB () ObjCols
getColumns objsMaybe =
  let
    cond = case objsMaybe of
      Nothing -> true
      Just objs -> arrAny #obj (.==) (inline objs)
    sql
      = from (subquery (selectRelations `as` #rels))
      & where_ (#relkind .== inline 'r')
      & where_ cond
      & select_
        ( #obj `as` #colstbl :*
          #attributes )
  in
    query sql

selectRelations :: Query lat with DB params '[
  "obj" ::: 'NotNull (PG ObjSchemum),
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "typ" ::: NullPG ObjType ])))]
selectRelations =
  from
    ( table (#pg_catalog ! #pg_namespace `as` #nsp)
      & innerJoin (table (#pg_catalog ! #pg_class `as` #rel))
        (#rel ! #relnamespace .== #nsp ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_attribute `as` #att))
        (#att ! #attrelid .== #rel ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_type `as` #typ))
        (#typ ! #oid .== #att ! #atttypid)
      & innerJoin (table (#pg_catalog ! #pg_namespace `as` #typnsp))
        (#typnsp ! #oid .== #typ ! #typnamespace)
      & leftOuterJoin (table (#pg_catalog ! #pg_type `as` #arrtyp))
        (#arrtyp ! #oid .== #typ ! #typarray)
      & leftOuterJoin (table (#pg_catalog ! #pg_namespace `as` #arrnsp))
        (#arrnsp ! #oid .== #arrtyp ! #typnamespace) )
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
              row
                ( row
                    ( mapAliased (cast text) (#typnsp ! #nspname) :*
                      cast text (#typ ! #typname) `as` #objname
                    ) `as` #typobj :*
                  #typ ! #typtype `as` #typtype :*
                  row
                    ( cast text (#arrnsp ! #nspname) `as` #arrtypnspname :*
                      cast text (#arrtyp ! #typname) `as` #arrtypname :*
                      #arrtyp ! #typtype `as` #arrtyptype
                    ) `as` #typarray
                ) `as` #typ
              ) )
            & orderBy [Asc (#att ! #attnum)]
          ) ) `as` #attributes )

getTableConstraints
  :: Maybe [ObjSchemum]
  -> Statement DB () ObjCons
getTableConstraints objsMaybe =
  let
    cond = case objsMaybe of
      Nothing -> true
      Just objs -> arrAny #tbl (.==) (inline objs)
    sql
      = from (subquery (selectTableConstraints `as` #cons))
      & where_ cond
      & groupBy #tbl
      & select_
        ( #tbl `as` #constbl :*
          fromNull (array0 record) (arrayAgg (All (row (
            #conname :*
            #contype :*
            #conkey_columns :*
            #confrel_schema :*
            #confrel_name :*
            #confkey_columns )))) `as` #constraints )
  in
    query sql

selectTableConstraints :: Query lat with DB params '[
  "tbl" ::: NullPG ObjSchemum,
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
    ( row
        ( mapAliased (cast text) (#nsp ! #nspname) :*
          cast text (#rel ! #relname) `as` #objname
        ) `as` #tbl :*
      mapAliased (cast text) (#con ! #conname) :*
      #con ! #contype :*
      fromNull (array0 text)
        (arrayAgg (All (cast text (#att ! #attname))))
        `as` #conkey_columns :*
      cast text (#confrel_nsp ! #nspname) `as` #confrel_schema :*
      cast text (#confrel ! #relname) `as` #confrel_name :*
      arrayAgg (All (cast text (#confatt ! #attname)))
        `as` #confkey_columns )
