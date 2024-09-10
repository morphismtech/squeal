{-# LANGUAGE
    DataKinds
  , DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  , DerivingVia
  , FlexibleContexts
  , FlexibleInstances
  , MonoLocalBinds
  , MultiWayIf
  , OverloadedLabels
  , PolyKinds
  , RankNTypes
  , RecordWildCards
  , StandaloneDeriving
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Catalog.Query2 where

import Control.Monad
import Data.Foldable
import Data.Int
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.TH.Syntax hiding (Inline)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

type ObjSchemum = 'PGcomposite '[
  "nspname" ::: 'NotNull 'PGtext,
  "objname" ::: 'NotNull 'PGtext]

selectEnums :: Query lat with DB params '[
  "enumobj" ::: 'NotNull ObjSchemum,
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

selectTables :: Query '[] with DB params '[
  "tblobj" ::: 'NotNull ObjSchemum,
  "tblcons" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "conname" ::: 'NotNull 'PGtext,
    "contype" ::: 'NotNull ('PGchar 1),
    "conkey_columns" ::: 'NotNull ('PGvararray ('NotNull 'PGtext)),
    "confrel_schema" ::: 'Null 'PGtext,
    "confrel_name" ::: 'Null 'PGtext,
    "confkey_columns" ::: 'Null ('PGvararray ('NotNull 'PGtext))]))),
  "tblcols" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull PGtext,
    "atthasdef" ::: 'NotNull PGbool,
     "typ" ::: 'NotNull ('PGcomposite '[
      "typobj" ::: 'NotNull ObjSchemum,
      "typtype" ::: 'NotNull ('PGchar 1),
      "typcategory" ::: 'NotNull ('PGchar 1),
      "typlen" ::: 'NotNull 'PGint2])])))]
selectTables =
  let
    selectCols
      = from (subquery (selectRelations `as` #rels))
      & where_ (#relkind .== inline 'r')
      & select_
        ( #obj :*
          #attributes )
    selectCons
      = from (subquery (selectTableConstraints `as` #cons))
      & groupBy #obj
      & select_
        ( #obj :*
          fromNull (array0 record) (arrayAgg (All (row (
            #conname :*
            #contype :*
            #conkey_columns :*
            #confrel_schema :*
            #confrel_name :*
            #confkey_columns )))) `as` #constraints )
  in
    from
      ( subquery (selectCons `as` #cons)
        & innerJoin (subquery (selectCols `as` #cols))
          (#cons ! #obj .== #cols ! #obj) )
    & select_
      ( #cons ! #obj `as` #tblobj :*
        #cons ! #constraints `as` #tblcons :*
        #cols ! #attributes `as` #tblcols )

selectRelations :: Query lat with DB params '[
  "obj" ::: 'NotNull ObjSchemum,
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "typ" ::: 'NotNull ('PGcomposite '[
      "typobj" ::: 'NotNull ObjSchemum,
      "typtype" ::: 'NotNull ('PGchar 1),
      "typcategory" ::: 'NotNull ('PGchar 1),
      "typlen" ::: 'NotNull 'PGint2])])))]
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
      & leftOuterJoin (table (#pg_catalog ! #pg_type `as` #arr))
        (#arr ! #typarray .== #typ ! #oid)
    )
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
                  #typ ! #typtype :*
                  #typ ! #typcategory :*
                  #typ ! #typlen
                ) `as` #typ
              ) )
            & orderBy [Asc (#att ! #attnum)]
          ) ) `as` #attributes )

selectTableConstraints :: Query lat with DB params '[
  "obj" ::: 'NotNull ObjSchemum,
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
        ) `as` #obj :*
      mapAliased (cast text) (#con ! #conname) :*
      #con ! #contype :*
      fromNull (array0 text)
        (arrayAgg (All (cast text (#att ! #attname))))
        `as` #conkey_columns :*
      cast text (#confrel_nsp ! #nspname) `as` #confrel_schema :*
      cast text (#confrel ! #relname) `as` #confrel_name :*
      arrayAgg (All (cast text (#confatt ! #attname)))
        `as` #confkey_columns )
