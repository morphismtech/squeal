{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , TypeOperators
#-}

module Squeal.PostgreSQL.Catalog.Query where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

selectEnums :: Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "typname" ::: 'NotNull 'PGtext,
  "enumlabels" ::: 'NotNull ('PGvararray ('NotNull 'PGtext))]
selectEnums =
  from
    ( table (#pg_catalog ! #pg_namespace `as` #nsp)
      & innerJoin (table (#pg_catalog ! #pg_type `as` #typ))
        (#typ ! #typnamespace .== #nsp ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_enum `as` #enum))
        (#enum ! #enumtypid .== #typ ! #oid)
    )
  & groupBy (#nsp ! #nspname :* #typ ! #typname)
  & select_
    ( mapAliased (cast text) (#nsp ! #nspname) :*
      mapAliased (cast text) (#typ ! #typname) :*
      fromNull (array0 text)
        ( arrayAgg
          ( All (cast text (#enum ! #enumlabel))
            & orderBy [Asc (#enum ! #enumsortorder)]
          )
        ) `as` #enumlabels
    )

selectRelations :: Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "relname" ::: 'NotNull 'PGtext,
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "typname" ::: 'NotNull 'PGtext])))]
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
        (#attnsp ! #oid .== #atttyp ! #typnamespace)
    )
  & groupBy (#nsp ! #nspname :* #rel ! #relname :* #rel ! #relkind)
  & select_
    ( mapAliased (cast text) (#nsp ! #nspname) :*
      mapAliased (cast text) (#rel ! #relname) :*
      #rel ! #relkind :*
      fromNull (array0 record)
        ( arrayAgg
          ( All (row (
              mapAliased (cast text) (#attnsp ! #nspname) :*
              mapAliased (cast text) (#att ! #attname) :*
              #att ! #atthasdef *:
              mapAliased (cast text) (#atttyp ! #typname)
              ))
            & orderBy [Asc (#att ! #attnum)]
          )
        ) `as` #attributes
    )

selectRelationsKind :: Char -> Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "relname" ::: 'NotNull 'PGtext,
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "typname" ::: 'NotNull 'PGtext])))]
selectRelationsKind c =
  from (subquery (selectRelations `as` #rel))
  & where_ (#rel ! #relkind .== inline c)
  & select Star

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
