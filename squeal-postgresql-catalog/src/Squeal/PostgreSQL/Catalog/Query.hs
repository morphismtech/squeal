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
