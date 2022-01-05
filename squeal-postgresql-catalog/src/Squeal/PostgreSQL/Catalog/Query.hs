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
selectEnums = select_ 
  ( mapAliased (cast text) (#n ! #nspname) :*
    mapAliased (cast text) (#t ! #typname) :*
    fromNull (array0 text)
      ( arrayAgg
        ( All (cast text (#e ! #enumlabel))
          & orderBy [Asc (#e ! #enumsortorder)]
        )
      ) `as` #enumlabels
  )
  ( from
    ( table (#pg_catalog ! #pg_type `as` #t)
      & innerJoin (table (#pg_catalog ! #pg_enum `as` #e))
        (#t ! #oid .== #e ! #enumtypid)
      & innerJoin (table (#pg_catalog ! #pg_namespace `as` #n))
        (#t ! #typnamespace  .== #n ! #oid)
    )
    & groupBy (#n ! #nspname :* #t ! #typname)
  )

selectRelations :: Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "relname" ::: 'NotNull 'PGtext,
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "nspname" ::: 'NotNull 'PGtext,
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
              mapAliased (cast text) (#att ! #attname) :*
              mapAliased (cast text) (#attnsp ! #nspname) *:
              mapAliased (cast text) (#atttyp ! #typname)
              ))
            & orderBy [Asc (#att ! #attnum)]
          )
        ) `as` #attributes
    )
