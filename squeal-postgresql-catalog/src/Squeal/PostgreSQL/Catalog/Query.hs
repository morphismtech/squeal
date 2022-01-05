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

selectComposites :: Query lat with DB params '[
  "nspname" ::: 'NotNull 'PGtext,
  "typname" ::: 'NotNull 'PGtext,
  "fields" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "nspname" ::: 'NotNull 'PGtext,
    "typname" ::: 'NotNull 'PGtext])))]
selectComposites =
  from
    ( table (#pg_catalog ! #pg_type `as` #t)
      & innerJoin (table (#pg_catalog ! #pg_class `as` #c))
        (#c ! #reltype .== #t ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_attribute `as` #a))
        (#a ! #atttypid .== #t ! #oid)
      & innerJoin (table (#pg_catalog ! #pg_namespace `as` #n))
        (#n ! #oid .== #t ! #typnamespace)
      & innerJoin (table (#pg_catalog ! #pg_type `as` #at))
        (#at ! #oid .== #a ! #atttypid)
      & innerJoin (table (#pg_catalog ! #pg_namespace `as` #an))
        (#an ! #oid .== #at ! #typnamespace)
    )
  & where_ (#t ! #typtype .== inline 'c')
  & where_ (#c ! #relkind .== inline 'c')
  & groupBy (#n ! #nspname :* #t ! #typname)
  & select_
    ( mapAliased (cast text) (#n ! #nspname) :*
      mapAliased (cast text) (#t ! #typname) :*
      fromNull (array0 record)
        ( arrayAgg
          ( All (row (
              mapAliased (cast text) (#a ! #attname) :*
              mapAliased (cast text) (#an ! #nspname) *:
              mapAliased (cast text) (#at ! #typname)
              ))
            & orderBy [Asc (#a ! #attnum)]
          )
        ) `as` #fields
    )
