{-# LANGUAGE
    DataKinds
  , OverloadedLabels
  , TypeOperators
#-}

module Squeal.PostgreSQL.Catalog.Query where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

selectEnums :: Query '[] '[] DB params '[
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
