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
  , ScopedTypeVariables
  , StandaloneDeriving
  , TypeOperators
  , UndecidableInstances
#-}

module Squeal.PostgreSQL.Catalog.Query2 where

import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Int
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.Exts.Syntax
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

data SchObj = SchObj {nspname :: String, objname :: String}
  deriving stock GHC.Generic
  deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
  deriving (IsPG, FromPG) via Composite SchObj

formatObj :: SchObj -> String
formatObj (SchObj "public" obj) = obj
formatObj (SchObj sch obj) = sch <> "_" <> obj

data SchCon = SchCon
  { conname :: String
  , contype :: Char
  , conkey_columns :: VarArray [String]
  , confrel_schema :: Maybe String
  , confrel_name :: Maybe String
  , confkey_columns :: Maybe (VarArray [String])
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite SchCon
deriving via VarArray [Composite SchCon]
  instance IsPG [SchCon]
deriving via VarArray [Composite SchCon]
  instance Inline [SchCon]
deriving via VarArray [Composite SchCon]
  instance FromPG [SchCon]

data SchType = SchType
  { typobj :: SchObj
  , typtype :: Char
  , typcategory :: Char
  , typlen :: Int16
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, FromPG) via Composite SchType

data SchAtt = SchAtt
  { attname :: String
  , atthasdef :: Bool
  , attnotnull :: Bool
  , typ :: SchType
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, FromPG) via Composite SchAtt
deriving via VarArray [Composite SchAtt]
  instance IsPG [SchAtt]
deriving via VarArray [Composite SchAtt]
  instance FromPG [SchAtt]

selectEnums :: Query lat with DB params '[
  "enumobj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
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
decodeEnum :: DecodeRow '[
  "enumobj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
  "enumlabels" ::: 'NotNull ('PGvararray ('NotNull 'PGtext))]
  (SchObj, Type ())
decodeEnum = do
  obj <- #enumobj
  VarArray lbls <- #enumlabels
  let
    tySymbol s = TyPromoted () (PromotedString () s s)
    tyList = TyPromoted () . PromotedList () False
    tyDataKind s = TyCon () (UnQual () (Ident () s))
    pgEnum = TyApp () (tyDataKind "PGenum")
    enumLbls = tyList [tySymbol lbl | lbl <- lbls]
  return (obj, pgEnum enumLbls)

selectTables :: Query '[] with DB params '[
  "tblobj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
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
    "attnotnull" ::: 'NotNull PGbool,
     "typ" ::: 'NotNull ('PGcomposite '[
      "typobj" ::: 'NotNull ('PGcomposite '[
        "nspname" ::: 'NotNull 'PGtext,
        "objname" ::: 'NotNull 'PGtext]),
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
decodeTable :: DecodeRow '[
  "tblobj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
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
    "attnotnull" ::: 'NotNull PGbool,
     "typ" ::: 'NotNull ('PGcomposite '[
      "typobj" ::: 'NotNull ('PGcomposite '[
        "nspname" ::: 'NotNull 'PGtext,
        "objname" ::: 'NotNull 'PGtext]),
      "typtype" ::: 'NotNull ('PGchar 1),
      "typcategory" ::: 'NotNull ('PGchar 1),
      "typlen" ::: 'NotNull 'PGint2])])))]
  (SchObj, Type (), Type ())
decodeTable = do
  undefined
  -- tbl <- #tblobj
  -- VarArray (cons :: [SchCon]) <- #tblcons
  -- VarArray (cols :: [SchAtt]) <- #tblcols
  -- let
  --   tySymbol s = TyPromoted () (PromotedString () s s)
  --   tyList = TyPromoted () . PromotedList () False
  --   tyDataKind s = TyCon () (UnQual () (Ident () s))
  --   tyInfix x s y = TyInfix () x (UnpromotedName () (UnQual () (Ident () s))) y
  --   -- pgEnum = TyApp () (tyDataKind "PGenum")
  --   -- enumLbls = tyList [tySymbol lbl | lbl <- lbls]
  -- columnTys <- for cols $ \SchAtt{..} -> do
  --   let
  --     SchType {..} = typ
  --     deflt = tyDataKind (if atthasdef then "Def" else "NoDef")
  --     nullty = TyApp () (tyDataKind (if attnotnull then "NotNull" else "Null"))
  --     column = tyInfix (tySymbol attname) ":::" (tyInfix deflt ":=>" nullty _) 
  --   return column
  -- return (tbl, _, cols)

selectRelations :: Query lat with DB params '[
  "obj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
  "relkind" ::: 'NotNull ('PGchar 1),
  "attributes" ::: 'NotNull ('PGvararray ('NotNull ('PGcomposite '[
    "attname" ::: 'NotNull 'PGtext,
    "atthasdef" ::: 'NotNull 'PGbool,
    "attnotnull" ::: 'NotNull 'PGbool,
    "typ" ::: 'NotNull ('PGcomposite '[
      "typobj" ::: 'NotNull ('PGcomposite '[
        "nspname" ::: 'NotNull 'PGtext,
        "objname" ::: 'NotNull 'PGtext]),
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
              #att ! #attnotnull :*
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
  "obj" ::: 'NotNull ('PGcomposite '[
    "nspname" ::: 'NotNull 'PGtext,
    "objname" ::: 'NotNull 'PGtext]),
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
