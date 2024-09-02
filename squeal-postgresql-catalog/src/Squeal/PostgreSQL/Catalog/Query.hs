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

module Squeal.PostgreSQL.Catalog.Query where

import Control.Monad
import Data.Foldable
import Data.Int
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Language.Haskell.TH.Syntax hiding (Inline)
import Squeal.PostgreSQL
import Squeal.PostgreSQL.Catalog

getDB
  :: Maybe [ObjSchemum] -- filter by qualified name
  -> (ObjSchemum -> String) -- format by qualified name
  -> Transaction DB [Dec]
getDB fltr frmt = do
  enums <- getRows =<< execute (objEnumTy <$> getEnums fltr)
  tables <- getRows =<< execute (getTables fltr)
  let
    mkqName str qname = mkName (str <> "_" <> frmt qname)
    enumDecls =
      [ TySynD (mkqName "Enum" qname) [] enumTy
      | (qname, enumTy) <- enums
      ]
    tableDecls = join
      [ [ TySynD (mkqName "Table" qname) [] $ InfixT
            (ConT (mkqName "Constraints" qname))
            (mkName ":=>")
            (ConT (mkqName "Columns" qname))
        , TySynD (mkqName "Columns" qname) [] $ promotedListT
            [objAttTy frmt obj | obj <- objAtts]
        , TySynD (mkqName "Constraints" qname) [] $ promotedListT
            [objConTy obj | obj <- objCons]
        ]
      | ObjTbl qname objCons objAtts <- tables
      ]
  return (enumDecls <> tableDecls)

data ObjSchemas = ObjSchemas
  { schemas :: [String]
  , schEnums :: [ObjEnum]
  , schTbls :: [ObjTbl]
  }

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

data ObjEnum = ObjEnum
  { enumobj :: ObjSchemum
  , enumlabels :: VarArray [String]
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjEnum
objEnumTy :: ObjEnum -> (ObjSchemum, Type)
objEnumTy ObjEnum{..} =
  let
    enumlabelsT = promotedListT
      [ LitT (StrTyLit lbl)
      | lbl <- getVarArray enumlabels
      ]
  in
    (enumobj, PromotedT (mkName "PGenum") `AppT` enumlabelsT)

data ObjTbl = ObjTbl
  { tblobj :: ObjSchemum
  , tblcons :: [ObjCon]
  , tblcols :: [ObjAtt]
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjTbl

data ObjType = ObjType
  { typobj :: ObjSchemum
  , typtype :: Char
  , typcategory :: Char
  , typlen :: Int16
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjType
objTypeTy :: (ObjSchemum -> String) -> ObjType -> Type
objTypeTy frmt ObjType{..} =
  if | typtype == 'b' && typcategory /= 'A' ->
       PromotedT (mkName ("PG" <> objname typobj))
     | typtype == 'e' && typcategory /= 'A' ->
       ConT (mkName ("Enum_" <> frmt typobj))
     | otherwise ->
       errorT "objTypeTy case not implemented."

data ObjAtt = ObjAtt
  { attname :: String
  , atthasdef :: Bool
  , typ :: ObjType
  } deriving stock GHC.Generic
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving (IsPG, Inline, FromPG) via Composite ObjAtt
deriving via VarArray [Composite ObjAtt]
  instance IsPG [ObjAtt]
deriving via VarArray [Composite ObjAtt]
  instance Inline [ObjAtt]
deriving via VarArray [Composite ObjAtt]
  instance FromPG [ObjAtt]
objAttTy :: (ObjSchemum -> String) -> ObjAtt -> Type
objAttTy frmt ObjAtt{..} =
  let
    def = PromotedT (mkName (if atthasdef then "Def" else "NoDef"))
  in
    attname `fieldT` (InfixT def (mkName ":=>") (objTypeTy frmt typ))

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
objConTy :: ObjCon -> Type
objConTy ObjCon{..} =
  let
    conkey_columnsT = promotedListT
      [ LitT (StrTyLit str)
      | str <- getVarArray conkey_columns
      ]
    confrel_err = mconcat
      [ "ForeignKey "
      , conname
      , " expected reference metadata."
      ]
    conty_err ty = mconcat
      [ "Constraint "
      , conname
      , " has unsupported type \'"
      , [ty]
      , "\'."
      ]
    confrel_schemaT = case confrel_schema of
      Nothing -> errorT confrel_err
      Just refsch -> LitT (StrTyLit refsch)
    confrel_nameT = case confrel_name of
      Nothing -> errorT confrel_err
      Just reftbl -> LitT (StrTyLit reftbl)
    confkey_columnsT = case confkey_columns of
      Nothing -> errorT confrel_err
      Just (VarArray refcols) ->
        promotedListT [LitT (StrTyLit refcol) | refcol <- refcols]
    conTy = case contype of
      'c' -> PromotedT (mkName "Check") `AppT` conkey_columnsT
      'u' -> PromotedT (mkName "Unique") `AppT` conkey_columnsT
      'p' -> PromotedT (mkName "PrimaryKey") `AppT` conkey_columnsT
      'f' -> PromotedT (mkName "ForeignKey") `AppT` conkey_columnsT
        `AppT` confrel_schemaT
        `AppT` confrel_nameT
        `AppT` confkey_columnsT
      ty -> errorT (conty_err ty)
  in
    conname `fieldT` conTy

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

getTables :: Maybe [ObjSchemum] -> Statement DB () ObjTbl
getTables objsMaybe =
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
    selectTbls
      = from
        ( subquery (selectCons `as` #cons)
          & innerJoin (subquery (selectCols `as` #cols))
            (#cons ! #obj .== #cols ! #obj) )
      & where_ ( case objsMaybe of
          Nothing -> true
          Just objs -> arrAny (#cons ! #obj) (.==) (inline objs) )
      & select_
        ( #cons ! #obj `as` #tblobj :*
          #cons ! #constraints `as` #tblcons :*
          #cols ! #attributes `as` #tblcols )
  in
    query selectTbls

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
  "obj" ::: NullPG ObjSchemum,
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

promotedListT :: [Type] -> Type
promotedListT =
  foldl' (\tyL tyR -> PromotedConsT `AppT` tyL `AppT` tyR) PromotedNilT

errorT :: String -> Type
errorT msg = ConT (mkName "TypeError") `AppT`
  (ParensT (PromotedT (mkName "Text") `AppT` LitT (StrTyLit msg)))

fieldT :: String -> Type -> Type
fieldT str ty = InfixT (LitT (StrTyLit str)) (mkName ":::") ty
