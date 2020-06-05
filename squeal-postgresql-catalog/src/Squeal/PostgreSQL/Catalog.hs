-- | This code was originally created by squealgen. Edit if you know how it got made and are willing to own it now.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Squeal.PostgreSQL.Catalog where
import Squeal.PostgreSQL
import GHC.TypeLits(Symbol)

type PGname = UnsafePGType "name"
type PGxml = UnsafePGType "xml"
type PGregtype = UnsafePGType "regtype"
type PGreltime = UnsafePGType "reltime"
type PGxid = UnsafePGType "xid"
type PGbox = UnsafePGType "box"
type PGcstring = UnsafePGType "cstring"
type PGvarbit = UnsafePGType "varbit"
type PGpoint = UnsafePGType "point"
type PGpolygon = UnsafePGType "polygon"
type PGunknown = UnsafePGType "unknown"
type PGtxid_snapshot = UnsafePGType "txid_snapshot"
type PGregdictionary = UnsafePGType "regdictionary"
type PGabstime = UnsafePGType "abstime"
type PGtinterval = UnsafePGType "tinterval"
type PGtid = UnsafePGType "tid"
type PGregclass = UnsafePGType "regclass"
type PGsmgr = UnsafePGType "smgr"
type PGregrole = UnsafePGType "regrole"
type PGregproc = UnsafePGType "regproc"
type PGregoperator = UnsafePGType "regoperator"
type PGregnamespace = UnsafePGType "regnamespace"
type PGregprocedure = UnsafePGType "regprocedure"
type PGregoper = UnsafePGType "regoper"
type PGregconfig = UnsafePGType "regconfig"
type PGcircle = UnsafePGType "circle"
type PGpath = UnsafePGType "path"
type PGpg_lsn = UnsafePGType "pg_lsn"
type PGpg_node_tree = UnsafePGType "pg_node_tree"
type PGline = UnsafePGType "line"
type PGlseg = UnsafePGType "lseg"
type PGoidvector = UnsafePGType "oidvector"
type PGcidr = UnsafePGType "cidr"
type PGaclitem = UnsafePGType "aclitem"
type PGmacaddr = UnsafePGType "macaddr"
type PGbpchar = UnsafePGType "bpchar"
type PGint2vector = UnsafePGType "int2vector"
type PGrefcursor = UnsafePGType "refcursor"
type PGcid = UnsafePGType "cid"
type PGbit = UnsafePGType "bit"
type PGanyarray = UnsafePGType "anyarray"

type DB = '["pg_catalog" ::: Schema]

type Schema = Join Tables (Join Views (Join Enums (Join Functions Domains)))
-- enums

-- decls
type Enums =
  ('[] :: [(Symbol,SchemumType)])
-- schema
type Tables = ('[
   "pg_aggregate" ::: 'Table PgAggregateTable
  ,"pg_am" ::: 'Table PgAmTable
  ,"pg_amop" ::: 'Table PgAmopTable
  ,"pg_amproc" ::: 'Table PgAmprocTable
  ,"pg_attrdef" ::: 'Table PgAttrdefTable
  ,"pg_attribute" ::: 'Table PgAttributeTable
  ,"pg_auth_members" ::: 'Table PgAuthMembersTable
  ,"pg_authid" ::: 'Table PgAuthidTable
  ,"pg_cast" ::: 'Table PgCastTable
  ,"pg_class" ::: 'Table PgClassTable
  ,"pg_collation" ::: 'Table PgCollationTable
  ,"pg_constraint" ::: 'Table PgConstraintTable
  ,"pg_conversion" ::: 'Table PgConversionTable
  ,"pg_database" ::: 'Table PgDatabaseTable
  ,"pg_db_role_setting" ::: 'Table PgDbRoleSettingTable
  ,"pg_default_acl" ::: 'Table PgDefaultAclTable
  ,"pg_depend" ::: 'Table PgDependTable
  ,"pg_description" ::: 'Table PgDescriptionTable
  ,"pg_enum" ::: 'Table PgEnumTable
  ,"pg_event_trigger" ::: 'Table PgEventTriggerTable
  ,"pg_extension" ::: 'Table PgExtensionTable
  ,"pg_foreign_data_wrapper" ::: 'Table PgForeignDataWrapperTable
  ,"pg_foreign_server" ::: 'Table PgForeignServerTable
  ,"pg_foreign_table" ::: 'Table PgForeignTableTable
  ,"pg_index" ::: 'Table PgIndexTable
  ,"pg_inherits" ::: 'Table PgInheritsTable
  ,"pg_language" ::: 'Table PgLanguageTable
  ,"pg_largeobject" ::: 'Table PgLargeobjectTable
  ,"pg_largeobject_metadata" ::: 'Table PgLargeobjectMetadataTable
  ,"pg_namespace" ::: 'Table PgNamespaceTable
  ,"pg_opclass" ::: 'Table PgOpclassTable
  ,"pg_operator" ::: 'Table PgOperatorTable
  ,"pg_opfamily" ::: 'Table PgOpfamilyTable
  ,"pg_pltemplate" ::: 'Table PgPltemplateTable
  ,"pg_policy" ::: 'Table PgPolicyTable
  ,"pg_proc" ::: 'Table PgProcTable
  ,"pg_range" ::: 'Table PgRangeTable
  ,"pg_replication_origin" ::: 'Table PgReplicationOriginTable
  ,"pg_rewrite" ::: 'Table PgRewriteTable
  ,"pg_seclabel" ::: 'Table PgSeclabelTable
  ,"pg_shdepend" ::: 'Table PgShdependTable
  ,"pg_shdescription" ::: 'Table PgShdescriptionTable
  ,"pg_shseclabel" ::: 'Table PgShseclabelTable
  ,"pg_statistic" ::: 'Table PgStatisticTable
  ,"pg_tablespace" ::: 'Table PgTablespaceTable
  ,"pg_transform" ::: 'Table PgTransformTable
  ,"pg_trigger" ::: 'Table PgTriggerTable
  ,"pg_ts_config" ::: 'Table PgTsConfigTable
  ,"pg_ts_config_map" ::: 'Table PgTsConfigMapTable
  ,"pg_ts_dict" ::: 'Table PgTsDictTable
  ,"pg_ts_parser" ::: 'Table PgTsParserTable
  ,"pg_ts_template" ::: 'Table PgTsTemplateTable
  ,"pg_type" ::: 'Table PgTypeTable
  ,"pg_user_mapping" ::: 'Table PgUserMappingTable]  :: [(Symbol,SchemumType)])

-- defs
type PgAggregateColumns = '["aggfnoid" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggkind" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"aggnumdirectargs" ::: 'NoDef :=> 'NotNull PGint2
  ,"aggtransfn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggfinalfn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggmtransfn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggminvtransfn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggmfinalfn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"aggfinalextra" ::: 'NoDef :=> 'NotNull PGbool
  ,"aggmfinalextra" ::: 'NoDef :=> 'NotNull PGbool
  ,"aggsortop" ::: 'NoDef :=> 'NotNull PGoid
  ,"aggtranstype" ::: 'NoDef :=> 'NotNull PGoid
  ,"aggtransspace" ::: 'NoDef :=> 'NotNull PGint4
  ,"aggmtranstype" ::: 'NoDef :=> 'NotNull PGoid
  ,"aggmtransspace" ::: 'NoDef :=> 'NotNull PGint4
  ,"agginitval" ::: 'NoDef :=> 'Null PGtext
  ,"aggminitval" ::: 'NoDef :=> 'Null PGtext]
type PgAggregateConstraints = '[]
type PgAggregateTable = PgAggregateConstraints :=> PgAggregateColumns

type PgAmColumns = '["amname" ::: 'NoDef :=> 'NotNull PGname
  ,"amstrategies" ::: 'NoDef :=> 'NotNull PGint2
  ,"amsupport" ::: 'NoDef :=> 'NotNull PGint2
  ,"amcanorder" ::: 'NoDef :=> 'NotNull PGbool
  ,"amcanorderbyop" ::: 'NoDef :=> 'NotNull PGbool
  ,"amcanbackward" ::: 'NoDef :=> 'NotNull PGbool
  ,"amcanunique" ::: 'NoDef :=> 'NotNull PGbool
  ,"amcanmulticol" ::: 'NoDef :=> 'NotNull PGbool
  ,"amoptionalkey" ::: 'NoDef :=> 'NotNull PGbool
  ,"amsearcharray" ::: 'NoDef :=> 'NotNull PGbool
  ,"amsearchnulls" ::: 'NoDef :=> 'NotNull PGbool
  ,"amstorage" ::: 'NoDef :=> 'NotNull PGbool
  ,"amclusterable" ::: 'NoDef :=> 'NotNull PGbool
  ,"ampredlocks" ::: 'NoDef :=> 'NotNull PGbool
  ,"amkeytype" ::: 'NoDef :=> 'NotNull PGoid
  ,"aminsert" ::: 'NoDef :=> 'NotNull PGregproc
  ,"ambeginscan" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amgettuple" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amgetbitmap" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amrescan" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amendscan" ::: 'NoDef :=> 'NotNull PGregproc
  ,"ammarkpos" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amrestrpos" ::: 'NoDef :=> 'NotNull PGregproc
  ,"ambuild" ::: 'NoDef :=> 'NotNull PGregproc
  ,"ambuildempty" ::: 'NoDef :=> 'NotNull PGregproc
  ,"ambulkdelete" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amvacuumcleanup" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amcanreturn" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amcostestimate" ::: 'NoDef :=> 'NotNull PGregproc
  ,"amoptions" ::: 'NoDef :=> 'NotNull PGregproc]
type PgAmConstraints = '[]
type PgAmTable = PgAmConstraints :=> PgAmColumns

type PgAmopColumns = '["amopfamily" ::: 'NoDef :=> 'NotNull PGoid
  ,"amoplefttype" ::: 'NoDef :=> 'NotNull PGoid
  ,"amoprighttype" ::: 'NoDef :=> 'NotNull PGoid
  ,"amopstrategy" ::: 'NoDef :=> 'NotNull PGint2
  ,"amoppurpose" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"amopopr" ::: 'NoDef :=> 'NotNull PGoid
  ,"amopmethod" ::: 'NoDef :=> 'NotNull PGoid
  ,"amopsortfamily" ::: 'NoDef :=> 'NotNull PGoid]
type PgAmopConstraints = '[]
type PgAmopTable = PgAmopConstraints :=> PgAmopColumns

type PgAmprocColumns = '["amprocfamily" ::: 'NoDef :=> 'NotNull PGoid
  ,"amproclefttype" ::: 'NoDef :=> 'NotNull PGoid
  ,"amprocrighttype" ::: 'NoDef :=> 'NotNull PGoid
  ,"amprocnum" ::: 'NoDef :=> 'NotNull PGint2
  ,"amproc" ::: 'NoDef :=> 'NotNull PGregproc]
type PgAmprocConstraints = '[]
type PgAmprocTable = PgAmprocConstraints :=> PgAmprocColumns

type PgAttrdefColumns = '["adrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"adnum" ::: 'NoDef :=> 'NotNull PGint2
  ,"adbin" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"adsrc" ::: 'NoDef :=> 'Null PGtext]
type PgAttrdefConstraints = '[]
type PgAttrdefTable = PgAttrdefConstraints :=> PgAttrdefColumns

type PgAttributeColumns = '["attrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"attname" ::: 'NoDef :=> 'NotNull PGname
  ,"atttypid" ::: 'NoDef :=> 'NotNull PGoid
  ,"attstattarget" ::: 'NoDef :=> 'NotNull PGint4
  ,"attlen" ::: 'NoDef :=> 'NotNull PGint2
  ,"attnum" ::: 'NoDef :=> 'NotNull PGint2
  ,"attndims" ::: 'NoDef :=> 'NotNull PGint4
  ,"attcacheoff" ::: 'NoDef :=> 'NotNull PGint4
  ,"atttypmod" ::: 'NoDef :=> 'NotNull PGint4
  ,"attbyval" ::: 'NoDef :=> 'NotNull PGbool
  ,"attstorage" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"attalign" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"attnotnull" ::: 'NoDef :=> 'NotNull PGbool
  ,"atthasdef" ::: 'NoDef :=> 'NotNull PGbool
  ,"attisdropped" ::: 'NoDef :=> 'NotNull PGbool
  ,"attislocal" ::: 'NoDef :=> 'NotNull PGbool
  ,"attinhcount" ::: 'NoDef :=> 'NotNull PGint4
  ,"attcollation" ::: 'NoDef :=> 'NotNull PGoid
  ,"attacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))
  ,"attoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))
  ,"attfdwoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgAttributeConstraints = '[]
type PgAttributeTable = PgAttributeConstraints :=> PgAttributeColumns

type PgAuthMembersColumns = '["roleid" ::: 'NoDef :=> 'NotNull PGoid
  ,"member" ::: 'NoDef :=> 'NotNull PGoid
  ,"grantor" ::: 'NoDef :=> 'NotNull PGoid
  ,"admin_option" ::: 'NoDef :=> 'NotNull PGbool]
type PgAuthMembersConstraints = '[]
type PgAuthMembersTable = PgAuthMembersConstraints :=> PgAuthMembersColumns

type PgAuthidColumns = '["rolname" ::: 'NoDef :=> 'NotNull PGname
  ,"rolsuper" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolinherit" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolcreaterole" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolcreatedb" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolcanlogin" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolreplication" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolbypassrls" ::: 'NoDef :=> 'NotNull PGbool
  ,"rolconnlimit" ::: 'NoDef :=> 'NotNull PGint4
  ,"rolpassword" ::: 'NoDef :=> 'Null PGtext
  ,"rolvaliduntil" ::: 'NoDef :=> 'Null PGtimestamptz]
type PgAuthidConstraints = '[]
type PgAuthidTable = PgAuthidConstraints :=> PgAuthidColumns

type PgCastColumns = '["castsource" ::: 'NoDef :=> 'NotNull PGoid
  ,"casttarget" ::: 'NoDef :=> 'NotNull PGoid
  ,"castfunc" ::: 'NoDef :=> 'NotNull PGoid
  ,"castcontext" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"castmethod" ::: 'NoDef :=> 'NotNull (PGchar 1)]
type PgCastConstraints = '[]
type PgCastTable = PgCastConstraints :=> PgCastColumns

type PgClassColumns = '["relname" ::: 'NoDef :=> 'NotNull PGname
  ,"relnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"reltype" ::: 'NoDef :=> 'NotNull PGoid
  ,"reloftype" ::: 'NoDef :=> 'NotNull PGoid
  ,"relowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"relam" ::: 'NoDef :=> 'NotNull PGoid
  ,"relfilenode" ::: 'NoDef :=> 'NotNull PGoid
  ,"reltablespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"relpages" ::: 'NoDef :=> 'NotNull PGint4
  ,"reltuples" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"relallvisible" ::: 'NoDef :=> 'NotNull PGint4
  ,"reltoastrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"relhasindex" ::: 'NoDef :=> 'NotNull PGbool
  ,"relisshared" ::: 'NoDef :=> 'NotNull PGbool
  ,"relpersistence" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"relkind" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"relnatts" ::: 'NoDef :=> 'NotNull PGint2
  ,"relchecks" ::: 'NoDef :=> 'NotNull PGint2
  ,"relhasoids" ::: 'NoDef :=> 'NotNull PGbool
  ,"relhaspkey" ::: 'NoDef :=> 'NotNull PGbool
  ,"relhasrules" ::: 'NoDef :=> 'NotNull PGbool
  ,"relhastriggers" ::: 'NoDef :=> 'NotNull PGbool
  ,"relhassubclass" ::: 'NoDef :=> 'NotNull PGbool
  ,"relrowsecurity" ::: 'NoDef :=> 'NotNull PGbool
  ,"relforcerowsecurity" ::: 'NoDef :=> 'NotNull PGbool
  ,"relispopulated" ::: 'NoDef :=> 'NotNull PGbool
  ,"relreplident" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"relfrozenxid" ::: 'NoDef :=> 'NotNull PGxid
  ,"relminmxid" ::: 'NoDef :=> 'NotNull PGxid
  ,"relacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))
  ,"reloptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgClassConstraints = '[]
type PgClassTable = PgClassConstraints :=> PgClassColumns

type PgCollationColumns = '["collname" ::: 'NoDef :=> 'NotNull PGname
  ,"collnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"collowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"collencoding" ::: 'NoDef :=> 'NotNull PGint4
  ,"collcollate" ::: 'NoDef :=> 'NotNull PGname
  ,"collctype" ::: 'NoDef :=> 'NotNull PGname]
type PgCollationConstraints = '[]
type PgCollationTable = PgCollationConstraints :=> PgCollationColumns

type PgConstraintColumns = '["conname" ::: 'NoDef :=> 'NotNull PGname
  ,"connamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"contype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"condeferrable" ::: 'NoDef :=> 'NotNull PGbool
  ,"condeferred" ::: 'NoDef :=> 'NotNull PGbool
  ,"convalidated" ::: 'NoDef :=> 'NotNull PGbool
  ,"conrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"contypid" ::: 'NoDef :=> 'NotNull PGoid
  ,"conindid" ::: 'NoDef :=> 'NotNull PGoid
  ,"confrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"confupdtype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"confdeltype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"confmatchtype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"conislocal" ::: 'NoDef :=> 'NotNull PGbool
  ,"coninhcount" ::: 'NoDef :=> 'NotNull PGint4
  ,"connoinherit" ::: 'NoDef :=> 'NotNull PGbool
  ,"conkey" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGint2))
  ,"confkey" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGint2))
  ,"conpfeqop" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"conppeqop" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"conffeqop" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"conexclop" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"conbin" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"consrc" ::: 'NoDef :=> 'Null PGtext]
type PgConstraintConstraints = '[]
type PgConstraintTable = PgConstraintConstraints :=> PgConstraintColumns

type PgConversionColumns = '["conname" ::: 'NoDef :=> 'NotNull PGname
  ,"connamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"conowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"conforencoding" ::: 'NoDef :=> 'NotNull PGint4
  ,"contoencoding" ::: 'NoDef :=> 'NotNull PGint4
  ,"conproc" ::: 'NoDef :=> 'NotNull PGregproc
  ,"condefault" ::: 'NoDef :=> 'NotNull PGbool]
type PgConversionConstraints = '[]
type PgConversionTable = PgConversionConstraints :=> PgConversionColumns

type PgDatabaseColumns = '["datname" ::: 'NoDef :=> 'NotNull PGname
  ,"datdba" ::: 'NoDef :=> 'NotNull PGoid
  ,"encoding" ::: 'NoDef :=> 'NotNull PGint4
  ,"datcollate" ::: 'NoDef :=> 'NotNull PGname
  ,"datctype" ::: 'NoDef :=> 'NotNull PGname
  ,"datistemplate" ::: 'NoDef :=> 'NotNull PGbool
  ,"datallowconn" ::: 'NoDef :=> 'NotNull PGbool
  ,"datconnlimit" ::: 'NoDef :=> 'NotNull PGint4
  ,"datlastsysoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"datfrozenxid" ::: 'NoDef :=> 'NotNull PGxid
  ,"datminmxid" ::: 'NoDef :=> 'NotNull PGxid
  ,"dattablespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"datacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgDatabaseConstraints = '[]
type PgDatabaseTable = PgDatabaseConstraints :=> PgDatabaseColumns

type PgDbRoleSettingColumns = '["setdatabase" ::: 'NoDef :=> 'NotNull PGoid
  ,"setrole" ::: 'NoDef :=> 'NotNull PGoid
  ,"setconfig" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgDbRoleSettingConstraints = '[]
type PgDbRoleSettingTable = PgDbRoleSettingConstraints :=> PgDbRoleSettingColumns

type PgDefaultAclColumns = '["defaclrole" ::: 'NoDef :=> 'NotNull PGoid
  ,"defaclnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"defaclobjtype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"defaclacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgDefaultAclConstraints = '[]
type PgDefaultAclTable = PgDefaultAclConstraints :=> PgDefaultAclColumns

type PgDependColumns = '["classid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objsubid" ::: 'NoDef :=> 'NotNull PGint4
  ,"refclassid" ::: 'NoDef :=> 'NotNull PGoid
  ,"refobjid" ::: 'NoDef :=> 'NotNull PGoid
  ,"refobjsubid" ::: 'NoDef :=> 'NotNull PGint4
  ,"deptype" ::: 'NoDef :=> 'NotNull (PGchar 1)]
type PgDependConstraints = '[]
type PgDependTable = PgDependConstraints :=> PgDependColumns

type PgDescriptionColumns = '["objoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"classoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objsubid" ::: 'NoDef :=> 'NotNull PGint4
  ,"description" ::: 'NoDef :=> 'NotNull PGtext]
type PgDescriptionConstraints = '[]
type PgDescriptionTable = PgDescriptionConstraints :=> PgDescriptionColumns

type PgEnumColumns = '["enumtypid" ::: 'NoDef :=> 'NotNull PGoid
  ,"enumsortorder" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"enumlabel" ::: 'NoDef :=> 'NotNull PGname]
type PgEnumConstraints = '[]
type PgEnumTable = PgEnumConstraints :=> PgEnumColumns

type PgEventTriggerColumns = '["evtname" ::: 'NoDef :=> 'NotNull PGname
  ,"evtevent" ::: 'NoDef :=> 'NotNull PGname
  ,"evtowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"evtfoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"evtenabled" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"evttags" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgEventTriggerConstraints = '[]
type PgEventTriggerTable = PgEventTriggerConstraints :=> PgEventTriggerColumns

type PgExtensionColumns = '["extname" ::: 'NoDef :=> 'NotNull PGname
  ,"extowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"extnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"extrelocatable" ::: 'NoDef :=> 'NotNull PGbool
  ,"extversion" ::: 'NoDef :=> 'NotNull PGtext
  ,"extconfig" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"extcondition" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgExtensionConstraints = '[]
type PgExtensionTable = PgExtensionConstraints :=> PgExtensionColumns

type PgForeignDataWrapperColumns = '["fdwname" ::: 'NoDef :=> 'NotNull PGname
  ,"fdwowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"fdwhandler" ::: 'NoDef :=> 'NotNull PGoid
  ,"fdwvalidator" ::: 'NoDef :=> 'NotNull PGoid
  ,"fdwacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))
  ,"fdwoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgForeignDataWrapperConstraints = '[]
type PgForeignDataWrapperTable = PgForeignDataWrapperConstraints :=> PgForeignDataWrapperColumns

type PgForeignServerColumns = '["srvname" ::: 'NoDef :=> 'NotNull PGname
  ,"srvowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"srvfdw" ::: 'NoDef :=> 'NotNull PGoid
  ,"srvtype" ::: 'NoDef :=> 'Null PGtext
  ,"srvversion" ::: 'NoDef :=> 'Null PGtext
  ,"srvacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))
  ,"srvoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgForeignServerConstraints = '[]
type PgForeignServerTable = PgForeignServerConstraints :=> PgForeignServerColumns

type PgForeignTableColumns = '["ftrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"ftserver" ::: 'NoDef :=> 'NotNull PGoid
  ,"ftoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgForeignTableConstraints = '[]
type PgForeignTableTable = PgForeignTableConstraints :=> PgForeignTableColumns

type PgIndexColumns = '["indexrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"indrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"indnatts" ::: 'NoDef :=> 'NotNull PGint2
  ,"indisunique" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisprimary" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisexclusion" ::: 'NoDef :=> 'NotNull PGbool
  ,"indimmediate" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisclustered" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisvalid" ::: 'NoDef :=> 'NotNull PGbool
  ,"indcheckxmin" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisready" ::: 'NoDef :=> 'NotNull PGbool
  ,"indislive" ::: 'NoDef :=> 'NotNull PGbool
  ,"indisreplident" ::: 'NoDef :=> 'NotNull PGbool
  ,"indkey" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGint2vector))
  ,"indcollation" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGoidvector))
  ,"indclass" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGoidvector))
  ,"indoption" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGint2vector))
  ,"indexprs" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"indpred" ::: 'NoDef :=> 'Null PGpg_node_tree]
type PgIndexConstraints = '[]
type PgIndexTable = PgIndexConstraints :=> PgIndexColumns

type PgInheritsColumns = '["inhrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"inhparent" ::: 'NoDef :=> 'NotNull PGoid
  ,"inhseqno" ::: 'NoDef :=> 'NotNull PGint4]
type PgInheritsConstraints = '[]
type PgInheritsTable = PgInheritsConstraints :=> PgInheritsColumns

type PgLanguageColumns = '["lanname" ::: 'NoDef :=> 'NotNull PGname
  ,"lanowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"lanispl" ::: 'NoDef :=> 'NotNull PGbool
  ,"lanpltrusted" ::: 'NoDef :=> 'NotNull PGbool
  ,"lanplcallfoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"laninline" ::: 'NoDef :=> 'NotNull PGoid
  ,"lanvalidator" ::: 'NoDef :=> 'NotNull PGoid
  ,"lanacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgLanguageConstraints = '[]
type PgLanguageTable = PgLanguageConstraints :=> PgLanguageColumns

type PgLargeobjectColumns = '["loid" ::: 'NoDef :=> 'NotNull PGoid
  ,"pageno" ::: 'NoDef :=> 'NotNull PGint4
  ,"data" ::: 'NoDef :=> 'NotNull PGbytea]
type PgLargeobjectConstraints = '[]
type PgLargeobjectTable = PgLargeobjectConstraints :=> PgLargeobjectColumns

type PgLargeobjectMetadataColumns = '["lomowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"lomacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgLargeobjectMetadataConstraints = '[]
type PgLargeobjectMetadataTable = PgLargeobjectMetadataConstraints :=> PgLargeobjectMetadataColumns

type PgNamespaceColumns = '["nspname" ::: 'NoDef :=> 'NotNull PGname
  ,"nspowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"nspacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgNamespaceConstraints = '[]
type PgNamespaceTable = PgNamespaceConstraints :=> PgNamespaceColumns

type PgOpclassColumns = '["opcmethod" ::: 'NoDef :=> 'NotNull PGoid
  ,"opcname" ::: 'NoDef :=> 'NotNull PGname
  ,"opcnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"opcowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"opcfamily" ::: 'NoDef :=> 'NotNull PGoid
  ,"opcintype" ::: 'NoDef :=> 'NotNull PGoid
  ,"opcdefault" ::: 'NoDef :=> 'NotNull PGbool
  ,"opckeytype" ::: 'NoDef :=> 'NotNull PGoid]
type PgOpclassConstraints = '[]
type PgOpclassTable = PgOpclassConstraints :=> PgOpclassColumns

type PgOperatorColumns = '["oprname" ::: 'NoDef :=> 'NotNull PGname
  ,"oprnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprkind" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"oprcanmerge" ::: 'NoDef :=> 'NotNull PGbool
  ,"oprcanhash" ::: 'NoDef :=> 'NotNull PGbool
  ,"oprleft" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprright" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprresult" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprcom" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprnegate" ::: 'NoDef :=> 'NotNull PGoid
  ,"oprcode" ::: 'NoDef :=> 'NotNull PGregproc
  ,"oprrest" ::: 'NoDef :=> 'NotNull PGregproc
  ,"oprjoin" ::: 'NoDef :=> 'NotNull PGregproc]
type PgOperatorConstraints = '[]
type PgOperatorTable = PgOperatorConstraints :=> PgOperatorColumns

type PgOpfamilyColumns = '["opfmethod" ::: 'NoDef :=> 'NotNull PGoid
  ,"opfname" ::: 'NoDef :=> 'NotNull PGname
  ,"opfnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"opfowner" ::: 'NoDef :=> 'NotNull PGoid]
type PgOpfamilyConstraints = '[]
type PgOpfamilyTable = PgOpfamilyConstraints :=> PgOpfamilyColumns

type PgPltemplateColumns = '["tmplname" ::: 'NoDef :=> 'NotNull PGname
  ,"tmpltrusted" ::: 'NoDef :=> 'NotNull PGbool
  ,"tmpldbacreate" ::: 'NoDef :=> 'NotNull PGbool
  ,"tmplhandler" ::: 'NoDef :=> 'NotNull PGtext
  ,"tmplinline" ::: 'NoDef :=> 'Null PGtext
  ,"tmplvalidator" ::: 'NoDef :=> 'Null PGtext
  ,"tmpllibrary" ::: 'NoDef :=> 'NotNull PGtext
  ,"tmplacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgPltemplateConstraints = '[]
type PgPltemplateTable = PgPltemplateConstraints :=> PgPltemplateColumns

type PgPolicyColumns = '["polname" ::: 'NoDef :=> 'NotNull PGname
  ,"polrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"polcmd" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"polroles" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"polqual" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"polwithcheck" ::: 'NoDef :=> 'Null PGpg_node_tree]
type PgPolicyConstraints = '[]
type PgPolicyTable = PgPolicyConstraints :=> PgPolicyColumns

type PgProcColumns = '["proname" ::: 'NoDef :=> 'NotNull PGname
  ,"pronamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"proowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"prolang" ::: 'NoDef :=> 'NotNull PGoid
  ,"procost" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"prorows" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"provariadic" ::: 'NoDef :=> 'NotNull PGoid
  ,"protransform" ::: 'NoDef :=> 'NotNull PGregproc
  ,"proisagg" ::: 'NoDef :=> 'NotNull PGbool
  ,"proiswindow" ::: 'NoDef :=> 'NotNull PGbool
  ,"prosecdef" ::: 'NoDef :=> 'NotNull PGbool
  ,"proleakproof" ::: 'NoDef :=> 'NotNull PGbool
  ,"proisstrict" ::: 'NoDef :=> 'NotNull PGbool
  ,"proretset" ::: 'NoDef :=> 'NotNull PGbool
  ,"provolatile" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"pronargs" ::: 'NoDef :=> 'NotNull PGint2
  ,"pronargdefaults" ::: 'NoDef :=> 'NotNull PGint2
  ,"prorettype" ::: 'NoDef :=> 'NotNull PGoid
  ,"proargtypes" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGoidvector))
  ,"proallargtypes" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"proargmodes" ::: 'NoDef :=> 'Null (PGvararray ('NotNull (PGchar 1)))
  ,"proargnames" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))
  ,"proargdefaults" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"protrftypes" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGoid))
  ,"prosrc" ::: 'NoDef :=> 'NotNull PGtext
  ,"probin" ::: 'NoDef :=> 'Null PGtext
  ,"proconfig" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))
  ,"proacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgProcConstraints = '[]
type PgProcTable = PgProcConstraints :=> PgProcColumns

type PgRangeColumns = '["rngtypid" ::: 'NoDef :=> 'NotNull PGoid
  ,"rngsubtype" ::: 'NoDef :=> 'NotNull PGoid
  ,"rngcollation" ::: 'NoDef :=> 'NotNull PGoid
  ,"rngsubopc" ::: 'NoDef :=> 'NotNull PGoid
  ,"rngcanonical" ::: 'NoDef :=> 'NotNull PGregproc
  ,"rngsubdiff" ::: 'NoDef :=> 'NotNull PGregproc]
type PgRangeConstraints = '[]
type PgRangeTable = PgRangeConstraints :=> PgRangeColumns

type PgReplicationOriginColumns = '["roident" ::: 'NoDef :=> 'NotNull PGoid
  ,"roname" ::: 'NoDef :=> 'NotNull PGtext]
type PgReplicationOriginConstraints = '[]
type PgReplicationOriginTable = PgReplicationOriginConstraints :=> PgReplicationOriginColumns

type PgRewriteColumns = '["rulename" ::: 'NoDef :=> 'NotNull PGname
  ,"ev_class" ::: 'NoDef :=> 'NotNull PGoid
  ,"ev_type" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"ev_enabled" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"is_instead" ::: 'NoDef :=> 'NotNull PGbool
  ,"ev_qual" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"ev_action" ::: 'NoDef :=> 'Null PGpg_node_tree]
type PgRewriteConstraints = '[]
type PgRewriteTable = PgRewriteConstraints :=> PgRewriteColumns

type PgSeclabelColumns = '["objoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"classoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objsubid" ::: 'NoDef :=> 'NotNull PGint4
  ,"provider" ::: 'NoDef :=> 'NotNull PGtext
  ,"label" ::: 'NoDef :=> 'NotNull PGtext]
type PgSeclabelConstraints = '[]
type PgSeclabelTable = PgSeclabelConstraints :=> PgSeclabelColumns

type PgShdependColumns = '["dbid" ::: 'NoDef :=> 'NotNull PGoid
  ,"classid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objid" ::: 'NoDef :=> 'NotNull PGoid
  ,"objsubid" ::: 'NoDef :=> 'NotNull PGint4
  ,"refclassid" ::: 'NoDef :=> 'NotNull PGoid
  ,"refobjid" ::: 'NoDef :=> 'NotNull PGoid
  ,"deptype" ::: 'NoDef :=> 'NotNull (PGchar 1)]
type PgShdependConstraints = '[]
type PgShdependTable = PgShdependConstraints :=> PgShdependColumns

type PgShdescriptionColumns = '["objoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"classoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"description" ::: 'NoDef :=> 'NotNull PGtext]
type PgShdescriptionConstraints = '[]
type PgShdescriptionTable = PgShdescriptionConstraints :=> PgShdescriptionColumns

type PgShseclabelColumns = '["objoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"classoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"provider" ::: 'NoDef :=> 'NotNull PGtext
  ,"label" ::: 'NoDef :=> 'NotNull PGtext]
type PgShseclabelConstraints = '[]
type PgShseclabelTable = PgShseclabelConstraints :=> PgShseclabelColumns

type PgStatisticColumns = '["starelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"staattnum" ::: 'NoDef :=> 'NotNull PGint2
  ,"stainherit" ::: 'NoDef :=> 'NotNull PGbool
  ,"stanullfrac" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"stawidth" ::: 'NoDef :=> 'NotNull PGint4
  ,"stadistinct" ::: 'NoDef :=> 'NotNull PGfloat4
  ,"stakind1" ::: 'NoDef :=> 'NotNull PGint2
  ,"stakind2" ::: 'NoDef :=> 'NotNull PGint2
  ,"stakind3" ::: 'NoDef :=> 'NotNull PGint2
  ,"stakind4" ::: 'NoDef :=> 'NotNull PGint2
  ,"stakind5" ::: 'NoDef :=> 'NotNull PGint2
  ,"staop1" ::: 'NoDef :=> 'NotNull PGoid
  ,"staop2" ::: 'NoDef :=> 'NotNull PGoid
  ,"staop3" ::: 'NoDef :=> 'NotNull PGoid
  ,"staop4" ::: 'NoDef :=> 'NotNull PGoid
  ,"staop5" ::: 'NoDef :=> 'NotNull PGoid
  ,"stanumbers1" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGfloat4))
  ,"stanumbers2" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGfloat4))
  ,"stanumbers3" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGfloat4))
  ,"stanumbers4" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGfloat4))
  ,"stanumbers5" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGfloat4))
  ,"stavalues1" ::: 'NoDef :=> 'Null PGanyarray
  ,"stavalues2" ::: 'NoDef :=> 'Null PGanyarray
  ,"stavalues3" ::: 'NoDef :=> 'Null PGanyarray
  ,"stavalues4" ::: 'NoDef :=> 'Null PGanyarray
  ,"stavalues5" ::: 'NoDef :=> 'Null PGanyarray]
type PgStatisticConstraints = '[]
type PgStatisticTable = PgStatisticConstraints :=> PgStatisticColumns

type PgTablespaceColumns = '["spcname" ::: 'NoDef :=> 'NotNull PGname
  ,"spcowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"spcacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))
  ,"spcoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgTablespaceConstraints = '[]
type PgTablespaceTable = PgTablespaceConstraints :=> PgTablespaceColumns

type PgTransformColumns = '["trftype" ::: 'NoDef :=> 'NotNull PGoid
  ,"trflang" ::: 'NoDef :=> 'NotNull PGoid
  ,"trffromsql" ::: 'NoDef :=> 'NotNull PGregproc
  ,"trftosql" ::: 'NoDef :=> 'NotNull PGregproc]
type PgTransformConstraints = '[]
type PgTransformTable = PgTransformConstraints :=> PgTransformColumns

type PgTriggerColumns = '["tgrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"tgname" ::: 'NoDef :=> 'NotNull PGname
  ,"tgfoid" ::: 'NoDef :=> 'NotNull PGoid
  ,"tgtype" ::: 'NoDef :=> 'NotNull PGint2
  ,"tgenabled" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"tgisinternal" ::: 'NoDef :=> 'NotNull PGbool
  ,"tgconstrrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"tgconstrindid" ::: 'NoDef :=> 'NotNull PGoid
  ,"tgconstraint" ::: 'NoDef :=> 'NotNull PGoid
  ,"tgdeferrable" ::: 'NoDef :=> 'NotNull PGbool
  ,"tginitdeferred" ::: 'NoDef :=> 'NotNull PGbool
  ,"tgnargs" ::: 'NoDef :=> 'NotNull PGint2
  ,"tgattr" ::: 'NoDef :=> 'NotNull (PGvararray (NotNull PGint2vector))
  ,"tgargs" ::: 'NoDef :=> 'NotNull PGbytea
  ,"tgqual" ::: 'NoDef :=> 'Null PGpg_node_tree]
type PgTriggerConstraints = '[]
type PgTriggerTable = PgTriggerConstraints :=> PgTriggerColumns

type PgTsConfigColumns = '["cfgname" ::: 'NoDef :=> 'NotNull PGname
  ,"cfgnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"cfgowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"cfgparser" ::: 'NoDef :=> 'NotNull PGoid]
type PgTsConfigConstraints = '[]
type PgTsConfigTable = PgTsConfigConstraints :=> PgTsConfigColumns

type PgTsConfigMapColumns = '["mapcfg" ::: 'NoDef :=> 'NotNull PGoid
  ,"maptokentype" ::: 'NoDef :=> 'NotNull PGint4
  ,"mapseqno" ::: 'NoDef :=> 'NotNull PGint4
  ,"mapdict" ::: 'NoDef :=> 'NotNull PGoid]
type PgTsConfigMapConstraints = '[]
type PgTsConfigMapTable = PgTsConfigMapConstraints :=> PgTsConfigMapColumns

type PgTsDictColumns = '["dictname" ::: 'NoDef :=> 'NotNull PGname
  ,"dictnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"dictowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"dicttemplate" ::: 'NoDef :=> 'NotNull PGoid
  ,"dictinitoption" ::: 'NoDef :=> 'Null PGtext]
type PgTsDictConstraints = '[]
type PgTsDictTable = PgTsDictConstraints :=> PgTsDictColumns

type PgTsParserColumns = '["prsname" ::: 'NoDef :=> 'NotNull PGname
  ,"prsnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"prsstart" ::: 'NoDef :=> 'NotNull PGregproc
  ,"prstoken" ::: 'NoDef :=> 'NotNull PGregproc
  ,"prsend" ::: 'NoDef :=> 'NotNull PGregproc
  ,"prsheadline" ::: 'NoDef :=> 'NotNull PGregproc
  ,"prslextype" ::: 'NoDef :=> 'NotNull PGregproc]
type PgTsParserConstraints = '[]
type PgTsParserTable = PgTsParserConstraints :=> PgTsParserColumns

type PgTsTemplateColumns = '["tmplname" ::: 'NoDef :=> 'NotNull PGname
  ,"tmplnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"tmplinit" ::: 'NoDef :=> 'NotNull PGregproc
  ,"tmpllexize" ::: 'NoDef :=> 'NotNull PGregproc]
type PgTsTemplateConstraints = '[]
type PgTsTemplateTable = PgTsTemplateConstraints :=> PgTsTemplateColumns

type PgTypeColumns = '["typname" ::: 'NoDef :=> 'NotNull PGname
  ,"typnamespace" ::: 'NoDef :=> 'NotNull PGoid
  ,"typowner" ::: 'NoDef :=> 'NotNull PGoid
  ,"typlen" ::: 'NoDef :=> 'NotNull PGint2
  ,"typbyval" ::: 'NoDef :=> 'NotNull PGbool
  ,"typtype" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"typcategory" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"typispreferred" ::: 'NoDef :=> 'NotNull PGbool
  ,"typisdefined" ::: 'NoDef :=> 'NotNull PGbool
  ,"typdelim" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"typrelid" ::: 'NoDef :=> 'NotNull PGoid
  ,"typelem" ::: 'NoDef :=> 'NotNull PGoid
  ,"typarray" ::: 'NoDef :=> 'NotNull PGoid
  ,"typinput" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typoutput" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typreceive" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typsend" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typmodin" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typmodout" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typanalyze" ::: 'NoDef :=> 'NotNull PGregproc
  ,"typalign" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"typstorage" ::: 'NoDef :=> 'NotNull (PGchar 1)
  ,"typnotnull" ::: 'NoDef :=> 'NotNull PGbool
  ,"typbasetype" ::: 'NoDef :=> 'NotNull PGoid
  ,"typtypmod" ::: 'NoDef :=> 'NotNull PGint4
  ,"typndims" ::: 'NoDef :=> 'NotNull PGint4
  ,"typcollation" ::: 'NoDef :=> 'NotNull PGoid
  ,"typdefaultbin" ::: 'NoDef :=> 'Null PGpg_node_tree
  ,"typdefault" ::: 'NoDef :=> 'Null PGtext
  ,"typacl" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGaclitem))]
type PgTypeConstraints = '[]
type PgTypeTable = PgTypeConstraints :=> PgTypeColumns

type PgUserMappingColumns = '["umuser" ::: 'NoDef :=> 'NotNull PGoid
  ,"umserver" ::: 'NoDef :=> 'NotNull PGoid
  ,"umoptions" ::: 'NoDef :=> 'Null (PGvararray (NotNull PGtext))]
type PgUserMappingConstraints = '[]
type PgUserMappingTable = PgUserMappingConstraints :=> PgUserMappingColumns

-- VIEWS
type Views = 
  '["pg_available_extension_versions" ::: 'View PgAvailableExtensionVersionsView,"pg_available_extensions" ::: 'View PgAvailableExtensionsView,"pg_cursors" ::: 'View PgCursorsView,"pg_file_settings" ::: 'View PgFileSettingsView,"pg_group" ::: 'View PgGroupView,"pg_indexes" ::: 'View PgIndexesView,"pg_locks" ::: 'View PgLocksView,"pg_matviews" ::: 'View PgMatviewsView,"pg_policies" ::: 'View PgPoliciesView,"pg_prepared_statements" ::: 'View PgPreparedStatementsView,"pg_prepared_xacts" ::: 'View PgPreparedXactsView,"pg_replication_origin_status" ::: 'View PgReplicationOriginStatusView,"pg_replication_slots" ::: 'View PgReplicationSlotsView,"pg_roles" ::: 'View PgRolesView,"pg_rules" ::: 'View PgRulesView,"pg_seclabels" ::: 'View PgSeclabelsView,"pg_settings" ::: 'View PgSettingsView,"pg_shadow" ::: 'View PgShadowView,"pg_stat_activity" ::: 'View PgStatActivityView,"pg_stat_all_indexes" ::: 'View PgStatAllIndexesView,"pg_stat_all_tables" ::: 'View PgStatAllTablesView,"pg_stat_archiver" ::: 'View PgStatArchiverView,"pg_stat_bgwriter" ::: 'View PgStatBgwriterView,"pg_stat_database" ::: 'View PgStatDatabaseView,"pg_stat_database_conflicts" ::: 'View PgStatDatabaseConflictsView,"pg_stat_replication" ::: 'View PgStatReplicationView,"pg_stat_ssl" ::: 'View PgStatSslView,"pg_stat_sys_indexes" ::: 'View PgStatSysIndexesView,"pg_stat_sys_tables" ::: 'View PgStatSysTablesView,"pg_stat_user_functions" ::: 'View PgStatUserFunctionsView,"pg_stat_user_indexes" ::: 'View PgStatUserIndexesView,"pg_stat_user_tables" ::: 'View PgStatUserTablesView,"pg_stat_xact_all_tables" ::: 'View PgStatXactAllTablesView,"pg_stat_xact_sys_tables" ::: 'View PgStatXactSysTablesView,"pg_stat_xact_user_functions" ::: 'View PgStatXactUserFunctionsView,"pg_stat_xact_user_tables" ::: 'View PgStatXactUserTablesView,"pg_statio_all_indexes" ::: 'View PgStatioAllIndexesView,"pg_statio_all_sequences" ::: 'View PgStatioAllSequencesView,"pg_statio_all_tables" ::: 'View PgStatioAllTablesView,"pg_statio_sys_indexes" ::: 'View PgStatioSysIndexesView,"pg_statio_sys_sequences" ::: 'View PgStatioSysSequencesView,"pg_statio_sys_tables" ::: 'View PgStatioSysTablesView,"pg_statio_user_indexes" ::: 'View PgStatioUserIndexesView,"pg_statio_user_sequences" ::: 'View PgStatioUserSequencesView,"pg_statio_user_tables" ::: 'View PgStatioUserTablesView,"pg_stats" ::: 'View PgStatsView,"pg_tables" ::: 'View PgTablesView,"pg_timezone_abbrevs" ::: 'View PgTimezoneAbbrevsView,"pg_timezone_names" ::: 'View PgTimezoneNamesView,"pg_user" ::: 'View PgUserView,"pg_user_mappings" ::: 'View PgUserMappingsView,"pg_views" ::: 'View PgViewsView]

type PgAvailableExtensionVersionsView = 
  '["name" ::: 'Null PGname
   ,"version" ::: 'Null PGtext
   ,"installed" ::: 'Null PGbool
   ,"superuser" ::: 'Null PGbool
   ,"relocatable" ::: 'Null PGbool
   ,"schema" ::: 'Null PGname
   ,"requires" ::: 'Null (PGvararray (NotNull PGname))
   ,"comment" ::: 'Null PGtext]

type PgAvailableExtensionsView = 
  '["name" ::: 'Null PGname
   ,"default_version" ::: 'Null PGtext
   ,"installed_version" ::: 'Null PGtext
   ,"comment" ::: 'Null PGtext]

type PgCursorsView = 
  '["name" ::: 'Null PGtext
   ,"statement" ::: 'Null PGtext
   ,"is_holdable" ::: 'Null PGbool
   ,"is_binary" ::: 'Null PGbool
   ,"is_scrollable" ::: 'Null PGbool
   ,"creation_time" ::: 'Null PGtimestamptz]

type PgFileSettingsView = 
  '["sourcefile" ::: 'Null PGtext
   ,"sourceline" ::: 'Null PGint4
   ,"seqno" ::: 'Null PGint4
   ,"name" ::: 'Null PGtext
   ,"setting" ::: 'Null PGtext
   ,"applied" ::: 'Null PGbool
   ,"error" ::: 'Null PGtext]

type PgGroupView = 
  '["groname" ::: 'Null PGname
   ,"grosysid" ::: 'Null PGoid
   ,"grolist" ::: 'Null (PGvararray (NotNull PGoid))]

type PgIndexesView = 
  '["schemaname" ::: 'Null PGname
   ,"tablename" ::: 'Null PGname
   ,"indexname" ::: 'Null PGname
   ,"tablespace" ::: 'Null PGname
   ,"indexdef" ::: 'Null PGtext]

type PgLocksView = 
  '["locktype" ::: 'Null PGtext
   ,"database" ::: 'Null PGoid
   ,"relation" ::: 'Null PGoid
   ,"page" ::: 'Null PGint4
   ,"tuple" ::: 'Null PGint2
   ,"virtualxid" ::: 'Null PGtext
   ,"transactionid" ::: 'Null PGxid
   ,"classid" ::: 'Null PGoid
   ,"objid" ::: 'Null PGoid
   ,"objsubid" ::: 'Null PGint2
   ,"virtualtransaction" ::: 'Null PGtext
   ,"pid" ::: 'Null PGint4
   ,"mode" ::: 'Null PGtext
   ,"granted" ::: 'Null PGbool
   ,"fastpath" ::: 'Null PGbool]

type PgMatviewsView = 
  '["schemaname" ::: 'Null PGname
   ,"matviewname" ::: 'Null PGname
   ,"matviewowner" ::: 'Null PGname
   ,"tablespace" ::: 'Null PGname
   ,"hasindexes" ::: 'Null PGbool
   ,"ispopulated" ::: 'Null PGbool
   ,"definition" ::: 'Null PGtext]

type PgPoliciesView = 
  '["schemaname" ::: 'Null PGname
   ,"tablename" ::: 'Null PGname
   ,"policyname" ::: 'Null PGname
   ,"roles" ::: 'Null (PGvararray (NotNull PGname))
   ,"cmd" ::: 'Null PGtext
   ,"qual" ::: 'Null PGtext
   ,"with_check" ::: 'Null PGtext]

type PgPreparedStatementsView = 
  '["name" ::: 'Null PGtext
   ,"statement" ::: 'Null PGtext
   ,"prepare_time" ::: 'Null PGtimestamptz
   ,"parameter_types" ::: 'Null (PGvararray (NotNull PGregtype))
   ,"from_sql" ::: 'Null PGbool]

type PgPreparedXactsView = 
  '["transaction" ::: 'Null PGxid
   ,"gid" ::: 'Null PGtext
   ,"prepared" ::: 'Null PGtimestamptz
   ,"owner" ::: 'Null PGname
   ,"database" ::: 'Null PGname]

type PgReplicationOriginStatusView = 
  '["local_id" ::: 'Null PGoid
   ,"external_id" ::: 'Null PGtext
   ,"remote_lsn" ::: 'Null PGpg_lsn
   ,"local_lsn" ::: 'Null PGpg_lsn]

type PgReplicationSlotsView = 
  '["slot_name" ::: 'Null PGname
   ,"plugin" ::: 'Null PGname
   ,"slot_type" ::: 'Null PGtext
   ,"datoid" ::: 'Null PGoid
   ,"database" ::: 'Null PGname
   ,"active" ::: 'Null PGbool
   ,"active_pid" ::: 'Null PGint4
   ,"xmin" ::: 'Null PGxid
   ,"catalog_xmin" ::: 'Null PGxid
   ,"restart_lsn" ::: 'Null PGpg_lsn]

type PgRolesView = 
  '["rolname" ::: 'Null PGname
   ,"rolsuper" ::: 'Null PGbool
   ,"rolinherit" ::: 'Null PGbool
   ,"rolcreaterole" ::: 'Null PGbool
   ,"rolcreatedb" ::: 'Null PGbool
   ,"rolcanlogin" ::: 'Null PGbool
   ,"rolreplication" ::: 'Null PGbool
   ,"rolconnlimit" ::: 'Null PGint4
   ,"rolpassword" ::: 'Null PGtext
   ,"rolvaliduntil" ::: 'Null PGtimestamptz
   ,"rolbypassrls" ::: 'Null PGbool
   ,"rolconfig" ::: 'Null (PGvararray (NotNull PGtext))
   ,"oid" ::: 'Null PGoid]

type PgRulesView = 
  '["schemaname" ::: 'Null PGname
   ,"tablename" ::: 'Null PGname
   ,"rulename" ::: 'Null PGname
   ,"definition" ::: 'Null PGtext]

type PgSeclabelsView = 
  '["objoid" ::: 'Null PGoid
   ,"classoid" ::: 'Null PGoid
   ,"objsubid" ::: 'Null PGint4
   ,"objtype" ::: 'Null PGtext
   ,"objnamespace" ::: 'Null PGoid
   ,"objname" ::: 'Null PGtext
   ,"provider" ::: 'Null PGtext
   ,"label" ::: 'Null PGtext]

type PgSettingsView = 
  '["name" ::: 'Null PGtext
   ,"setting" ::: 'Null PGtext
   ,"unit" ::: 'Null PGtext
   ,"category" ::: 'Null PGtext
   ,"short_desc" ::: 'Null PGtext
   ,"extra_desc" ::: 'Null PGtext
   ,"context" ::: 'Null PGtext
   ,"vartype" ::: 'Null PGtext
   ,"source" ::: 'Null PGtext
   ,"min_val" ::: 'Null PGtext
   ,"max_val" ::: 'Null PGtext
   ,"enumvals" ::: 'Null (PGvararray (NotNull PGtext))
   ,"boot_val" ::: 'Null PGtext
   ,"reset_val" ::: 'Null PGtext
   ,"sourcefile" ::: 'Null PGtext
   ,"sourceline" ::: 'Null PGint4
   ,"pending_restart" ::: 'Null PGbool]

type PgShadowView = 
  '["usename" ::: 'Null PGname
   ,"usesysid" ::: 'Null PGoid
   ,"usecreatedb" ::: 'Null PGbool
   ,"usesuper" ::: 'Null PGbool
   ,"userepl" ::: 'Null PGbool
   ,"usebypassrls" ::: 'Null PGbool
   ,"passwd" ::: 'Null PGtext
   ,"valuntil" ::: 'Null PGabstime
   ,"useconfig" ::: 'Null (PGvararray (NotNull PGtext))]

type PgStatActivityView = 
  '["datid" ::: 'Null PGoid
   ,"datname" ::: 'Null PGname
   ,"pid" ::: 'Null PGint4
   ,"usesysid" ::: 'Null PGoid
   ,"usename" ::: 'Null PGname
   ,"application_name" ::: 'Null PGtext
   ,"client_addr" ::: 'Null PGinet
   ,"client_hostname" ::: 'Null PGtext
   ,"client_port" ::: 'Null PGint4
   ,"backend_start" ::: 'Null PGtimestamptz
   ,"xact_start" ::: 'Null PGtimestamptz
   ,"query_start" ::: 'Null PGtimestamptz
   ,"state_change" ::: 'Null PGtimestamptz
   ,"waiting" ::: 'Null PGbool
   ,"state" ::: 'Null PGtext
   ,"backend_xid" ::: 'Null PGxid
   ,"backend_xmin" ::: 'Null PGxid
   ,"query" ::: 'Null PGtext]

type PgStatAllIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_read" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8]

type PgStatAllTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8
   ,"n_live_tup" ::: 'Null PGint8
   ,"n_dead_tup" ::: 'Null PGint8
   ,"n_mod_since_analyze" ::: 'Null PGint8
   ,"last_vacuum" ::: 'Null PGtimestamptz
   ,"last_autovacuum" ::: 'Null PGtimestamptz
   ,"last_analyze" ::: 'Null PGtimestamptz
   ,"last_autoanalyze" ::: 'Null PGtimestamptz
   ,"vacuum_count" ::: 'Null PGint8
   ,"autovacuum_count" ::: 'Null PGint8
   ,"analyze_count" ::: 'Null PGint8
   ,"autoanalyze_count" ::: 'Null PGint8]

type PgStatArchiverView = 
  '["archived_count" ::: 'Null PGint8
   ,"last_archived_wal" ::: 'Null PGtext
   ,"last_archived_time" ::: 'Null PGtimestamptz
   ,"failed_count" ::: 'Null PGint8
   ,"last_failed_wal" ::: 'Null PGtext
   ,"last_failed_time" ::: 'Null PGtimestamptz
   ,"stats_reset" ::: 'Null PGtimestamptz]

type PgStatBgwriterView = 
  '["checkpoints_timed" ::: 'Null PGint8
   ,"checkpoints_req" ::: 'Null PGint8
   ,"checkpoint_write_time" ::: 'Null PGfloat8
   ,"checkpoint_sync_time" ::: 'Null PGfloat8
   ,"buffers_checkpoint" ::: 'Null PGint8
   ,"buffers_clean" ::: 'Null PGint8
   ,"maxwritten_clean" ::: 'Null PGint8
   ,"buffers_backend" ::: 'Null PGint8
   ,"buffers_backend_fsync" ::: 'Null PGint8
   ,"buffers_alloc" ::: 'Null PGint8
   ,"stats_reset" ::: 'Null PGtimestamptz]

type PgStatDatabaseView = 
  '["datid" ::: 'Null PGoid
   ,"datname" ::: 'Null PGname
   ,"numbackends" ::: 'Null PGint4
   ,"xact_commit" ::: 'Null PGint8
   ,"xact_rollback" ::: 'Null PGint8
   ,"blks_read" ::: 'Null PGint8
   ,"blks_hit" ::: 'Null PGint8
   ,"tup_returned" ::: 'Null PGint8
   ,"tup_fetched" ::: 'Null PGint8
   ,"tup_inserted" ::: 'Null PGint8
   ,"tup_updated" ::: 'Null PGint8
   ,"tup_deleted" ::: 'Null PGint8
   ,"conflicts" ::: 'Null PGint8
   ,"temp_files" ::: 'Null PGint8
   ,"temp_bytes" ::: 'Null PGint8
   ,"deadlocks" ::: 'Null PGint8
   ,"blk_read_time" ::: 'Null PGfloat8
   ,"blk_write_time" ::: 'Null PGfloat8
   ,"stats_reset" ::: 'Null PGtimestamptz]

type PgStatDatabaseConflictsView = 
  '["datid" ::: 'Null PGoid
   ,"datname" ::: 'Null PGname
   ,"confl_tablespace" ::: 'Null PGint8
   ,"confl_lock" ::: 'Null PGint8
   ,"confl_snapshot" ::: 'Null PGint8
   ,"confl_bufferpin" ::: 'Null PGint8
   ,"confl_deadlock" ::: 'Null PGint8]

type PgStatReplicationView = 
  '["pid" ::: 'Null PGint4
   ,"usesysid" ::: 'Null PGoid
   ,"usename" ::: 'Null PGname
   ,"application_name" ::: 'Null PGtext
   ,"client_addr" ::: 'Null PGinet
   ,"client_hostname" ::: 'Null PGtext
   ,"client_port" ::: 'Null PGint4
   ,"backend_start" ::: 'Null PGtimestamptz
   ,"backend_xmin" ::: 'Null PGxid
   ,"state" ::: 'Null PGtext
   ,"sent_location" ::: 'Null PGpg_lsn
   ,"write_location" ::: 'Null PGpg_lsn
   ,"flush_location" ::: 'Null PGpg_lsn
   ,"replay_location" ::: 'Null PGpg_lsn
   ,"sync_priority" ::: 'Null PGint4
   ,"sync_state" ::: 'Null PGtext]

type PgStatSslView = 
  '["pid" ::: 'Null PGint4
   ,"ssl" ::: 'Null PGbool
   ,"version" ::: 'Null PGtext
   ,"cipher" ::: 'Null PGtext
   ,"bits" ::: 'Null PGint4
   ,"compression" ::: 'Null PGbool
   ,"clientdn" ::: 'Null PGtext]

type PgStatSysIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_read" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8]

type PgStatSysTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8
   ,"n_live_tup" ::: 'Null PGint8
   ,"n_dead_tup" ::: 'Null PGint8
   ,"n_mod_since_analyze" ::: 'Null PGint8
   ,"last_vacuum" ::: 'Null PGtimestamptz
   ,"last_autovacuum" ::: 'Null PGtimestamptz
   ,"last_analyze" ::: 'Null PGtimestamptz
   ,"last_autoanalyze" ::: 'Null PGtimestamptz
   ,"vacuum_count" ::: 'Null PGint8
   ,"autovacuum_count" ::: 'Null PGint8
   ,"analyze_count" ::: 'Null PGint8
   ,"autoanalyze_count" ::: 'Null PGint8]

type PgStatUserFunctionsView = 
  '["funcid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"funcname" ::: 'Null PGname
   ,"calls" ::: 'Null PGint8
   ,"total_time" ::: 'Null PGfloat8
   ,"self_time" ::: 'Null PGfloat8]

type PgStatUserIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_read" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8]

type PgStatUserTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8
   ,"n_live_tup" ::: 'Null PGint8
   ,"n_dead_tup" ::: 'Null PGint8
   ,"n_mod_since_analyze" ::: 'Null PGint8
   ,"last_vacuum" ::: 'Null PGtimestamptz
   ,"last_autovacuum" ::: 'Null PGtimestamptz
   ,"last_analyze" ::: 'Null PGtimestamptz
   ,"last_autoanalyze" ::: 'Null PGtimestamptz
   ,"vacuum_count" ::: 'Null PGint8
   ,"autovacuum_count" ::: 'Null PGint8
   ,"analyze_count" ::: 'Null PGint8
   ,"autoanalyze_count" ::: 'Null PGint8]

type PgStatXactAllTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8]

type PgStatXactSysTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8]

type PgStatXactUserFunctionsView = 
  '["funcid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"funcname" ::: 'Null PGname
   ,"calls" ::: 'Null PGint8
   ,"total_time" ::: 'Null PGfloat8
   ,"self_time" ::: 'Null PGfloat8]

type PgStatXactUserTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"seq_scan" ::: 'Null PGint8
   ,"seq_tup_read" ::: 'Null PGint8
   ,"idx_scan" ::: 'Null PGint8
   ,"idx_tup_fetch" ::: 'Null PGint8
   ,"n_tup_ins" ::: 'Null PGint8
   ,"n_tup_upd" ::: 'Null PGint8
   ,"n_tup_del" ::: 'Null PGint8
   ,"n_tup_hot_upd" ::: 'Null PGint8]

type PgStatioAllIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8]

type PgStatioAllSequencesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"blks_read" ::: 'Null PGint8
   ,"blks_hit" ::: 'Null PGint8]

type PgStatioAllTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"heap_blks_read" ::: 'Null PGint8
   ,"heap_blks_hit" ::: 'Null PGint8
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8
   ,"toast_blks_read" ::: 'Null PGint8
   ,"toast_blks_hit" ::: 'Null PGint8
   ,"tidx_blks_read" ::: 'Null PGint8
   ,"tidx_blks_hit" ::: 'Null PGint8]

type PgStatioSysIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8]

type PgStatioSysSequencesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"blks_read" ::: 'Null PGint8
   ,"blks_hit" ::: 'Null PGint8]

type PgStatioSysTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"heap_blks_read" ::: 'Null PGint8
   ,"heap_blks_hit" ::: 'Null PGint8
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8
   ,"toast_blks_read" ::: 'Null PGint8
   ,"toast_blks_hit" ::: 'Null PGint8
   ,"tidx_blks_read" ::: 'Null PGint8
   ,"tidx_blks_hit" ::: 'Null PGint8]

type PgStatioUserIndexesView = 
  '["relid" ::: 'Null PGoid
   ,"indexrelid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"indexrelname" ::: 'Null PGname
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8]

type PgStatioUserSequencesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"blks_read" ::: 'Null PGint8
   ,"blks_hit" ::: 'Null PGint8]

type PgStatioUserTablesView = 
  '["relid" ::: 'Null PGoid
   ,"schemaname" ::: 'Null PGname
   ,"relname" ::: 'Null PGname
   ,"heap_blks_read" ::: 'Null PGint8
   ,"heap_blks_hit" ::: 'Null PGint8
   ,"idx_blks_read" ::: 'Null PGint8
   ,"idx_blks_hit" ::: 'Null PGint8
   ,"toast_blks_read" ::: 'Null PGint8
   ,"toast_blks_hit" ::: 'Null PGint8
   ,"tidx_blks_read" ::: 'Null PGint8
   ,"tidx_blks_hit" ::: 'Null PGint8]

type PgStatsView = 
  '["schemaname" ::: 'Null PGname
   ,"tablename" ::: 'Null PGname
   ,"attname" ::: 'Null PGname
   ,"inherited" ::: 'Null PGbool
   ,"null_frac" ::: 'Null PGfloat4
   ,"avg_width" ::: 'Null PGint4
   ,"n_distinct" ::: 'Null PGfloat4
   ,"most_common_vals" ::: 'Null PGanyarray
   ,"most_common_freqs" ::: 'Null (PGvararray (NotNull PGfloat4))
   ,"histogram_bounds" ::: 'Null PGanyarray
   ,"correlation" ::: 'Null PGfloat4
   ,"most_common_elems" ::: 'Null PGanyarray
   ,"most_common_elem_freqs" ::: 'Null (PGvararray (NotNull PGfloat4))
   ,"elem_count_histogram" ::: 'Null (PGvararray (NotNull PGfloat4))]

type PgTablesView = 
  '["schemaname" ::: 'Null PGname
   ,"tablename" ::: 'Null PGname
   ,"tableowner" ::: 'Null PGname
   ,"tablespace" ::: 'Null PGname
   ,"hasindexes" ::: 'Null PGbool
   ,"hasrules" ::: 'Null PGbool
   ,"hastriggers" ::: 'Null PGbool
   ,"rowsecurity" ::: 'Null PGbool]

type PgTimezoneAbbrevsView = 
  '["abbrev" ::: 'Null PGtext
   ,"utc_offset" ::: 'Null PGinterval
   ,"is_dst" ::: 'Null PGbool]

type PgTimezoneNamesView = 
  '["name" ::: 'Null PGtext
   ,"abbrev" ::: 'Null PGtext
   ,"utc_offset" ::: 'Null PGinterval
   ,"is_dst" ::: 'Null PGbool]

type PgUserView = 
  '["usename" ::: 'Null PGname
   ,"usesysid" ::: 'Null PGoid
   ,"usecreatedb" ::: 'Null PGbool
   ,"usesuper" ::: 'Null PGbool
   ,"userepl" ::: 'Null PGbool
   ,"usebypassrls" ::: 'Null PGbool
   ,"passwd" ::: 'Null PGtext
   ,"valuntil" ::: 'Null PGabstime
   ,"useconfig" ::: 'Null (PGvararray (NotNull PGtext))]

type PgUserMappingsView = 
  '["umid" ::: 'Null PGoid
   ,"srvid" ::: 'Null PGoid
   ,"srvname" ::: 'Null PGname
   ,"umuser" ::: 'Null PGoid
   ,"usename" ::: 'Null PGname
   ,"umoptions" ::: 'Null (PGvararray (NotNull PGtext))]

type PgViewsView = 
  '["schemaname" ::: 'Null PGname
   ,"viewname" ::: 'Null PGname
   ,"viewowner" ::: 'Null PGname
   ,"definition" ::: 'Null PGtext]

-- functions
type Functions = 
  '[ "abstimeeq" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimege" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimegt" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimele" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimelt" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimene" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGbool) )
   , "abstimesend" ::: Function ('[ NotNull PGabstime ] :=> 'Returns ( 'Null PGbytea) )
   , "aclcontains" ::: Function ('[ NotNull (PGvararray (NotNull PGaclitem)),  NotNull PGaclitem ] :=> 'Returns ( 'Null PGbool) )
   , "acldefault" ::: Function ('[ 'NotNull (PGchar 1),  NotNull PGoid ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGaclitem))) )
   , "aclinsert" ::: Function ('[ NotNull (PGvararray (NotNull PGaclitem)),  NotNull PGaclitem ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGaclitem))) )
   , "aclitemeq" ::: Function ('[ NotNull PGaclitem,  NotNull PGaclitem ] :=> 'Returns ( 'Null PGbool) )
   , "aclremove" ::: Function ('[ NotNull (PGvararray (NotNull PGaclitem)),  NotNull PGaclitem ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGaclitem))) )
   , "acos" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "ascii" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "asin" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "atan" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "atan2" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "bit_send" ::: Function ('[ NotNull PGbit ] :=> 'Returns ( 'Null PGbytea) )
   , "bitand" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbit) )
   , "bitcat" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGvarbit) )
   , "bitcmp" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGint4) )
   , "biteq" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitge" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitgt" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitle" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitlt" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitne" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbool) )
   , "bitnot" ::: Function ('[ NotNull PGbit ] :=> 'Returns ( 'Null PGbit) )
   , "bitor" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbit) )
   , "bitshiftleft" ::: Function ('[ NotNull PGbit,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbit) )
   , "bitshiftright" ::: Function ('[ NotNull PGbit,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbit) )
   , "bittypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "bitxor" ::: Function ('[ NotNull PGbit,  NotNull PGbit ] :=> 'Returns ( 'Null PGbit) )
   , "bool" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "bool_and" ::: Function ('[ Null PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "bool_or" ::: Function ('[ Null PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "booland_statefunc" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "booleq" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolge" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolgt" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolle" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boollt" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolne" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolor_statefunc" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "boolsend" ::: Function ('[ NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "bound_box" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbox) )
   , "box_above" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_above_eq" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_add" ::: Function ('[ NotNull PGbox,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbox) )
   , "box_below" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_below_eq" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_center" ::: Function ('[ NotNull PGbox ] :=> 'Returns ( 'Null PGpoint) )
   , "box_contain" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_contain_pt" ::: Function ('[ NotNull PGbox,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "box_contained" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_distance" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "box_div" ::: Function ('[ NotNull PGbox,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbox) )
   , "box_eq" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_ge" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_gt" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_intersect" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbox) )
   , "box_le" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_left" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_lt" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_mul" ::: Function ('[ NotNull PGbox,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbox) )
   , "box_overabove" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_overbelow" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_overlap" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_overleft" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_overright" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_right" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_same" ::: Function ('[ NotNull PGbox,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "box_send" ::: Function ('[ NotNull PGbox ] :=> 'Returns ( 'Null PGbytea) )
   , "box_sub" ::: Function ('[ NotNull PGbox,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbox) )
   , "bpchar_larger" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbpchar) )
   , "bpchar_pattern_ge" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchar_pattern_gt" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchar_pattern_le" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchar_pattern_lt" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchar_smaller" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbpchar) )
   , "bpcharcmp" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGint4) )
   , "bpchareq" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharge" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchargt" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpchariclike" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharicnlike" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharicregexeq" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharicregexne" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharle" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharlike" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharlt" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharne" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharnlike" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharregexeq" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharregexne" ::: Function ('[ NotNull PGbpchar,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "bpcharsend" ::: Function ('[ NotNull PGbpchar ] :=> 'Returns ( 'Null PGbytea) )
   , "bpchartypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "brin_summarize_new_values" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint4) )
   , "brinoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "broadcast" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "btabstimecmp" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGint4) )
   , "btboolcmp" ::: Function ('[ NotNull PGbool,  NotNull PGbool ] :=> 'Returns ( 'Null PGint4) )
   , "btbpchar_pattern_cmp" ::: Function ('[ NotNull PGbpchar,  NotNull PGbpchar ] :=> 'Returns ( 'Null PGint4) )
   , "btcharcmp" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGint4) )
   , "btfloat48cmp" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGint4) )
   , "btfloat4cmp" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGint4) )
   , "btfloat84cmp" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGint4) )
   , "btfloat8cmp" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGint4) )
   , "btint24cmp" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "btint28cmp" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint4) )
   , "btint2cmp" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "btint42cmp" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "btint48cmp" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint4) )
   , "btint4cmp" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "btint82cmp" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "btint84cmp" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "btint8cmp" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint4) )
   , "btnamecmp" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGint4) )
   , "btoidcmp" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGint4) )
   , "btoidvectorcmp" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGint4) )
   , "btoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "btreltimecmp" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGint4) )
   , "bttext_pattern_cmp" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "bttextcmp" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "bttidcmp" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGint4) )
   , "bttintervalcmp" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGint4) )
   , "byteacat" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbytea) )
   , "byteacmp" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGint4) )
   , "byteaeq" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteage" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteagt" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteale" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "bytealike" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "bytealt" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteane" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteanlike" ::: Function ('[ NotNull PGbytea,  NotNull PGbytea ] :=> 'Returns ( 'Null PGbool) )
   , "byteasend" ::: Function ('[ NotNull PGbytea ] :=> 'Returns ( 'Null PGbytea) )
   , "cash_cmp" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGint4) )
   , "cash_div_cash" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGfloat8) )
   , "cash_div_flt4" ::: Function ('[ NotNull PGmoney,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_div_flt8" ::: Function ('[ NotNull PGmoney,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_div_int2" ::: Function ('[ NotNull PGmoney,  NotNull PGint2 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_div_int4" ::: Function ('[ NotNull PGmoney,  NotNull PGint4 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_eq" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_ge" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_gt" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_le" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_lt" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_mi" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_mul_flt4" ::: Function ('[ NotNull PGmoney,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_mul_flt8" ::: Function ('[ NotNull PGmoney,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_mul_int2" ::: Function ('[ NotNull PGmoney,  NotNull PGint2 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_mul_int4" ::: Function ('[ NotNull PGmoney,  NotNull PGint4 ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_ne" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGbool) )
   , "cash_pl" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "cash_send" ::: Function ('[ NotNull PGmoney ] :=> 'Returns ( 'Null PGbytea) )
   , "cash_words" ::: Function ('[ NotNull PGmoney ] :=> 'Returns ( 'Null PGtext) )
   , "cashlarger" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "cashsmaller" ::: Function ('[ NotNull PGmoney,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "cbrt" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "chareq" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "charge" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "chargt" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "charle" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "charlt" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "charne" ::: Function ('[ 'NotNull (PGchar 1),  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbool) )
   , "charsend" ::: Function ('[ 'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGbytea) )
   , "chr" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "cideq" ::: Function ('[ NotNull PGcid,  NotNull PGcid ] :=> 'Returns ( 'Null PGbool) )
   , "cidr" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGcidr) )
   , "cidr_send" ::: Function ('[ NotNull PGcidr ] :=> 'Returns ( 'Null PGbytea) )
   , "cidsend" ::: Function ('[ NotNull PGcid ] :=> 'Returns ( 'Null PGbytea) )
   , "circle_above" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_add_pt" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGcircle) )
   , "circle_below" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_center" ::: Function ('[ NotNull PGcircle ] :=> 'Returns ( 'Null PGpoint) )
   , "circle_contain" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_contain_pt" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "circle_contained" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_distance" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGfloat8) )
   , "circle_div_pt" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGcircle) )
   , "circle_eq" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_ge" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_gt" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_le" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_left" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_lt" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_mul_pt" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGcircle) )
   , "circle_ne" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_overabove" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_overbelow" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_overlap" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_overleft" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_overright" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_right" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_same" ::: Function ('[ NotNull PGcircle,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "circle_send" ::: Function ('[ NotNull PGcircle ] :=> 'Returns ( 'Null PGbytea) )
   , "circle_sub_pt" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGcircle) )
   , "close_lb" ::: Function ('[ NotNull PGline,  NotNull PGbox ] :=> 'Returns ( 'Null PGpoint) )
   , "close_ls" ::: Function ('[ NotNull PGline,  NotNull PGlseg ] :=> 'Returns ( 'Null PGpoint) )
   , "close_lseg" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGpoint) )
   , "close_pb" ::: Function ('[ NotNull PGpoint,  NotNull PGbox ] :=> 'Returns ( 'Null PGpoint) )
   , "close_pl" ::: Function ('[ NotNull PGpoint,  NotNull PGline ] :=> 'Returns ( 'Null PGpoint) )
   , "close_ps" ::: Function ('[ NotNull PGpoint,  NotNull PGlseg ] :=> 'Returns ( 'Null PGpoint) )
   , "close_sb" ::: Function ('[ NotNull PGlseg,  NotNull PGbox ] :=> 'Returns ( 'Null PGpoint) )
   , "close_sl" ::: Function ('[ NotNull PGlseg,  NotNull PGline ] :=> 'Returns ( 'Null PGpoint) )
   , "col_description" ::: Function ('[ NotNull PGoid,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "convert" ::: Function ('[ NotNull PGbytea,  NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbytea) )
   , "convert_from" ::: Function ('[ NotNull PGbytea,  NotNull PGname ] :=> 'Returns ( 'Null PGtext) )
   , "convert_to" ::: Function ('[ NotNull PGtext,  NotNull PGname ] :=> 'Returns ( 'Null PGbytea) )
   , "corr" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "cos" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "cot" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "covar_pop" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "covar_samp" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "current_schemas" ::: Function ('[ NotNull PGbool ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGname))) )
   , "current_setting" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "currtid" ::: Function ('[ NotNull PGoid,  NotNull PGtid ] :=> 'Returns ( 'Null PGtid) )
   , "currtid2" ::: Function ('[ NotNull PGtext,  NotNull PGtid ] :=> 'Returns ( 'Null PGtid) )
   , "currval" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint8) )
   , "cursor_to_xml" ::: Function ('[ NotNull PGrefcursor,  NotNull PGint4,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "cursor_to_xmlschema" ::: Function ('[ NotNull PGrefcursor,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "database_to_xml" ::: Function ('[ NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "database_to_xml_and_xmlschema" ::: Function ('[ NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "database_to_xmlschema" ::: Function ('[ NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "date_cmp" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGint4) )
   , "date_cmp_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGint4) )
   , "date_cmp_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGint4) )
   , "date_eq" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_eq_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_eq_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_ge" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_ge_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_ge_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_gt" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_gt_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_gt_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_larger" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGdate) )
   , "date_le" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_le_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_le_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_lt" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_lt_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_lt_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_mi" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGint4) )
   , "date_mi_interval" ::: Function ('[ NotNull PGdate,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamp) )
   , "date_mii" ::: Function ('[ NotNull PGdate,  NotNull PGint4 ] :=> 'Returns ( 'Null PGdate) )
   , "date_ne" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "date_ne_timestamp" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "date_ne_timestamptz" ::: Function ('[ NotNull PGdate,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "date_pl_interval" ::: Function ('[ NotNull PGdate,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamp) )
   , "date_pli" ::: Function ('[ NotNull PGdate,  NotNull PGint4 ] :=> 'Returns ( 'Null PGdate) )
   , "date_send" ::: Function ('[ NotNull PGdate ] :=> 'Returns ( 'Null PGbytea) )
   , "date_smaller" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGdate) )
   , "daterange_canonical" ::: Function ('[ NotNull ('PGrange PGdate) ] :=> 'Returns ( 'Null ('PGrange PGdate)) )
   , "daterange_subdiff" ::: Function ('[ NotNull PGdate,  NotNull PGdate ] :=> 'Returns ( 'Null PGfloat8) )
   , "datetime_pl" ::: Function ('[ NotNull PGdate,  NotNull PGtime ] :=> 'Returns ( 'Null PGtimestamp) )
   , "datetimetz_pl" ::: Function ('[ NotNull PGdate,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "dcbrt" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "decode" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbytea) )
   , "degrees" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dexp" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "diagonal" ::: Function ('[ NotNull PGbox ] :=> 'Returns ( 'Null PGlseg) )
   , "diameter" ::: Function ('[ NotNull PGcircle ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_cpoint" ::: Function ('[ NotNull PGcircle,  NotNull PGpoint ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_cpoly" ::: Function ('[ NotNull PGcircle,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_lb" ::: Function ('[ NotNull PGline,  NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_pb" ::: Function ('[ NotNull PGpoint,  NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_pc" ::: Function ('[ NotNull PGpoint,  NotNull PGcircle ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_pl" ::: Function ('[ NotNull PGpoint,  NotNull PGline ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_polyp" ::: Function ('[ NotNull PGpolygon,  NotNull PGpoint ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_ppath" ::: Function ('[ NotNull PGpoint,  NotNull PGpath ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_ppoly" ::: Function ('[ NotNull PGpoint,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_ps" ::: Function ('[ NotNull PGpoint,  NotNull PGlseg ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_sb" ::: Function ('[ NotNull PGlseg,  NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "dist_sl" ::: Function ('[ NotNull PGlseg,  NotNull PGline ] :=> 'Returns ( 'Null PGfloat8) )
   , "div" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "dlog1" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dlog10" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dpow" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dround" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dsqrt" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "dtrunc" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "encode" ::: Function ('[ NotNull PGbytea,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "every" ::: Function ('[ Null PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "factorial" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGnumeric) )
   , "family" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGint4) )
   , "float48div" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float48eq" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48ge" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48gt" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48le" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48lt" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48mi" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float48mul" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float48ne" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float48pl" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float4_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)),  NotNull PGfloat4 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGfloat8))) )
   , "float4abs" ::: Function ('[ NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4div" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4eq" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4ge" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4gt" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4larger" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4le" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4lt" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4mi" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4mul" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4ne" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float4pl" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4send" ::: Function ('[ NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbytea) )
   , "float4smaller" ::: Function ('[ NotNull PGfloat4,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4um" ::: Function ('[ NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float4up" ::: Function ('[ NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat4) )
   , "float84div" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float84eq" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84ge" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84gt" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84le" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84lt" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84mi" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float84mul" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float84ne" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGbool) )
   , "float84pl" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat4 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)),  NotNull PGfloat8 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGfloat8))) )
   , "float8_avg" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_corr" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_covar_pop" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_covar_samp" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)),  NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGfloat8))) )
   , "float8_regr_avgx" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_avgy" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_intercept" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_r2" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_slope" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_sxx" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_sxy" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_regr_syy" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_stddev_pop" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_stddev_samp" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_var_pop" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8_var_samp" ::: Function ('[ NotNull (PGvararray (NotNull PGfloat8)) ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8abs" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8div" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8eq" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8ge" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8gt" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8larger" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8le" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8lt" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8mi" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8mul" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8ne" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbool) )
   , "float8pl" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8send" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGbytea) )
   , "float8smaller" ::: Function ('[ NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8um" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "float8up" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "flt4_mul_cash" ::: Function ('[ NotNull PGfloat4,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "flt8_mul_cash" ::: Function ('[ NotNull PGfloat8,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "format_type" ::: Function ('[ Null PGoid,  Null PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "get_byte" ::: Function ('[ NotNull PGbytea,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "gin_cmp_tslexeme" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "gin_compare_jsonb" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "ginoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "gistoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "hash_aclitem" ::: Function ('[ NotNull PGaclitem ] :=> 'Returns ( 'Null PGint4) )
   , "hash_numeric" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGint4) )
   , "hashbpchar" ::: Function ('[ NotNull PGbpchar ] :=> 'Returns ( 'Null PGint4) )
   , "hashchar" ::: Function ('[ 'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGint4) )
   , "hashfloat4" ::: Function ('[ NotNull PGfloat4 ] :=> 'Returns ( 'Null PGint4) )
   , "hashfloat8" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGint4) )
   , "hashinet" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGint4) )
   , "hashint2" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "hashint2vector" ::: Function ('[ NotNull (PGvararray (NotNull PGint2vector)) ] :=> 'Returns ( 'Null PGint4) )
   , "hashint4" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "hashint8" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint4) )
   , "hashmacaddr" ::: Function ('[ NotNull PGmacaddr ] :=> 'Returns ( 'Null PGint4) )
   , "hashname" ::: Function ('[ NotNull PGname ] :=> 'Returns ( 'Null PGint4) )
   , "hashoid" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint4) )
   , "hashoidvector" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGint4) )
   , "hashoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "hashtext" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "height" ::: Function ('[ NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "host" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGtext) )
   , "hostmask" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "inet_merge" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGcidr) )
   , "inet_same_family" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "inet_send" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGbytea) )
   , "inetand" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "inetmi" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGint8) )
   , "inetmi_int8" ::: Function ('[ NotNull PGinet,  NotNull PGint8 ] :=> 'Returns ( 'Null PGinet) )
   , "inetnot" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "inetor" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "inetpl" ::: Function ('[ NotNull PGinet,  NotNull PGint8 ] :=> 'Returns ( 'Null PGinet) )
   , "initcap" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "int24div" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int24eq" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24ge" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24gt" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24le" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24lt" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24mi" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int24mul" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int24ne" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int24pl" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int28div" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int28eq" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28ge" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28gt" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28le" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28lt" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28mi" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int28mul" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int28ne" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int28pl" ::: Function ('[ NotNull PGint2,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int2_avg_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)),  NotNull PGint2 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGint8))) )
   , "int2_avg_accum_inv" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)),  NotNull PGint2 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGint8))) )
   , "int2_mul_cash" ::: Function ('[ NotNull PGint2,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "int2_sum" ::: Function ('[ Null PGint8,  Null PGint2 ] :=> 'Returns ( 'Null PGint8) )
   , "int2abs" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2and" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2div" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2eq" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2ge" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2gt" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2int4_sum" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)) ] :=> 'Returns ( 'Null PGint8) )
   , "int2larger" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2le" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2lt" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2mi" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2mod" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2mul" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2ne" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int2not" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2or" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2pl" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2send" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGbytea) )
   , "int2shl" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint2) )
   , "int2shr" ::: Function ('[ NotNull PGint2,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint2) )
   , "int2smaller" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2um" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2up" ::: Function ('[ NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int2vectoreq" ::: Function ('[ NotNull (PGvararray (NotNull PGint2vector)),  NotNull (PGvararray (NotNull PGint2vector)) ] :=> 'Returns ( 'Null PGbool) )
   , "int2vectorsend" ::: Function ('[ NotNull (PGvararray (NotNull PGint2vector)) ] :=> 'Returns ( 'Null PGbytea) )
   , "int2xor" ::: Function ('[ NotNull PGint2,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint2) )
   , "int42div" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "int42eq" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42ge" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42gt" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42le" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42lt" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42mi" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "int42mul" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "int42ne" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int42pl" ::: Function ('[ NotNull PGint4,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint4) )
   , "int48div" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int48eq" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48ge" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48gt" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48le" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48lt" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48mi" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int48mul" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int48ne" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int48pl" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int4_avg_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)),  NotNull PGint4 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGint8))) )
   , "int4_avg_accum_inv" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)),  NotNull PGint4 ] :=> 'Returns ( 'Null ('PGvararray ('Null 'PGint8))) )
   , "int4_mul_cash" ::: Function ('[ NotNull PGint4,  NotNull PGmoney ] :=> 'Returns ( 'Null PGmoney) )
   , "int4_sum" ::: Function ('[ Null PGint8,  Null PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int4abs" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4and" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4div" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4eq" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4ge" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4gt" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4inc" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4larger" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4le" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4lt" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4mi" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4mod" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4mul" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4ne" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int4not" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4or" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4pl" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4range_canonical" ::: Function ('[ NotNull ('PGrange 'PGint4) ] :=> 'Returns ( 'Null ('PGrange 'PGint4)) )
   , "int4range_subdiff" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGfloat8) )
   , "int4send" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGbytea) )
   , "int4shl" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4shr" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4smaller" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4um" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4up" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int4xor" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "int82div" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint8) )
   , "int82eq" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82ge" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82gt" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82le" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82lt" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82mi" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint8) )
   , "int82mul" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint8) )
   , "int82ne" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGbool) )
   , "int82pl" ::: Function ('[ NotNull PGint8,  NotNull PGint2 ] :=> 'Returns ( 'Null PGint8) )
   , "int84div" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int84eq" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84ge" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84gt" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84le" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84lt" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84mi" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int84mul" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int84ne" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "int84pl" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int8_avg" ::: Function ('[ NotNull (PGvararray (NotNull PGint8)) ] :=> 'Returns ( 'Null PGnumeric) )
   , "int8_sum" ::: Function ('[ Null PGnumeric,  Null PGint8 ] :=> 'Returns ( 'Null PGnumeric) )
   , "int8abs" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8and" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8dec" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8div" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8eq" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8ge" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8gt" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8inc" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8inc_float8_float8" ::: Function ('[ NotNull PGint8,  NotNull PGfloat8,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8larger" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8le" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8lt" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8mi" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8mod" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8mul" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8ne" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGbool) )
   , "int8not" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8or" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8pl" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8pl_inet" ::: Function ('[ NotNull PGint8,  NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "int8range_canonical" ::: Function ('[ NotNull ('PGrange 'PGint8) ] :=> 'Returns ( 'Null ('PGrange 'PGint8)) )
   , "int8range_subdiff" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "int8send" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGbytea) )
   , "int8shl" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int8shr" ::: Function ('[ NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "int8smaller" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8um" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8up" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "int8xor" ::: Function ('[ NotNull PGint8,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint8) )
   , "integer_pl_date" ::: Function ('[ NotNull PGint4,  NotNull PGdate ] :=> 'Returns ( 'Null PGdate) )
   , "inter_lb" ::: Function ('[ NotNull PGline,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "inter_sb" ::: Function ('[ NotNull PGlseg,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "inter_sl" ::: Function ('[ NotNull PGlseg,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "interval_accum" ::: Function ('[ NotNull (PGvararray (NotNull PGinterval)),  NotNull PGinterval ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGinterval))) )
   , "interval_accum_inv" ::: Function ('[ NotNull (PGvararray (NotNull PGinterval)),  NotNull PGinterval ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGinterval))) )
   , "interval_avg" ::: Function ('[ NotNull (PGvararray (NotNull PGinterval)) ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_cmp" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGint4) )
   , "interval_div" ::: Function ('[ NotNull PGinterval,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_eq" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_ge" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_gt" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_hash" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGint4) )
   , "interval_larger" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_le" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_lt" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_mi" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_mul" ::: Function ('[ NotNull PGinterval,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_ne" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGbool) )
   , "interval_pl" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_pl_date" ::: Function ('[ NotNull PGinterval,  NotNull PGdate ] :=> 'Returns ( 'Null PGtimestamp) )
   , "interval_pl_time" ::: Function ('[ NotNull PGinterval,  NotNull PGtime ] :=> 'Returns ( 'Null PGtime) )
   , "interval_pl_timestamp" ::: Function ('[ NotNull PGinterval,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGtimestamp) )
   , "interval_pl_timestamptz" ::: Function ('[ NotNull PGinterval,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "interval_pl_timetz" ::: Function ('[ NotNull PGinterval,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGtimetz) )
   , "interval_send" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGbytea) )
   , "interval_smaller" ::: Function ('[ NotNull PGinterval,  NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "interval_um" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "intervaltypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "intinterval" ::: Function ('[ NotNull PGabstime,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "isclosed" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "isopen" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "json_array_element" ::: Function ('[ NotNull PGjson,  NotNull PGint4 ] :=> 'Returns ( 'Null PGjson) )
   , "json_array_element_text" ::: Function ('[ NotNull PGjson,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "json_array_elements" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGjson) )
   , "json_array_elements_text" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGtext) )
   , "json_array_length" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGint4) )
   , "json_extract_path" ::: Function ('[ NotNull PGjson,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGjson) )
   , "json_extract_path_text" ::: Function ('[ NotNull PGjson,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGtext) )
   , "json_object_field" ::: Function ('[ NotNull PGjson,  NotNull PGtext ] :=> 'Returns ( 'Null PGjson) )
   , "json_object_field_text" ::: Function ('[ NotNull PGjson,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "json_object_keys" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGtext) )
   , "json_send" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGbytea) )
   , "json_strip_nulls" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGjson) )
   , "json_typeof" ::: Function ('[ NotNull PGjson ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_array_element" ::: Function ('[ NotNull PGjsonb,  NotNull PGint4 ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_array_element_text" ::: Function ('[ NotNull PGjsonb,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_array_elements" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_array_elements_text" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_array_length" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGint4) )
   , "jsonb_cmp" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGint4) )
   , "jsonb_concat" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_contained" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_contains" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_delete_path" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_eq" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_exists" ::: Function ('[ NotNull PGjsonb,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_exists_all" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_exists_any" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_extract_path" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_extract_path_text" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)) ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_ge" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_gt" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_hash" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGint4) )
   , "jsonb_le" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_lt" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_ne" ::: Function ('[ NotNull PGjsonb,  NotNull PGjsonb ] :=> 'Returns ( 'Null PGbool) )
   , "jsonb_object_field" ::: Function ('[ NotNull PGjsonb,  NotNull PGtext ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_object_field_text" ::: Function ('[ NotNull PGjsonb,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_object_keys" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_pretty" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGtext) )
   , "jsonb_send" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGbytea) )
   , "jsonb_set" ::: Function ('[ NotNull PGjsonb,  NotNull (PGvararray (NotNull PGtext)),  NotNull PGjsonb,  NotNull PGbool ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_strip_nulls" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGjsonb) )
   , "jsonb_typeof" ::: Function ('[ NotNull PGjsonb ] :=> 'Returns ( 'Null PGtext) )
   , "justify_days" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "justify_hours" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "justify_interval" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "left" ::: Function ('[ NotNull PGtext,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "line" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGline) )
   , "line_distance" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGfloat8) )
   , "line_eq" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "line_horizontal" ::: Function ('[ NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "line_interpt" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGpoint) )
   , "line_intersect" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "line_parallel" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "line_perp" ::: Function ('[ NotNull PGline,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "line_send" ::: Function ('[ NotNull PGline ] :=> 'Returns ( 'Null PGbytea) )
   , "line_vertical" ::: Function ('[ NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "lo_close" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_creat" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGoid) )
   , "lo_create" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGoid) )
   , "lo_export" ::: Function ('[ NotNull PGoid,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "lo_from_bytea" ::: Function ('[ NotNull PGoid,  NotNull PGbytea ] :=> 'Returns ( 'Null PGoid) )
   , "lo_lseek" ::: Function ('[ NotNull PGint4,  NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_lseek64" ::: Function ('[ NotNull PGint4,  NotNull PGint8,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "lo_open" ::: Function ('[ NotNull PGoid,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_tell" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_tell64" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint8) )
   , "lo_truncate" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_truncate64" ::: Function ('[ NotNull PGint4,  NotNull PGint8 ] :=> 'Returns ( 'Null PGint4) )
   , "lo_unlink" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint4) )
   , "loread" ::: Function ('[ NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbytea) )
   , "lowrite" ::: Function ('[ NotNull PGint4,  NotNull PGbytea ] :=> 'Returns ( 'Null PGint4) )
   , "lseg_center" ::: Function ('[ NotNull PGlseg ] :=> 'Returns ( 'Null PGpoint) )
   , "lseg_distance" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGfloat8) )
   , "lseg_eq" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_ge" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_gt" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_horizontal" ::: Function ('[ NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_interpt" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGpoint) )
   , "lseg_intersect" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_le" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_length" ::: Function ('[ NotNull PGlseg ] :=> 'Returns ( 'Null PGfloat8) )
   , "lseg_lt" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_ne" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_parallel" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_perp" ::: Function ('[ NotNull PGlseg,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "lseg_send" ::: Function ('[ NotNull PGlseg ] :=> 'Returns ( 'Null PGbytea) )
   , "lseg_vertical" ::: Function ('[ NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_and" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGmacaddr) )
   , "macaddr_cmp" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGint4) )
   , "macaddr_eq" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_ge" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_gt" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_le" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_lt" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_ne" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbool) )
   , "macaddr_not" ::: Function ('[ NotNull PGmacaddr ] :=> 'Returns ( 'Null PGmacaddr) )
   , "macaddr_or" ::: Function ('[ NotNull PGmacaddr,  NotNull PGmacaddr ] :=> 'Returns ( 'Null PGmacaddr) )
   , "macaddr_send" ::: Function ('[ NotNull PGmacaddr ] :=> 'Returns ( 'Null PGbytea) )
   , "make_date" ::: Function ('[ NotNull PGint4,  NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGdate) )
   , "make_interval" ::: Function ('[ NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGinterval) )
   , "make_time" ::: Function ('[ NotNull PGint4,  NotNull PGint4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGtime) )
   , "make_timestamp" ::: Function ('[ NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGint4,  NotNull PGfloat8 ] :=> 'Returns ( 'Null PGtimestamp) )
   , "makeaclitem" ::: Function ('[ NotNull PGoid,  NotNull PGoid,  NotNull PGtext,  NotNull PGbool ] :=> 'Returns ( 'Null PGaclitem) )
   , "masklen" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGint4) )
   , "mktinterval" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGtinterval) )
   , "mul_d_interval" ::: Function ('[ NotNull PGfloat8,  NotNull PGinterval ] :=> 'Returns ( 'Null PGinterval) )
   , "mxid_age" ::: Function ('[ NotNull PGxid ] :=> 'Returns ( 'Null PGint4) )
   , "nameeq" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "namege" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "namegt" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "nameiclike" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "nameicnlike" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "nameicregexeq" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "nameicregexne" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "namele" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "namelike" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "namelt" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "namene" ::: Function ('[ NotNull PGname,  NotNull PGname ] :=> 'Returns ( 'Null PGbool) )
   , "namenlike" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "nameregexeq" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "nameregexne" ::: Function ('[ NotNull PGname,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "namesend" ::: Function ('[ NotNull PGname ] :=> 'Returns ( 'Null PGbytea) )
   , "netmask" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "network" ::: Function ('[ NotNull PGinet ] :=> 'Returns ( 'Null PGcidr) )
   , "network_cmp" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGint4) )
   , "network_eq" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_ge" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_gt" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_larger" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "network_le" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_lt" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_ne" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_overlap" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_smaller" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGinet) )
   , "network_sub" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_subeq" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_sup" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "network_supeq" ::: Function ('[ NotNull PGinet,  NotNull PGinet ] :=> 'Returns ( 'Null PGbool) )
   , "nextval" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint8) )
   , "ntile" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "numeric_abs" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_add" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_cmp" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGint4) )
   , "numeric_div" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_div_trunc" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_eq" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_exp" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_fac" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_ge" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_gt" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_inc" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_larger" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_le" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_ln" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_log" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_lt" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_mod" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_mul" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_ne" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGbool) )
   , "numeric_power" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_send" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGbytea) )
   , "numeric_smaller" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_sqrt" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_sub" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_uminus" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numeric_uplus" ::: Function ('[ NotNull PGnumeric ] :=> 'Returns ( 'Null PGnumeric) )
   , "numerictypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "numnode" ::: Function ('[ NotNull PGtsquery ] :=> 'Returns ( 'Null PGint4) )
   , "numrange_subdiff" ::: Function ('[ NotNull PGnumeric,  NotNull PGnumeric ] :=> 'Returns ( 'Null PGfloat8) )
   , "oid" ::: Function ('[ NotNull PGint8 ] :=> 'Returns ( 'Null PGoid) )
   , "oideq" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidge" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidgt" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidlarger" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGoid) )
   , "oidle" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidlt" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidne" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "oidsend" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbytea) )
   , "oidsmaller" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGoid) )
   , "oidvectoreq" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorge" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorgt" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorle" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorlt" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorne" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)),  NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbool) )
   , "oidvectorsend" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGbytea) )
   , "oidvectortypes" ::: Function ('[ NotNull (PGvararray (NotNull PGoidvector)) ] :=> 'Returns ( 'Null PGtext) )
   , "on_pb" ::: Function ('[ NotNull PGpoint,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "on_pl" ::: Function ('[ NotNull PGpoint,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "on_ppath" ::: Function ('[ NotNull PGpoint,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "on_ps" ::: Function ('[ NotNull PGpoint,  NotNull PGlseg ] :=> 'Returns ( 'Null PGbool) )
   , "on_sb" ::: Function ('[ NotNull PGlseg,  NotNull PGbox ] :=> 'Returns ( 'Null PGbool) )
   , "on_sl" ::: Function ('[ NotNull PGlseg,  NotNull PGline ] :=> 'Returns ( 'Null PGbool) )
   , "path" ::: Function ('[ NotNull PGpolygon ] :=> 'Returns ( 'Null PGpath) )
   , "path_add" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGpath) )
   , "path_add_pt" ::: Function ('[ NotNull PGpath,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpath) )
   , "path_center" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGpoint) )
   , "path_contain_pt" ::: Function ('[ NotNull PGpath,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "path_distance" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGfloat8) )
   , "path_div_pt" ::: Function ('[ NotNull PGpath,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpath) )
   , "path_inter" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_length" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGfloat8) )
   , "path_mul_pt" ::: Function ('[ NotNull PGpath,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpath) )
   , "path_n_eq" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_n_ge" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_n_gt" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_n_le" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_n_lt" ::: Function ('[ NotNull PGpath,  NotNull PGpath ] :=> 'Returns ( 'Null PGbool) )
   , "path_npoints" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGint4) )
   , "path_send" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGbytea) )
   , "path_sub_pt" ::: Function ('[ NotNull PGpath,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpath) )
   , "pclose" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGpath) )
   , "pg_cancel_backend" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "pg_char_to_encoding" ::: Function ('[ NotNull PGname ] :=> 'Returns ( 'Null PGint4) )
   , "pg_collation_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_column_is_updatable" ::: Function ('[ NotNull PGregclass,  NotNull PGint2,  NotNull PGbool ] :=> 'Returns ( 'Null PGbool) )
   , "pg_conversion_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_create_restore_point" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGpg_lsn) )
   , "pg_describe_object" ::: Function ('[ NotNull PGoid,  NotNull PGoid,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "pg_encoding_max_length" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "pg_encoding_to_char" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGname) )
   , "pg_filenode_relation" ::: Function ('[ NotNull PGoid,  NotNull PGoid ] :=> 'Returns ( 'Null PGregclass) )
   , "pg_function_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_get_function_arg_default" ::: Function ('[ NotNull PGoid,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_function_arguments" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_function_identity_arguments" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_function_result" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_functiondef" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_serial_sequence" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "pg_get_userbyid" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGname) )
   , "pg_indexes_size" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint8) )
   , "pg_is_other_temp_schema" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_cmp" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGint4) )
   , "pg_lsn_eq" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_ge" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_gt" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_hash" ::: Function ('[ NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGint4) )
   , "pg_lsn_le" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_lt" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_mi" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGnumeric) )
   , "pg_lsn_ne" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbool) )
   , "pg_lsn_send" ::: Function ('[ NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGbytea) )
   , "pg_node_tree_send" ::: Function ('[ NotNull PGpg_node_tree ] :=> 'Returns ( 'Null PGbytea) )
   , "pg_opclass_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_operator_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_opfamily_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_relation_filenode" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGoid) )
   , "pg_relation_filepath" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGtext) )
   , "pg_relation_is_updatable" ::: Function ('[ NotNull PGregclass,  NotNull PGbool ] :=> 'Returns ( 'Null PGint4) )
   , "pg_replication_origin_create" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGoid) )
   , "pg_replication_origin_oid" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGoid) )
   , "pg_replication_origin_progress" ::: Function ('[ NotNull PGtext,  NotNull PGbool ] :=> 'Returns ( 'Null PGpg_lsn) )
   , "pg_replication_origin_session_progress" ::: Function ('[ NotNull PGbool ] :=> 'Returns ( 'Null PGpg_lsn) )
   , "pg_start_backup" ::: Function ('[ NotNull PGtext,  NotNull PGbool ] :=> 'Returns ( 'Null PGpg_lsn) )
   , "pg_stat_get_analyze_count" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_autoanalyze_count" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_autovacuum_count" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_backend_activity" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "pg_stat_get_backend_activity_start" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_backend_client_addr" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGinet) )
   , "pg_stat_get_backend_client_port" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "pg_stat_get_backend_dbid" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGoid) )
   , "pg_stat_get_backend_pid" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGint4) )
   , "pg_stat_get_backend_start" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_backend_userid" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGoid) )
   , "pg_stat_get_backend_waiting" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "pg_stat_get_backend_xact_start" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_blocks_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_blocks_hit" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_blk_read_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_db_blk_write_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_db_blocks_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_blocks_hit" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_all" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_bufferpin" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_lock" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_snapshot" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_startup_deadlock" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_conflict_tablespace" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_deadlocks" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_numbackends" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint4) )
   , "pg_stat_get_db_stat_reset_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_db_temp_bytes" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_temp_files" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_tuples_deleted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_tuples_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_tuples_inserted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_tuples_returned" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_tuples_updated" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_xact_commit" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_db_xact_rollback" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_dead_tuples" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_function_calls" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_function_self_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_function_total_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_last_analyze_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_last_autoanalyze_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_last_autovacuum_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_last_vacuum_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_stat_get_live_tuples" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_mod_since_analyze" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_numscans" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_deleted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_hot_updated" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_inserted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_returned" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_tuples_updated" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_vacuum_count" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_blocks_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_blocks_hit" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_function_calls" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_function_self_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_xact_function_total_time" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGfloat8) )
   , "pg_stat_get_xact_numscans" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_deleted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_fetched" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_hot_updated" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_inserted" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_returned" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_stat_get_xact_tuples_updated" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGint8) )
   , "pg_table_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_table_size" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint8) )
   , "pg_tablespace_databases" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGoid) )
   , "pg_tablespace_location" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGtext) )
   , "pg_terminate_backend" ::: Function ('[ NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "pg_total_relation_size" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGint8) )
   , "pg_ts_config_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_ts_dict_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_ts_parser_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_ts_template_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_type_is_visible" ::: Function ('[ NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pg_xact_commit_timestamp" ::: Function ('[ NotNull PGxid ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "pg_xlog_location_diff" ::: Function ('[ NotNull PGpg_lsn,  NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGnumeric) )
   , "pg_xlogfile_name" ::: Function ('[ NotNull PGpg_lsn ] :=> 'Returns ( 'Null PGtext) )
   , "point_above" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_add" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpoint) )
   , "point_below" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_distance" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGfloat8) )
   , "point_div" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpoint) )
   , "point_eq" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_horiz" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_left" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_mul" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpoint) )
   , "point_ne" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_right" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "point_send" ::: Function ('[ NotNull PGpoint ] :=> 'Returns ( 'Null PGbytea) )
   , "point_sub" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGpoint) )
   , "point_vert" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "poly_above" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_below" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_center" ::: Function ('[ NotNull PGpolygon ] :=> 'Returns ( 'Null PGpoint) )
   , "poly_contain" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_contain_pt" ::: Function ('[ NotNull PGpolygon,  NotNull PGpoint ] :=> 'Returns ( 'Null PGbool) )
   , "poly_contained" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_distance" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGfloat8) )
   , "poly_left" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_npoints" ::: Function ('[ NotNull PGpolygon ] :=> 'Returns ( 'Null PGint4) )
   , "poly_overabove" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_overbelow" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_overlap" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_overleft" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_overright" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_right" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_same" ::: Function ('[ NotNull PGpolygon,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "poly_send" ::: Function ('[ NotNull PGpolygon ] :=> 'Returns ( 'Null PGbytea) )
   , "popen" ::: Function ('[ NotNull PGpath ] :=> 'Returns ( 'Null PGpath) )
   , "postgresql_fdw_validator" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGoid ] :=> 'Returns ( 'Null PGbool) )
   , "pt_contained_circle" ::: Function ('[ NotNull PGpoint,  NotNull PGcircle ] :=> 'Returns ( 'Null PGbool) )
   , "pt_contained_poly" ::: Function ('[ NotNull PGpoint,  NotNull PGpolygon ] :=> 'Returns ( 'Null PGbool) )
   , "query_to_xml" ::: Function ('[ NotNull PGtext,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "query_to_xml_and_xmlschema" ::: Function ('[ NotNull PGtext,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "query_to_xmlschema" ::: Function ('[ NotNull PGtext,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "querytree" ::: Function ('[ NotNull PGtsquery ] :=> 'Returns ( 'Null PGtext) )
   , "quote_ident" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "radians" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "radius" ::: Function ('[ NotNull PGcircle ] :=> 'Returns ( 'Null PGfloat8) )
   , "regclass" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGregclass) )
   , "regclasssend" ::: Function ('[ NotNull PGregclass ] :=> 'Returns ( 'Null PGbytea) )
   , "regconfigsend" ::: Function ('[ NotNull PGregconfig ] :=> 'Returns ( 'Null PGbytea) )
   , "regdictionarysend" ::: Function ('[ NotNull PGregdictionary ] :=> 'Returns ( 'Null PGbytea) )
   , "regnamespacesend" ::: Function ('[ NotNull PGregnamespace ] :=> 'Returns ( 'Null PGbytea) )
   , "regoperatorsend" ::: Function ('[ NotNull PGregoperator ] :=> 'Returns ( 'Null PGbytea) )
   , "regopersend" ::: Function ('[ NotNull PGregoper ] :=> 'Returns ( 'Null PGbytea) )
   , "regproceduresend" ::: Function ('[ NotNull PGregprocedure ] :=> 'Returns ( 'Null PGbytea) )
   , "regprocsend" ::: Function ('[ NotNull PGregproc ] :=> 'Returns ( 'Null PGbytea) )
   , "regr_avgx" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_avgy" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_count" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGint8) )
   , "regr_intercept" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_r2" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_slope" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_sxx" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_sxy" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regr_syy" ::: Function ('[ Null PGfloat8,  Null PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "regrolesend" ::: Function ('[ NotNull PGregrole ] :=> 'Returns ( 'Null PGbytea) )
   , "regtypesend" ::: Function ('[ NotNull PGregtype ] :=> 'Returns ( 'Null PGbytea) )
   , "reltime" ::: Function ('[ NotNull PGinterval ] :=> 'Returns ( 'Null PGreltime) )
   , "reltimeeq" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimege" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimegt" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimele" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimelt" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimene" ::: Function ('[ NotNull PGreltime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "reltimesend" ::: Function ('[ NotNull PGreltime ] :=> 'Returns ( 'Null PGbytea) )
   , "repeat" ::: Function ('[ NotNull PGtext,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "replace" ::: Function ('[ NotNull PGtext,  NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "reverse" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "right" ::: Function ('[ NotNull PGtext,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "schema_to_xml" ::: Function ('[ NotNull PGname,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "schema_to_xml_and_xmlschema" ::: Function ('[ NotNull PGname,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "schema_to_xmlschema" ::: Function ('[ NotNull PGname,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "set_byte" ::: Function ('[ NotNull PGbytea,  NotNull PGint4,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbytea) )
   , "set_config" ::: Function ('[ Null PGtext,  Null PGtext,  Null PGbool ] :=> 'Returns ( 'Null PGtext) )
   , "setweight" ::: Function ('[ NotNull PGtsvector,  'NotNull (PGchar 1) ] :=> 'Returns ( 'Null PGtsvector) )
   , "shobj_description" ::: Function ('[ NotNull PGoid,  NotNull PGname ] :=> 'Returns ( 'Null PGtext) )
   , "similar_escape" ::: Function ('[ Null PGtext,  Null PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "sin" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "slope" ::: Function ('[ NotNull PGpoint,  NotNull PGpoint ] :=> 'Returns ( 'Null PGfloat8) )
   , "smgreq" ::: Function ('[ NotNull PGsmgr,  NotNull PGsmgr ] :=> 'Returns ( 'Null PGbool) )
   , "smgrne" ::: Function ('[ NotNull PGsmgr,  NotNull PGsmgr ] :=> 'Returns ( 'Null PGbool) )
   , "spgoptions" ::: Function ('[ NotNull (PGvararray (NotNull PGtext)),  NotNull PGbool ] :=> 'Returns ( 'Null PGbytea) )
   , "split_part" ::: Function ('[ NotNull PGtext,  NotNull PGtext,  NotNull PGint4 ] :=> 'Returns ( 'Null PGtext) )
   , "strip" ::: Function ('[ NotNull PGtsvector ] :=> 'Returns ( 'Null PGtsvector) )
   , "strpos" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "table_to_xml" ::: Function ('[ NotNull PGregclass,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "table_to_xml_and_xmlschema" ::: Function ('[ NotNull PGregclass,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "table_to_xmlschema" ::: Function ('[ NotNull PGregclass,  NotNull PGbool,  NotNull PGbool,  NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "tan" ::: Function ('[ NotNull PGfloat8 ] :=> 'Returns ( 'Null PGfloat8) )
   , "text_ge" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_gt" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_larger" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "text_le" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_lt" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_pattern_ge" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_pattern_gt" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_pattern_le" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_pattern_lt" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "text_smaller" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "textcat" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "texteq" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "texticlike" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "texticnlike" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "texticregexeq" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "texticregexne" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textlen" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGint4) )
   , "textlike" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textne" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textnlike" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textregexeq" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textregexne" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "textsend" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGbytea) )
   , "tideq" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidge" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidgt" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidlarger" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGtid) )
   , "tidle" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidlt" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidne" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGbool) )
   , "tidsend" ::: Function ('[ NotNull PGtid ] :=> 'Returns ( 'Null PGbytea) )
   , "tidsmaller" ::: Function ('[ NotNull PGtid,  NotNull PGtid ] :=> 'Returns ( 'Null PGtid) )
   , "time_cmp" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGint4) )
   , "time_eq" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_ge" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_gt" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_hash" ::: Function ('[ NotNull PGtime ] :=> 'Returns ( 'Null PGint4) )
   , "time_larger" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGtime) )
   , "time_le" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_lt" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_mi_interval" ::: Function ('[ NotNull PGtime,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtime) )
   , "time_mi_time" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGinterval) )
   , "time_ne" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGbool) )
   , "time_pl_interval" ::: Function ('[ NotNull PGtime,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtime) )
   , "time_send" ::: Function ('[ NotNull PGtime ] :=> 'Returns ( 'Null PGbytea) )
   , "time_smaller" ::: Function ('[ NotNull PGtime,  NotNull PGtime ] :=> 'Returns ( 'Null PGtime) )
   , "timedate_pl" ::: Function ('[ NotNull PGtime,  NotNull PGdate ] :=> 'Returns ( 'Null PGtimestamp) )
   , "timemi" ::: Function ('[ NotNull PGabstime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGabstime) )
   , "timepl" ::: Function ('[ NotNull PGabstime,  NotNull PGreltime ] :=> 'Returns ( 'Null PGabstime) )
   , "timestamp_cmp" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGint4) )
   , "timestamp_cmp_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGint4) )
   , "timestamp_cmp_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGint4) )
   , "timestamp_eq" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_eq_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_eq_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_ge" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_ge_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_ge_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_gt" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_gt_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_gt_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_hash" ::: Function ('[ NotNull PGtimestamp ] :=> 'Returns ( 'Null PGint4) )
   , "timestamp_larger" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGtimestamp) )
   , "timestamp_le" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_le_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_le_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_lt" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_lt_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_lt_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_mi" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGinterval) )
   , "timestamp_mi_interval" ::: Function ('[ NotNull PGtimestamp,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamp) )
   , "timestamp_ne" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_ne_date" ::: Function ('[ NotNull PGtimestamp,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_ne_timestamptz" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamp_pl_interval" ::: Function ('[ NotNull PGtimestamp,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamp) )
   , "timestamp_send" ::: Function ('[ NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbytea) )
   , "timestamp_smaller" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGtimestamp) )
   , "timestamptypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "timestamptz_cmp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGint4) )
   , "timestamptz_cmp_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGint4) )
   , "timestamptz_cmp_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGint4) )
   , "timestamptz_eq" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_eq_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_eq_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_ge" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_ge_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_ge_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_gt" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_gt_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_gt_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_larger" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "timestamptz_le" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_le_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_le_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_lt" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_lt_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_lt_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_mi" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGinterval) )
   , "timestamptz_mi_interval" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "timestamptz_ne" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_ne_date" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGdate ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_ne_timestamp" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGbool) )
   , "timestamptz_pl_interval" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "timestamptz_send" ::: Function ('[ NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGbytea) )
   , "timestamptz_smaller" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "timestamptztypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "timetypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "timetz_cmp" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGint4) )
   , "timetz_eq" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_ge" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_gt" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_hash" ::: Function ('[ NotNull PGtimetz ] :=> 'Returns ( 'Null PGint4) )
   , "timetz_larger" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGtimetz) )
   , "timetz_le" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_lt" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_mi_interval" ::: Function ('[ NotNull PGtimetz,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimetz) )
   , "timetz_ne" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGbool) )
   , "timetz_pl_interval" ::: Function ('[ NotNull PGtimetz,  NotNull PGinterval ] :=> 'Returns ( 'Null PGtimetz) )
   , "timetz_send" ::: Function ('[ NotNull PGtimetz ] :=> 'Returns ( 'Null PGbytea) )
   , "timetz_smaller" ::: Function ('[ NotNull PGtimetz,  NotNull PGtimetz ] :=> 'Returns ( 'Null PGtimetz) )
   , "timetzdate_pl" ::: Function ('[ NotNull PGtimetz,  NotNull PGdate ] :=> 'Returns ( 'Null PGtimestamptz) )
   , "timetztypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "tinterval" ::: Function ('[ NotNull PGabstime,  NotNull PGabstime ] :=> 'Returns ( 'Null PGtinterval) )
   , "tintervalct" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalend" ::: Function ('[ NotNull PGtinterval ] :=> 'Returns ( 'Null PGabstime) )
   , "tintervaleq" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalge" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalgt" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalle" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalleneq" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallenge" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallengt" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallenle" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallenlt" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallenne" ::: Function ('[ NotNull PGtinterval,  NotNull PGreltime ] :=> 'Returns ( 'Null PGbool) )
   , "tintervallt" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalne" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalov" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalrel" ::: Function ('[ NotNull PGtinterval ] :=> 'Returns ( 'Null PGreltime) )
   , "tintervalsame" ::: Function ('[ NotNull PGtinterval,  NotNull PGtinterval ] :=> 'Returns ( 'Null PGbool) )
   , "tintervalsend" ::: Function ('[ NotNull PGtinterval ] :=> 'Returns ( 'Null PGbytea) )
   , "tintervalstart" ::: Function ('[ NotNull PGtinterval ] :=> 'Returns ( 'Null PGabstime) )
   , "to_date" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGdate) )
   , "to_number" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGnumeric) )
   , "translate" ::: Function ('[ NotNull PGtext,  NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGtext) )
   , "ts_lexize" ::: Function ('[ NotNull PGregdictionary,  NotNull PGtext ] :=> 'Returns ( 'Null ('PGvararray ('NotNull PGtext))) )
   , "ts_match_qv" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "ts_match_tq" ::: Function ('[ NotNull PGtext,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "ts_match_tt" ::: Function ('[ NotNull PGtext,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "ts_match_vq" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsq_mcontained" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsq_mcontains" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_and" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGtsquery) )
   , "tsquery_cmp" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGint4) )
   , "tsquery_eq" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_ge" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_gt" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_le" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_lt" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_ne" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGbool) )
   , "tsquery_not" ::: Function ('[ NotNull PGtsquery ] :=> 'Returns ( 'Null PGtsquery) )
   , "tsquery_or" ::: Function ('[ NotNull PGtsquery,  NotNull PGtsquery ] :=> 'Returns ( 'Null PGtsquery) )
   , "tsquerysend" ::: Function ('[ NotNull PGtsquery ] :=> 'Returns ( 'Null PGbytea) )
   , "tsrange_subdiff" ::: Function ('[ NotNull PGtimestamp,  NotNull PGtimestamp ] :=> 'Returns ( 'Null PGfloat8) )
   , "tstzrange_subdiff" ::: Function ('[ NotNull PGtimestamptz,  NotNull PGtimestamptz ] :=> 'Returns ( 'Null PGfloat8) )
   , "tsvector_cmp" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGint4) )
   , "tsvector_concat" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGtsvector) )
   , "tsvector_eq" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvector_ge" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvector_gt" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvector_le" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvector_lt" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvector_ne" ::: Function ('[ NotNull PGtsvector,  NotNull PGtsvector ] :=> 'Returns ( 'Null PGbool) )
   , "tsvectorsend" ::: Function ('[ NotNull PGtsvector ] :=> 'Returns ( 'Null PGbytea) )
   , "txid_snapshot_send" ::: Function ('[ NotNull PGtxid_snapshot ] :=> 'Returns ( 'Null PGbytea) )
   , "txid_snapshot_xip" ::: Function ('[ NotNull PGtxid_snapshot ] :=> 'Returns ( 'Null PGint8) )
   , "txid_snapshot_xmax" ::: Function ('[ NotNull PGtxid_snapshot ] :=> 'Returns ( 'Null PGint8) )
   , "txid_snapshot_xmin" ::: Function ('[ NotNull PGtxid_snapshot ] :=> 'Returns ( 'Null PGint8) )
   , "txid_visible_in_snapshot" ::: Function ('[ NotNull PGint8,  NotNull PGtxid_snapshot ] :=> 'Returns ( 'Null PGbool) )
   , "unknownsend" ::: Function ('[ NotNull PGunknown ] :=> 'Returns ( 'Null PGbytea) )
   , "uuid_cmp" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGint4) )
   , "uuid_eq" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_ge" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_gt" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_hash" ::: Function ('[ NotNull PGuuid ] :=> 'Returns ( 'Null PGint4) )
   , "uuid_le" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_lt" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_ne" ::: Function ('[ NotNull PGuuid,  NotNull PGuuid ] :=> 'Returns ( 'Null PGbool) )
   , "uuid_send" ::: Function ('[ NotNull PGuuid ] :=> 'Returns ( 'Null PGbytea) )
   , "varbit" ::: Function ('[ NotNull PGvarbit,  NotNull PGint4,  NotNull PGbool ] :=> 'Returns ( 'Null PGvarbit) )
   , "varbit_send" ::: Function ('[ NotNull PGvarbit ] :=> 'Returns ( 'Null PGbytea) )
   , "varbitcmp" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGint4) )
   , "varbiteq" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbitge" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbitgt" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbitle" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbitlt" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbitne" ::: Function ('[ NotNull PGvarbit,  NotNull PGvarbit ] :=> 'Returns ( 'Null PGbool) )
   , "varbittypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "varcharsend" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGbytea) )
   , "varchartypmodin" ::: Function ('[ NotNull (PGvararray (NotNull PGcstring)) ] :=> 'Returns ( 'Null PGint4) )
   , "width" ::: Function ('[ NotNull PGbox ] :=> 'Returns ( 'Null PGfloat8) )
   , "xideq" ::: Function ('[ NotNull PGxid,  NotNull PGxid ] :=> 'Returns ( 'Null PGbool) )
   , "xideqint4" ::: Function ('[ NotNull PGxid,  NotNull PGint4 ] :=> 'Returns ( 'Null PGbool) )
   , "xidsend" ::: Function ('[ NotNull PGxid ] :=> 'Returns ( 'Null PGbytea) )
   , "xml" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "xml_is_well_formed" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "xml_is_well_formed_content" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "xml_is_well_formed_document" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGbool) )
   , "xml_send" ::: Function ('[ NotNull PGxml ] :=> 'Returns ( 'Null PGbytea) )
   , "xmlagg" ::: Function ('[ Null PGxml ] :=> 'Returns ( 'Null PGxml) )
   , "xmlcomment" ::: Function ('[ NotNull PGtext ] :=> 'Returns ( 'Null PGxml) )
   , "xmlconcat2" ::: Function ('[ Null PGxml,  Null PGxml ] :=> 'Returns ( 'Null PGxml) )
   , "xmlexists" ::: Function ('[ NotNull PGtext,  NotNull PGxml ] :=> 'Returns ( 'Null PGbool) )
   , "xmlvalidate" ::: Function ('[ NotNull PGxml,  NotNull PGtext ] :=> 'Returns ( 'Null PGbool) ) ]
type Domains = '[]

