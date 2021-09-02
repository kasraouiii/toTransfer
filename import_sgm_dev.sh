#!/bin/sh
wdate=$(date +"%Y%m%d")

echo DATAPUMPIMPORT $SGM45DEV $TRSGM45DEV $TRES5PRG $TRSGM45 $20210826_KSIOP45_SGM45_20210826.DMP $AVSGM45DEV $prospect $AVSGM45 $20210826_KSIOP45_SGM45_20210826.DMP

sleep 20
echo "START KILL Session BO"
# $(sqlplus sys/system as sysdba @KILL_SESS.SQL TRSGM45DEV> SGM45DEV_TRSGM45DEV_KILL_SESSION.log)
echo "START KILL Session FO"
$(sqlplus sys/system as sysdba @KILL_SESS.SQL AVSGM45DEV > SGM45DEV_AVSGM45DEV_KILL_SESSION.log)
echo "Drop $AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @DROP.SQL > SGM45DEV_AVSGM45DEV_drop.log)
echo "Drop $TRSGM45DEV"
# $(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @DROP.SQL > SGM45DEV_TRSGM45DEV_DROP.log)
echo "Droits $TRSGM45DEV"
# $(sqlplus sys/system as sysdba @DROITS_KSIOP.SQL TRSGM45DEV > SGM45DEV_TRSGM45DEV_DROITS.log)
echo "Droits $AVSGM45DEV"
$(sqlplus sys/system as sysdba @DROITS_ANDRO.SQL AVSGM45DEV TRSGM45DEV > SGM45DEV_AVSGM45DEV_DROITS.log)
echo "Types $AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @TYPES.SQL >SGM45DEV_AVSGM45DEV_Types.log)
echo "Types $TRSGM45DEV"
# $(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @TYPES.SQL >SGM45DEV_TRSGM45DEV_Types.log)
echo "Start Import TRSGM45 Into TRSGM45DEV"
# $(impdp USERid=system/manager@SGM45DEV dumpfile=20210826_KSIOP45_SGM45_20210826.DMP directory=DATAPUMP version=12.1.0.2.0 LOGFILE=SGM45DEV_TRSGM45DEV_import.log   IGNORE=Y SCHEMAS=TRSGM45 REMAP_SCHEMA=TRSGM45:TRSGM45DEV,TRSGM45_USER:TRSGM45DEV_USER,TRSGM45_VIEWER:TRSGM45DEV_VIEWER EXCLUDE=GRANT EXCLUDE=STATISTICS table_exists_action=replace REMAP_TABLESPACE=TBS_TRSGM45:TBS_TRSGM45DEV,TBS_CASTEMPORARY:TBS_TRSGM45DEV,PRD_CASDEFAULT_INDEX:TBS_TRSGM45DEV,LPTESTDB_INDEX:TBS_TRSGM45DEV,PRD_CASDEFAULT:TBS_TRSGM45DEV,LPPRODDB_DATA:TBS_TRSGM45DEV,LPPRODDB_INDEX:TBS_TRSGM45DEV,CASIOPE:TBS_TRSGM45DEV,TBS_ANDIMP_DATA:TBS_TRSGM45DEV,TBS_ANDIMP_INDEX:TBS_TRSGM45DEV,TBS_ANDPAR_DATA:TBS_TRSGM45DEV,TBS_ANDTRV_INDEX:TBS_TRSGM45DEV,TBS_ANDTRV_DATA:TBS_TRSGM45DEV,TBS_ANDPAR_INDEX:TBS_TRSGM45DEV,TBS_ANDVID_DATA:TBS_TRSGM45DEV,TBS_ANDDEFAULT:TBS_TRSGM45DEV,TBS_CASTMP_INDEX:TBS_TRSGM45DEV,TBS_CASTRV_DATA:TBS_TRSGM45DEV,TBS_CASTRV_INDEX:TBS_TRSGM45DEV,TBS_CASDEFAULT:TBS_TRSGM45DEV)
echo "Start Import AVSGM45 Into AVSGM45DEV"
$(impdp USERid=system/manager@SGM45DEV dumpfile=20210826_KSIOP45_SGM45_20210826.DMP directory=DATAPUMP LOGFILE=SGM45DEV_AVSGM45DEV_import.log   IGNORE=Y SCHEMAS=AVSGM45 REMAP_SCHEMA=AVSGM45:AVSGM45DEV,AVSGM45_SEL:AVSGM45DEV_SEL,AVSGM45_MOD:AVSGM45DEV_MOD  EXCLUDE=STATISTICS table_exists_action=replace REMAP_TABLESPACE=TBS_AVSGM45:TBS_AVSGM45DEV,TBS_ANDDEFAULT:TBS_AVSGM45DEV,TBS_ANDTRV_DATA:TBS_AVSGM45DEV,TBS_ANDVID_DATA:TBS_AVSGM45DEV,TBS_ANDPAR_DATA:TBS_AVSGM45DEV,TBS_ANDIMP_DATA:TBS_AVSGM45DEV,TBS_ANDTRV_INDEX:TBS_AVSGM45DEV,TBS_ANDVID_INDEX:TBS_AVSGM45DEV,TBS_ANDPAR_INDEX:TBS_AVSGM45DEV,TBS_ANDIMP_INDEX:TBS_AVSGM45DEV)
echo "Change Synonymes TRSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @CHANGESYNONYMES.SQL TRSGM45DEV >SGM45DEV_AVSGM45DEV_ChangeSynonymes.log)
echo "Change Synonymes AVSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @CHANGESYNONYMES.SQL AVSGM45DEV >SGM45DEV_TRSGM45DEV_ChangeSynonymes.log)
echo "Parsysteme AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @PARSYSTEME.SQL AVSGM45DEV AVSGM45DEV_SEL AVSGM45DEV_MOD TRSGM45DEV >SGM45DEV_AVSGM45DEV_PARSYSTEME.log)
echo "Parsysteme TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @PARSYSTEME.SQL TRSGM45DEV TRSGM45DEV_VIEWER TRSGM45DEV_USER AVSGM45DEV >SGM45DEV_TRSGM45DEV_PARSYSTEME.log)
echo "Views AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @VIEWS.SQL >SGM45DEV_AVSGM45DEV_VIEWS.log)
echo "STATISTIQUE TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @STATISTIQUE.SQL > SGM45DEV_TRSGM45DEV_statistique.log)
echo "STATISTIQUE AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @STATISTIQUE.SQL > SGM45DEV_AVSGM45DEV_statistique.log)
echo "GRANTS KSIOP TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @GRANTSKSIOP.SQL > SGM45DEV_TRSGM45DEV_grant.log)
echo "GRANTS ANDRO AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @GRANTSANDRO.SQL > SGM45DEV_AVSGM45DEV_grant.log)
echo "Compile AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect@SGM45DEV @COMPILE.SQL > SGM45DEV_AVSGM45DEV_compile.log)
echo "Compile TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG@SGM45DEV @COMPILE.SQL > SGM45DEV_TRSGM45DEV_compile.log)
echo "CHECK SEQUENCES AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect @CHECKSEQUENCE.sql > SGM45DEV_AVSGM45DEV_CHECKSEQUENCE.log)
echo "CHECK SEQUENCES TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG @CHECKSEQUENCE.sql > SGM45DEV_TRSGM45DEV_CHECKSEQUENCE.log)
echo "BUILD INDEX BO"
$(sqlplus sys/system as sysdba @BUILD_INDEX.sql TRSGM45DEV > SGM45DEV_TRSGM45DEV_BUILD_INDEX.log)
echo "BUILD INDEX FO"
$(sqlplus sys/system as sysdba @BUILD_INDEX.sql AVSGM45DEV > SGM45DEV_AVSGM45DEV_BUILD_INDEX.log)
echo "COMPILE ADVANCED AVSGM45DEV"
$(sqlplus AVSGM45DEV/prospect @COMPILE_ADVANCED.SQL > SGM45DEV_AVSGM45DEV_COMPILE_ADVANCED.log)
echo "COMPILE ADVANCED TRSGM45DEV"
$(sqlplus TRSGM45DEV/TRES5PRG @COMPILE_ADVANCED.SQL > SGM45DEV_TRSGM45DEV_COMPILE_ADVANCED.log)

