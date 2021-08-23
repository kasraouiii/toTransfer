update  UTIPREFERENCE set UPRBOOLEANVALUE=0 where  uprcode='INSPECTION';
update  UTIPREFERENCE set UPRBOOLEANVALUE=1 where  uprcode='INSPECTION' 
and uticode in ('ORFI',
'GLOBAL',
'SGMORFI',
'SGMADMIN',
'SGMFATABD',
'BOSGLM');
commit;