
----EXTSGMAFOT-134


---------checklist clipro 
------FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50038','50038','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0',null,'DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'ITEM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,'64');


---LANFORMALITE
Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50038','FR','Notice de Décision',null);


---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50038','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'GRPORFI',null,null,null,null,null,null,null,null);

---- FORITEM

Insert into FORITEM (FORID,FITORDER,FORIDPARENT,FITSTATUS,TPGCODE,PHACODE,PHADEST,JALCODE,TEVDEST,TACCODE,TMOMODULE,TMFFONCTION,DOCID,FITDMATYPE,FITDMASTATUS,RULID,FITFLAGAUTOCOMPLETE,FITACTION,TWCID)
 values ('50038','10','30300','EC','TOUT','NEGO',null,null,'AVDOSS','GLOBAL','AVDOSS','EVF_VALIDER',null,null,null,null,null,null,null);

--- L1FORMALITE

Insert into L1FORMALITE (FORID,FORIDLIEE,FO1TYPELINK,FO1ITEMORDER) values ('30300','50038',null,null);

-----LANTUSPARAM
Insert into LANTUSPARAM (TUSNOM,TUPCODE,LANCODE,TUPLIBELLE,TUPHELPTEXT) values ('DMATYPE','64','FR','Notice de Décision',null);

-----TUSPARAM
Insert into TUSPARAM (TUSNOM,TUPCODE,TUPFLAGORFI) values ('DMATYPE','64',null);

---- RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('ACTTYPEDMA','TOUT','P','64','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('ACTTYPEDMA','TOUT','E','64','1');

----------RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','CBI01','DOCPRO','64','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','CBM01','DOCPRO','64','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','AUT','DOCPRO','64','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','AUT02','DOCPRO','64','1');





---------checklist clicom 
------FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50039','50039','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0',null,'DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'ITEM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,'65');


---LANFORMALITE
Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50039','FR','Notice de Décision',null);


---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50039','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'GRPORFI',null,null,null,null,null,null,null,null);

---- FORITEM

Insert into FORITEM (FORID,FITORDER,FORIDPARENT,FITSTATUS,TPGCODE,PHACODE,PHADEST,JALCODE,TEVDEST,TACCODE,TMOMODULE,TMFFONCTION,DOCID,FITDMATYPE,FITDMASTATUS,RULID,FITFLAGAUTOCOMPLETE,FITACTION,TWCID)
 values ('50039','4','30200','EC','TOUT','NEGO',null,null,'AVDOSS','GLOBAL','AVDOSS','EVF_VALIDER',null,null,null,null,null,null,null);

--- L1FORMALITE

Insert into L1FORMALITE (FORID,FORIDLIEE,FO1TYPELINK,FO1ITEMORDER) values ('30200','50039',null,null);

-----LANTUSPARAM
Insert into LANTUSPARAM (TUSNOM,TUPCODE,LANCODE,TUPLIBELLE,TUPHELPTEXT) values ('DMATYPE','65','FR','Notice de Décision',null);

-----TUSPARAM
Insert into TUSPARAM (TUSNOM,TUPCODE,TUPFLAGORFI) values ('DMATYPE','65',null);

---- RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('ACTTYPEDMA','TOUT','P','65','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('ACTTYPEDMA','TOUT','E','65','1');

----------RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','CBI01','DOCCOM','65','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','CBM01','DOCCOM','65','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','AUT','DOCCOM','65','1');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('CATYPDOC','AUT02','DOCCOM','65','1');



commit ;