-------------EXTSGMAFOT-189

update wstjump  set wstorderdest=18 where  worcode='WFCLIPRO'  and wstorder=12  and wstorderdest =27 and rulid =1002 ;

delete from wstjump where  worcode='WFCLIPRO'  and wstorder=12  and wstorderdest =10 and rulid =1002 ;


update lanavtdecision  set adelibelle='Avis Favorable' where  adecode='DECIFA01' and lancode ='FR';

update cddrul   set rulid =513   where cseid=19 and cddordre =1018 and rulid =3007134   ; 



Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','RISQ','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','RISQ','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','RISQ','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','RISQ','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','RISQUE','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','RISQUE','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','RISQUE','DECIFA01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','RISQUE','DECIFA01','0');


 update rulvalue   set rulidvalue=300760 where rulid =4013 and rulidvalue =1003 ;
update lanformalite  set forlibelle= 'Avis Favorable-Demande Avis Directeur Risque' where forid =50040;
--update wstconsequence  SET WSCACTIONCODE='50041' where   worcode='WFCLIPRO' and wstorder=18  AND wscactiontype='FORMALITE' and wscorder=10   and  WSCACTIONCODE='50041';

update wstconsequence  SET WSCACTIONCODE='30541' where   worcode='WFCLIPRO' and wstorder=18  AND wscactiontype='FORMALITE' and wscorder=10;

Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('4012','FR','lan rule ');

----formalite pour avis favorable risq--transfert au Dir risq
-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLIPRO','18','12','FORMALITE','50040',null,null,null);

----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','18','12','FR','Notification',null);

----habilitation pour groupe risque 
---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','101812',null,'12',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','101812','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','101812',null,'18',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','101812','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','101812','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','101812','1003');


update cddrul set rulid =300761 where  cddordre=10187 and  rulid =1005;

update lanworstep set WSTLABEL  ='Avis Directeur Risque'  where  worcode='WFCLIPRO'  and wstorder=	27 ;
update worstep set WSTACTIONCODE  ='EVF_DECISION'  where  worcode='WFCLIPRO'  and wstorder=	27 ;



update wstjump  set wstorderdest='27' where   worcode='WFCLIPRO' and  wstorderdest=28 and rulid =1002;
---habilitaion pour dir risque

update cddrul    set rulid =3007134 where cddordre=1027    and rulid =513 ;





----RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','RISQSGL','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','RISQSGL','DECIFA01','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','RISQSGL','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','RISQSGL','DECIFA01','0');



Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','MOHAMEDEA','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','MOHAMEDEA','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','MOHAMEDEA','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','MOHAMEDEA','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','MOHAMEDEA','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','MOHAMEDEA','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','MOHAMEDEA','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','MOHAMEDEA','DECIFA01','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','MOHAMEDEA','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','MOHAMEDEA','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','MOHAMEDEA','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','MOHAMEDEA','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','MOHAMEDEA','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','MOHAMEDEA','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','MOHAMEDEA','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','MOHAMEDEA','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','RISQSGL','DECIFA01','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','RISQSGL','DECIFA01','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','RISQSGL','DECIFA01','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','RISQSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','RISQSGL','REFUS','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','RISQSGL','DECIAJ01','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','RISQSGL','DECIFA01','0');


delete  from RELATIONVALEURPROFIL where reccode='USERGRPDECI' and revmerecode='RISQSGL'  and revfillecode  ='DECIDFA01';

delete  from RELATIONVALEURPROFIL where reccode='USERDECI' and revmerecode='MOHAMEDEA'  and revfillecode  ='DECIDFA01';


delete  from RELATIONVALEURPROFIL where reccode='USERDECI' and revmerecode='RISQSGL'  and revfillecode  ='DECIDFA01';




----formalite pour avis dir risque 
-------- accord 

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 
values ('WFCLIPRO','27','2','FORMALITE','30531',null,null,null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','27','2','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10272',null,'2',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10272','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10272',null,'27',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10272','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10272','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10272','300760');


-------- ajournement 

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 
values ('WFCLIPRO','27','3','FORMALITE','30532',null,null,null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','27','3','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10273',null,'3',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10273','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10273',null,'27',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10273','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10273','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10273','1001');

----------Refus

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 values ('WFCLIPRO','27','4','FORMALITE','30535',null,null,null);

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','27','4','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10274',null,'4',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10274','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10274',null,'27',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10274','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10274','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10274','300761');

----------Avis favorable
--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50046','50046','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50046','FR','Soumission au DG pour validation',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50046','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'DG_SGL','SGMLEA',null,null,null,null,null,null,null);



Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 values ('WFCLIPRO','27','5','FORMALITE','50046',null,null,null);

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','27','5','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10275',null,'5',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10275','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10275',null,'27',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10275','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10275','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10275','1003');

----wstjump
DELETE from wstjump where worcode='WFCLIPRO'  and wstorder=27 and wstorderdest=18; 
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','27','WFCLIPRO','10','4009');--accord 
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','27','WFCLIPRO','28','1002');
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','27','WFCLIPRO','16','1000');--ajourn
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','27','WFCLIPRO','15','300763');---refus 


------------Validation DG 

update worstep set WSTACTIONCODE  ='EVF_DECISION'  where  worcode='WFCLIPRO'  and wstorder=	28 ;

----RELATIONVALEURPROFIL

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','DGSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBM01','DGSGL','REFUS','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','DGSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','CBI01','DGSGL','REFUS','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','DGSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT','DGSGL','REFUS','0');

Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','DGSGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERDECI','AUT02','DGSGL','REFUS','0');



Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','DG_SGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBM01','DG_SGL','REFUS','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','DG_SGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','CBI01','DG_SGL','REFUS','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','DG_SGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT','DG_SGL','REFUS','0');


Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','DG_SGL','ACCORD','0');
Insert into RELATIONVALEURPROFIL (RECCODE,TPGCODE,REVMERECODE,REVFILLECODE,REVFLAGDEFAUT) values ('USERGRPDECI','AUT02','DG_SGL','REFUS','0');




update wstjump   set rulid = 4009  where  worcode='WFCLIPRO'  and wstorder=	28  and WSTORDERDEST  =10   ;---accord 
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','28','WFCLIPRO','15','300763');---refus 


update wstconsequence  set wscactioncode ='50046'  , wscactiontype='CLOSE_FORMALITE' where  worcode='WFCLIPRO'  and wstorder=	28 and wscorder =1  ;


----formalite pour decesion DG 
-------- accord 

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 
values ('WFCLIPRO','28','2','FORMALITE','30531',null,null,null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','28','2','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10282',null,'2',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10282','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10282',null,'28',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10282','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10282','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10282','300760');

---------Refus

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
 values ('WFCLIPRO','28','3','FORMALITE','30535',null,null,null);

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','28','3','FR','Notification',null);

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10283',null,'3',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10283','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10283',null,'28',null,null);
---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10283','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10283','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10283','300761');


update fordestination  set FDEULYRECEIVER='GRORECEIVE' , FDEGROCODERECEIVER ='CASGL' where forid =30535;



commit ;


--- FORMALITE
Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE)
values ('50051','50051','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);
---- LANFORMALITE
Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50051','FR','Soumission au DGA pour validation',null);
---- FORDESTINATION
Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE)
values ('50051','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'DGA_SGL','SGMLEA',null,null,null,null,null,null,null);

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST)
values ('WFCLIPRO','27','6','FORMALITE','50051',null,null,null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
values ('WFCLIPRO','27','6','FR','Notification',null);
---- CUSDEFDATA
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10276',null,'6',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10276','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10276',null,'27',null,null);
---- CDDRUL
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10276','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10276','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10276','1003');


commit;

update wstconsequence set WSCACTIONTYPE= 'CLOSE_FORMALITE' where worcode='WFCLIPRO' and wstorder=27 and wscorder =1 ;

commit;

update rulvalue set rulidvalue=300762 where rulid=4009 and rulidvalue=1002;
commit;

==============

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007139','3007139','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

 

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007139','FR','Rule Notification cbi');

 

----- RULVALUE

 

Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007139','1',null,'4011',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007139','2',null,'300760',null);

 

 

update cddrul set rulid =3007139  where cddordre = 10145  and cseid =108 and rulid=4013  ;

=====
---rule pour habilitation DGA -DG

 

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007140','3007140','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

 

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007140','FR','Rule DG DGA');

 

----- RULVALUE

 

Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007140','1',null,'511',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007140','2',null,'512',null);

 

 

update cddrul set rulid =3007140  where cddordre = 1028  and cseid =19 and rulid=511  ;

====

update wstjump set rulid =3007138 where worcode='WFCLIPRO' and wstorder=18 and wstorderdest=10 and rulid =4009 ;

===
update cddrul set rulid =3007139 where cddordre = 10185 and cseid =108 and rulid=4013 ;

===

EXTSGMAFOT-191 :  

update rulvalue set rulidvalue ='300762' where rulid =4007 and rulidvalue=1002 ;
commit;



