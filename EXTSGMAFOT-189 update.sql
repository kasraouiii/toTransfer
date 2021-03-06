

---EXTSGMAFOT-189
----rajout de la notification au DGA
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




    update wstconsequence set WSCACTIONTYPE= 'CLOSE_FORMALITE' where worcode='WFCLIPRO' and wstorder=27 and wscorder =1 ;



---rule pour habilitation DGA -DG

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007140','3007140','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007140','FR','Rule DG DGA');

----- RULVALUE

Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007140','1',null,'511',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007140','2',null,'512',null);



update cddrul set rulid =3007140  where cddordre = 1028  and cseid =19 and rulid=511  ;


commit ;