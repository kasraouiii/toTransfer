
------EXTSGMAFOT-196



update rulvalue set RULIDVALUE =300799  where rulid =3007128  and rvaordre=1 and rulidvalue=3007100  ;

-- LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','8','4','FR','Notification',null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','8','6','FR','Notification',null);




--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50052','50052','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50052','FR','Saisir décision Finale',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50052','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'DGA_SGL','SGMLEA',null,null,null,null,null,null,null);

-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLIPRO','8','11','FORMALITE','50052',null,null,null);

----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','8','11','FR','Notification',null);



---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10811',null,'11',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10811','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10811','null',8,null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10811','3007128');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10811','3007128');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10811','3007128');


commit ;


















