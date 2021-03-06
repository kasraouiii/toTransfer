
---EXTSGMAFOT-184
----Habilitations des événements pour les COMSGL et les CASGL CLIPRO

-----initiation --forma pour comsgl pour instruction

--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50042','50042','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50042','FR','Consommation crée par SGL,Procéder à l''analyse',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50042','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'COMSGL','SGMLEA',null,null,null,null,null,null,null);

-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLIPRO','5','11','FORMALITE','50042',null,null,null);


----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','5','11','FR','Notification',null);




---instruction dossier clipro pour comsgl

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (521, 3, '','506', '');
insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (521, 4, '','507', '');

--forma pour casgl pour avis commercial

--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50043','50043','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50043','FR','Saisir Avis Commercial',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50043','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'CASGL','SGMLEA',null,null,null,null,null,null,null);

-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLIPRO','6','8','FORMALITE','50043',null,null,null);


----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLIPRO','6','8','FR','Notification',null);


----avis commercial  pour CASGL
------ RULE grpe casgl+comsgl
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007138','GRP','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007138','FR','GRP CASGL ET COMSGL CLIPRO');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007138, 1, '','506', '');
insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007138, 2, '','508', '');


update cddrul   set rulid =3007138 where cddordre=107  and  rulid =506 ;



----------Forçage montant/Durée d'autorisation pour comsgl


update cddrul set rulid =3007138  where cddordre =1024 and  rulid =508 ;

------------***********************Habilitations des événements pour les COMSGL et les CASGL CLICOM
---------initiation

---forma pour instruction comsgl

--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50045','50045','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50045','FR','Consommation validée par SGL,Procéder à l''analyse',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50045','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'COMSGL','SGMLEA',null,null,null,null,null,null,null);

-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLICOM','1','11','FORMALITE','50045',null,null,null);


----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLICOM','1','11','FR','Notification',null);




----instruction clicom

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007108, 3, '','507', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007108, 4, '','506', '');


-----avis commerc clicom casgl

------ RULE grpe casgl+comsgl
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007139','GRP','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007139','FR','GRP CASGL ET COMSGL CLICOM');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007139, 1, '','507', '');
insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007139, 2, '','509', '');


update cddrul   set rulid =3007139 where cddordre=2017  and cseid=19 and  rulid =507 ;


---forma pour avis comm casgl


--- FORMALITE

Insert into FORMALITE (FORID,FORCODE,FORDEST,TACCODE,UGECODE,DOCIDEMIS,FORIDRAPPEL,FORDELAIEMIS,FORFLAGDOCEMIS,FORFLAGREPONSE,FORDELAIREP,FORFLAGFORRAPPEL,FORFLAGOBLIG,FORTYPEDATE,FORMODELANCEMENT,FORFLAGRECURRENT,FORPERIODE,FORMULTIPLE,FORJOUR,FORMOIS,FORDELAIREACTIVATION,FORDELAIREPONSEHEURE,FORNBACREER,FORTYPEJOUR,FORTYPERECURRENCE,FORPRIORITY,FORTYPE,FORPEREMIS,FORPERACT,FORPERREP,FORPERREPONSE,FORTYPEMSG,ANMID,FORTELTYPE,FORPROCEDURE,FORGROUP,CALID,FORGAPTYPE,TGRID,FORCLOSING,FORSTEPCODESOURCE,FORSTEPCODETARGET,FORFLAGREMINDER,FORREMMETHOD,FORREMPERIODBEFORE,FORREMMULTIPLEBEFORE,FORREMPERIODAFTER,FORREMMULTIPLEAFTER,FORPROCRUN,FORDTFREQUENCYTIME,FORWEIGHT,RMAID,SLAID,FORMESSAGE,FORTARGETTYPE,FOREXTERNALADDRESS,DMATYPE) 
values ('50044','50044','AVDOSS','GLOBAL','SGM',null,null,'0','0','2','1','0','0','DTIMME','EVT','0','J','0','0',null,'0',null,null,null,null,null,'GESTADM',null,null,null,null,null,null,null,null,null,'5','OUVRE',null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null,null);

---- LANFORMALITE

Insert into LANFORMALITE (FORID,LANCODE,FORLIBELLE,FORDESCRIPTION) values ('50044','FR','Saisir Avis Commercial',null);

---- FORDESTINATION

Insert into FORDESTINATION (FORID,TPGCODE,FDEORDRE,FDEULYSENDER,FDEUTICODESENDER,FDEGROCODESENDER,FDESECTGESTIONSENDER,FDEMETIERSENDER,FDEULYRECEIVER,FDEUTICODERECEIVER,FDEGROCODERECEIVER,FDESECTGESTIONRECEIVER,FDEMETIERRECEIVER,FDEACTION,FDEPRIORITY,FDEWORCODE,FDEWSTORDER,FDEWEIGHT,TCUCODE) 
values ('50044','TOUT','1',null,null,null,null,null,'GRORECEIVE',null,'CASGL','SGMLEA',null,null,null,null,null,null,null);

-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLICOM','3','10','FORMALITE','50044',null,null,null);


----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLICOM','3','10','FR','Notification',null);


---Forçage montant/Durée d'autorisation

update cddrul  set rulid =  3007139    where cddordre =2019 and rulid =508 and cseid =19 ;

commit ;


















