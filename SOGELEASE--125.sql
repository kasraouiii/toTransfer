
----SOGELEASE--125
---rule pour statut instruction


--- FORFICHE

Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','AUT','GEN','10078','0','PROD','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','AUT02','GEN','10078','0','PROD','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','CBM01','GEN','10078','0','PROD','AVDOSS',null,null,null);


Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','AUT','GEN','10078','0','FIN','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','AUT02','GEN','10078','0','FIN','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50018','CBM01','GEN','10078','0','FIN','AVDOSS',null,null,null);

Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50019','CBI01','GEN','10078','0','FIN','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50019','CBI01','GEN','10078','0','PROD','AVDOSS',null,null,null);
---FORFICHE

Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50016','AUT','GEN','10077','0','FIN','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50016','AUT02','GEN','10077','0','FIN','AVDOSS',null,null,null);
Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50016','CBM01','GEN','10077','0','FIN','AVDOSS',null,null,null);

Insert into FORFICHE (FORID,TPGCODE,FFICODERF,DOCID,FFIDELAIEMIS,PHACODE,PHADEST,FFIFLAGEMAIL,FFINBMAXCYCLES,FFIDTEND) values ('50017','CBI01','GEN','10077','0','FIN','AVDOSS',null,null,null);


---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007109,'STSFICH','AVDOSS','IN',null,null,'ACTIVE','8',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007109,'FR','Rule pour credit bail');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 1, 'CONSVFV','', '');


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 2, 'CONSSGL', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 3, 'DVCLT', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 4, 'DAJCOM', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 5, 'DVAC', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 6, 'DVCLT', '', '');
insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 7, 'DECDCI', '', '');
insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 8, 'DACPT', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 9, 'WPROD', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 10, 'TRAKO', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 11, 'DRFSE', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 12, 'TRANSOK', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007109, 13, 'DTBO', '', '');     




--rule generale pour doc scoring
---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007110,'RUSCORG','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007110,'FR','Rule pour doc scoring');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007110, 1, '','3007107', '');


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007110, 2, '', '3007109', '');





UPdate lkforrul   set rulid =3007110  where  forid in (50030  , 50031) and rulid =3007107 ;---doc 10085 
UPdate lkforrul   set rulid =3007110  where  forid in  (50016  , 50017) and rulid =3007107 ;---doc 10077 


----Fiche Info du BO au niveau du FO ==> Affichage à partir du Transfert --DACPT

---rule pour statut instruction
---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007111,'INFOBO','AVDOSS','IN',null,null,'ACTIVE','8',null,null,null,null,'LOGICAL',null,null);

----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007111,'FR','Rule Info du BO');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 1, 'DACPT', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 2, 'WPROD', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 3, 'TRAKO', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 4, 'DRFSE', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 5, 'TRANSOK', '', '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007111, 6, 'DTBO', '', '');  




---rule generale pour doc info bo
---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007112,'RULINBO','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007112,'FR','Rule GENE pour info bo');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007112, 1, '','3007107', '');


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007112, 2, '', '3007111', '');


UPdate lkforrul   set rulid =3007112  where  forid in  ( 50018 ,50019) and rulid =3007107 ;




-----Décision de crédit ==> Affichage à partir de la Phase décision 

---rule pour fiche decision
---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007113,'FISIMU','AVDOSS','<>','MOTIFI',null,'ACTIVE','20',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007113,'FR','Rule fiche deci');


---rule generale pour doc fiche decision
---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007114,'RUSIMU','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007114,'FR','Rule doc fiche decision');
update lanrule   set rullabel  ='Rule doc fiche decision'   where rulid =3007114  ;

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007114, 1, '','3007107', '');


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007114, 2, '', '3007113', '');


UPdate lkforrul   set rulid =3007114  where  forid in  ( 50015 ,50014) and rulid =3007107 ;  



commit ;




