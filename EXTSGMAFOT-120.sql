

----EXTSGMAFOT-120

---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (SEQ_rulid.nextval,'GRP_INIT','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values ((select rulid from rule where rulcode='GRP_INIT'),'FR','Rule utilisateur initia');


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values ((select rulid from rule where rulcode='GRP_INIT'), 1, '', 506, '');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values ((select rulid from rule where rulcode='GRP_INIT'), 2, '',508 , '');

UPDATE cddrul set rulid =(select rulid from rule where rulcode='GRP_INIT')     where cddordre=105  ;



update  lanworstep   set   wstlabel  ='Initiation de la demande' where  worcode='WFCLIPRO'   and wstorder =5 
and wstlabel  ='Initiation SGLM'; 



commit ;