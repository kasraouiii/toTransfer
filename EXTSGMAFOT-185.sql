---EXTSGMAFOT-185
------rule sur classer ss demande clipro

select * from cddrul where cddordre=30015  ;

delete from cddrul where cddordre=30015  and cseid =19 ;

update worstep  set WSTTYPE='BEGIN'  , WSTEXECUTIONMODE='BOTH' where worcode='WFCLIPRO' and wstorder =15 ;


-- WSTJUMP from initi to class ss

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','5','WFCLIPRO','15',null);

-- WSTJUMP from instruction to class ss

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','6','WFCLIPRO','15',null);


-- WSTJUMP from avis comm to class ss

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','7','WFCLIPRO','15',null);

--- WSTJUMP from tirage to class ss

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','21','WFCLIPRO','15',null);


--- WSTJUMP from Demande Arbitrage DG

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','17','WFCLIPRO','15',null);


--- WSTJUMP from  Retour pour complitude

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','16','WFCLIPRO','15',null);


--- WSTJUMP from  Transfert vers Commercial SGLM
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','4','WFCLIPRO','15',null);


--- WSTJUMP from  Initiation de la demande et Transfert vers SGMA

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','3','WFCLIPRO','15',null);

---wstjump from Initiation de la demande et Transfert vers CAFV 

Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLIPRO','1','WFCLIPRO','15',null);


---------------clicom
update worstep  set WSTTYPE='BEGIN'  , WSTEXECUTIONMODE='BOTH' where worcode='WFCLICOM' and wstorder =2 

----IITIation to css
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLICOM','1','WFCLICOM','2',null);

---instruction to css
Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLICOM','3','WFCLICOM','2',null);



commit ;



