
---EXTSGMAFOT-140

  --Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('4','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard Sogecontact, vendors et SGMA CLIPRO : Leasing mobilier
  --  Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('8','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
 -- ---Barème standard Sogecontact, vendors et SGMA CLICOM : Leasing mobilier


    Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('16','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard Sogecontact, vendors et SGMA CLIPRO ---CBI
     Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('20','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard Sogecontact, vendors et SGMA CLICOM ---CBI
   
   
    Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('1','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème linéaire SGL CLIPRO ---CBM

   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('2','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---BBarème linéaire avec franchise SGL CLIPRO : CBM
   
     Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('3','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème dégressif /Progressif SGL CLIPRO : CBM
   
       Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('5','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème linéaire SGL CLICOM : Leasing mobilier

   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('6','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème linéaire avec franchise SGL CLICOM : Leasing mobilier
   
     Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('7','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème  dégressif /Progressif SGL CLICOM : Leasing mobilier 
    Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('9','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard SGL CLIPRO : Leasing mobilier
   
   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('11','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard SGL CLICOM : Leasing mobilier
   
     Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('10','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard SGMA CLIPRO : Leasing mobilier

   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('12','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème standard SGMA CLICOM : Leasing mobilier
   
   
      Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('14','1','300758 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème linéaire avec franchise SGL CLIPRO : Leasing immobilier

   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('18','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   ---Barème linéaire avec franchise SGL CLICOM : Leasing immobilier
   
   Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('19','1','300759 ',to_date('01/01/20','DD/MM/RR'),null,null);
   
   
  
	
	delete from lkpcrrul  where  pcrid =1  and rulid =300758;
	delete from lkpcrrul  where  pcrid =2  and rulid =300758;
	delete from lkpcrrul  where  pcrid =3  and rulid =300758;



	delete from lkpcrrul  where  pcrid =5  and rulid =300759;
	delete from lkpcrrul  where  pcrid =6  and rulid =300759;
	delete from lkpcrrul  where  pcrid =7  and rulid =300759;
   
   
	delete from lkpcrrul  where  pcrid =9  and rulid =300758;
	delete from lkpcrrul  where  pcrid =11  and rulid =300759;
	
	delete from lkpcrrul  where  pcrid =18  and rulid =300759;
	delete from lkpcrrul  where  pcrid =19  and rulid =300759;
    delete  from lkpcrrul where pcrid =14 and rulid =300758 ;


------------pcrid=8

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007157','RCCOMV','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007157','FR','GRP COM VEN ET CA');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007157','1',null,'300749',null);---vendor 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007157','2',null,'300759',null);---ca 


/

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007158','RCCOMS','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007158','FR','GRP COM VEN ET CA');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007158','1',null,'300751',null);---SGEC 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007158','2',null,'300759',null);---ca 

/

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007159','GRPCC','AVDOSS','=','CC_COM',null,'ACTIVE','8508',null,null,null,null,'LOGICAL',null,null);
---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007159','FR','GRP  CC COM');

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007160','RCCCA','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007160','FR','GRP CC COM ET CA');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007160','1',null,'3007159',null);---CCOM
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007160','2',null,'300759',null);---ca 

/
---rule groupe comsgl
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007148','GRPCOM','AVDOSS','=','COMSGL',null,'ACTIVE','8508',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007148','FR','Groupe Clipro');

---BIG RULE
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007161','RCCCA','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007161','FR','GRP CC COM ET CA');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007161','1',null,'3007157',null);---VENDOR
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007161','2',null,'3007158',null);---SGEC
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007161','3',null,'3007160',null);---CCOM
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007161','3',null,'3007148',null);---COMSGL


  delete from lkpcrrul  where  pcrid =8  ; 
  Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('8','1','3007161',to_date('01/01/20','DD/MM/RR'),null,null);


-----PCRID=4


Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007147','GRPCLI','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007147','FR','GRP CCCOM ET CA');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007147','1',null,'300749',null); 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007147','2',null,'300758',null);-

---------
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007153','RULCOM','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007153','FR','Comm et ca');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007153','1',null,'300751',null);---vendor 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007153','2',null,'300758',null);---ca 

---rule groupe ccpro
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007152','GRPPRO','AVDOSS','=','CC_PRO',null,'ACTIVE','8508',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007152','FR','Groupe Clipro');


---------
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007156','RULPRO','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007156','FR','Rule ckipro et ca inf 10');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007156','1',null,'3007152',null);---vendor 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007156','2',null,'300758',null);---ca 


---big rule pcrid4


---------
Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007149','RGRPCA','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007149','FR','Rule bareme sogvendor');


Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007149','1',null,'3007147',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007149','2',null,'3007153',null); 
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007149','2',null,'3007156',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007149','2',null,'3007148',null);

	delete from lkpcrrul  where  pcrid =4  ;
    Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('4','1','3007149',to_date('01/01/20','DD/MM/RR'),null,null);

commit;
