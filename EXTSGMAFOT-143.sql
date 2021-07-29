-------demande lot 1 --demande num 4


---RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values (3007116,'GRPTIR','AVDOSS','IN',null,null,'ACTIVE','4',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values (3007116,'FR','Rule pour grpes tirage');

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 1, '','506', '');--COMSGL


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 2, '', '503', '');--CC_PRO


insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 3, '', '505', '');--CC_COM

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 4, '', '508', '');--CASGL

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 5, '', '501', '');--CAFV

insert into rulvalue (RULID, RVAORDRE, RVAVALUE, RULIDVALUE, CRIID) 
values (3007116, 6, '', '509', '');--CAFV


---clipro
update cddrul  set rulid = 3007116 where CDDORDRE  = 1021   and rulid =508 ;



---clicom

update cddrul   set rulid = 3007116  where CDDORDRE  =2012  and rulid =509 ;
commit ;