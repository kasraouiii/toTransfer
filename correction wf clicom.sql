

--correction wf clicom

update cddrul   set rulid =3007139  where cddordre=2067  and cseid=108 and rulid =4013  ;



Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007142','3007142','AVDOSS','IN',null,null,'ACTIVE','3',null,null,null,null,'LOGICAL',null,null);

---- LANRULE
Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007142','FR','tpgcode diff aut et deci favo');

----- RULVALUE

Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007142','1',null,'4008',null);
Insert into RULVALUE (RULID,RVAORDRE,RVAVALUE,RULIDVALUE,CRIID) values ('3007142','2',null,'1002',null);


Insert into WSTJUMP (WORCODE,WSTORDER,WORCODEDEST,WSTORDERDEST,RULID) values ('WFCLICOM','6','WFCLICOM','7',3007142 );




-------- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) 
values ('WFCLICOM','6','16','FORMALITE','30506',null,null,null);

----LANWSTCONSEQUENCE

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION)
 values ('WFCLICOM','6','16','FR','Notification',null);



Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','20616',null,'16',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','20616','WFCLICOM',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','20616',null,'6',null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','20616','4013');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','20616','4013');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','20616','4013');


commit ;



