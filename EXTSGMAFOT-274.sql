
---EXTSGMAFOT-274


---- WSTCONSEQUENCE
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','8','11','CHANGE_PHASE','DAJRN',null,null,null);
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','8','0','CHANGE_PHASE','DRFSE',null,null,null);

---LANWSTCONSEQUENCE
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','8','11','FR','Status',null);
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','8','0','FR','Status',null);




update wstconsequence  set wscorder=1  where  worcode= 'WFCLIPRO'   and wstorder = 8  and wscorder= 11  and WSCACTIONCODE= 'DAJRN';
update wstconsequence  set wscorder=11  where  worcode= 'WFCLIPRO'   and wstorder = 8  and wscorder= 1  ;

----avis favo
---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','1082',null,'2',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','1082','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','1082',null,'8',null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','1082','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','1082','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','1082','1003');

-----ajournem
---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10814',null,'1',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10814','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10814',null,'8',null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10814','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10814','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10814','1001');


---avis defavo
---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10815',null,'0',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10815','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10815',null,'8',null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10815','1005');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10815','1005');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10815','1005');



update lkckegre   set egrflagdisabled =0 where ckeid = 2073  and egrflagdisabled=1 ;


commit ;





