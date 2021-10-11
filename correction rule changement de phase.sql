--------correction rule changement de phase 
----saisie avis favo --clicom

update cddrul set rulid =1003 where cddordre =2017 and cseid =108 and rulid =1002 ;---demande Favor
update cddrul set rulid =1001 where cddordre =20172 and cseid =108 and rulid =1000 ;---demande ajour
update cddrul set rulid =1005 where cddordre =20173 and cseid =108 and rulid =1004 ;---demande refusee

----Process CLICOM : Saisie décision Depuis DCCIT
update cddrul set rulid =300760 where cddordre =2064 and cseid =108 and rulid =300762 ;
update cddrul set rulid =1001 where cddordre =2065 and cseid =108 and rulid =1000 ;
update cddrul set rulid =300761 where cddordre =2066 and cseid =108 and rulid =300763 ;---



----Process CLICOM : Saisie décision Depuis DCCIT
update cddrul set rulid =300760 where cddordre =2064 and cseid =108 and rulid =300762 ;---demande accord
update cddrul set rulid =1001 where cddordre =2065 and cseid =108 and rulid =1000 ;---demande accord
update cddrul set rulid =300761 where cddordre =2066 and cseid =108 and rulid =300763 ;---demande accord



---saisie avis commercial -- process clipro
update cddrul set rulid =1003 where cddordre =1071 and cseid =108 and rulid =1002 ;
update cddrul set rulid =1001 where cddordre =1072 and cseid =108 and rulid =1000 ;
update cddrul set rulid =1005 where cddordre =1073 and cseid =108 and rulid =1004 ;


---Process CLIPRO : Saisie décision RMCP/Risque SGL ( rmcp) 
 	 update cddrul set rulid =1003 where cddordre =10122 and cseid =108 and rulid =300762 ;--avis favo
    update cddrul set rulid =1001 where cddordre =10123 and cseid =108 and rulid =1000 ;--avis ajour
    update cddrul set rulid = 1005 where cddordre =10124 and cseid =108 and rulid =300763 ;--avis DEFAV



---processc clipro Saisie décision RMCP/Risque SGL ( risq)


  update cddrul set rulid =300760 where cddordre =10182 and cseid =108 and rulid =300762 ;--avis acco
    update cddrul set rulid =1001 where cddordre =10183 and cseid =108 and rulid =1000 ;--avis ajour
    update cddrul set rulid = 300761 where cddordre =10184 and cseid =108 and rulid =300763 ;--avis refu

---process clipro -- decision direction risque

----- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','27','8','CHANGE_PHASE','DACPT',null,null,null);
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','27','9','CHANGE_PHASE','DAJRN',null,null,null);
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','27','10','CHANGE_PHASE','DRFSE',null,null,null);
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','27','11','CHANGE_PHASE','DADG',null,null,null);
 
 Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','27','8','FR','Status',null);
 Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','27','9','FR','Status',null);
 Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','27','10','FR','Status',null);
 Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','27','11','FR','Status',null);
                
      ---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10278',null,'8',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10278','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10278',null,'27',null,null);

 ---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10278','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10278','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10278','300760');

---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10279',null,'9',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10279','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10279',null,'27',null,null);

 ---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10279','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10279','1001');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10279','1001');      

         ---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','102710',null,'10',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','102710','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','102710',null,'27',null,null);

 ---- CDDRUL
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','102710','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','102710','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','102710','300761');      

  ---- CUSDEFDATA
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','102711',null,'11',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','102711','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','102711',null,'27',null,null);

 ---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','102711','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','102711','1003');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','102711','1003');    

--------process clipro --validation DG 
               select * from lanjalon where lancode='FR' ; 
----- WSTCONSEQUENCE

Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','28','5','CHANGE_PHASE','DACPT',null,null,null);
Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','28','6','CHANGE_PHASE','DRFSE',null,null,null);

 
 Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','28','5','FR','Status',null);
    Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','28','6','FR','Status',null);      
      
      ---- CUSDEFDATA
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10285',null,'5',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10285','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10285',null,'28',null,null);

 ---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10285','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10285','300760');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10285','300760');

         ---- CUSDEFDATA

Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','1','10286',null,'6',null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','2','10286','WFCLIPRO',null,null,null);
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('108','3','10286',null,'28',null,null);

 ---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','1','10286','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','2','10286','300761');
Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('108','3','10286','300761');      



----clipro---Verification demande tirée
 
 update cddrul set rulid =300760 where cddordre =10222 and cseid =108 and rulid =300762 ;--avis acco
    update cddrul set rulid =1001 where cddordre =10223 and cseid =108 and rulid =1000 ;--avis ajour
    update cddrul set rulid = 300761 where cddordre =10224 and cseid =108 and rulid =300763 ;--avis refu


commit ; 
