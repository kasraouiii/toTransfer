
-----EXTSGMAFOT-127


----initiation clipro 

update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid =30522 ;
delete from wstconsequence   where worcode='WFCLIPRO'   and WSTORDER=5 and wscorder=11 ;

----instruction clipro 


update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (50043 ,30522 );

-----saisir avis commercial 
---30524	FR	Retour pour complétude
---30525	FR	Classer Sans Suite la demande


update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (30524,30525);

---Retour pour complitude

update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid =30538;


---proceder au tirage 
update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid =30541;---DEALINVUSER--CEPRO


---Procéder au tirage aprés Forçage
update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid =50021;--GRORECEIVE---CASGL



---Initiation du dossier CLICOM

---Consommation validée par SGL,Procéder a l'analyse 
update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (30511 ,50045);

delete from lanwstconsequence  where worcode='WFCLICOM' and wstorder =1 and WSCORDER=11  ;---50045

---Instruction du dossier CLICOM
---Saisir Avis Commercial

update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (50002 ,50044);


----Avis Commercial

--Avis Défavorable ,Classer Sans Suite la demande--Retour pour complétude
update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (50004,50005);


-----Classer Sans Suite la demande---Retour pour complétude
update fordestination  set FDEULYRECEIVER ='JOB'   ,FDEGROCODERECEIVER=null ,FDEMETIERRECEIVER='CACOM'  where forid in (30507,30508);


















