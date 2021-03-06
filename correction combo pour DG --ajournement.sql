
-----correction combo pour DG --ajournement 

   
----CRITERIA
Insert into CRITERIA (CRIID,CRICODE,CRITARGET,CRITABLETOSEARCH,CRICOLUMNTOSEARCH,CRILANDESCRIPTIONCOLUMN,CRIFUNCTION,CRIVALUETYPE,CRIFILTERCLAUSE,CRIPARAMETER,CRITABLESETTING,CRITARGETTABLE,CRITARGETCOLUMN,CRIFLAGCASSIOPAE,CRITABLETYPE) 
values ('8532','AV_DEC_DG','AVDOSS',null,null,null,'PAV4_CRITERIA.F_AV_DEC_DG','STRING',null,null,null,null,null,null,null);

---- LANCRITERIA
Insert into LANCRITERIA (LANCODE,CRIID,CRILABEL) values ('FR','8532','Decision availability DG');

---- RULE

Insert into RULE (RULID,RULCODE,RULTARGET,RULOPERATOR,RULLOWERLIMIT,RULUPPERLIMIT,RULSTATUS,CRIIDFIRST,RULIDFIRST,RULIDSECOND,CRIIDSECONDMAX,CRIIDSECONDMIN,RULTYPE,RULFLAGCASSIOPAE,RULGROUP) 
values ('3007177','AVDGAJ','AVDOSS','=','DECIAJ01',null,'ACTIVE','8532',null,null,null,null,'LOGICAL',null,null);


----LANRULE

Insert into LANRULE (RULID,LANCODE,RULLABEL) values ('3007177','FR','avis DG ajourn');

----CUSDEFDATA
Insert into CUSDEFDATA (CSEID,CDEORDRE,CDDORDRE,CDDSTRINGVALUE,CDDNUMERICVALUE,CDDDATEVALUE,CDDBOOLEANVALUE) values ('3','1','8','DECIAJ01',null,null,null);

---- CDDRUL

Insert into CDDRUL (CSEID,CDEORDRE,CDDORDRE,RULID) values ('3','1','8','3007177');


commit ;