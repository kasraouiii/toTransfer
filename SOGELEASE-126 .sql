
---SOGELEASE-126


delete  from  FILTREPARAMETRE where FIPNOM = 'TPROFILGESTION'   and fipcode ='AUT02'   ;
Insert into FILTREPARAMETRE (FIPTYPE,FIPNOM,FIPCODE,UGECODE) values ('0','TPROFILGESTION','AUT02','SGM');



commit ;