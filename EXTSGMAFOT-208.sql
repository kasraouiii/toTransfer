
----EXTSGMAFOT-208

update   wstconsequence  set WSCFLAGMAIL ='1'     where wscactiontype='FORMALITE'   and worcode  in  ('WFCLIPRO' , 'WFCLICOM' );


--- TUSPARAM
Insert into TUSPARAM (TUSNOM,TUPCODE,TUPFLAGORFI) values ('NETDOMAINE','06','1');
Insert into TUSPARAM (TUSNOM,TUPCODE,TUPFLAGORFI) values ('NETDOMAINE','07','1');
Insert into TUSPARAM (TUSNOM,TUPCODE,TUPFLAGORFI) values ('NETDOMAINE','08','1');


---- LANTUSPARAM

Insert into LANTUSPARAM (TUSNOM,TUPCODE,LANCODE,TUPLIBELLE,TUPHELPTEXT) values ('NETDOMAINE','06','FR','socgen.com ',null);
Insert into LANTUSPARAM (TUSNOM,TUPCODE,LANCODE,TUPLIBELLE,TUPHELPTEXT) values ('NETDOMAINE','07','FR','teamwillgroup.com',null);
Insert into LANTUSPARAM (TUSNOM,TUPCODE,LANCODE,TUPLIBELLE,TUPHELPTEXT) values ('NETDOMAINE','08','FR','soprasteria.com',null);

delete from lantusparam   where tusnom='NETDOMAINE'  and tupcode in ('01' ,'02' ,'03' ,'04' , '05'  );


commit ;