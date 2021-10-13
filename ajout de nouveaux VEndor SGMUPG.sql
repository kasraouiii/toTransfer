-------ajout de nouveaux VEndor SGMUPG

Begin
 
P_duplicate_user_fo ('REDAB', 'U_VENDOR', 'Bouazzaoui', 'REDA');
End;
 
Begin
p_duplicate_accesrights_bo('REDAB','U_VENDOR') ;
End;
 
Begin
p_md5_utipwd('REDAB','REDAB') ;
End;
 
Insert into UTICOORDONNEE (UTICODE,UCOORDRE,UCOTYPE,UCOREFERENCE,UCOEXTENSION,UCODOMAIN) 
values ('REDAB','1','NET','reda.bouazzaoui@volvo.com',null,null);
--------------------
Begin
 
P_duplicate_user_fo ('KHADIJAC', 'U_VENDOR', 'CHRAIBI', 'Khadija');
End;
 
Begin
p_duplicate_accesrights_bo('KHADIJAC','U_VENDOR') ;
End;
 
Begin
p_md5_utipwd('KHADIJAC','KHADIJAC') ;
End;
 
Insert into UTICOORDONNEE (UTICODE,UCOORDRE,UCOTYPE,UCOREFERENCE,UCOEXTENSION,UCODOMAIN) 
values ('KHADIJAC','1','NET','khadija.chraibi@volvo.com',null,null);
----------------
Begin
 
P_duplicate_user_fo ('MOHAMMEDA', 'U_VENDOR', 'Aslal', 'Mohammed');
End;
 
Begin
p_duplicate_accesrights_bo('MOHAMMEDA','MOHAMMEDA') ;
End;
 
Begin
p_md5_utipwd('MOHAMMEDA','MOHAMMEDA') ;
End;
 
Insert into UTICOORDONNEE (UTICODE,UCOORDRE,UCOTYPE,UCOREFERENCE,UCOEXTENSION,UCODOMAIN) 
values ('MOHAMMEDA','1','NET','mohamed.aslal@scania.co.ma',null,null);

-----

Begin
 
P_duplicate_user_fo ('HICHAMBA', 'U_VENDOR', 'BARHOUL', 'Hicham');
End;
 
Begin
p_duplicate_accesrights_bo('HICHAMBA','U_VENDOR') ;
End;
 
Begin
p_md5_utipwd('HICHAMBA','HICHAMBA') ;
End;
 
Insert into UTICOORDONNEE (UTICODE,UCOORDRE,UCOTYPE,UCOREFERENCE,UCOEXTENSION,UCODOMAIN) 
values ('HICHAMBA','1','NET','hicham.barhoul@scania.co.ma',null,null);
-----------
Begin
 
P_duplicate_user_fo ('HAMIDA', 'U_VENDOR', 'Amnay', 'Hamid');
End;
 
Begin
p_duplicate_accesrights_bo('HAMIDA','U_VENDOR') ;
End;
 
Begin
p_md5_utipwd('HAMIDA','HAMIDA') ;
End;
 
Insert into UTICOORDONNEE (UTICODE,UCOORDRE,UCOTYPE,UCOREFERENCE,UCOEXTENSION,UCODOMAIN) 
values ('HAMIDA','1','NET','hamid.amnay@volvo.com',null,null);







commit;





























