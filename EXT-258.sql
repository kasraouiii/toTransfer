-----EXT-258


	Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','5','11','PROCESS','2',null,null,null);
        Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','5','11','FR',null,null);
        
    Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','2','11','PROCESS','2',null,null,null);
        Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','2','11','FR',null,null);
             
    Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','3','5','PROCESS','2',null,null,null);
        Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','3','5','FR',null,null);
		
		
		  update wstconsequence   set WSCACTIONCODE = '1' where worcode  ='WFCLICOM'   and wstorder =1  and wscorder=5 and WSCACTIONTYPE='PROCESS' AND WSCACTIONCODE=50 ;
    
    Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLICOM','1','12','PROCESS','2',null,null,null);
        Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLICOM','1','12','FR',null,null);
        
        update wstconsequence   set WSCACTIONCODE = '1' where worcode  ='WFCLICOM'   and wstorder =14  and wscorder=8 and WSCACTIONTYPE='PROCESS' AND WSCACTIONCODE=50 ; 
    
    Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLICOM','14','13','PROCESS','2',null,null,null);
        Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLICOM','14','13','FR',null,null);
 
 
 
 
 commit ;