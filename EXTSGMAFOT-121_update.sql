
---EXTSGMAFOT-121_update
 ELSIF SCONTROLE IN ('DAUT')           THEN
               DECLARE
                ncount  NUMBER := 0;
				ncount1   NUMBER := 0;
				ncount2   NUMBER := 0;
		
					
					
                BEGIN
			
					 
					 SELECT NVL (COUNT (DPMDTRELEASE), 0)            
                      INTO ncount
                     FROM DPRMATERIEL
                    WHERE dosid = ndosid  ;
					
					 SELECT NVL (COUNT (DPMEXTERNALREF), 0)            
                      INTO ncount1
                      FROM DPRMATERIEL
                    WHERE dosid = ndosid  ;
					 
				  SELECT NVL (COUNT (AADORDRESUPPLIER), 0)            
                     INTO ncount2
                    FROM DPRMATERIEL
                     WHERE dosid = ndosid  ;
                     

                   IF ncount = 0 OR ncount1 = 0 OR  ncount2 = 0  
                   THEN
                      NOK :=0; 
                   ELSE
                           nok := 1;  --OK
                   END IF;
                 
		
               EXCEPTION
                   WHEN OTHERS
             THEN
                       NOK := 0;
              END;   
    
    
-----------------

delete  from WSTCONSEQUENCE  where  WORCODE = 'WFCLIPRO'  and WSCACTIONCODE ='U_DAUT'  ;
delete  from WSTCONSEQUENCE  where  WORCODE = 'WFCLICOM'  and WSCACTIONCODE ='U_DAUT'  ;


delete  from lanWSTCONSEQUENCE  where  WORCODE = 'WFCLIPRO'  and   WSTORDER=21 and WSCORDER=9  ;

delete  from lanWSTCONSEQUENCE  where  WORCODE = 'WFCLICOM'  and   WSTORDER=12 and WSCORDER=7  ;


Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST)
 values ('WFCLIPRO','22','10','CONTROLS','U_DAUT',null,null,null);

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','22','10','FR',null,null);


Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST)
 values ('WFCLICOM','18','9','CONTROLS','U_DAUT',null,null,null);

Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLICOM','18','9','FR',null,null);



update lkttctev set TMFFONCTION='EVF_DECISION'  where ttccode='U_DAUT'  ;


commit ;



