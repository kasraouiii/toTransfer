----------export  LKPCRRUL

--------Insert into LKPCRRUL (PCRID,PCRORDRE,RULID,PRUDTSTART,PRUDTEND,PRUFLAGFORCING) values ('10','1','300765',to_date('01/01/20','DD/MM/RR'),null,null);

---SOGELEASE-129


delete from   lkpcrrul   where  pcrid =10 and rulid =300765 ;



commit ;