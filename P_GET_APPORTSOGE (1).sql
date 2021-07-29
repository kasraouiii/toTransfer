PROCEDURE P_GET_APPORTSOGE(
       p_actid                 NUMBER,
      p_dosid                 NUMBER,
      p_dprversion            VARCHAR2,
      p_exercice              NUMBER,
      p_tefclasse             VARCHAR2,
      p_lancode      IN       VARCHAR2,
      P_ANAID               NUMBER,
       o_valeur       IN OUT   VARCHAR2,
      o_libvaleur    IN OUT   VARCHAR2
      )  IS

  VAL1    VARCHAR2(100);
  VAL2    VARCHAR2(100);
  VAL3    VARCHAR2(100);
  RES VARCHAR2(100);
  BAREM VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
  BAREM := '';

 --Select premier loyer HT
 /*  SELECT nvl(PFIMTPREMIERLOYER,0) 
        --SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (ACTDTIMMATRICULATION, 'YYYY')
          INTO VAL1
          FROM propositionfinanciere
         WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1);*/

  --Select premier loyer TTC 
select PFIAPPROCHEFLUX 
into BAREM
from PROPOSITIONFINANCIERE 
where pfiid in (select pfiid from DPRPROPFINANCE where dosid=p_dosid and DPFFLAGRETENUE=1);

  IF BAREM='PALMT'
  then 
      select PFRMTLOYER
      into VAL1
      from PFIRUBRIQUE
      where pfiid in (select pfiid from  DPRPROPFINANCE where  DPFFLAGRETENUE=1 and dosid in
      ( select DOS.dosid from DOSSIERPROSPECT DOS where DOS.dosid=p_dosid));
 
 ELSE
        select max((nvl(PFIMTPREMIERLOYER,0) * txt.TTAVAL/100)+PFIMTPREMIERLOYER)
        INTO VAL1
        from propositionfinanciere prop , dossierprospect dospr, taxtaux txt
        where txt.TAXCODE=dospr.taxcode
        and dospr.dosid=p_dosid
        and prop.PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1); 
        
 end IF;
 -- -- mnt investissement TTC 
       SELECT max((nvl(PFIINVESTISSEMENT,0) * txt.TTAVAL/100)+PFIINVESTISSEMENT)
       INTO VAL2
          FROM propositionfinanciere prop , dossierprospect dospr , dprpropfinance dprpop ,taxtaux txt
         WHERE dospr.dosid=p_dosid
         AND dospr.dosid=dprpop.dosid
         AND prop.PFIID=dprpop.pfiid
         AND txt.TAXCODE=dospr.taxcode;
--CALCULER 50% de Mt investissement

        SELECT 	(VAL2*50/100) INTO VAL3 FROM DUAL;

  IF ((to_number(VAL1)<to_number(VAL3)) or (VAL1 is null)) THEN RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_GET_APPORTSOGE;