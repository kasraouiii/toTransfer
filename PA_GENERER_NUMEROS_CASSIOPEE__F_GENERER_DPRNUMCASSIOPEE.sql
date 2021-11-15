   FUNCTION F_GENERER_DPRNUMCASSIOPEE (
        P_DOSID                    DOSSIERPROSPECT.DOSID%TYPE,
        P_DPRNUMERO                DOSSIERPROSPECT.DPRNUMERO%TYPE,
        P_TPGCODE                  DOSSIERPROSPECT.TPGCODE%TYPE,
        P_DPRNUMCASSIOPEE   IN OUT DOSSIERPROSPECT.DPRNUMCASSIOPEE%TYPE)
        RETURN VARCHAR2
    AS
        BFLAGGUARANTEE   TPROFILGESTION.TPGFLAGGUARANTEECONTRACT%TYPE := 0;
        BFLAGOPTION      TOPTPGPARAM.TTPLOGIQUE%TYPE := 0;
        SDEPARTMENT      VARCHAR2 (3) := NULL;
        SYEAR            VARCHAR2 (4) := NULL;
        SPRODUCT         VARCHAR2 (1) := NULL;
        SSERIAL          VARCHAR2 (7) := NULL;
        NMAXNUMBER       NUMBER := NULL;
        SMAXDOSNM        VARCHAR2 (7) := NULL;
        NUMEROCKIOP      VARCHAR2 (7) := NULL;
        SFINALNUMBER     DOSSIERPROSPECT.DPRNUMCASSIOPEE%TYPE := NULL;
    BEGIN


  /*    
        SELECT
                TPGFLAGGUARANTEECONTRACT
              , PAV4_SELECTPARAMETRE.F_TOPTPGPARAM_TTPLOGIQUE('FRONT_DEAL', 'NUMPRODUCT', P_TPGCODE)
              , SUBSTR(TPGDPTCODE, 1, 3)
            INTO
                BFLAGGUARANTEE
              , BFLAGOPTION
              , SDEPARTMENT
            FROM
                TPROFILGESTION
            WHERE
                TPGCODE       = P_TPGCODE;
            IF BFLAGGUARANTEE = 1 OR BFLAGOPTION = 1 THEN
                PAV4_TRACE.DEBUG('Type=Guarantee => specific algorithm for DOSNUM') ;
                SELECT
                    SUBSTR(DPRSECTGESTION, 1, 3)
                INTO
                    SDEPARTMENT
                FROM
                    DOSSIERPROSPECT
                WHERE
                    DOSID          = P_DOSID
                    AND DPRVERSION =
                    (
                        SELECT DPRVERSION FROM V_DEAL VDE WHERE VDE.DOSID = P_DOSID
                    ) ;
                PAV4_TRACE.DEBUG('Dept=' || SDEPARTMENT) ;
                SYEAR    := TO_CHAR(SYSDATE, 'YYYY') ;
                SPRODUCT := SUBSTR(P_TPGCODE, 1, 1) ;
                SELECT
                    NVL(TO_NUMBER(MAX(SUBSTR(DPRNUMCASSIOPEE, - 7))), 0)
                INTO
                    NMAXNUMBER
                FROM
                    DOSSIERPROSPECT
                WHERE
                    DPRNUMCASSIOPEE LIKE SDEPARTMENT
                    || '-'
                    || SYEAR
                    || '-'
                    || SPRODUCT
                    || '-%'
                    AND LENGTH(DPRNUMCASSIOPEE) = 6;----18
                PAV4_TRACE.DEBUG('Max DPRNUMEROCASSIOPAE = ' || NMAXNUMBER) ; 
                SELECT
                    GREATEST(NVL(TO_NUMBER(MAX(SUBSTR(DOSNUM, -6))), 0), NMAXNUMBER)--7
                INTO
                    NMAXNUMBER
                FROM
                    DOSSIER
                WHERE
                    DOSNUM LIKE SDEPARTMENT
                    || '-'
                    || SYEAR
                    || '-'
                    || SPRODUCT
                    || '-%'
                    AND LENGTH(DOSNUM) = 6;--18
                PAV4_TRACE.DEBUG('Max including DOSNUM = ' || NMAXNUMBER) ;
                SSERIAL      := LPAD(TO_CHAR(NMAXNUMBER + 1), 7, 0) ;
                SFINALNUMBER := SDEPARTMENT || '-' || SYEAR || '-' || SPRODUCT || '-' || SSERIAL;
            ELSE
                SFINALNUMBER := P_DPRNUMCASSIOPEE;
            END IF; */


     /*  select max (dosnum)  
       INTO  SMAXDOSNM
        from dossier@SGM_LINK 
        where dosnum  not like'%P%'  
        and dosnum not like '%AUT%'; */


      select max (NUMNUMERO)  
       INTO  SMAXDOSNM
        from NUMERO@SGM_LINK
        where numcible ='DOSNUM'  ; 
        
   
     NUMEROCKIOP:=  lpad(to_number(SMAXDOSNM + 1), 6, 0) ;
  
    -- RETURN NMAXNUMBER; 
     RETURN NUMEROCKIOP;
    END F_GENERER_DPRNUMCASSIOPEE;