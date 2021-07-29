create or replace PACKAGE BODY pav4_ratio
AS
    PROCEDURE P_LIST_RATIO (P_LANCODE             VARCHAR2,
                            P_RATID               NUMBER,
                            P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION   IN     VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_ANMID               NUMBER,
                            P_ANAID               NUMBER,
                            PC_LIST        IN OUT T_CURSOR)
    IS
        L_FUNCTION    RATIO.RATLISTVALUESFUNCTION%TYPE := NULL;
        L_SQL         VARCHAR2 (100) := NULL;
        O_LIBVALEUR   VARCHAR2 (100) := NULL;
    BEGIN
        SELECT RATLISTVALUESFUNCTION
          INTO L_FUNCTION
          FROM RATIO
         WHERE RATID = P_RATID;

        CASE L_FUNCTION
            WHEN 'P_LIST_FICP'    --MBN 07112017 fonctions reportees de TLG V4
            THEN
                P_LIST_FICP (p_lancode, pc_list);
            WHEN 'P_LIST_TYPECLIENT'
            THEN
                p_list_typeclient (p_lancode, pc_list);
            /*   WHEN 'P_LIST_PRESLIASSE'
               THEN
                  p_list_presliasse (p_lancode, pc_list);*/
            WHEN 'P_LIST_ANTETABLFIN'
            THEN
                p_list_antetablfin (p_lancode, pc_list);
            WHEN 'P_LIST_ANTBANQUE'
            THEN
                p_list_antbanque (p_lancode, pc_list);
            WHEN 'P_LIST_FIBEN'
            THEN
                p_list_fiben (p_lancode, pc_list);
            WHEN 'P_LIST_WATCHLIST'
            THEN
                p_list_watchlist (p_lancode, pc_list);
            WHEN 'P_LIST_TYPECTTRAVAIL'
            THEN
                p_list_typecttravail (p_lancode, pc_list);
            /* WHEN 'P_LIST_ANTITERRORISTE'
             THEN
                P_LIST_ANTITERRORISTE (p_lancode, pc_list);*/
            /*  WHEN 'P_LIST_FCC'
               THEN
                  P_LIST_FCC (p_lancode, pc_list);*/
            WHEN 'P_LIST_STATUT'
            THEN
                P_LIST_STATUT (p_lancode, pc_list);
            WHEN 'P_LIST_CLASSE_RISQ_BCT'
            THEN
                P_LIST_CLASSE_RISQ_BCT (p_lancode, pc_list);
            WHEN 'P_LIST_CLASSE_RISQUE'
            THEN
                P_LIST_CLASSE_RISQUE (p_lancode, pc_list);
            WHEN 'P_LIST_OUINON'
            THEN
                P_LIST_OUINON (p_lancode, pc_list);
             WHEN 'P_LIST_SCORECALIBRE'
            THEN
                P_LIST_SCORECALIBRE (p_lancode, pc_list);    
            WHEN 'P_LIST_SOGEACT'
            THEN
                P_LIST_SOGEACT (p_lancode, pc_list);
            WHEN 'P_LIST_SOGERELATION'
            THEN
                P_LIST_SOGERELATION (p_lancode, pc_list);
            WHEN 'P_LIST_SOGEPROF'
            THEN
                P_LIST_SOGEPROF (p_lancode, pc_list);
            WHEN 'P_LIST_CLASSE_RISQ_COMM'
            THEN
                P_LIST_CLASSE_RISQ_COMM (p_lancode, pc_list);
            WHEN 'P_LIST_TYPECONCENTRA'
            THEN
                P_LIST_TYPECONCENTRA (p_lancode, pc_list);
            WHEN 'P_LIST_STATUTCC'
            THEN
                p_list_STATUTCC (p_lancode, pc_list);
            WHEN 'P_LIST_AVISSOURCE'
            THEN
                P_LIST_AVISSOURCE (p_lancode, pc_list);
            WHEN 'P_LIST_TYPESOURCE'
            THEN
                P_LIST_TYPESOURCE (p_lancode, pc_list);
            WHEN 'P_LIST_BANK'
            THEN
                P_LIST_BANK (p_lancode, pc_list);
            WHEN 'P_LIST_ACCESS'
            THEN
                P_LIST_ACCESS (p_lancode, pc_list);
            WHEN 'P_LIST_USAGE'
            THEN
                P_LIST_USAGE (p_lancode, pc_list);
            WHEN 'P_LIST_AGENCE'
            THEN
                P_LIST_AGENCE (p_lancode, pc_list);
            WHEN 'P_LIST_MODEPAIMENET'
            THEN
                P_LIST_MODEPAIMENET (p_lancode, pc_list);
            WHEN 'P_LIST_TYPE'
            THEN
                P_LIST_TYPE (p_lancode, pc_list);
            WHEN 'P_LIST_CLASS'
            THEN
                P_LIST_CLASS (p_lancode, pc_list);
            WHEN 'P_LIST_ETAT'
            THEN
                P_LIST_ETAT (p_lancode, pc_list);
            WHEN 'P_LIST_OCCUPY'
            THEN
                P_LIST_OCCUPY (p_lancode, pc_list);
            WHEN 'P_LIST_AQUISITION'
            THEN
                P_LIST_AQUISITION (p_lancode, pc_list);
            WHEN 'P_LIST_PERIODICITE'
            THEN
                P_LIST_PERIODICITE (p_lancode, pc_list);
            WHEN 'P_LIST_PAYS'
            THEN
                P_LIST_PAYS (p_lancode, pc_list);
            WHEN 'P_LIST_STATUTMATERIEL'
            THEN
                P_LIST_STATUTMATERIEL (p_lancode, pc_list);
            WHEN 'P_LIST_HAS_AUTO'
            THEN
                P_LIST_HAS_AUTO (p_lancode, pc_list);
              WHEN 'P_LIST_LADSGM'
            THEN
                P_LIST_LADSGM (p_lancode, pc_list);   

               WHEN 'P_LIST_KYCEFF'
            THEN
                P_LIST_KYCEFF (p_lancode, pc_list);     

          WHEN 'P_LIST_GARANTIE'
            THEN
                P_LIST_GARANTIE (p_lancode, pc_list); 

            ELSE
                OPEN PC_LIST FOR
                    SELECT TO_CHAR (NULL) AS CODE_VALEUR,
                           TO_CHAR (NULL) AS LIBELLE
                      FROM DUAL
                     WHERE 1 = 2;
        END CASE;
    EXCEPTION
        WHEN NO_DATA_FOUND
        THEN
            OPEN PC_LIST FOR
                SELECT TO_CHAR (NULL) AS CODE, TO_CHAR (NULL) AS LIBELLE
                  FROM DUAL
                 WHERE 1 = 2;
    END P_LIST_RATIO;

    PROCEDURE P_TABLE_VALUES_RATIO (
        P_RATID        IN     NUMBER,
        P_RATCODE      IN     VARCHAR2,
        P_ACTID        IN     NUMBER,
        P_DOSID        IN     NUMBER,
        P_DPRVERSION   IN     VARCHAR2,
        P_TEFCLASSE    IN     VARCHAR2,
        P_EXERCICE     IN     NUMBER,
        P_ANAID        IN     NUMBER,
        P_ANMID        IN     NUMBER,
        P_LANCODE      IN     LANGUE.LANCODE%TYPE,
        PC_LIST        IN OUT T_CURSOR)
    IS
        L_FUNCTION   RATIO.RATLISTVALUESFUNCTION%TYPE := NULL;
        L_SQL        VARCHAR2 (100) := NULL;
    BEGIN
        SELECT RATLISTVALUESFUNCTION
          INTO L_FUNCTION
          FROM RATIO
         WHERE RATID = P_RATID;

        CASE L_FUNCTION
            WHEN 'P_LIST_BILAN'
            THEN
                P_LIST_BILAN (P_DOSID,
                              P_DPRVERSION,
                              P_ACTID,
                              P_LANCODE,
                              PC_LIST);
            WHEN 'P_ALERT'
            THEN
                P_ALERT (P_DOSID,
                         P_DPRVERSION,
                         P_ACTID,
                         P_LANCODE,
                         PC_LIST);
            WHEN 'P_LIST_PFIGUARANTEE'
            THEN
                P_LIST_PFIGUARANTEE (P_DOSID,
                                     P_DPRVERSION,
                                     P_LANCODE,
                                     PC_LIST);
               WHEN 'P_LIST_GEOGPART'                        
            THEN 
               P_LIST_GEOGPART (P_DOSID,
                                     P_DPRVERSION,
                                     P_ACTID,
                                     P_LANCODE,
                                     PC_LIST);                         
                    WHEN 'P_LIST_REPRLEG'                        
            THEN 
               P_LIST_REPRLEG (P_DOSID,
                                     P_DPRVERSION,
                                     P_ACTID,
                                     P_LANCODE,
                                     PC_LIST); 

                   WHEN 'P_LIST_AUAFFGR'                        
            THEN 
               P_LIST_AUAFFGR (P_DOSID,
                                     P_DPRVERSION,
                                     P_ACTID,
                                     P_LANCODE,
                                     PC_LIST);                     

            WHEN 'P_LIST_OBJET_FINANCE'
            THEN
                P_LIST_OBJET_FINANCE (P_DOSID,
                                      P_DPRVERSION,
                                      P_LANCODE,
                                      PC_LIST);

           WHEN 'P_LIST_FRAICOMM' 
         THEN
                P_LIST_FRAICOMM (P_DOSID,
                                      P_DPRVERSION,
                                      P_LANCODE,
                                      PC_LIST);        
        WHEN 'P_LIST_PROPOSITION' 
         THEN
                P_LIST_PROPOSITION (P_DOSID,
                                      P_DPRVERSION,
                                      P_LANCODE,
                                      PC_LIST);   
       WHEN 'P_LIST_ENGSGLM' 
         THEN
                P_LIST_ENGSGLM (P_DOSID,
                                      P_DPRVERSION,
                                      P_ACTID,
                                      P_LANCODE,
                                      PC_LIST);



         WHEN 'P_LIST_ENGSGLMAUTAFFG'
          THEN
               P_LIST_ENGSGLMAUTAFFG(P_DOSID,
                                      P_DPRVERSION,
                                      P_ACTID,
                                      P_LANCODE,
                                      PC_LIST);

         WHEN 'P_LIST_AUT'
          THEN
           P_LIST_AUT(P_DOSID,
                                      P_DPRVERSION,
                                      P_ACTID,
                                      P_LANCODE,
                                      PC_LIST);

              WHEN 'P_LIST_ATAUTO'
          THEN
           P_LIST_ATAUTO(P_DOSID,
                                      P_DPRVERSION,
                                      P_ACTID,
                                      P_LANCODE,
                                      PC_LIST);                        



             WHEN 'P_LISTLEASA'
          THEN
           P_LISTLEASA(P_DOSID,
                                      P_DPRVERSION,
                                      P_ACTID,
                                      P_LANCODE,
                                      PC_LIST);

            ELSE
                OPEN PC_LIST FOR
                    SELECT TO_CHAR (NULL) AS CODE_VALEUR,
                           TO_CHAR (NULL) AS LIBELLE
                      FROM DUAL
                     WHERE 1 = 2;
        END CASE;
    END P_TABLE_VALUES_RATIO;

    PROCEDURE P_EVAL_RATIO (p_anaid              NUMBER,
                            P_FORMULE            VARCHAR2,
                            P_LANCODE            VARCHAR2,
                            O_VALEUR      IN OUT VARCHAR2,
                            O_LIBVALEUR   IN OUT VARCHAR2)
    IS
        L_START      NUMBER;
        L_CC         VARCHAR2 (1);
        L_SQL        VARCHAR2 (500);
        L_RATIO      VARCHAR2 (50);
        L_VALRATIO   VARCHAR2 (20);
    BEGIN
        L_START := 0;
        L_SQL := '';

        FOR I IN 1 .. LENGTH (P_FORMULE)
        LOOP
            L_CC := SUBSTR (P_FORMULE, I, 1);

            IF L_START = 1
            THEN
                IF L_CC = ']'
                THEN
                    L_RATIO := L_RATIO || '';
                ELSE
                    L_RATIO := L_RATIO || L_CC;
                END IF;
            END IF;

            IF L_CC = '['
            THEN
                L_START := 1;
                L_RATIO := '';
            ELSE
                IF L_CC = ']' AND L_START = 1
                THEN
                    L_START := 0;
                    DBMS_OUTPUT.PUT_LINE ('L_RATIO:' || L_RATIO || '-');

                    BEGIN
                        SELECT NVL (L.RATCONCLUSION, 0)
                          INTO L_VALRATIO
                          FROM LKANARAT L, RATIO R
                         WHERE     L.ANAID = p_anaid
                               AND L.RATID = R.RATID
                               AND R.RATCODE = TRIM (L_RATIO);

                        L_SQL :=
                               L_SQL
                            || ' SELECT '
                            || L_VALRATIO
                            || ' AS A1 FROM DUAL UNION';
                        DBMS_OUTPUT.PUT_LINE (
                            'SQL:' || SUBSTR (L_VALRATIO, 1, 200));
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            L_SQL := L_SQL;
                            DBMS_OUTPUT.PUT_LINE (
                                SUBSTR (
                                       'NO_DATA : L_VALEUR : '
                                    || P_ANAID
                                    || '-'
                                    || L_SQL
                                    || '-'
                                    || SQLERRM,
                                    1,
                                    250));
                    END;
                ELSE
                    IF L_START = 0
                    THEN
                        IF L_CC NOT IN (';', ',')
                        THEN
                            L_SQL := L_SQL || L_CC;
                        END IF;
                    END IF;
                END IF;
            END IF;
        END LOOP;

        --DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);
        IF    UPPER (SUBSTR (L_SQL, 1, 3)) = 'MIN'
           OR UPPER (SUBSTR (L_SQL, 1, 3)) = 'MAX'
        THEN
            L_SQL := SUBSTR (L_SQL, 4, LENGTH (L_SQL) - 3);
        END IF;

        --DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);
        IF SUBSTR (L_SQL, LENGTH (L_SQL) - 5, 5) = 'UNION'
        THEN
            L_SQL := SUBSTR (L_SQL, 1, LENGTH (L_SQL) - 6);
        END IF;

        L_SQL :=
               'SELECT '
            || SUBSTR (P_FORMULE, 1, 3)
            || '(A1) FROM ( '
            || L_SQL
            || '))';
        DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || SUBSTR (L_SQL, 1, 200));

        EXECUTE IMMEDIATE L_SQL INTO O_VALEUR;
    --O_VALEUR := 'GOOD';
    --O_LIBVALEUR := 'OK EVAL RATIO';
    END;

    PROCEDURE P_EVAL_CONC (P_ANAID              NUMBER,
                           P_FORMULE            VARCHAR2,
                           P_LANCODE            VARCHAR2,
                           O_VALEUR      IN OUT VARCHAR2,
                           O_LIBVALEUR   IN OUT VARCHAR2)
    IS
        L_START      NUMBER;
        L_CC         VARCHAR2 (1);
        L_SQL        VARCHAR2 (500);
        L_RATIO      VARCHAR2 (50);
        L_VALRATIO   VARCHAR2 (20);
    BEGIN
        L_START := 0;
        L_SQL := '';

        FOR I IN 1 .. LENGTH (P_FORMULE)
        LOOP
            L_CC := SUBSTR (P_FORMULE, I, 1);

            IF L_START = 1
            THEN
                IF L_CC = ']'
                THEN
                    L_RATIO := L_RATIO || '';
                ELSE
                    L_RATIO := L_RATIO || L_CC;
                END IF;
            END IF;

            IF L_CC = '['
            THEN
                L_START := 1;
                L_RATIO := '';
            ELSE
                IF L_CC = ']' AND L_START = 1
                THEN
                    L_START := 0;
                    DBMS_OUTPUT.PUT_LINE ('L_RATIO:' || L_RATIO || '-');

                    BEGIN
                        SELECT NVL (L.RATCONCLUSION, 0)
                          INTO L_VALRATIO
                          FROM LKANARAT L, RATIO R
                         WHERE     L.ANAID = P_ANAID
                               AND L.RATID = R.RATID
                               AND R.RATCODE = TRIM (L_RATIO);

                        L_SQL := L_SQL || L_VALRATIO;
                        DBMS_OUTPUT.PUT_LINE (
                               'SQL POUR '
                            || L_RATIO
                            || '-'
                            || P_ANAID
                            || ' : '
                            || SUBSTR (L_VALRATIO, 1, 200));
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            L_SQL := L_SQL;
                            DBMS_OUTPUT.PUT_LINE (
                                SUBSTR (
                                       'NO_DATA : L_VALEUR : '
                                    || P_ANAID
                                    || '-'
                                    || L_SQL
                                    || '-'
                                    || SQLERRM,
                                    1,
                                    250));
                    END;
                ELSE
                    IF L_START = 0
                    THEN
                        IF L_CC NOT IN (';', ',')
                        THEN
                            L_SQL := L_SQL || L_CC;
                        END IF;
                    END IF;
                END IF;
            END IF;
        END LOOP;

        DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);
        L_SQL := 'SELECT ' || L_SQL || ' FROM DUAL';
        DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);

        EXECUTE IMMEDIATE L_SQL INTO O_VALEUR;
    --O_VALEUR := 'GOOD';
    --O_LIBVALEUR := 'OK EVAL RATIO';
    END;

    PROCEDURE P_EVAL_NBPTS (P_ANAID              NUMBER,
                            P_FORMULE            VARCHAR2,
                            P_LANCODE            VARCHAR2,
                            O_VALEUR      IN OUT VARCHAR2,
                            O_LIBVALEUR   IN OUT VARCHAR2)
    IS
        L_START      NUMBER;
        L_CC         VARCHAR2 (1);
        L_SQL        VARCHAR2 (500);
        L_RATIO      VARCHAR2 (50);
        L_VALRATIO   VARCHAR2 (20);
    BEGIN
        L_START := 0;
        L_SQL := '';

        FOR I IN 1 .. LENGTH (P_FORMULE)
        LOOP
            L_CC := SUBSTR (P_FORMULE, I, 1);

            IF L_START = 1
            THEN
                IF L_CC = ']'
                THEN
                    L_RATIO := L_RATIO || '';
                ELSE
                    L_RATIO := L_RATIO || L_CC;
                END IF;
            END IF;

            IF L_CC = '['
            THEN
                L_START := 1;
                L_RATIO := '';
            ELSE
                IF L_CC = ']' AND L_START = 1
                THEN
                    L_START := 0;
                    DBMS_OUTPUT.PUT_LINE ('L_RATIO:' || L_RATIO || '-');

                    BEGIN
                        SELECT NVL (L.RATNBPOINTS, 0)
                          INTO L_VALRATIO
                          FROM LKANARAT L, RATIO R
                         WHERE     L.ANAID = P_ANAID
                               AND L.RATID = R.RATID
                               AND R.RATCODE = TRIM (L_RATIO);

                        L_SQL := L_SQL || L_VALRATIO;
                        DBMS_OUTPUT.PUT_LINE (
                            'SQL:' || SUBSTR (L_VALRATIO, 1, 200));
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            L_SQL := L_SQL;
                            DBMS_OUTPUT.PUT_LINE (
                                SUBSTR (
                                       'NO_DATA : L_VALEUR : '
                                    || P_ANAID
                                    || '-'
                                    || L_SQL
                                    || '-'
                                    || SQLERRM,
                                    1,
                                    250));
                    END;
                ELSE
                    IF L_START = 0
                    THEN
                        IF L_CC NOT IN (';', ',')
                        THEN
                            L_SQL := L_SQL || L_CC;
                        END IF;
                    END IF;
                END IF;
            END IF;
        END LOOP;

        --DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);
        L_SQL := 'SELECT ' || L_SQL || ' FROM DUAL';
        DBMS_OUTPUT.PUT_LINE ('L_SQL : ' || L_SQL);

        EXECUTE IMMEDIATE L_SQL INTO O_VALEUR;
    --O_VALEUR := 'GOOD';
    --O_LIBVALEUR := 'OK EVAL RATIO';
    END;

    PROCEDURE P_SET_CUSTOMDEAL (I_ACTID       IN     NUMBER,
                                I_DOSID       IN     NUMBER,
                                I_EXERCICE    IN     NUMBER,
                                I_TEFCLASSE   IN     VARCHAR2,
                                L_RATCODE     IN     VARCHAR2,
                                I_VALEUR      IN     VARCHAR2,
                                O_RETOUR         OUT VARCHAR2)
    AS
        NCVAID     CCHVALUE.CVAID%TYPE := NULL;
        SCOMTYPE   CUSTOMCHARACTERISTIC.COMTYPE%TYPE := NULL;
    BEGIN
        O_RETOUR := '0';

        SELECT CVAID, COMTYPE
          INTO NCVAID, SCOMTYPE
          FROM CUSTOMCHARACTERISTIC CCH, CCHVALUE CVA
         WHERE     CCH.CCHSID = CVA.CCHSID
               AND CVA.DOSID = I_DOSID
               AND CVA.DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = I_DOSID)
               AND CCH.CCHVALUECODE = L_RATCODE;

        IF SCOMTYPE IN ('ALPHAN', 'COMBOTUSPARAM', 'MEMO')
        THEN
            UPDATE CCHVALUE
               SET CVASTRINGVALUE = I_VALEUR
             WHERE CVAID = NCVAID;
        ELSIF SCOMTYPE IN ('DECIMAL', 'ENTIER')
        THEN
            BEGIN
                UPDATE CCHVALUE
                   SET CVANUMERICVALUE =
                           TO_NUMBER (REPLACE (I_VALEUR, ',', '.'))
                 WHERE CVAID = NCVAID;
            EXCEPTION
                WHEN OTHERS
                THEN
                    -- 1722 : Nombre non valide => Si ca a plant??vec des ., on essaye avec des ,
                    IF SQLCODE = -1722
                    THEN
                        UPDATE CCHVALUE
                           SET CVANUMERICVALUE = TO_NUMBER (I_VALEUR)
                         WHERE CVAID = NCVAID;
                    END IF;
            END;
        ELSIF SCOMTYPE IN ('LOGIQUE')
        THEN
            UPDATE CCHVALUE
               SET CVABOOLEANVALUE = TO_NUMBER (I_VALEUR)
             WHERE CVAID = NCVAID;
        ELSE
            -- For 'DATE', we don't do anything for now (until we decide what format i_valeur will be in
            NULL;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_RETOUR := 0;
    END P_SET_CUSTOMDEAL;

    --P_FRAUDE
    PROCEDURE P_FRAUDE (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTIDServicing   ACTEUR.ACTIDSERVICING%TYPE;
    BEGIN
        IF P_ACTID IS NULL
        THEN
            O_VALEUR := '0';
        ELSE
            nACTIDServicing := F_GET_ACTIDSERVICING (P_ACTID);

            SELECT MAX (lantco.tvalibelle)
              INTO O_VALEUR
              FROM lantcovaleur lantco, acttcovaleur actco, tcotation tco
             WHERE     actco.actid IN (P_ACTID, nACTIDServicing)
                   AND actco.tcoid = tco.tcoid
                   AND actco.tcoid = lantco.tcoid
                   AND tco.tcocode IN ('FRAUDE1', 'FRAUDE2')
                   AND actco.atvdtfin IS NULL
                   AND actco.tvacode = lantco.TVACODE
                   AND lantco.lancode = 'FR'
                   AND actco.creid =
                       (SELECT MAX (actco.creid)
                          FROM lantcovaleur  lantco,
                               acttcovaleur  actco,
                               tcotation     tco
                         WHERE     actco.actid IN (P_ACTID, nACTIDServicing)
                               AND actco.tcoid = tco.tcoid
                               AND actco.tcoid = lantco.tcoid
                               AND tco.tcocode IN ('FRAUDE1', 'FRAUDE2')
                               AND actco.tvacode = lantco.TVACODE
                               AND actco.atvdtfin IS NULL);

            --O_VALEUR     := PA_PLUGIN_SCFSCORING.F_FRAUDE(P_ACTID) ;
            IF O_VALEUR IS NULL
            THEN
                O_VALEUR := 'Non';
            END IF;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '0';
    END P_FRAUDE;

    ----------
    PROCEDURE P_FRAUDE_EMP (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_FRAUDE (P_ACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_FRAUDE_EMP;

    --------------
    PROCEDURE P_WECOMECALL_EMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_WECOMECALL (P_ACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_WECOMECALL_EMP;

    PROCEDURE P_WO_EMP (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_WO (P_ACTID,
              P_DOSID,
              P_DPRVERSION,
              P_EXERCICE,
              P_TEFCLASSE,
              P_LANCODE,
              P_ANAID,
              O_VALEUR,
              O_LIBVALEUR);
    END P_WO_EMP;

    PROCEDURE P_QUANTITE (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        --SELECT DPMQUANTITY INTO O_VALEUR FROM DPRMATERIEL dpm  WHERE dpm.DOSID=P_DOSID AND dpm.DPRVERSION=P_DPRVERSION  and rownum = 1;
        SELECT 1 INTO O_VALEUR FROM DUAL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_QUANTITE;

    ---MTR:12/07/2018 MAJ DE LA FONCTION
    PROCEDURE P_GET_TLGARLIB (p_actid               NUMBER,
                              p_dosid               NUMBER,
                              p_dprversion          VARCHAR2,
                              p_exercice            NUMBER,
                              p_tefclasse           VARCHAR2,
                              p_lancode      IN     VARCHAR2,
                              p_anaid               NUMBER,
                              o_valeur       IN OUT VARCHAR2,
                              o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        --  l_sql:='select nvl(DGCLIBELLE,''Non renseigne'') from DPGCARACTERISTIQUE where dosid=' || p_dosid || ' and dprversion=''' || p_dprversion || ''' and ' || p_tefclasse || '';
        /*  l_sql :=
                'select nvl(DGCLIBELLE,''Non renseigne''), nvl(TGACODE,''31'') from DPGCARACTERISTIQUE where dosid='
             || p_dosid
             || ' and dprversion='''
             || p_dprversion
             || ''' and '
             || p_tefclasse
             || '';*/
        /*   l_sql :=
           'SELECT (SELECT nvl(MAX (TGALIBELLE),''Non renseigne''), nvl(TGACODE,''31'')
                        FROM LANTGARANTIE
                       WHERE     TGACODE = PFIGUARANTEE.TGACODE
                             AND LANCODE ='''|| P_LANCODE||''')

                FROM PFIGUARANTEE
               WHERE PFIID IN
                        (SELECT PFIID
                           FROM DPRPROPFINANCE
                          WHERE DOSID =' || p_dosid||')'
              || 'and '
              || p_tefclasse
              || '';*/

        l_sql :=
               'SELECT nvl (TGALIBELLE,''Non renseigne''), nvl(TGACODE,''31'')
                   FROM LANTGARANTIE
                  WHERE     LANCODE ='''
            || P_LANCODE
            || '''
				  AND TGACODE IN (SELECT TGACODE
           FROM PFIGUARANTEE
          WHERE PFIID IN
                   (SELECT PFIID
                      FROM DPRPROPFINANCE
                     WHERE   DPFFLAGRETENUE=1 and DOSID ='
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''')and '
            || p_tefclasse
            || ')';
        DBMS_OUTPUT.put_line (l_sql);

        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur, o_libvaleur;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLGARLIB;

    -----------MTR 12/07/2018 MAJ de la fonction
    PROCEDURE P_GET_TLGARMDESCRIPT (p_actid               NUMBER,
                                    p_dosid               NUMBER,
                                    p_dprversion          VARCHAR2,
                                    p_exercice            NUMBER,
                                    p_tefclasse           VARCHAR2,
                                    p_lancode      IN     VARCHAR2,
                                    p_anaid               NUMBER,
                                    o_valeur       IN OUT VARCHAR2,
                                    o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        /*  l_sql :=
                'select nvl(DGCDESCRIPTION,''Non renseigne'') from DPGCARACTERISTIQUE where dosid='
             || p_dosid
             || ' and dprversion='''
             || p_dprversion
             || ''' and '
             || p_tefclasse
             || '';*/

        l_sql :=
               'select nvl(PFGDESCRIPTION,''Non renseigne'')
           FROM PFIGUARANTEE
          WHERE PFIID IN
                   (SELECT PFIID
                      FROM DPRPROPFINANCE
                     WHERE  DPFFLAGRETENUE=1 and DOSID ='
            --|| p_dosid||')'
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || ')';

        DBMS_OUTPUT.put_line (l_sql);

        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLGARMDESCRIPT;

    ----------------MTR 12/07/2018 MAJ Fonction
    PROCEDURE P_GET_TLGARMTHT (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        /*   l_sql :=
                 'select ROUND(nvl(DGCMTGARANTIE,-5)) from DPGCARACTERISTIQUE where dosid='
              || p_dosid
              || ' and dprversion='''
              || p_dprversion
              || ''' and '
              || p_tefclasse
              || '';*/

        l_sql :=
               'select ROUND(nvl(PFGMTGUARANTEE,-5))
           FROM PFIGUARANTEE
          WHERE PFIID IN
                   (SELECT PFIID
                      FROM DPRPROPFINANCE
                   WHERE  DPFFLAGRETENUE=1 and DOSID  ='
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || ')';
        DBMS_OUTPUT.put_line (l_sql);

        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END P_GET_TLGARMTHT;
    END;

    ----MTR 12/07/2018 REPORT ET ADAPTATION FONCTION TLG 4.0
    PROCEDURE P_GET_TLGARPOURCT (p_actid               NUMBER,
                                 p_dosid               NUMBER,
                                 p_dprversion          VARCHAR2,
                                 p_exercice            NUMBER,
                                 p_tefclasse           VARCHAR2,
                                 p_lancode      IN     VARCHAR2,
                                 p_anaid               NUMBER,
                                 o_valeur       IN OUT VARCHAR2,
                                 o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        -- l_sql:='select ROUND(nvl(DGCPCTINVEST,-5),2) from DPGCARACTERISTIQUE where dosid=' || p_dosid || ' and dprversion=''' || p_dprversion || ''' and ' || p_tefclasse || '';
        l_sql :=
               'select ROUND(nvl(PFGPCTINVESTMENT,-5),2)
           FROM PFIGUARANTEE
          WHERE PFIID IN
                   (SELECT PFIID
                      FROM DPRPROPFINANCE
                     WHERE  DPFFLAGRETENUE=1 and DOSID ='
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || ')';

        DBMS_OUTPUT.put_line (l_sql);

        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END;



    PROCEDURE P_TYPEVEHICULE (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMLIBELLE
          INTO O_VALEUR
          FROM DPRMATERIEL DPM, LANASSETCATEGORY LAN
         WHERE     DPM.DOSID = P_DOSID
               AND DPM.DPRVERSION = P_DPRVERSION
               AND DPM.ACACODE = LAN.ACACODE
               AND LAN.LANCODE = P_LANCODE
               AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_TYPEVEHICULE;

    PROCEDURE P_ASSETCAEGORY (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMLIBELLE
          INTO O_VALEUR
          FROM DPRMATERIEL DPM, LANASSETCATEGORY LAN
         WHERE     DPM.DOSID = P_DOSID
               AND DPM.DPRVERSION = P_DPRVERSION
               AND DPM.ACACODE = LAN.ACACODE
               AND LAN.LANCODE = P_LANCODE 
              AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ASSETCAEGORY;

    PROCEDURE P_NAPLIBELLE (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT NAPLIBELLE
          INTO O_VALEUR
          FROM DPRMATERIEL DPM, LANNAP
         WHERE     DPM.DOSID = P_DOSID
               AND DPM.DPRVERSION = P_DPRVERSION
               AND DPM.NAPCODE = LANNAP.NAPCODE
               AND LANNAP.LANCODE = P_LANCODE
               AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NAPLIBELLE;

    PROCEDURE P_ASSET_VN_VO (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT CASE DPMFLAGNEUF WHEN 1 THEN 'VN' ELSE 'VO' END
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ASSET_VN_VO;

    PROCEDURE P_IMMATRICULATION (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMIMMATRICULATION
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_IMMATRICULATION;

    PROCEDURE P_DT_IMMATRICULATION (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';

          SELECT TO_CHAR (DPMDTPREMIERCIRCULATION, 'DD/MM/YYYY')
            INTO O_VALEUR
            FROM DPRMATERIEL DPRM
           WHERE     DPRM.DOSID = P_DOSID
                 AND DPRM.DPRVERSION = P_DPRVERSION
                 AND ROWNUM = 1
        ORDER BY DPRM.DOSID;

        IF O_VALEUR IS NULL
        THEN
            O_VALEUR := 'N/A';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 'N/A';
    END P_DT_IMMATRICULATION;

    --

    ---MTR 10/04/2018
    PROCEDURE P_GET_ACTDIRIGEANT (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS
        CURSOR c_dir
        IS
            SELECT SUBSTR (
                       a.acotitre || ' ' || a.acoprenom || ' ' || a.aconom,
                       0,
                       50),
                   t.tuplibelle
              FROM actcorrespondant a, lantusparam t
             WHERE     a.actid = p_actid
                   AND a.acoqualite IN (1,
                                        2,
                                        5,
                                        6,
                                        7,
                                        29,
                                        3)
                   AND a.acoqualite = t.tupcode
                   AND t.lancode = p_lancode
                   AND t.tusnom = 'QUALITE';

        l_valeur      VARCHAR2 (2000);
        l_libvaleur   VARCHAR2 (2000);
    BEGIN
        BEGIN
            o_libvaleur := '';

            OPEN c_dir;

            LOOP
                FETCH c_dir INTO l_valeur, l_libvaleur;

                EXIT WHEN c_dir%NOTFOUND;
                o_valeur := o_valeur || CHR (13) || l_valeur;
                o_libvaleur := o_libvaleur || CHR (13) || l_libvaleur;
            END LOOP;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_ACTDIRIGEANT;

    ------MTR 27/04/2018
    PROCEDURE P_TOTAL_MVT (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_CHAR (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVBANK')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Mouvements';
        --   O_VALEUR:=80;

        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_MVT;

    ------------MTR 19/09/2018
    PROCEDURE P_GET_SEC_ACT (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ACTIVITE
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_SEC_ACT;

    PROCEDURE P_TOTAL_MVT_MOY_AFF (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVBANK')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Mouvt';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_MVT_MOY_AFF;

    PROCEDURE P_TOTAL_SOLDE_MOY (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVBANK')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Solde';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_SOLDE_MOY;

    PROCEDURE P_TOTAL_NB_INCIDENTS (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVBANK')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Nnincident';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_NB_INCIDENTS;

    PROCEDURE P_TOTAL_NB_REG_EFFETS (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVBANK')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Nbegefet';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_NB_REG_EFFETS;

    PROCEDURE P_TOTAL_MVT_AA (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVAUT')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Mouvements';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_MVT_AA;

    PROCEDURE P_TOTAL_MVT_MOY_AA (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVAUT')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Mouvt';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_MVT_MOY_AA;

    PROCEDURE P_TOTAL_SOLDE_MOY_AA (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVAUT')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Solde';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_SOLDE_MOY_AA;

    PROCEDURE P_TOTAL_NB_INCIDENTS_AA (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVAUT')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Nnincident';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_NB_INCIDENTS_AA;

    PROCEDURE P_TOTAL_NB_REG_EFFETS_AA (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'RELEVAUT')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Nbegefet';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_NB_REG_EFFETS_AA;

    --END MTR 27-04-2018
    PROCEDURE P_AGEASSET (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';

          SELECT   TO_CHAR (SYSDATE, 'yyyy')
                 - TO_CHAR (DPMDTPREMIERCIRCULATION, 'yyyy')
            INTO O_VALEUR
            FROM DPRMATERIEL DPRM
           WHERE     DPRM.DOSID = P_DOSID
                 AND DPRM.DPRVERSION = P_DPRVERSION
                 AND ROWNUM = 1
        ORDER BY DPRM.DOSID;

        IF O_VALEUR IS NULL
        THEN
            O_VALEUR := 'N/A';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 'N/A';
    END P_AGEASSET;

    --
    -- Argus   4123
    PROCEDURE P_ARGUSMTTRANSACT (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CRE.CREID)
          INTO NCREID
          FROM CREVT CRE, CREDATA CDA
         WHERE     CRE.CREID = CDA.CREID
               AND DOSIDPROSPECT = P_DOSID
               AND CDATABLE = 'ARGUS'
               AND CDACOLONNE = 'MONTANT';

        SELECT REPLACE (TO_CHAR (MAX (CDADATANUMBER)), ',', '.')
          INTO O_VALEUR
          FROM CREVT CRE, CREDATA CDA
         WHERE     CRE.CREID = CDA.CREID
               AND DOSIDPROSPECT = P_DOSID
               AND CDATABLE = 'ARGUS'
               AND CDACOLONNE = 'MONTANT'
               AND CRE.CREID = NCREID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ARGUSMTTRANSACT;

    --
    PROCEDURE P_VIN (P_ACTID               NUMBER,
                     P_DOSID               NUMBER,
                     P_DPRVERSION          VARCHAR2,
                     P_EXERCICE            NUMBER,
                     P_TEFCLASSE           VARCHAR2,
                     P_LANCODE             VARCHAR2,
                     P_ANAID               NUMBER,
                     O_VALEUR       IN OUT VARCHAR2,
                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMNUMSERIE
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_VIN;

    --
    PROCEDURE P_MAKE (P_ACTID               NUMBER,
                      P_DOSID               NUMBER,
                      P_DPRVERSION          VARCHAR2,
                      P_EXERCICE            NUMBER,
                      P_TEFCLASSE           VARCHAR2,
                      P_LANCODE             VARCHAR2,
                      P_ANAID               NUMBER,
                      --        P_UTICODE      IN     VARCHAR2,
                      O_VALEUR       IN OUT VARCHAR2,
                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        SMAKECODE   MAKE.MAKCODE%TYPE := '';
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        SELECT MAKE.MAKCODE, LANMAKE.MAKLIBELLE
          INTO SMAKECODE, O_VALEUR
          FROM LANMAKE, MAKE, DPRMATERIEL
         WHERE     DPRMATERIEL.MAKID = MAKE.MAKID
               AND DPRMATERIEL.MAKID = LANMAKE.MAKID
               AND MAKE.MAKID = LANMAKE.MAKID
               AND DPRMATERIEL.DOSID = P_DOSID
               AND DPRMATERIEL.DPRVERSION = P_DPRVERSION
               AND LANMAKE.LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_MAKE;

    PROCEDURE P_MODEL (P_ACTID               NUMBER,
                       P_DOSID               NUMBER,
                       P_DPRVERSION          VARCHAR2,
                       P_EXERCICE            NUMBER,
                       P_TEFCLASSE           VARCHAR2,
                       P_LANCODE             VARCHAR2,
                       P_ANAID               NUMBER,
                       --        P_UTICODE      IN     VARCHAR2,
                       O_VALEUR       IN OUT VARCHAR2,
                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        SMMOCODE   MAKMODEL.MMOCODE%TYPE := '';
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        SELECT MAKMODEL.MMOCODE, LANMAKMODEL.MMOLIBELLE
          INTO SMMOCODE, O_VALEUR
          FROM LANMAKMODEL, MAKMODEL, DPRMATERIEL
         WHERE     DPRMATERIEL.MAKID = MAKMODEL.MAKID
               AND DPRMATERIEL.MAKID = LANMAKMODEL.MAKID
               AND MAKMODEL.MAKID = LANMAKMODEL.MAKID
               AND DPRMATERIEL.MMOCODE = LANMAKMODEL.MMOCODE
               AND MAKMODEL.MMOCODE = LANMAKMODEL.MMOCODE
               AND DPRMATERIEL.DOSID = P_DOSID
               AND DPRMATERIEL.DPRVERSION = P_DPRVERSION
               AND LANMAKMODEL.LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_MODEL;

    PROCEDURE P_CARROSSERIE (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TUPLIBELLE
          INTO O_VALEUR
          FROM DPRMATERIEL DPM, LANTUSPARAM TUP
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND DPM.DPMCARROSSERIE = TUP.TUPCODE
               AND TUP.LANCODE = P_LANCODE
               AND TUP.TUSNOM = 'CGCARROSSERIE'
               AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_CARROSSERIE;

    ---
    PROCEDURE P_ENERGIE (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TUPLIBELLE
          INTO O_VALEUR
          FROM DPRMATERIEL DPM, LANTUSPARAM TUP
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND DPM.DPMENERGIE = TUP.TUPCODE
               AND TUP.LANCODE = P_LANCODE
               AND TUP.TUSNOM = 'CGENERGIE'
               AND ROWNUM = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ENERGIE;

    PROCEDURE P_PUISSANCE (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMPUISSANCE
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_PUISSANCE;

    PROCEDURE P_FINITION (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCVAID   CCHVALUE.CVAID%TYPE;
    BEGIN
        O_VALEUR := '';

        SELECT MAX (CVAID)
          INTO NCVAID
          FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
         WHERE     CV2.CCHSID = CC2.CCHSID
               AND CC2.CCHVALUECODE = 'FINITION'
               AND CV2.CVAPKEYVALUE LIKE
                          'Dosid-'
                       || P_DOSID
                       || '||Dprversion-'
                       || P_DPRVERSION
                       || '||Dpmordre-%';

        SELECT DISTINCT CVASTRINGVALUE
          INTO O_VALEUR
          FROM CCHVALUE CV
         WHERE CVAID = NCVAID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_FINITION;

    PROCEDURE P_KILOMETRAGE (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPMKILOMETRAGE
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_KILOMETRAGE;

    --Acquisition value TTC
    PROCEDURE P_INVESTTTC (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        STACCODE   DOSSIERPROSPECT.TACCODE%TYPE;
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        --Taxe PRET = 1.24 en dur
        SELECT TACCODE
          INTO STACCODE
          FROM DOSSIERPROSPECT DPR
         WHERE DPR.DOSID = P_DOSID AND DPR.DPRVERSION = P_DPRVERSION;

        IF STACCODE = 'PRET'
        THEN
            SELECT SUM (DPMMTINVEST * (1 + 24 / 100))
              INTO O_VALEUR
              FROM DPRMATERIEL DPM
             WHERE DPM.DOSID = P_DOSID AND DPM.DPRVERSION = P_DPRVERSION;
        ELSE
            SELECT SUM (DPMMTINVEST * (1 + TTAVAL / 100))
              INTO O_VALEUR
              FROM DPRMATERIEL DPM, TAXTAUX TAX
             WHERE     DPM.DOSID = P_DOSID
                   AND DPM.DPRVERSION = P_DPRVERSION
                   AND TAX.TAXCODE = NVL (DPM.TAXCODE, 'EXO')
                   AND TTADTFIN IS NULL;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            --function is supposed to return a number and o_valeur can't be a string (ORA-1403 for example)
            --o_valeur := SUBSTR(SQLERRM,1,10);
            O_VALEUR := -0;
    END P_INVESTTTC;

    PROCEDURE P_FRAIS_DOS (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        SELECT F_GETPFIID (P_DOSID) INTO NPFIID FROM DUAL;

        SELECT ROUND (SUM (PFPMT))
          INTO O_VALEUR
          FROM PFIPRESTATION PFI
         WHERE PFIID = NPFIID AND PFI.TPRCODE = 'FRATEST';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 0;
    END P_FRAIS_DOS;

    PROCEDURE P_DWNPYMT (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NDWP_HT    NUMBER;
        NTAXE      NUMBER;
        STACCODE   DOSSIERPROSPECT.TACCODE%TYPE;
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        SELECT TACCODE
          INTO STACCODE
          FROM DOSSIERPROSPECT DPR
         WHERE DPR.DOSID = P_DOSID AND DPR.DPRVERSION = P_DPRVERSION;

        IF STACCODE = 'PRET'
        THEN
            SELECT ROUND (MAX (PFV.PFVMT))
              INTO O_VALEUR
              FROM DPRPROPFINANCE DPF, PFIVERSEMENT PFV
             WHERE     DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION = P_DPRVERSION
                   AND DPFFLAGRETENUE = 1
                   AND DPF.PFIID = PFV.PFIID
                   AND PFVTYPEVERSEMENT IN ('FIRSTPAYMENT', 'DOWNPAYMENTARR');
        ELSE
            SELECT ROUND (MAX (PFV.PFVMT) * 1.2)
              INTO O_VALEUR
              FROM DPRPROPFINANCE DPF, PFIVERSEMENT PFV
             WHERE     DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION = P_DPRVERSION
                   AND DPFFLAGRETENUE = 1
                   AND DPF.PFIID = PFV.PFIID
                   AND PFVTYPEVERSEMENT IN ('FIRSTPAYMENT', 'DOWNPAYMENTARR');
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            --function is supposed to return a number and o_valeur can't be a string (ORA-1403 for example)
            --o_valeur := SUBSTR(SQLERRM,1,10);
            O_VALEUR := -0;
    END P_DWNPYMT;

    PROCEDURE P_FINANCED_AMOUNT (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NRETURN    NUMBER;
        NORDRE     NUMBER;
        NTAXE      NUMBER;
        NPFIID     NUMBER;
        STACCODE   DOSSIERPROSPECT.TACCODE%TYPE;
    BEGIN
        O_LIBVALEUR := '';

        SELECT TACCODE
          INTO STACCODE
          FROM DOSSIERPROSPECT DPR
         WHERE DPR.DOSID = P_DOSID AND DPR.DPRVERSION = P_DPRVERSION;

        IF STACCODE = 'PRET'
        THEN
            SELECT ROUND (MAX (PFADOUBLE))
              INTO O_VALEUR
              FROM DPRPROPFINANCE DPF, PFIATTRIBUT PFA
             WHERE     DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION = P_DPRVERSION
                   AND DPFFLAGRETENUE = 1
                   AND DPF.PFIID = PFA.PFIID
                   AND PFACODE = 'FINANCEDVALUE';
        ELSE
            SELECT ROUND (MAX (PFADOUBLE) * 1.2)
              INTO O_VALEUR
              FROM DPRPROPFINANCE DPF, PFIATTRIBUT PFA
             WHERE     DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION = P_DPRVERSION
                   AND DPFFLAGRETENUE = 1
                   AND DPF.PFIID = PFA.PFIID
                   AND PFACODE = 'FINANCEDVALUE';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 0;
    END P_FINANCED_AMOUNT;

    PROCEDURE P_EXIST_CAUTION (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
        NCOUNT   NUMBER;
    BEGIN
        SELECT F_GETPFIID (P_DOSID) INTO NPFIID FROM DUAL;

        SELECT COUNT (*)
          INTO NCOUNT
          FROM PFIGUARANTEE
         WHERE     PFIID = NPFIID
               AND TGACODE IN ('GAR01',
                               'GAR02',
                               'GAR04',
                               'GAR05',
                               'GAR06');

        IF NCOUNT > 0
        THEN
            O_VALEUR := 'YES';
        ELSE
            O_VALEUR := 'NO';
        END IF;
    END P_EXIST_CAUTION;

    PROCEDURE P_EXIST_GAGE (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
        NCOUNT   NUMBER;
    BEGIN
        NPFIID := F_GETPFIID (P_DOSID);

        SELECT COUNT (*)
          INTO NCOUNT
          FROM PFIGUARANTEE
         WHERE PFIID = NPFIID AND TGACODE IN ('GAR03');

        IF NCOUNT > 0
        THEN
            O_VALEUR := 'YES';
        ELSE
            O_VALEUR := 'NO';
        END IF;
    END P_EXIST_GAGE;

    PROCEDURE P_NOT_EXIST_GAGE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
        NCOUNT   NUMBER;
    BEGIN
        NPFIID := F_GETPFIID (P_DOSID);

        SELECT COUNT (*)
          INTO NCOUNT
          FROM PFIGUARANTEE
         WHERE PFIID = NPFIID AND TGACODE IN ('GAR03');

        IF NCOUNT = 0
        THEN
            O_VALEUR := 'YES';
        ELSE
            O_VALEUR := 'NO';
        END IF;
    END P_NOT_EXIST_GAGE;

    PROCEDURE P_PRODUCT (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TPGLIBELLE
          INTO O_VALEUR
          FROM TPROFILGESTION TPG, LANTPROFILGESTION LAN, DOSSIERPROSPECT DPR
         WHERE     DPR.DOSID = P_DOSID
               AND DPR.DPRVERSION = P_DPRVERSION
               AND DPR.TPGCODE = TPG.TPGCODE
               AND TPG.TPGCODE = LAN.TPGCODE
               AND LANCODE = P_LANCODE;
    END P_PRODUCT;

    PROCEDURE P_TRI (P_ACTID               NUMBER,
                     P_DOSID               NUMBER,
                     P_DPRVERSION          VARCHAR2,
                     P_EXERCICE            NUMBER,
                     P_TEFCLASSE           VARCHAR2,
                     P_LANCODE             VARCHAR2,
                     P_ANAID               NUMBER,
                     O_VALEUR       IN OUT VARCHAR2,
                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        NPFIID := F_GETPFIID (P_DOSID);

        SELECT TO_CHAR (PFRTXNOMINAL + PFRMARGENOMINALE)
          INTO O_VALEUR
          FROM PFIRUBRIQUE
         WHERE PFIID = NPFIID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_TRI;

    PROCEDURE P_TAEG (P_ACTID               NUMBER,
                      P_DOSID               NUMBER,
                      P_DPRVERSION          VARCHAR2,
                      P_EXERCICE            NUMBER,
                      P_TEFCLASSE           VARCHAR2,
                      P_LANCODE             VARCHAR2,
                      P_ANAID               NUMBER,
                      O_VALEUR       IN OUT VARCHAR2,
                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT PFADOUBLE
              INTO O_VALEUR
              FROM PFIATTRIBUT
             WHERE PFIID = NPFIID AND PFACODE = 'TAEG';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_TAEG;

    PROCEDURE P_DUREE_MOIS (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        SELECT SUM (TO_NUMBER (PFIPERIODICITE) * PFINBPERIODES / 30)
          INTO O_VALEUR
          FROM PROPOSITIONFINANCIERE PFI, DPRPROPFINANCE DPF
         WHERE     DPF.PFIID = PFI.PFIID
               AND DPF.DOSID = P_DOSID
               AND DPF.DPRVERSION = P_DPRVERSION
               AND DPFFLAGRETENUE = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_DUREE_MOIS;

    PROCEDURE P_ASSURANCE_MT (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT ROUND (SUM (PFPMT))
              INTO O_VALEUR
              FROM PFIPRESTATION PFP
             WHERE PFIID = NPFIID AND PFP.TPRCODE LIKE '%INS%';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ASSURANCE_MT;

    PROCEDURE P_ASS_PPI (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT ROUND (SUM (PFPMT))
              INTO O_VALEUR
              FROM PFIPRESTATION PFP
             WHERE     PFIID = NPFIID
                   AND PFP.TPRCODE IN ('INSPPI2',
                                       'INSPPI1',
                                       'INSPPI6',
                                       'INSPPI5',
                                       'INSPPI4',
                                       'INSPPI3');
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ASS_PPI;

    PROCEDURE P_ASS_GAP (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT ROUND (SUM (PFPMT))
              INTO O_VALEUR
              FROM PFIPRESTATION PFP
             WHERE PFIID = NPFIID AND PFP.TPRCODE IN ('INSGAP1');
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ASS_GAP;

    PROCEDURE P_ASS_EW (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT ROUND (SUM (PFPMT))
              INTO O_VALEUR
              FROM PFIPRESTATION PFP
             WHERE     PFIID = NPFIID
                   AND PFP.TPRCODE IN ('INSEW2',
                                       'INSPPI7',
                                       'INSEW1',
                                       'INSPPI8',
                                       'INSPPI9',
                                       'INSEW4',
                                       'INSEW3');
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ASS_EW;

    PROCEDURE P_NB_ECHEANCES (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';
        O_LIBVALEUR := '';

        SELECT PFINBPERIODES
          INTO O_VALEUR
          FROM PROPOSITIONFINANCIERE PFI, DPRPROPFINANCE DPF
         WHERE     DPF.PFIID = PFI.PFIID
               AND DPF.DOSID = P_DOSID
               AND DPF.DPRVERSION = P_DPRVERSION
               AND DPFFLAGRETENUE = 1;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NB_ECHEANCES;

    PROCEDURE P_ECH_INCL_SERIVES (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NSUMSERVICE   NUMBER;
    BEGIN
        P_LOYER_TTC (P_ACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);

        --TRA 04012017: on arrondi la somme des prestation a la 2eme decimale
        SELECT ROUND (SUM (PFPMT), 2)
          INTO NSUMSERVICE
          FROM PFIPRESTATION PFI
         WHERE PFIID = F_GETPFIID (P_DOSID);

        --SELECT ROUND(SUM(PFPMT)) INTO NSUMSERVICE FROM PFIPRESTATION PFI WHERE PFIID = F_GETPFIID(P_DOSID) ;
        O_VALEUR := O_VALEUR + NSUMSERVICE;
    END P_ECH_INCL_SERIVES;

    PROCEDURE P_ARGUSPCTRV (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nPFIID   NUMBER;
    BEGIN
        BEGIN
            nPFIID := f_getpfiid (P_DOSID);

            SELECT pfadouble
              INTO O_VALEUR
              FROM pfiattribut
             WHERE pfiid = nPFIID AND pfacode = 'ARGUSPCTRV';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ARGUSPCTRV;

    PROCEDURE P_ARGUSMTRV (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nPFIID   NUMBER;
    BEGIN
        BEGIN
            nPFIID := f_getpfiid (P_DOSID);

            SELECT pfadouble
              INTO O_VALEUR
              FROM pfiattribut
             WHERE pfiid = nPFIID AND pfacode = 'ARGUSMTRV';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_ARGUSMTRV;

    PROCEDURE P_RVPCT (P_ACTID               NUMBER,
                       P_DOSID               NUMBER,
                       P_DPRVERSION          VARCHAR2,
                       P_EXERCICE            NUMBER,
                       P_TEFCLASSE           VARCHAR2,
                       P_LANCODE             VARCHAR2,
                       P_ANAID               NUMBER,
                       O_VALEUR       IN OUT VARCHAR2,
                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '%';

        SELECT PFRPCTVR
          INTO O_VALEUR
          FROM (SELECT PFR.PFRPCTVR
                  FROM PFIRUBRIQUE PFR
                 WHERE PFR.PFIID = F_GETPFIID (P_DOSID))
         WHERE ROWNUM = 1;
    END P_RVPCT;

    PROCEDURE P_RV (P_ACTID               NUMBER,
                    P_DOSID               NUMBER,
                    P_DPRVERSION          VARCHAR2,
                    P_EXERCICE            NUMBER,
                    P_TEFCLASSE           VARCHAR2,
                    P_LANCODE             VARCHAR2,
                    P_ANAID               NUMBER,
                    O_VALEUR       IN OUT VARCHAR2,
                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NRETURN   NUMBER;
        NORDRE    NUMBER;
        NTAXE     NUMBER;
    BEGIN
        O_LIBVALEUR := '';

        BEGIN
            SELECT MAX (F_TAXE (P_DOSID)) INTO NTAXE FROM DUAL;

            SELECT TO_CHAR (ROUND (NVL (PFRVR, 0) * (1 + NTAXE)))
              INTO O_VALEUR
              FROM PFIRUBRIQUE PF
             WHERE PF.PFIID = F_GETPFIID (P_DOSID);
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_RV;

    FUNCTION F_TAXE (NDOSID IN DOSSIERPROSPECT.DOSID%TYPE)
        RETURN NUMBER
    AS
        NRETURN   TAXTAUX.TTAVAL%TYPE;
    BEGIN
        BEGIN
            SELECT MAX (TTAVAL) / 100
              INTO NRETURN
              FROM TAXTAUX, TAXE, DOSSIERPROSPECT DPR
             WHERE     DPR.DOSID = NDOSID
                   AND DPR.DPRVERSION = (SELECT DPRVERSION
                                           FROM V_DEAL VDE
                                          WHERE VDE.DOSID = NDOSID)
                   AND DPR.TAXCODE = TAXE.TAXCODE
                   AND TAXE.TAXCODE = TAXTAUX.TAXCODE
                   AND NVL (TAXDTFIN, SYSDATE) >= SYSDATE
                   AND NVL (TTADTFIN, SYSDATE) >= SYSDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                NRETURN := 0;
        END;

        RETURN NRETURN;
    END F_TAXE;

    PROCEDURE P_LASTDECISION (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID      ACTEUR.ACTID%TYPE;
        NDDEORDRE   NUMBER;
    BEGIN
        SELECT MAX (DDEORDRE)
          INTO NDDEORDRE
          FROM DPRDECISION
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND ADECODE IN ('SCOREKO',
                               'SCOREOK',
                               'RISKL2',
                               'RISKL1');

        SELECT ADELIBELLE
          INTO O_VALEUR
          FROM DPRDECISION DDE, LANAVTDECISION LAN
         WHERE     DDE.ADECODE = LAN.ADECODE
               AND LAN.LANCODE = P_LANCODE
               AND DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND DDEORDRE = NDDEORDRE;
    END P_LASTDECISION;

    PROCEDURE P_LASTDECI_COMMENT (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID      ACTEUR.ACTID%TYPE;
        NDDEORDRE   NUMBER;
    BEGIN
        SELECT MAX (DDEORDRE)
          INTO NDDEORDRE
          FROM DPRDECISION
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND ADECODE IN ('SCOREKO',
                               'SCOREOK',
                               'RISKL2',
                               'RISKL1');

        SELECT DPRDECISION.DDECOMMENT
          INTO O_VALEUR
          FROM DPRDECISION
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND DDEORDRE = NDDEORDRE;
    END P_LASTDECI_COMMENT;

    PROCEDURE P_SCORE_FINAL (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CRE.CREID)
          INTO NCREID
          FROM CREVT CRE, CREDATA CDA
         WHERE     CRE.DOSIDPROSPECT = P_DOSID
               AND CRE.TMFFONCTION = 'EVF_VALIDER'
               AND CRE.CREID = CDA.CREID
               AND CDA.CDACOLONNE = 'Fillernum1';

        SELECT CDADATANUMBER
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID = NCREID
               AND CDATABLE = 'OUTPUT'
               AND CDACOLONNE = 'Fillernum1';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_SCORE_FINAL;

    PROCEDURE P_NIVEAU_VIGILANCE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        P_GET_AMLRATING_EMP (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
        NULL;
    END P_NIVEAU_VIGILANCE;

    PROCEDURE P_RISQ_JUNIOR_DATE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID      ACTEUR.ACTID%TYPE;
        nDDEORDRE   NUMBER;
    BEGIN
        SELECT MAX (DDEORDRE)
          INTO nDDEORDRE
          FROM dprdecision
         WHERE     dosid = P_DOSID
               AND dprversion = P_DPRVERSION
               AND adecode IN ('RUWOK');

        IF nDDEORDRE IS NOT NULL
        THEN
            SELECT TO_CHAR (DDEDTCREATION, 'DD/MM/YYYY')
              INTO O_VALEUR
              FROM dprdecision dde
             WHERE     dosid = P_DOSID
                   AND dprversion = P_DPRVERSION
                   AND ddeordre = nDDEORDRE;
        ELSE
            O_VALEUR := NULL;
        END IF;
    END P_RISQ_JUNIOR_DATE;

    PROCEDURE P_RISQ_JUNIOR_HEURE (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID      ACTEUR.ACTID%TYPE;
        nDDEORDRE   NUMBER;
    BEGIN
        SELECT MAX (DDEORDRE)
          INTO nDDEORDRE
          FROM dprdecision
         WHERE     dosid = P_DOSID
               AND dprversion = P_DPRVERSION
               AND adecode IN ('RUWOK');

        IF nDDEORDRE IS NOT NULL
        THEN
            --select to_char(to_date(DDEDTCREATION,'dd/mm/yyyy hh24:mi:ss'),'hh24:mi:ss') into O_VALEUR from dprdecision dde where dosid = P_DOSID and dprversion = P_DPRVERSION and ddeordre = nDDEORDRE;
            SELECT TO_CHAR (DDEDTCREATION, 'HH24:MI:SS')
              INTO O_VALEUR
              FROM dprdecision dde
             WHERE     dosid = P_DOSID
                   AND dprversion = P_DPRVERSION
                   AND ddeordre = nDDEORDRE;
        ELSE
            O_VALEUR := NULL;
        END IF;
    END P_RISQ_JUNIOR_HEURE;

    PROCEDURE P_RISQ_SENIOR_DATE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID      ACTEUR.ACTID%TYPE;
        nDDEORDRE   NUMBER;
    BEGIN
        -- TRA 04012017 : on selectionne la 1ere decision faite par un senior dont les adecode sont dans l'analyse manuelle : decision
        SELECT MIN (DDEORDRE)
          INTO nDDEORDRE
          FROM dprdecision
         WHERE     dosid = P_DOSID
               AND dprversion = P_DPRVERSION
               AND adecode IN ('RISKOK',
                               'RISKKO',
                               'COMPDOC',
                               'RDUPLIC')
               AND uticodedecision IN (SELECT uticode
                                         FROM utilisateur
                                        WHERE grocode = 'GRPMANA');

        --    select max (DDEORDRE) into nDDEORDRE from dprdecision where dosid = P_DOSID and dprversion = P_DPRVERSION and adecode in ('RISKL2');
        IF nDDEORDRE IS NOT NULL
        THEN
            SELECT TO_CHAR (DDEDTCREATION, 'DD/MM/YYYY')
              INTO O_VALEUR
              FROM dprdecision dde
             WHERE     dosid = P_DOSID
                   AND dprversion = P_DPRVERSION
                   AND ddeordre = nDDEORDRE;
        --select to_char(to_date(DDEDTCREATION,'dd/mm/yyyy hh24:mi:ss'),'dd/mm/YYYY') into O_VALEUR from dprdecision dde where dosid = P_DOSID and dprversion = P_DPRVERSION and ddeordre = nDDEORDRE;
        ELSE
            O_VALEUR := NULL;
        END IF;
    END P_RISQ_SENIOR_DATE;

    PROCEDURE P_RISQ_SENIOR_HEURE (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID      ACTEUR.ACTID%TYPE;
        nDDEORDRE   NUMBER;
    BEGIN
        -- TRA 04012017 : on selectionne la 1ere decision faite par un senior dont les adecode sont dans l'analyse manuelle : decision
        SELECT MIN (DDEORDRE)
          INTO nDDEORDRE
          FROM dprdecision
         WHERE     dosid = P_DOSID
               AND dprversion = P_DPRVERSION
               AND adecode IN ('RISKOK',
                               'RISKKO',
                               'COMPDOC',
                               'RDUPLIC')
               AND uticodedecision IN (SELECT uticode
                                         FROM utilisateur
                                        WHERE grocode = 'GRPMANA');

        --select max (DDEORDRE) into nDDEORDRE from dprdecision where dosid = P_DOSID and dprversion = P_DPRVERSION and adecode in ('RISKL2');
        IF nDDEORDRE IS NOT NULL
        THEN
            SELECT TO_CHAR (DDEDTCREATION, 'HH24:MI:SS')
              INTO O_VALEUR
              FROM dprdecision dde
             WHERE     dosid = P_DOSID
                   AND dprversion = P_DPRVERSION
                   AND ddeordre = nDDEORDRE;
        -- select to_char(to_date(DDEDTCREATION,'dd/mm/yyyy hh24:mi:ss'),'hh24:mi:ss') into O_VALEUR from dprdecision dde where dosid = P_DOSID and dprversion = P_DPRVERSION and ddeordre = nDDEORDRE;
        ELSE
            O_VALEUR := NULL;
        END IF;
    END P_RISQ_SENIOR_HEURE;

    PROCEDURE P_GET_SECTACTIVITE_EMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_SECTACTIVITE (P_ACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SECTACTIVITE_EMP;

    PROCEDURE P_GET_NUMACTEUR_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NUMACTEUR (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_NUMACTEUR_COEMP;

    PROCEDURE P_GET_TYPEACTEUR_COEMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_TYPEACTEUR (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_TYPEACTEUR_COEMP;

    PROCEDURE P_GET_CIVILITE_COEMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_CIVILITE (NACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_CIVILITE_COEMP;

    PROCEDURE P_GET_NOM_COEMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NOM (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_NOM_COEMP;

    PROCEDURE P_GET_NOMNAISSANCE_COEMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NOMNAISSANCE (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_NOMNAISSANCE_COEMP;

    PROCEDURE P_GET_PRENOM_COEMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PRENOM (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_PRENOM_COEMP;

    PROCEDURE P_GET_DATENAISSANCE_COEMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_DATENAISSANCE (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_DATENAISSANCE_COEMP;

    PROCEDURE P_GET_AGE_COEMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_AGE (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_AGE_COEMP;

    PROCEDURE P_GET_PAYSNAISSANCE_COEMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PAYSNAISSANCE (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PAYSNAISSANCE_COEMP;

    PROCEDURE P_GET_CPNAISSANCE_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_CPNAISSANCE (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_CPNAISSANCE_COEMP;

    PROCEDURE P_GET_VILLENAISSANCE_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_VILLENAISSANCE (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLENAISSANCE_COEMP;

    PROCEDURE P_GET_NATIONALITE_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NATIONALITE (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_NATIONALITE_COEMP;

    PROCEDURE P_GET_LIENEMPRUNTEUR_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_LIENEMPRUNTEUR (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_LIENEMPRUNTEUR_COEMP;

    PROCEDURE P_GET_NBENFANT_COEMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NBENFANT (NACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_NBENFANT_COEMP;

    PROCEDURE P_GET_SITUFAMILLIALE_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_SITUFAMILLIALE (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_SITUFAMILLIALE_COEMP;

    PROCEDURE P_GET_RESIDENCE_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_RESIDENCE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_RESIDENCE_COEMP;

    PROCEDURE P_GET_ADRESSE_COEMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_ADRESSE (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_ADRESSE_COEMP;

    PROCEDURE P_GET_CP_COEMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_CP (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_GET_CP_COEMP;

    PROCEDURE P_GET_VILLE_COEMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_VILLE (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_VILLE_COEMP;

    PROCEDURE P_GET_PAYS_COEMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PAYS (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_PAYS_COEMP;

    PROCEDURE P_GET_TELEPHONE_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_TELEPHONE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_TELEPHONE_COEMP;

    PROCEDURE P_GET_MAIL_COEMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_MAIL (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_MAIL_COEMP;

    PROCEDURE P_GET_DATEENTREADRESSE_COEMP (P_ACTID               NUMBER,
                                            P_DOSID               NUMBER,
                                            P_DPRVERSION          VARCHAR2,
                                            P_EXERCICE            NUMBER,
                                            P_TEFCLASSE           VARCHAR2,
                                            P_LANCODE             VARCHAR2,
                                            P_ANAID               NUMBER,
                                            O_VALEUR       IN OUT VARCHAR2,
                                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_DATEENTREADRESSE (NACTID,
                                P_DOSID,
                                P_DPRVERSION,
                                P_EXERCICE,
                                P_TEFCLASSE,
                                P_LANCODE,
                                P_ANAID,
                                O_VALEUR,
                                O_LIBVALEUR);
    END P_GET_DATEENTREADRESSE_COEMP;

    PROCEDURE P_GET_ANCADRESSE_COEMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_ANCADRESSE (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCADRESSE_COEMP;

    PROCEDURE P_GET_BANQUE_COEMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_BANQUE (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_BANQUE_COEMP;

    PROCEDURE P_GET_IBAN_COEMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_IBAN (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_IBAN_COEMP;

    PROCEDURE P_GET_DATERIB_COEMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_DATERIB (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_DATERIB_COEMP;

    PROCEDURE P_GET_ANCBANQUE_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_ANCBANQUE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_ANCBANQUE_COEMP;

    PROCEDURE P_GET_PROFESCSP_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PROFESCSP (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_PROFESCSP_COEMP;

    PROCEDURE P_GET_PROFESSION_COEMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PROFESSION (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_PROFESSION_COEMP;

    PROCEDURE P_GET_CONTRAT_COEMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_CONTRAT (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_CONTRAT_COEMP;

    PROCEDURE P_GET_DATEEMBAUCHE_COEMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_DATEEMBAUCHE (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_DATEEMBAUCHE_COEMP;

    PROCEDURE P_GET_ANCMBAUCHE_COEMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_ANCMBAUCHE (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCMBAUCHE_COEMP;

    PROCEDURE P_GET_EMPLOYEUR_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_EMPLOYEUR (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_EMPLOYEUR_COEMP;

    PROCEDURE P_GET_VILLEEMPLOYEUR_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_VILLEEMPLOYEUR (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLEEMPLOYEUR_COEMP;

    PROCEDURE P_GET_TELEMPLOYEUR_COEMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_TELEMPLOYEUR (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_TELEMPLOYEUR_COEMP;

    PROCEDURE P_GET_CLEBDF_COEMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_CLEBDF (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_CLEBDF_COEMP;

    PROCEDURE P_GET_FICP_COEMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_FICP (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_FICP_COEMP;

    PROCEDURE P_GET_FCC_COEMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_FCC (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_FCC_COEMP;

    PROCEDURE P_GET_AMLRATING_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_AMLRATING (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_AMLRATING_COEMP;

    PROCEDURE P_GET_SANCTIONLIST_COEMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_SANCTIONLIST (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SANCTIONLIST_COEMP;

    PROCEDURE P_GET_PPE_COEMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PPE (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_PPE_COEMP;

    PROCEDURE P_GET_SALAIRE_COEMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_SALAIRE (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_SALAIRE_COEMP;

    PROCEDURE P_GET_REVENUSFONCIERS_COEMP (P_ACTID               NUMBER,
                                           P_DOSID               NUMBER,
                                           P_DPRVERSION          VARCHAR2,
                                           P_EXERCICE            NUMBER,
                                           P_TEFCLASSE           VARCHAR2,
                                           P_LANCODE             VARCHAR2,
                                           P_ANAID               NUMBER,
                                           O_VALEUR       IN OUT VARCHAR2,
                                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_REVENUSFONCIERS (NACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_REVENUSFONCIERS_COEMP;

    PROCEDURE P_GET_PENSIONRECUES_COEMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PENSIONRECUES (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONRECUES_COEMP;

    PROCEDURE P_GET_AUTRESREVENUS_COEMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_AUTRESREVENUS (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_AUTRESREVENUS_COEMP;

    PROCEDURE P_GET_LOYER_COEMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_LOYER (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_LOYER_COEMP;

    PROCEDURE P_GET_IMPOT_COEMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_IMPOT (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_IMPOT_COEMP;

    PROCEDURE P_GET_PENSIONPAYEES_COEMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_PENSIONPAYEES (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONPAYEES_COEMP;

    PROCEDURE P_GET_REMBEMPIMMO_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_REMBEMPIMMO (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPIMMO_COEMP;

    PROCEDURE P_GET_REMBEMPHIMM_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_REMBEMPHIMM (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPHIMM_COEMP;

    PROCEDURE P_GET_NBCREDITSACTIFS_COEMP (P_ACTID               NUMBER,
                                           P_DOSID               NUMBER,
                                           P_DPRVERSION          VARCHAR2,
                                           P_EXERCICE            NUMBER,
                                           P_TEFCLASSE           VARCHAR2,
                                           P_LANCODE             VARCHAR2,
                                           P_ANAID               NUMBER,
                                           O_VALEUR       IN OUT VARCHAR2,
                                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_NBCREDITSACTIFS (NACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_NBCREDITSACTIFS_COEMP;

    PROCEDURE P_GETENCOURSAVANT_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETENCOURSAVANT (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAVANT_COEMP;

    PROCEDURE P_GETENCOURSAPRES_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETENCOURSAPRES (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAPRES_COEMP;

    PROCEDURE P_GETMTACCORDENCOURS_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETMTACCORDENCOURS (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETMTACCORDENCOURS_COEMP;

    PROCEDURE P_GETNBACCORDENCOURS_COEMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETNBACCORDENCOURS (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETNBACCORDENCOURS_COEMP;

    PROCEDURE P_GETMTREFUS18MTH_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETMTREFUS18MTH (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETMTREFUS18MTH_COEMP;

    PROCEDURE P_GETNBREFUS18MTH_COEMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GETNBREFUS18MTH (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETNBREFUS18MTH_COEMP;

    PROCEDURE P_IMPAYE_COEMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_IMPAYE (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_IMPAYE_COEMP;

    PROCEDURE P_EXISTIMPAYE30_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_EXISTIMPAYE30 (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE30_COEMP;

    PROCEDURE P_FRAUDE_COEMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_FRAUDE (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_FRAUDE_COEMP;

    PROCEDURE P_WECOMECALL_COEMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_WECOMECALL (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_WECOMECALL_COEMP;

    PROCEDURE P_WO_COEMP (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_WO (NACTID,
              P_DOSID,
              P_DPRVERSION,
              P_EXERCICE,
              P_TEFCLASSE,
              P_LANCODE,
              P_ANAID,
              O_VALEUR,
              O_LIBVALEUR);
    END P_WO_COEMP;

    PROCEDURE P_GET_SECTACTIVITE_COEMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        nACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_GET_SECTACTIVITE (nACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SECTACTIVITE_COEMP;

    PROCEDURE P_GET_NUMACTEUR_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NUMACTEUR (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_NUMACTEUR_GAR;

    PROCEDURE P_GET_TYPEACTEUR_GAR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_TYPEACTEUR (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_TYPEACTEUR_GAR;

    PROCEDURE P_GET_CIVILITE_GAR (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_CIVILITE (NACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_CIVILITE_GAR;

    PROCEDURE P_GET_NOM_GAR (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NOM (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_NOM_GAR;

    PROCEDURE P_GET_NOMNAISSANCE_GAR (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NOMNAISSANCE (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_NOMNAISSANCE_GAR;

    PROCEDURE P_GET_PRENOM_GAR (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PRENOM (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_PRENOM_GAR;

    PROCEDURE P_GET_DATENAISSANCE_GAR (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_DATENAISSANCE (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_DATENAISSANCE_GAR;

    PROCEDURE P_GET_AGE_GAR (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_AGE (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_AGE_GAR;

    PROCEDURE P_GET_PAYSNAISSANCE_GAR (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PAYSNAISSANCE (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PAYSNAISSANCE_GAR;

    PROCEDURE P_GET_CPNAISSANCE_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_CPNAISSANCE (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_CPNAISSANCE_GAR;

    PROCEDURE P_GET_VILLENAISSANCE_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_VILLENAISSANCE (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLENAISSANCE_GAR;

    PROCEDURE P_GET_NATIONALITE_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NATIONALITE (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_NATIONALITE_GAR;

    PROCEDURE P_GET_LIENEMPRUNTEUR_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_LIENEMPRUNTEUR (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_LIENEMPRUNTEUR_GAR;

    PROCEDURE P_GET_NBENFANT_GAR (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NBENFANT (NACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_NBENFANT_GAR;

    PROCEDURE P_GET_SITUFAMILLIALE_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_SITUFAMILLIALE (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_SITUFAMILLIALE_GAR;

    PROCEDURE P_GET_RESIDENCE_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_RESIDENCE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_RESIDENCE_GAR;

    PROCEDURE P_GET_ADRESSE_GAR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_ADRESSE (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_ADRESSE_GAR;

    PROCEDURE P_GET_CP_GAR (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_CP (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_GET_CP_GAR;

    PROCEDURE P_GET_VILLE_GAR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_VILLE (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_VILLE_GAR;

    PROCEDURE P_GET_PAYS_GAR (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PAYS (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_PAYS_GAR;

    PROCEDURE P_GET_TELEPHONE_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_TELEPHONE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_TELEPHONE_GAR;

    PROCEDURE P_GET_MAIL_GAR (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_MAIL (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_MAIL_GAR;

    PROCEDURE P_GET_DATEENTREADRESSE_GAR (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_DATEENTREADRESSE (NACTID,
                                P_DOSID,
                                P_DPRVERSION,
                                P_EXERCICE,
                                P_TEFCLASSE,
                                P_LANCODE,
                                P_ANAID,
                                O_VALEUR,
                                O_LIBVALEUR);
    END P_GET_DATEENTREADRESSE_GAR;

    PROCEDURE P_GET_ANCADRESSE_GAR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_ANCADRESSE (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCADRESSE_GAR;

    PROCEDURE P_GET_BANQUE_GAR (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_BANQUE (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_BANQUE_GAR;

    PROCEDURE P_GET_IBAN_GAR (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_IBAN (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_IBAN_GAR;

    PROCEDURE P_GET_DATERIB_GAR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_DATERIB (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_DATERIB_GAR;

    PROCEDURE P_GET_ANCBANQUE_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_ANCBANQUE (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_ANCBANQUE_GAR;

    PROCEDURE P_GET_PROFESCSP_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PROFESCSP (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_PROFESCSP_GAR;

    PROCEDURE P_GET_PROFESSION_GAR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PROFESSION (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_PROFESSION_GAR;

    PROCEDURE P_GET_CONTRAT_GAR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_CONTRAT (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_CONTRAT_GAR;

    PROCEDURE P_GET_DATEEMBAUCHE_GAR (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_DATEEMBAUCHE (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_DATEEMBAUCHE_GAR;

    PROCEDURE P_GET_ANCMBAUCHE_GAR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_ANCMBAUCHE (NACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCMBAUCHE_GAR;

    PROCEDURE P_GET_EMPLOYEUR_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_EMPLOYEUR (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_EMPLOYEUR_GAR;

    PROCEDURE P_GET_VILLEEMPLOYEUR_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_VILLEEMPLOYEUR (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLEEMPLOYEUR_GAR;

    PROCEDURE P_GET_CLEBDF_GAR (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_CLEBDF (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_CLEBDF_GAR;

    PROCEDURE P_GET_FICP_GAR (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_FICP (NACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_FICP_GAR;

    PROCEDURE P_GET_FCC_GAR (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_FCC (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_FCC_GAR;

    PROCEDURE P_GET_AMLRATING_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_AMLRATING (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_AMLRATING_GAR;

    PROCEDURE P_GET_SANCTIONLIST_GAR (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_SANCTIONLIST (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SANCTIONLIST_GAR;

    PROCEDURE P_GET_PPE_GAR (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PPE (NACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_PPE_GAR;

    PROCEDURE P_GET_SALAIRE_GAR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_SALAIRE (NACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_SALAIRE_GAR;

    PROCEDURE P_GET_REVENUSFONCIERS_GAR (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_REVENUSFONCIERS (NACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_REVENUSFONCIERS_GAR;

    PROCEDURE P_GET_PENSIONRECUES_GAR (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PENSIONRECUES (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONRECUES_GAR;

    PROCEDURE P_GET_AUTRESREVENUS_GAR (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_AUTRESREVENUS (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_AUTRESREVENUS_GAR;

    PROCEDURE P_GET_LOYER_GAR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_LOYER (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_LOYER_GAR;

    PROCEDURE P_GET_IMPOT_GAR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_IMPOT (NACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_IMPOT_GAR;

    PROCEDURE P_GET_PENSIONPAYEES_GAR (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_PENSIONPAYEES (NACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONPAYEES_GAR;

    PROCEDURE P_GET_REMBEMPIMMO_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_REMBEMPIMMO (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPIMMO_GAR;

    PROCEDURE P_GET_ANTETABLFIN (p_actid               NUMBER,
                                 p_dosid               NUMBER,
                                 p_dprversion          VARCHAR2,
                                 p_exercice            NUMBER,
                                 p_tefclasse           VARCHAR2,
                                 p_lancode             VARCHAR2,
                                 p_anaid               NUMBER,
                                 o_valeur       IN OUT VARCHAR2,
                                 o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT astlibre
              INTO o_valeur
              FROM actstatistique
             WHERE actid = p_actid AND saccode = 'ANTETABLFIN';
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
            WHEN OTHERS
            THEN
                o_valeur := '';
                DBMS_OUTPUT.put_line (
                    SUBSTR ('GET ANTETABLFIN ' || SQLERRM, 1, 250));
        END;
    END P_GET_ANTETABLFIN;

    ------------MTR 17/09/2018
    PROCEDURE P_GET_ANALYSTE (p_actid               NUMBER,
                              p_dosid               NUMBER,
                              p_dprversion          VARCHAR2,
                              p_exercice            NUMBER,
                              p_tefclasse           VARCHAR2,
                              p_lancode      IN     VARCHAR2,
                              P_anaid               NUMBER,
                              o_valeur       IN OUT VARCHAR2,
                              o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT U.UTICODE, U.UTIPRENOM || ' ' || U.UTINOM
              INTO o_valeur, o_libvaleur
              FROM UTILISATEUR U, analysis D
             WHERE     D.dosid = p_dosid
                   AND d.dprversion = p_dprversion
                   AND d.UTICODE = U.UTICODE
                   AND D.Anaid = P_anaid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := SQLERRM;
        END;
    END;

    ------------MTR 25/09/2018
    PROCEDURE P_GET_ANAID (p_actid               NUMBER,
                           p_dosid               NUMBER,
                           p_dprversion          VARCHAR2,
                           p_exercice            NUMBER,
                           p_tefclasse           VARCHAR2,
                           p_lancode      IN     VARCHAR2,
                           P_anaid               NUMBER,
                           o_valeur       IN OUT VARCHAR2,
                           o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ANAID
              INTO o_libvaleur
              FROM analysis
             WHERE     dosid = p_dosid
                   AND dprversion = p_dprversion
                   AND Anaid = P_anaid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := SQLERRM;
        END;
    END;

    -------mtr 12/07/2018
    PROCEDURE p_GET_CLASSE_RISQUE (p_actid               NUMBER,
                                   p_dosid               NUMBER,
                                   p_dprversion          VARCHAR2,
                                   p_exercice            NUMBER,
                                   p_tefclasse           VARCHAR2,
                                   p_lancode      IN     VARCHAR2,
                                   p_anaid               NUMBER,
                                   o_valeur       IN OUT VARCHAR2,
                                   o_libvaleur    IN OUT VARCHAR2)
    IS
        l_atvordre   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT MAX (atvordre)
              INTO l_atvordre
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 10 AND atvdtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;

        IF l_atvordre > 0
        THEN
            -- il existe une cotation alors la comparer :
            SELECT tvacode
              INTO o_valeur
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 10 AND atvordre = l_atvordre;
        ELSE
            o_valeur := '';
            o_libvaleur := 'Non renseigne';
        END IF;
    END;

    -----MTR 12/07/2018
    PROCEDURE p_GET_CLASSE_RISQ_COMM (p_actid               NUMBER,
                                      p_dosid               NUMBER,
                                      p_dprversion          VARCHAR2,
                                      p_exercice            NUMBER,
                                      p_tefclasse           VARCHAR2,
                                      p_lancode      IN     VARCHAR2,
                                      p_anaid               NUMBER,
                                      o_valeur       IN OUT VARCHAR2,
                                      o_libvaleur    IN OUT VARCHAR2)
    IS
        l_atvordre   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT MAX (atvordre)
              INTO l_atvordre
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 9 AND atvdtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;

        IF l_atvordre > 0
        THEN
            -- il existe une cotation alors la comparer :
            SELECT tvacode
              INTO o_valeur
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 9 AND atvordre = l_atvordre;
        ELSE
            o_valeur := '';
            o_libvaleur := 'Non renseigne';
        END IF;
    END;

    -----MTR 12/07/2018
    PROCEDURE p_GET_CLASSE_RISQ_BCT (p_actid               NUMBER,
                                     p_dosid               NUMBER,
                                     p_dprversion          VARCHAR2,
                                     p_exercice            NUMBER,
                                     p_tefclasse           VARCHAR2,
                                     p_lancode      IN     VARCHAR2,
                                     p_anaid               NUMBER,
                                     o_valeur       IN OUT VARCHAR2,
                                     o_libvaleur    IN OUT VARCHAR2)
    IS
        l_atvordre   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT MAX (atvordre)
              INTO l_atvordre
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 8 AND atvdtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;

        IF l_atvordre > 0
        THEN
            -- il existe une cotation alors la comparer :
            SELECT tvacode
              INTO o_valeur
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 8 AND atvordre = l_atvordre;
        ELSE
            o_valeur := '';
            o_libvaleur := 'Non renseigne';
        END IF;
    END;

    ----------MTR 12/07/2018
    PROCEDURE p_get_clientTF (p_actid               NUMBER,
                              p_dosid               NUMBER,
                              p_dprversion          VARCHAR2,
                              p_exercice            NUMBER,
                              p_tefclasse           VARCHAR2,
                              p_lancode             VARCHAR2,
                              p_anaid               NUMBER,
                              o_valeur       IN OUT VARCHAR2,
                              o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT astlibre
              INTO o_valeur
              FROM actstatistique
             WHERE actid = p_actid AND saccode = 'CLIENTTF';
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
            WHEN OTHERS
            THEN
                o_valeur := '';
                DBMS_OUTPUT.put_line (
                    SUBSTR ('GET CLIENTTF ' || SQLERRM, 1, 250));
        END;
    END p_get_clientTF;

    PROCEDURE P_GET_TLCLINBAFFAIRES (p_actid               NUMBER,
                                     p_dosid               NUMBER,
                                     p_dprversion          VARCHAR2,
                                     p_exercice            NUMBER,
                                     p_tefclasse           VARCHAR2,
                                     p_lancode      IN     VARCHAR2,
                                     p_anaid               NUMBER,
                                     o_valeur       IN OUT VARCHAR2,
                                     o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT NVL (c.nb_client_groupe, 0)
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_TLCLINBAFFAIRES;

    PROCEDURE P_GET_REMBEMPHIMM_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_REMBEMPHIMM (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPHIMM_GAR;

    PROCEDURE P_GET_NBCREDITSACTIFS_GAR (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NBCREDITSACTIFS (NACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_NBCREDITSACTIFS_GAR;

    PROCEDURE P_GETENCOURSAVANT_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETENCOURSAVANT (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAVANT_GAR;

    PROCEDURE P_GETENCOURSAPRES_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETENCOURSAPRES (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAPRES_GAR;

    PROCEDURE P_GETMTACCORDENCOURS_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETMTACCORDENCOURS (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETMTACCORDENCOURS_GAR;

    PROCEDURE P_GETNBACCORDENCOURS_GAR (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETNBACCORDENCOURS (NACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETNBACCORDENCOURS_GAR;

    PROCEDURE P_GETMTREFUS18MTH_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETMTREFUS18MTH (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETMTREFUS18MTH_GAR;

    PROCEDURE P_GETNBREFUS18MTH_GAR (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GETNBREFUS18MTH (NACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETNBREFUS18MTH_GAR;

    PROCEDURE P_IMPAYE_GAR (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_IMPAYE (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_IMPAYE_GAR;

    PROCEDURE P_EXISTIMPAYE30_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_EXISTIMPAYE30 (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE30_GAR;

    PROCEDURE P_EXISTIMPAYE90_GAR (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_EXISTIMPAYE90 (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE90_GAR;

    PROCEDURE P_FRAUDE_GAR (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_FRAUDE (NACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_FRAUDE_GAR;

    PROCEDURE P_WECOMECALL_GAR (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_WECOMECALL (NACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_WECOMECALL_GAR;

    PROCEDURE P_WO_GAR (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_WO (NACTID,
              P_DOSID,
              P_DPRVERSION,
              P_EXERCICE,
              P_TEFCLASSE,
              P_LANCODE,
              P_ANAID,
              O_VALEUR,
              O_LIBVALEUR);
    END P_WO_GAR;

    PROCEDURE P_GET_SECTACTIVITE_GAR1 (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        nACTID := F_getGarant1 (P_ACTID, P_DOSID, P_DPRVERSION);
        P_GET_SECTACTIVITE (nACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SECTACTIVITE_GAR1;

    PROCEDURE P_GET_NUMPROPOSITION (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DPRNUMERO
          INTO O_VALEUR
          FROM DOSSIERPROSPECT
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;
    END P_GET_NUMPROPOSITION;

    PROCEDURE P_GET_NUMANCPROPOSITION (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NDOSIDORIG           DOSSIERPROSPECT.DOSID%TYPE;
        NDPRVERSIONORIGINE   DOSSIERPROSPECT.DPRVERSIONORIGINE%TYPE;
    BEGIN
        O_VALEUR := '';

        SELECT DOSIDORIGINE, DPRVERSIONORIGINE
          INTO NDOSIDORIG, NDPRVERSIONORIGINE
          FROM DOSSIERPROSPECT
         WHERE     DOSIDORIGINE IS NOT NULL
               AND DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION;

        IF NDOSIDORIG IS NOT NULL
        THEN
            SELECT DPRNUMERO
              INTO O_VALEUR
              FROM DOSSIERPROSPECT
             WHERE DOSID = NDOSIDORIG AND DPRVERSION = NDPRVERSIONORIGINE;
        END IF;
    END P_GET_NUMANCPROPOSITION;

    PROCEDURE P_GET_DATESOUMISSION (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';

        IF P_DPRVERSION != 'NEGO'
        THEN
            SELECT TO_CHAR (DPHDTEFFECT, 'DD/MM/YYYY')
              INTO O_VALEUR
              FROM DPRPHASE
             WHERE DOSID = P_DOSID AND REPLACE (JALCODE, ' ', '') = 'UNDSUB';
        END IF;
    END P_GET_DATESOUMISSION;

    PROCEDURE P_GET_HEURESOUMISSION (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';

        IF P_DPRVERSION != 'NEGO'
        THEN
            SELECT TO_CHAR (DPHDTEFFECT, 'HH24:MI:SS')
              INTO O_VALEUR
              FROM DPRPHASE
             WHERE     DOSID = P_DOSID
                   AND REPLACE (JALCODE, ' ', '') = 'UNDSUB'
                   AND DPHDTEND IS NOT NULL;
        END IF;
    END P_GET_HEURESOUMISSION;

    PROCEDURE P_GET_APPORTEUR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT A.ACTNOM
          INTO O_VALEUR
          FROM DPRACTEUR D, ACTEUR A
         WHERE     D.ACTID = A.ACTID
               AND D.DOSID = P_DOSID
               AND D.ROLCODE = 'APPORT'
               AND D.DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_APPORTEUR;

    PROCEDURE P_GET_RAISONSOC (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
   AS
   BEGIN
      SELECT A.ACTNOM
        INTO O_VALEUR
        FROM DPRACTEUR D, ACTEUR A
       WHERE     D.ACTID = A.ACTID
             AND D.DOSID = P_DOSID
             AND D.ROLCODE = 'CLIENT'
             AND D.DPRVERSION =(SELECT DPRVERSION FROM V_DEAL WHERE  dosid= P_DOSID);
   EXCEPTION
      WHEN OTHERS
      THEN
         O_VALEUR := '';
   END P_GET_RAISONSOC;




    PROCEDURE P_GET_IDAPPORTEUR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT A.ACTCODE
          INTO O_VALEUR
          FROM DPRACTEUR D, ACTEUR A
         WHERE     D.ACTID = A.ACTID
               AND D.DOSID = P_DOSID
               AND D.ROLCODE = 'APPORT'
               AND D.DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_IDAPPORTEUR;

    PROCEDURE P_GET_CPAPPORTEUR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ADRCODEPOST
          INTO O_VALEUR
          FROM DPRACTEUR   D,
               ACTEUR      A,
               ACTADRESSE  AA,
               ADRESSE     AD
         WHERE     D.ACTID = A.ACTID
               AND AA.ADRID = AD.ADRID
               AND AADDTREMPLACE IS NULL
               AND AA.ACTID = A.ACTID
               AND D.AADORDRECOURRIER = AA.AADORDRE
               AND D.DOSID = P_DOSID
               AND D.ROLCODE = 'APPORT'
               AND D.DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CPAPPORTEUR;

    PROCEDURE P_GET_VENDEUR (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT UTINOM || ' ' || UTIPRENOM
          INTO O_VALEUR
          FROM DOSSIERPROSPECT D, UTILISATEUR U
         WHERE     D.UTICODECREATION = U.UTICODE
               AND D.DOSID = P_DOSID
               AND D.DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_VENDEUR;

    PROCEDURE P_GET_IDVENDEUR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT UTICODECREATION
          INTO O_VALEUR
          FROM DOSSIERPROSPECT
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_IDVENDEUR;

    PROCEDURE P_GET_NUMACTEUR_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NUMACTEUR (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_NUMACTEUR_EMP;

    PROCEDURE P_GET_TYPEACTEUR_EMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_TYPEACTEUR (P_ACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_TYPEACTEUR_EMP;

    PROCEDURE P_GET_CIVILITE_EMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_CIVILITE (P_ACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_CIVILITE_EMP;

    PROCEDURE P_GET_NOM_EMP (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NOM (P_ACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_NOM_EMP;

    PROCEDURE P_GET_NOMNAISSANCE_EMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NOMNAISSANCE (P_ACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_NOMNAISSANCE_EMP;

    PROCEDURE P_GET_PRENOM_EMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PRENOM (P_ACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_PRENOM_EMP;

    PROCEDURE P_GET_DATENAISSANCE_EMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_DATENAISSANCE (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_DATENAISSANCE_EMP;

    PROCEDURE P_GET_AGE_EMP (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_AGE (P_ACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_AGE_EMP;

    PROCEDURE P_GET_PAYSNAISSANCE_EMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PAYSNAISSANCE (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PAYSNAISSANCE_EMP;

    PROCEDURE P_GET_CPNAISSANCE_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_CPNAISSANCE (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_CPNAISSANCE_EMP;

    PROCEDURE P_GET_VILLENAISSANCE_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_VILLENAISSANCE (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLENAISSANCE_EMP;

    PROCEDURE P_GET_NATIONALITE_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NATIONALITE (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_NATIONALITE_EMP;

    PROCEDURE P_GET_NBENFANT_EMP (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NBENFANT (P_ACTID,
                        P_DOSID,
                        P_DPRVERSION,
                        P_EXERCICE,
                        P_TEFCLASSE,
                        P_LANCODE,
                        P_ANAID,
                        O_VALEUR,
                        O_LIBVALEUR);
    END P_GET_NBENFANT_EMP;

    PROCEDURE P_GET_SITUFAMILLIALE_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_SITUFAMILLIALE (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_SITUFAMILLIALE_EMP;

    PROCEDURE P_GET_RESIDENCE_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_RESIDENCE (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_RESIDENCE_EMP;

    PROCEDURE P_GET_ADRESSE_EMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_ADRESSE (P_ACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_ADRESSE_EMP;

    PROCEDURE P_GET_CP_EMP (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_CP (P_ACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_GET_CP_EMP;

    PROCEDURE P_GET_VILLE_EMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_VILLE (P_ACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_VILLE_EMP;

    PROCEDURE P_GET_PAYS_EMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PAYS (P_ACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_PAYS_EMP;

    PROCEDURE P_GET_TELEPHONE_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_TELEPHONE (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_TELEPHONE_EMP;

    PROCEDURE P_GET_MAIL_EMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_MAIL (P_ACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_MAIL_EMP;

    PROCEDURE P_GET_DATEENTREADRESSE_EMP (P_ACTID               NUMBER,
                                          P_DOSID               NUMBER,
                                          P_DPRVERSION          VARCHAR2,
                                          P_EXERCICE            NUMBER,
                                          P_TEFCLASSE           VARCHAR2,
                                          P_LANCODE             VARCHAR2,
                                          P_ANAID               NUMBER,
                                          O_VALEUR       IN OUT VARCHAR2,
                                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_DATEENTREADRESSE (P_ACTID,
                                P_DOSID,
                                P_DPRVERSION,
                                P_EXERCICE,
                                P_TEFCLASSE,
                                P_LANCODE,
                                P_ANAID,
                                O_VALEUR,
                                O_LIBVALEUR);
    END P_GET_DATEENTREADRESSE_EMP;

    PROCEDURE P_GET_ANCADRESSE_EMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_ANCADRESSE (P_ACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCADRESSE_EMP;

    PROCEDURE P_GET_BANQUE_EMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_BANQUE (P_ACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_BANQUE_EMP;





    PROCEDURE P_GET_IBAN_EMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_IBAN (P_ACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_IBAN_EMP;

    PROCEDURE P_GET_DATERIB_EMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_DATERIB (P_ACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_DATERIB_EMP;

    PROCEDURE P_GET_ANCBANQUE_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_ANCBANQUE (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_ANCBANQUE_EMP;

    PROCEDURE P_GET_PROFESCSP_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PROFESCSP (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_PROFESCSP_EMP;

    PROCEDURE P_GET_PROFESSION_EMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PROFESSION (P_ACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_PROFESSION_EMP;

    PROCEDURE P_GET_CONTRAT_EMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_CONTRAT (P_ACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_CONTRAT_EMP;

    PROCEDURE P_GET_DATEEMBAUCHE_EMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_DATEEMBAUCHE (P_ACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_DATEEMBAUCHE_EMP;

    PROCEDURE P_GET_ANCMBAUCHE_EMP (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_ANCMBAUCHE (P_ACTID,
                          P_DOSID,
                          P_DPRVERSION,
                          P_EXERCICE,
                          P_TEFCLASSE,
                          P_LANCODE,
                          P_ANAID,
                          O_VALEUR,
                          O_LIBVALEUR);
    END P_GET_ANCMBAUCHE_EMP;

    PROCEDURE P_GET_EMPLOYEUR_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_EMPLOYEUR (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_EMPLOYEUR_EMP;

    PROCEDURE P_GET_VILLEEMPLOYEUR_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_VILLEEMPLOYEUR (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_VILLEEMPLOYEUR_EMP;

    PROCEDURE P_GET_TELEMPLOYEUR_EMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_TELEMPLOYEUR (P_ACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_TELEMPLOYEUR_EMP;

    PROCEDURE P_GET_CLEBDF_EMP (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_CLEBDF (P_ACTID,
                      P_DOSID,
                      P_DPRVERSION,
                      P_EXERCICE,
                      P_TEFCLASSE,
                      P_LANCODE,
                      P_ANAID,
                      O_VALEUR,
                      O_LIBVALEUR);
    END P_GET_CLEBDF_EMP;

    PROCEDURE P_GET_FICP_EMP (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_FICP (P_ACTID,
                    P_DOSID,
                    P_DPRVERSION,
                    P_EXERCICE,
                    P_TEFCLASSE,
                    P_LANCODE,
                    P_ANAID,
                    O_VALEUR,
                    O_LIBVALEUR);
    END P_GET_FICP_EMP;

    PROCEDURE P_GET_FCC_EMP (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_FCC (P_ACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_FCC_EMP;

    PROCEDURE P_GET_AMLRATING_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_AMLRATING (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_GET_AMLRATING_EMP;

    PROCEDURE P_GET_SANCTIONLIST_EMP (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_SANCTIONLIST (P_ACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SANCTIONLIST_EMP;

    PROCEDURE P_GET_PPE_EMP (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PPE (P_ACTID,
                   P_DOSID,
                   P_DPRVERSION,
                   P_EXERCICE,
                   P_TEFCLASSE,
                   P_LANCODE,
                   P_ANAID,
                   O_VALEUR,
                   O_LIBVALEUR);
    END P_GET_PPE_EMP;

    PROCEDURE P_GET_SALAIRE_EMP (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_SALAIRE (P_ACTID,
                       P_DOSID,
                       P_DPRVERSION,
                       P_EXERCICE,
                       P_TEFCLASSE,
                       P_LANCODE,
                       P_ANAID,
                       O_VALEUR,
                       O_LIBVALEUR);
    END P_GET_SALAIRE_EMP;

    PROCEDURE P_GET_REVENUSFONCIERS_EMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_REVENUSFONCIERS (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_REVENUSFONCIERS_EMP;

    PROCEDURE P_GET_PENSIONRECUES_EMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PENSIONRECUES (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONRECUES_EMP;

    PROCEDURE P_GET_AUTRESREVENUS_EMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_AUTRESREVENUS (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_AUTRESREVENUS_EMP;

    PROCEDURE P_GET_LOYER_EMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_LOYER (P_ACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_LOYER_EMP;

    PROCEDURE P_GET_IMPOT_EMP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_IMPOT (P_ACTID,
                     P_DOSID,
                     P_DPRVERSION,
                     P_EXERCICE,
                     P_TEFCLASSE,
                     P_LANCODE,
                     P_ANAID,
                     O_VALEUR,
                     O_LIBVALEUR);
    END P_GET_IMPOT_EMP;

    PROCEDURE P_GET_PENSIONPAYEES_EMP (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_PENSIONPAYEES (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
    END P_GET_PENSIONPAYEES_EMP;

    PROCEDURE P_GET_REMBEMPIMMO_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_REMBEMPIMMO (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPIMMO_EMP;

    PROCEDURE P_GET_REMBEMPHIMM_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_REMBEMPHIMM (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GET_REMBEMPHIMM_EMP;

    PROCEDURE P_GET_NBCREDITSACTIFS_EMP (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_NBCREDITSACTIFS (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
    END P_GET_NBCREDITSACTIFS_EMP;

    PROCEDURE P_GET_SALAIRE_FOYER (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        --nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_SALAIRE_EMP (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
            P_GET_SALAIRE_COEMP (P_ACTID,
                                 P_DOSID,
                                 P_DPRVERSION,
                                 P_EXERCICE,
                                 P_TEFCLASSE,
                                 P_LANCODE,
                                 P_ANAID,
                                 O_VALEUR2,
                                 O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_SALAIRE_FOYER;

    PROCEDURE P_GET_REVENUSFONCIERS_FOYER (P_ACTID               NUMBER,
                                           P_DOSID               NUMBER,
                                           P_DPRVERSION          VARCHAR2,
                                           P_EXERCICE            NUMBER,
                                           P_TEFCLASSE           VARCHAR2,
                                           P_LANCODE             VARCHAR2,
                                           P_ANAID               NUMBER,
                                           O_VALEUR       IN OUT VARCHAR2,
                                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_REVENUSFONCIERS_EMP (P_ACTID,
                                       P_DOSID,
                                       P_DPRVERSION,
                                       P_EXERCICE,
                                       P_TEFCLASSE,
                                       P_LANCODE,
                                       P_ANAID,
                                       O_VALEUR,
                                       O_LIBVALEUR);
            P_GET_REVENUSFONCIERS_COEMP (P_ACTID,
                                         P_DOSID,
                                         P_DPRVERSION,
                                         P_EXERCICE,
                                         P_TEFCLASSE,
                                         P_LANCODE,
                                         P_ANAID,
                                         O_VALEUR2,
                                         O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_REVENUSFONCIERS_FOYER;

    PROCEDURE P_GET_PENSIONRECUES_FOYER (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        --nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_PENSIONRECUES_EMP (P_ACTID,
                                     P_DOSID,
                                     P_DPRVERSION,
                                     P_EXERCICE,
                                     P_TEFCLASSE,
                                     P_LANCODE,
                                     P_ANAID,
                                     O_VALEUR,
                                     O_LIBVALEUR);
            P_GET_PENSIONRECUES_COEMP (P_ACTID,
                                       P_DOSID,
                                       P_DPRVERSION,
                                       P_EXERCICE,
                                       P_TEFCLASSE,
                                       P_LANCODE,
                                       P_ANAID,
                                       O_VALEUR2,
                                       O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_PENSIONRECUES_FOYER;

    PROCEDURE P_GET_AUTRESREVENUS_FOYER (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_AUTRESREVENUS_EMP (P_ACTID,
                                     P_DOSID,
                                     P_DPRVERSION,
                                     P_EXERCICE,
                                     P_TEFCLASSE,
                                     P_LANCODE,
                                     P_ANAID,
                                     O_VALEUR,
                                     O_LIBVALEUR);
            P_GET_AUTRESREVENUS_COEMP (P_ACTID,
                                       P_DOSID,
                                       P_DPRVERSION,
                                       P_EXERCICE,
                                       P_TEFCLASSE,
                                       P_LANCODE,
                                       P_ANAID,
                                       O_VALEUR2,
                                       O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_AUTRESREVENUS_FOYER;

    PROCEDURE P_GET_LOYER_FOYER (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_LOYER_EMP (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
            P_GET_LOYER_COEMP (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR2,
                               O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_LOYER_FOYER;

    PROCEDURE P_GET_IMPOT_FOYER (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_IMPOT_EMP (P_ACTID,
                             P_DOSID,
                             P_DPRVERSION,
                             P_EXERCICE,
                             P_TEFCLASSE,
                             P_LANCODE,
                             P_ANAID,
                             O_VALEUR,
                             O_LIBVALEUR);
            P_GET_IMPOT_COEMP (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR2,
                               O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_IMPOT_FOYER;

    PROCEDURE P_GET_PENSIONPAYEES_FOYER (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_PENSIONPAYEES_EMP (P_ACTID,
                                     P_DOSID,
                                     P_DPRVERSION,
                                     P_EXERCICE,
                                     P_TEFCLASSE,
                                     P_LANCODE,
                                     P_ANAID,
                                     O_VALEUR,
                                     O_LIBVALEUR);
            P_GET_PENSIONPAYEES_COEMP (P_ACTID,
                                       P_DOSID,
                                       P_DPRVERSION,
                                       P_EXERCICE,
                                       P_TEFCLASSE,
                                       P_LANCODE,
                                       P_ANAID,
                                       O_VALEUR2,
                                       O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_PENSIONPAYEES_FOYER;

    PROCEDURE P_GET_REMBEMPIMMO_FOYER (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        -- nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_REMBEMPIMMO_EMP (P_ACTID,
                                   P_DOSID,
                                   P_DPRVERSION,
                                   P_EXERCICE,
                                   P_TEFCLASSE,
                                   P_LANCODE,
                                   P_ANAID,
                                   O_VALEUR,
                                   O_LIBVALEUR);
            P_GET_REMBEMPIMMO_COEMP (P_ACTID,
                                     P_DOSID,
                                     P_DPRVERSION,
                                     P_EXERCICE,
                                     P_TEFCLASSE,
                                     P_LANCODE,
                                     P_ANAID,
                                     O_VALEUR2,
                                     O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_REMBEMPIMMO_FOYER;

    PROCEDURE P_GET_REMBEMPHIMM_FOYER (P_ACTID               NUMBER,
                                       P_DOSID               NUMBER,
                                       P_DPRVERSION          VARCHAR2,
                                       P_EXERCICE            NUMBER,
                                       P_TEFCLASSE           VARCHAR2,
                                       P_LANCODE             VARCHAR2,
                                       P_ANAID               NUMBER,
                                       O_VALEUR       IN OUT VARCHAR2,
                                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        --nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_REMBEMPHIMM_EMP (P_ACTID,
                                   P_DOSID,
                                   P_DPRVERSION,
                                   P_EXERCICE,
                                   P_TEFCLASSE,
                                   P_LANCODE,
                                   P_ANAID,
                                   O_VALEUR,
                                   O_LIBVALEUR);
            P_GET_REMBEMPHIMM_COEMP (P_ACTID,
                                     P_DOSID,
                                     P_DPRVERSION,
                                     P_EXERCICE,
                                     P_TEFCLASSE,
                                     P_LANCODE,
                                     P_ANAID,
                                     O_VALEUR2,
                                     O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_REMBEMPHIMM_FOYER;

    PROCEDURE P_GET_NBCREDITSACTIFS_FOYER (P_ACTID               NUMBER,
                                           P_DOSID               NUMBER,
                                           P_DPRVERSION          VARCHAR2,
                                           P_EXERCICE            NUMBER,
                                           P_TEFCLASSE           VARCHAR2,
                                           P_LANCODE             VARCHAR2,
                                           P_ANAID               NUMBER,
                                           O_VALEUR       IN OUT VARCHAR2,
                                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nLien       NUMBER := 1;
        O_VALEUR2   VARCHAR2 (100);
    BEGIN
        --  nLien   := F_LIEN_MARITAL(P_DOSID , P_DPRVERSION ) ;
        IF nLien = 1
        THEN
            P_GET_NBCREDITSACTIFS (P_ACTID,
                                   P_DOSID,
                                   P_DPRVERSION,
                                   P_EXERCICE,
                                   P_TEFCLASSE,
                                   P_LANCODE,
                                   P_ANAID,
                                   O_VALEUR,
                                   O_LIBVALEUR);
            P_GET_NBCREDITSACTIFS_COEMP (P_ACTID,
                                         P_DOSID,
                                         P_DPRVERSION,
                                         P_EXERCICE,
                                         P_TEFCLASSE,
                                         P_LANCODE,
                                         P_ANAID,
                                         O_VALEUR2,
                                         O_LIBVALEUR);
            O_VALEUR :=
                TO_CHAR (
                      TO_NUMBER (NVL (O_VALEUR, 0))
                    + TO_NUMBER (NVL (O_VALEUR2, 0)));
        END IF;
    END P_GET_NBCREDITSACTIFS_FOYER;

    PROCEDURE P_GETENCOURSAVANT_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETENCOURSAVANT (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAVANT_EMP;

    PROCEDURE P_GETENCOURSAPRES_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETENCOURSAPRES (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETENCOURSAPRES_EMP;

    PROCEDURE P_GETMTACCORDENCOURS_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETMTACCORDENCOURS (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETMTACCORDENCOURS_EMP;

    PROCEDURE P_GETNBACCORDENCOURS_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETNBACCORDENCOURS (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GETNBACCORDENCOURS_EMP;

    PROCEDURE P_GETMTREFUS18MTH_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETMTREFUS18MTH (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETMTREFUS18MTH_EMP;

    PROCEDURE P_GETNBREFUS18MTH_EMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GETNBREFUS18MTH (P_ACTID,
                           P_DOSID,
                           P_DPRVERSION,
                           P_EXERCICE,
                           P_TEFCLASSE,
                           P_LANCODE,
                           P_ANAID,
                           O_VALEUR,
                           O_LIBVALEUR);
    END P_GETNBREFUS18MTH_EMP;

    PROCEDURE P_IMPAYE_EMP (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_IMPAYE (P_ACTID,
                  P_DOSID,
                  P_DPRVERSION,
                  P_EXERCICE,
                  P_TEFCLASSE,
                  P_LANCODE,
                  P_ANAID,
                  O_VALEUR,
                  O_LIBVALEUR);
    END P_IMPAYE_EMP;

    PROCEDURE P_EXISTIMPAYE30_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_EXISTIMPAYE30 (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE30_EMP;

    FUNCTION F_GET_ACTIDSERVICING (P_ACTID ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTIDSERVICING%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDSERVICING
              INTO NACTID
              FROM ACTEUR
             WHERE ACTID = P_ACTID;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := -1;
        END;

        RETURN NACTID;
    END F_GET_ACTIDSERVICING;

    PROCEDURE P_WECOMECALL (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DISTINCT CVASTRINGVALUE
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'WELCOMECALL'
               AND CV.ACTID = P_ACTID
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2
                     WHERE CV2.CCHSID = CV.CCHSID AND CV2.ACTID = CV.ACTID);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 'Non';
    END P_WECOMECALL;

    PROCEDURE P_WO (P_ACTID               NUMBER,
                    P_DOSID               NUMBER,
                    P_DPRVERSION          VARCHAR2,
                    P_EXERCICE            NUMBER,
                    P_TEFCLASSE           VARCHAR2,
                    P_LANCODE             VARCHAR2,
                    P_ANAID               NUMBER,
                    O_VALEUR       IN OUT VARCHAR2,
                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        IF P_ACTID IS NOT NULL
        THEN
            --  SELECT DECODE(PA_PLUGIN_SCFSCORING.F_WRITTENOFF(P_ACTID), 0, 'Non', 'YES')
            SELECT DECODE (0, 0, 'Non', 'YES') INTO O_VALEUR FROM DUAL;
        ELSE
            O_VALEUR := '';
        END IF;
    END P_WO;

    FUNCTION F_GETPFIID (P_DOSID DOSSIERPROSPECT.DOSID%TYPE)
        RETURN NUMBER
    AS
    BEGIN
        RETURN PA_AVCOMMUN.F_GETPFIRETENUE (
                   P_DOSID,
                   PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID));
    END F_GETPFIID;

    FUNCTION F_GET_ACTIDCOCLIENT (P_DOSID NUMBER, P_DPRVERSION VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM DPRACTEUR
             WHERE     DOSID = P_DOSID
                   AND DPRVERSION = P_DPRVERSION
                   AND ROLCODE IN ('COCLIEN', 'COEMPRU');
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GET_ACTIDCOCLIENT;

    PROCEDURE P_GET_NUMACTEUR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ACTCODE
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_NUMACTEUR;

    PROCEDURE P_GET_TYPEACTEUR (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TTPLIBELLE
          INTO O_VALEUR
          FROM ACTEUR, LANTTRPARAM L
         WHERE     ACTTYPE = TTPCODE
               AND TTRNOM = 'TYPECLIENT'
               AND ACTID = P_ACTID
               AND L.LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_TYPEACTEUR;

    -- Civilit??A?
    PROCEDURE P_GET_CIVILITE (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TTPLIBELLE
          INTO O_VALEUR
          FROM ACTEURPARTICULIER, LANTTRPARAM L
         WHERE     APATITRE = TTPCODE
               AND TTRNOM = 'TITRE'
               AND ACTID = P_ACTID
               AND L.LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CIVILITE;

    -- Nom
    -- Nom -- 03012017 TRA : on prend en compte le nom marital dans acteurparticulier
    PROCEDURE P_GET_NOM (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT APANOMMARITAL
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_NOM;

    -- Nom JF
    PROCEDURE P_GET_NOMNAISSANCE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER, LANTTRPARAM L
         WHERE     APATITRE = TTPCODE
               AND TTRNOM = 'TITRE'
               AND ACTID = P_ACTID
               AND L.LANCODE = P_LANCODE
               AND TTPCODE IN ('M', 'MAITRE');

        IF NCOUNT = 0
        THEN
            SELECT APANOMPATRONYMIQUE
              INTO O_VALEUR
              FROM ACTEURPARTICULIER
             WHERE ACTID = P_ACTID;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_NOMNAISSANCE;

    -- Pr??A?nom
    PROCEDURE P_GET_PRENOM (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT APAPRENOM
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PRENOM;

    -- Date Naissance
    PROCEDURE P_GET_DATENAISSANCE (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (APADTNAISS, 'DD/MM/YYYY')
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_DATENAISSANCE;

    -- Age
    PROCEDURE P_GET_AGE (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (APADTNAISS, 'YYYY')
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_AGE;

    -- Pays de naissance
    PROCEDURE P_GET_PAYSNAISSANCE (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT PAYLIBELLE
          INTO O_VALEUR
          FROM ACTEURPARTICULIER A, LANPAYS L
         WHERE     A.PAYCODE = L.PAYCODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PAYSNAISSANCE;

    PROCEDURE P_GET_CPNAISSANCE (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT APADEPTNAISS || APACOMNAISS
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CPNAISSANCE;

    -- Ville de naissance
    PROCEDURE P_GET_VILLENAISSANCE (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT APAVILLENAISS
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_VILLENAISSANCE;

    -- Nationalit??A?
    PROCEDURE P_GET_NATIONALITE (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT TUPLIBELLE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC, LANTUSPARAM L
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CV.CVASTRINGVALUE = L.TUPCODE
                   AND TUSNOM = 'NATIONALITE'
                   AND CC.CCHVALUECODE = 'NATIONALITE'
                   AND CV.ACTID = P_ACTID
                   AND L.LANCODE = P_LANCODE
                   AND CV.CVAID =
                       (SELECT MAX (CVAID)
                          FROM CCHVALUE CV2
                         WHERE CV2.CCHSID = CC.CCHSID AND CV2.ACTID = P_ACTID);
        END IF;
    END P_GET_NATIONALITE;

    -- Lien avec emprunteur
    PROCEDURE P_GET_LIENEMPRUNTEUR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCLIENT   NUMBER;
    BEGIN
        NCLIENT := F_GET_ACTIDCLIENT (P_DOSID, P_DPRVERSION);

        SELECT MAX (LIEN)
          INTO O_VALEUR
          FROM (SELECT TRELIBELLEASC LIEN
                  FROM LANTRELATION
                 WHERE     TRECODE =
                           (SELECT TRECODE
                              FROM ACTRELATION
                             WHERE     (    ACTID = P_ACTID
                                        AND ACTIDRELATION = NCLIENT)
                                   AND TRECODE <> 'BRANDDEALERS'
                                   AND ACTIDRELATION IS NOT NULL)
                       AND LANCODE = P_LANCODE
                UNION
                SELECT TRELIBELLEDESC LIEN
                  FROM LANTRELATION
                 WHERE     TRECODE =
                           (SELECT TRECODE
                              FROM ACTRELATION
                             WHERE     (    ACTIDRELATION = P_ACTID
                                        AND ACTID = NCLIENT)
                                   AND TRECODE <> 'BRANDDEALERS'
                                   AND ACTIDRELATION IS NOT NULL)
                       AND LANCODE = P_LANCODE);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_LIENEMPRUNTEUR;

    -- Nb enfants
    PROCEDURE P_GET_NBENFANT (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (APANBENFANT)
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_NBENFANT;

    -- Situation familliale MODIFIED
    PROCEDURE P_GET_SITUFAMILLIALE (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TUPLIBELLE
          INTO O_VALEUR
          FROM ACTEURPARTICULIER A, LANTUSPARAM L
         WHERE     A.APASITFAM = L.TUPCODE
               AND ACTID = P_ACTID
               AND L.TUSNOM = 'SITFAM'
               AND LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_SITUFAMILLIALE;

    -- Residence
    PROCEDURE P_GET_RESIDENCE (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';

        SELECT TUPLIBELLE
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC, LANTUSPARAM L
         WHERE     CV.CCHSID = CC.CCHSID
               AND CV.CVASTRINGVALUE = L.TUPCODE
               AND CC.CCHVALUECODE = 'STATUTRESID'
               AND CV.ACTID = P_ACTID
               AND L.LANCODE = P_LANCODE
               AND CV.CVAID =
                   (SELECT MAX (CVAID)
                      FROM CCHVALUE CV2
                     WHERE CV2.CCHSID = CC.CCHSID AND CV2.ACTID = P_ACTID);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_RESIDENCE;

    -- Adresse
    --Modified on August 17th by MAB.
    PROCEDURE P_GET_ADRESSE (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NAADORDRE   NUMBER;
        STYPEVOIE   VARCHAR2 (100);
    BEGIN
        NAADORDRE := F_GETAADORDRE (P_ACTID);

        SELECT TUPLIBELLE
          INTO STYPEVOIE
          FROM LANTUSPARAM
         WHERE     TUSNOM = 'TYPEDEVOIE'
               AND LANCODE = P_LANCODE
               AND TUPCODE =
                   (SELECT DISTINCT CVASTRINGVALUE
                      FROM CCHVALUE
                     WHERE     ACTID = P_ACTID
                           AND CCHSID = (SELECT MAX (CCHSID)
                                           FROM CUSTOMCHARACTERISTIC
                                          WHERE CCHVALUECODE = 'TYPEDEVOIE')
                           AND CVAPKEYVALUE =
                                  'Actid-'
                               || P_ACTID
                               || '||Aadordre-'
                               || NAADORDRE
                               || '||'
                           AND ROWNUM = 1);

        --SELECT ADRVOIE || ' ' || adrlieudit INTO O_VALEUR FROM ACTADRESSE AA,ADRESSE AD WHERE AA.ADRID=AD.ADRID AND AA.ACTID=P_ACTID AND AADORDRE = nAADORDRE;
        SELECT AD.ADRSTREETNUMBER || ' ' || STYPEVOIE || ' ' || AD.ADRVOIE
          INTO O_VALEUR
          FROM ACTADRESSE AA, ADRESSE AD
         WHERE     AA.ADRID = AD.ADRID
               AND AA.ACTID = P_ACTID
               AND AADORDRE = NAADORDRE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_ADRESSE;

    PROCEDURE P_GET_CP (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NAADORDRE   NUMBER;
    BEGIN
        NAADORDRE := F_GETAADORDRE (P_ACTID);

        SELECT ADRCODEPOST
          INTO O_VALEUR
          FROM ACTADRESSE AA, ADRESSE AD
         WHERE     AA.ADRID = AD.ADRID
               AND AA.ACTID = P_ACTID
               AND AADORDRE = NAADORDRE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CP;

    -- Ville
    PROCEDURE P_GET_VILLE (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NAADORDRE   NUMBER;
    BEGIN
        NAADORDRE := F_GETAADORDRE (P_ACTID);

        SELECT ADRVILLE
          INTO O_VALEUR
          FROM ACTADRESSE AA, ADRESSE AD
         WHERE     AA.ADRID = AD.ADRID
               AND AA.ACTID = P_ACTID
               AND AADORDRE = NAADORDRE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_VILLE;

    -- Pays
    PROCEDURE P_GET_PAYS (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NAADORDRE   NUMBER;
    BEGIN
        NAADORDRE := F_GETAADORDRE (P_ACTID);

        SELECT DISTINCT (PAYLIBELLE)
          INTO O_VALEUR
          FROM ACTADRESSE AA, ADRESSE AD, LANPAYS LP
         WHERE     AA.ADRID = AD.ADRID
               AND AD.PAYCODE = LP.PAYCODE
               AND AA.ACTID = P_ACTID
               AND AADORDRE = NAADORDRE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PAYS;

    -- T??A?l??A?phone MODIFIED
    PROCEDURE P_GET_TELEPHONE (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ATENUM
          INTO O_VALEUR
          FROM ACTTELECOM A
         WHERE     ACTID = P_ACTID
               AND ATETYPE IN ('TEL', 'MOB')
               AND ATEDTFIN IS NULL
               AND ATEORDRE =
                   (SELECT MAX (ATEORDRE)
                      FROM ACTTELECOM
                     WHERE     ACTID = A.ACTID
                           AND ATETYPE IN ('TEL', 'MOB')
                           AND ATEDTFIN IS NULL
                           AND ATENUM NOT IN ('0101010101'));
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_TELEPHONE;

    -- Email
    PROCEDURE P_GET_MAIL (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ATENUM
          INTO O_VALEUR
          FROM ACTTELECOM A
         WHERE     ACTID = P_ACTID
               AND ATETYPE = 'NET'
               AND ATEDTFIN IS NULL
               AND ATEORDRE =
                   (SELECT MAX (ATEORDRE)
                      FROM ACTTELECOM
                     WHERE     ACTID = A.ACTID
                           AND ATETYPE = A.ATETYPE
                           AND ATEDTFIN IS NULL);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_MAIL;

    -- Date entr??A? ??A  l'adresse
    PROCEDURE P_GET_DATEENTREADRESSE (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (CVADTVALUE, 'DD/MM/YYYY')
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'DATEENTREELOGEMENT'
               AND CV.ACTID = P_ACTID
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                     WHERE     CV2.CCHSID = CC2.CCHSID
                           AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                           AND CV2.ACTID = CV.ACTID);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_DATEENTREADRESSE;

    -- Anc. ??A  l''adresse (ann??A?es)
    PROCEDURE P_GET_ANCADRESSE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NAADORDRE   NUMBER;
    BEGIN
        P_GET_DATEENTREADRESSE (P_ACTID,
                                P_DOSID,
                                P_DPRVERSION,
                                P_EXERCICE,
                                P_TEFCLASSE,
                                P_LANCODE,
                                P_ANAID,
                                O_VALEUR,
                                O_LIBVALEUR);

        -- nAADORDRE := F_GETAADORDRE(P_ACTID);
        SELECT MAX (
                     TO_CHAR (SYSDATE, 'YYYY')
                   - TO_CHAR (TO_DATE (O_VALEUR, 'DD/MM/YYYY'), 'YYYY'))
          INTO O_VALEUR
          FROM DUAL;
    /*to_char(AADDTDEB,'YYYY') ) INTO O_VALEUR
    from actadresse a where actid=P_ACTID and aadordre = nAADORDRE;*/
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_ANCADRESSE;

    -- Banque
   PROCEDURE P_GET_BANQUE (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS
        CURSOR C_BANQ
        IS
             SELECT BANLIBELLE
          INTO O_VALEUR
          FROM ACTRIB A, RIB R, BANQUE B
         WHERE     A.RIBID = R.RIBID
               AND R.BGUBANQUE = B.BANCODE
               AND ACTID = P_ACTID

               AND ARIDTREMPLACE IS NULL;

        l_valeur      VARCHAR2 (2000);

    BEGIN
        BEGIN
            o_libvaleur := '';

            OPEN C_BANQ;

            LOOP
                FETCH C_BANQ INTO l_valeur;

                EXIT WHEN C_BANQ%NOTFOUND;
                o_valeur := o_valeur || CHR (5) || l_valeur;

            END LOOP;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';

        END;
    END P_GET_BANQUE;

    -- IBAN
    PROCEDURE P_GET_IBAN (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT PAYCODE || RIBIBANCLE || RIBIBANCOMPTE
          INTO O_VALEUR
          FROM ACTRIB A, RIB R, BANQUE B
         WHERE     A.RIBID = R.RIBID
               AND R.BGUBANQUE = B.BANCODE
               AND ACTID = P_ACTID
               AND ARITYPE = 'MAIN'
               AND ARIDTREMPLACE IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_IBAN;

    -- Date RIB
    PROCEDURE P_GET_DATERIB (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        -- select TO_CHAR(ARIDTDEB,'DD/MM/YYYY') INTO O_VALEUR from actrib a where actid=P_ACTID and ARITYPE='MAIN' AND ARIDTREMPLACE is null and RIBID = (select max(RIBID) from actrib where actid=a.actid and ARITYPE='MAIN' AND ARIDTREMPLACE is null);
        SELECT TO_CHAR (CVADTVALUE, 'DD/MM/YYYY')
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'ANCIENNETEBANQ'
               AND CV.ACTID = P_ACTID
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                     WHERE     CV2.CCHSID = CC2.CCHSID
                           AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                           AND CV2.ACTID = CV.ACTID);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_DATERIB;

    -- Anc. ??A  la banque (ann??A?es)
    PROCEDURE P_GET_ANCBANQUE (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (CVADTVALUE, 'YYYY')
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'ANCIENNETEBANQ'
               AND CV.ACTID = P_ACTID
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                     WHERE     CV2.CCHSID = CC2.CCHSID
                           AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                           AND CV2.ACTID = CV.ACTID);
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_ANCBANQUE;

    -- Profession / CSP
    PROCEDURE P_GET_PROFESCSP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT TUPLIBELLE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC, LANTUSPARAM L
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CV.CVASTRINGVALUE = L.TUPCODE
                   AND TUSNOM = 'PROFESSION'
                   AND CC.CCHVALUECODE = 'PROFESSION'
                   AND CV.ACTID = P_ACTID
                   AND L.LANCODE = P_LANCODE
                   AND CVAID =
                       (SELECT MAX (CVAID)
                          FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                         WHERE     CV2.CCHSID = CC2.CCHSID
                               AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                               AND CV2.ACTID = CV.ACTID);
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PROFESCSP;

    -- Profession
    PROCEDURE P_GET_PROFESSION (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT DISTINCT CVASTRINGVALUE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CC.CCHVALUECODE = 'PROFESS'
                   AND CV.ACTID = P_ACTID;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PROFESSION;

    -- Contrat
    PROCEDURE P_GET_CONTRAT (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT TUPLIBELLE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC, LANTUSPARAM L
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CV.CVASTRINGVALUE = L.TUPCODE
                   AND TUSNOM = 'CONTRATTRAVAIL'
                   AND CC.CCHVALUECODE = 'CONTRATTRAVAIL'
                   AND CV.ACTID = P_ACTID
                   AND L.LANCODE = P_LANCODE
                   AND CVAID =
                       (SELECT MAX (CVAID)
                          FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                         WHERE     CV2.CCHSID = CC2.CCHSID
                               AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                               AND CV2.ACTID = CV.ACTID);
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CONTRAT;

    -- Date embauche
    PROCEDURE P_GET_DATEEMBAUCHE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT TO_CHAR (ACTDTEXTHIRING, 'DD/MM/YYYY')
              INTO O_VALEUR
              FROM ACTEUR
             WHERE ACTID = P_ACTID;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_DATEEMBAUCHE;

    -- Anc. ??A  l''emploi (ann??A?es)
    PROCEDURE P_GET_ANCMBAUCHE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT   TO_CHAR (SYSDATE, 'YYYY')
                   - TO_CHAR (ACTDTEXTHIRING, 'YYYY')
              INTO O_VALEUR
              FROM ACTEUR
             WHERE ACTID = P_ACTID;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_ANCMBAUCHE;

    -- Employeur
    PROCEDURE P_GET_EMPLOYEUR (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT APAEMPLOYEUR
          INTO O_VALEUR
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_EMPLOYEUR;

    -- Ville employeur
    PROCEDURE P_GET_VILLEEMPLOYEUR (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT DISTINCT CVASTRINGVALUE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CC.CCHVALUECODE = 'VILEMP'
                   AND CV.ACTID = P_ACTID
                   AND CVAID =
                       (SELECT MAX (CVAID)
                          FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                         WHERE     CV2.CCHSID = CC2.CCHSID
                               AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                               AND CV2.ACTID = CV.ACTID);
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_VILLEEMPLOYEUR;

    -- Tel employeur
    PROCEDURE P_GET_TELEMPLOYEUR (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT   NUMBER;
    BEGIN
        O_VALEUR := '';

        SELECT COUNT (*)
          INTO NCOUNT
          FROM ACTEURPARTICULIER
         WHERE ACTID = P_ACTID;

        IF NCOUNT > 0
        THEN
            SELECT DISTINCT CVASTRINGVALUE
              INTO O_VALEUR
              FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
             WHERE     CV.CCHSID = CC.CCHSID
                   AND CC.CCHVALUECODE = 'TELEMP'
                   AND CV.ACTID = P_ACTID
                   AND CVAID =
                       (SELECT MAX (CVAID)
                          FROM CCHVALUE CV2, CUSTOMCHARACTERISTIC CC2
                         WHERE     CV2.CCHSID = CC2.CCHSID
                               AND CC2.CCHVALUECODE = CC.CCHVALUECODE
                               AND CV2.ACTID = CV.ACTID);
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_TELEMPLOYEUR;

    -- Clef BDF
    PROCEDURE P_GET_CLEBDF (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CRE.CREID)
          INTO NCREID
          FROM CREVT CRE, CREDATA CDA
         WHERE     CRE.CREID = CDA.CREID
               AND ACTID = P_ACTID
               AND CDATABLE = 'BASEEXT'
               AND CDACOLONNE = 'CLEBDF';

        SELECT MAX (CDADATASTRING)
          INTO O_VALEUR
          FROM CREVT CRE, CREDATA CDA
         WHERE     CRE.CREID = CDA.CREID
               AND ACTID = P_ACTID
               AND CDATABLE = 'BASEEXT'
               AND CDACOLONNE = 'CLEBDF'
               AND CRE.CREID = NCREID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_CLEBDF;

    -- FICP
    PROCEDURE P_GET_FICP (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TVALIBELLE
          INTO O_VALEUR
          FROM ACTTCOVALEUR A, LANTCOVALEUR L, TCOTATION T
         WHERE     A.TCOID = L.TCOID
               AND L.TCOID = T.TCOID
               AND TCOCODE = 'FICP'
               AND A.TVACODE = L.TVACODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE
               AND ATVDTFIN IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_FICP;

    -- FCC
    PROCEDURE P_GET_FCC (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TVALIBELLE
          INTO O_VALEUR
          FROM ACTTCOVALEUR A, LANTCOVALEUR L, TCOTATION T
         WHERE     A.TCOID = L.TCOID
               AND L.TCOID = T.TCOID
               AND TCOCODE = 'FCC'
               AND A.TVACODE = L.TVACODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE
               AND ATVDTFIN IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_FCC;

    -- AML RATING  (Experian)
    PROCEDURE P_GET_AMLRATING (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TVALIBELLE
          INTO O_VALEUR
          FROM ACTTCOVALEUR A, LANTCOVALEUR L, TCOTATION T
         WHERE     A.TCOID = L.TCOID
               AND L.TCOID = T.TCOID
               AND TCOCODE = 'RATAML'
               AND A.TVACODE = L.TVACODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE
               AND ATVDTFIN IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_AMLRATING;

    -- Sanction List (Cotation AML)
    PROCEDURE P_GET_SANCTIONLIST (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TVALIBELLE
          INTO O_VALEUR
          FROM ACTTCOVALEUR A, LANTCOVALEUR L, TCOTATION T
         WHERE     A.TCOID = L.TCOID
               AND L.TCOID = T.TCOID
               AND TCOCODE = 'BRISANC'
               AND A.TVACODE = L.TVACODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE
               AND ATVDTFIN IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_SANCTIONLIST;

    -- PEP (Cotation AML)
    PROCEDURE P_GET_PPE (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TVALIBELLE
          INTO O_VALEUR
          FROM ACTTCOVALEUR A, LANTCOVALEUR L, TCOTATION T
         WHERE     A.TCOID = L.TCOID
               AND L.TCOID = T.TCOID
               AND TCOCODE = 'BRIPEP'
               AND A.TVACODE = L.TVACODE
               AND ACTID = P_ACTID
               AND LANCODE = P_LANCODE
               AND ATVDTFIN IS NULL;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GET_PPE;

    -- PROCEDURE de repuperation des studyfiles
    FUNCTION F_GETACTRATVALUE (NACTID       IN ACTEUR.ACTID%TYPE,
                               SRATCODE     IN RATIO.RATCODE%TYPE,
                               P_EXERCICE      NUMBER)
        RETURN VARCHAR2
    AS
        SRETURN     NUMBER;                                 --Varchar2(50); --
        NANAID      ANALYSIS.ANAID%TYPE;
        NANMID      ANALYSIS.ANMID%TYPE;
        NRATID      RATIO.RATID%TYPE;
        NACTTYPE    ACTEUR.ACTTYPE%TYPE;
        NEXERCICE   NUMBER;
    BEGIN
        SELECT CASE
                   WHEN P_EXERCICE > 0 THEN P_EXERCICE
                   ELSE TO_NUMBER (TO_CHAR (SYSDATE, 'YYYY'))
               END
          INTO NEXERCICE
          FROM DUAL;

        BEGIN
            --TRA 180112017 -- on ne prend plus en compte l'annee de reference pour les PART et les RATCODE salaire,loyer,... et on prend en compte les status en cours
            SELECT ACTTYPE
              INTO NACTTYPE
              FROM ACTEUR
             WHERE ACTID = NACTID;

            IF     NACTTYPE IN ('PART', 'EI')
               AND SRATCODE IN ('INDPRSAL',
                                'INDPRFON',
                                'INDPRPEN',
                                'INDPRAUT',
                                'INDCHLOY',
                                'INDCHIMP',
                                'INDCHPEN',
                                'INDCHREMIMM',
                                'INDCHREMNIMM',
                                'INDNBCRE')
            THEN
                SELECT MAX (ANAID)
                  INTO NANAID
                  FROM ANALYSIS
                 WHERE     ANATARGET = 'ACTEUR'
                       AND ACTID = NACTID
                       AND ANATYPE = 'BILANSIMP'
                       AND ANASTATUS = 'EC';
            ELSE
                SELECT MAX (ANAID)
                  INTO NANAID
                  FROM ANALYSIS
                 WHERE     ANATARGET = 'ACTEUR'
                       AND ACTID = NACTID
                       AND ANATYPE = 'BILANSIMP'
                       AND ANAREFERENCEYEAR = NEXERCICE
                       AND ANASTATUS = 'EC';
            END IF;

            SELECT ANMID
              INTO NANMID
              FROM ANALYSIS
             WHERE ANAID = NANAID;

            SELECT MAX (RATIO.RATID)
              INTO NRATID
              FROM RATIO, LKANCANL
             WHERE     LKANCANL.RATID = RATIO.RATID
                   AND RATCODE = SRATCODE
                   AND ANMID = NANMID;

            ---GGE - 15/07/2016
            SELECT ROUND (TO_NUMBER (RATVALUE, '9999999999999.99'))
              INTO SRETURN
              FROM LKANARAT
             WHERE ANAID = NANAID AND RATID = NRATID;
        --SELECT RATVALUE INTO sReturn FROM LKANARAT WHERE ANAID=nANAID AND RATID=nRATID;
        EXCEPTION
            WHEN OTHERS
            THEN
                SRETURN := 0;
        END;

        RETURN REPLACE (TO_CHAR (SRETURN), ',', '.');
    ---  return replace(to_char(sReturn), ',' , '.');
    END F_GETACTRATVALUE;

    -- Salaire
    PROCEDURE P_GET_SALAIRE (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDPRSAL', P_EXERCICE);
        O_VALEUR := ROUND (TO_NUMBER (O_VALEUR, '9999999999999.99'));
    END P_GET_SALAIRE;

    -- Revenus fonciers
    PROCEDURE P_GET_REVENUSFONCIERS (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDPRFON', P_EXERCICE);
    END P_GET_REVENUSFONCIERS;

    -- Pensions alim. re??A?ues
    PROCEDURE P_GET_PENSIONRECUES (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDPRPEN', P_EXERCICE);
    END P_GET_PENSIONRECUES;

    -- Autres revenus
    PROCEDURE P_GET_AUTRESREVENUS (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDPRAUT', P_EXERCICE);
    END P_GET_AUTRESREVENUS;

    -- Total
    PROCEDURE P_GET_TOTALRECUE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR :=
              F_GETACTRATVALUE (P_ACTID, 'INDPRSAL', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDPRFON', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDPRPEN', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDPRAUT', P_EXERCICE);
    END P_GET_TOTALRECUE;

    -- Loyer
    /*
    PROCEDURE P_GET_LOYER (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
       O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDCHLOY', P_EXERCICE);
    END P_GET_LOYER;
 */
    ------------MTR 17/09/2018
    PROCEDURE p_get_loyer (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';


            SELECT ROUND (pfi.pfrmtloyer * (100 + NVL (ttaval, 0)) / 100)
              INTO o_valeur
              FROM dprpropfinance   dpf,
                   pfirubrique      pfi,
                   dossierprospect  d,
                   taxtaux          tx
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND pfi.pfrordre = 1
                   AND dpf.dosid = d.dosid
                   AND dpf.dprversion = d.dprversion
                   AND d.taxcode = tx.taxcode(+)
                   AND tx.ttadtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := SQLERRM;
        END;
    END p_get_loyer;

    -- Impot
    PROCEDURE P_GET_IMPOT (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDCHIMP', P_EXERCICE);
    END P_GET_IMPOT;

    -- Pensions alim. pay??A?es
    PROCEDURE P_GET_PENSIONPAYEES (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDCHPEN', P_EXERCICE);
    END P_GET_PENSIONPAYEES;

    -- Remb. emprunt immo.
    PROCEDURE P_GET_REMBEMPIMMO (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDCHREMIMM', P_EXERCICE);
    END P_GET_REMBEMPIMMO;

    -- Remb. prets - Hors immo.
    PROCEDURE P_GET_REMBEMPHIMM (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDCHREMNIMM', P_EXERCICE);
    END P_GET_REMBEMPHIMM;

    -- Total
    PROCEDURE P_GET_TOTALPAYEES (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR :=
              F_GETACTRATVALUE (P_ACTID, 'INDCHLOY', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDCHIMP', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDCHPEN', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDCHREMIMM', P_EXERCICE)
            + F_GETACTRATVALUE (P_ACTID, 'INDCHREMNIMM', P_EXERCICE);
    END P_GET_TOTALPAYEES;

    -- Nb Cr??A?dits Actifs
    PROCEDURE P_GET_NBCREDITSACTIFS (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'INDNBCRE', P_EXERCICE);
    END P_GET_NBCREDITSACTIFS;

    FUNCTION F_GET_ACTIDCLIENT (P_DOSID NUMBER, P_DPRVERSION VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM DPRACTEUR
             WHERE     DOSID = P_DOSID
                   AND DPRVERSION = P_DPRVERSION
                   AND ROLCODE IN ('CLIENT', 'EMPRUNT');
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GET_ACTIDCLIENT;

    --Pour le dossier d etude PART, on prend 1 seul garant
    FUNCTION F_GET_ACTIDGARANT (P_DOSID NUMBER, P_DPRVERSION VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
        NPFIID   DPRPROPFINANCE.PFIID%TYPE;
    BEGIN
        NPFIID := F_GETPFIID (P_DOSID);

        SELECT MAX (ACTID)
          INTO NACTID
          FROM PFIGUARANTEE
         WHERE PFIID = NPFIID AND TGACODE <> 'BUYBACK';

        RETURN NACTID;
    END F_GET_ACTIDGARANT;

    --TRA 1301 prendre en compte les garants et dirigeants
    PROCEDURE P_GETENCOURSAVANT (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTIDSERVICING   ACTEUR.ACTIDSERVICING%TYPE;
    BEGIN
        NACTIDSERVICING := F_GET_ACTIDSERVICING (P_ACTID);

        SELECT ROUND (
                   NVL (
                       SUM (
                           CASE
                               WHEN DOSDREF.DREDTECH <= SYSDATE
                               THEN
                                   DOSDREF.DREMTECF
                               ELSE
                                   DOSDREF.DREMTECF + DOSDREF.DREMTAMO
                           END),
                       0))
          INTO O_VALEUR
          FROM (SELECT DISTINCT DOSACTEUR.DOSID,
                                DOSRUBECHEANCIER.DREDTECH,
                                DOSRUBECHEANCIER.DREMTECF,
                                DOSRUBECHEANCIER.DREMTAMO
                  FROM DOSACTEUR
                       LEFT JOIN DOSRUBRIQUE
                           ON DOSRUBRIQUE.DOSID = DOSACTEUR.DOSID
                       LEFT JOIN DOSRUBECHEANCIER
                           ON     DOSRUBECHEANCIER.DOSID = DOSACTEUR.DOSID
                              AND DOSRUBECHEANCIER.DRUORDRE =
                                  DOSRUBRIQUE.DRUORDRE
                 WHERE -- R??A?cup??A?ration des dossiers li??A?s aux acteurs avec r??A'les d'emprunteur et de co-emprunteur
                           DOSACTEUR.ACTID = NACTIDSERVICING
                       AND DOSACTEUR.ROLCODE IN ('EMPRUNT',
                                                 'COEMPRU',
                                                 'CLIENT',
                                                 'COCLIEN',
                                                 'INTERV',
                                                 'GARANT')
                       -- R??A?cup??A?ration de la rubrique financi??A?re active au moment de l'extraction
                       AND DOSRUBRIQUE.DRUCLASSE = 'F'
                       AND DOSRUBRIQUE.DRUTYPE = 'F'
                       AND DOSRUBRIQUE.DRUDTDEB <= SYSDATE
                       AND SYSDATE <= DOSRUBRIQUE.DRUDTFIN
                       -- R??A?cup??A?ration de l'??A?ch??A?ance actuelle
                       AND DOSRUBECHEANCIER.DREDTDEB <= SYSDATE
                       AND SYSDATE <= DOSRUBECHEANCIER.DREDTFIN
                       -- R??A?cup??A?ration du role actif de l'acteur sur le dossier
                       AND DACDTDEB <= SYSDATE
                       AND (DACDTFIN < SYSDATE OR DACDTFIN IS NULL)) DOSDREF;
    END P_GETENCOURSAVANT;

    --TRA 2301 : ne pas prendre en compte si on n'a pas d'acteurs
    PROCEDURE P_GETENCOURSAPRES (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        O_VALEUR_FINANCED_AMOUNT   VARCHAR2 (100) := NULL;
        NACTID                     ACTEUR.ACTID%TYPE;
    BEGIN
        IF P_ACTID IS NULL
        THEN
            O_VALEUR := 0;
        ELSE
            --nACTID := F_GET_ACTIDSERVICING (P_ACTID);
            P_GETENCOURSAVANT (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR,
                               O_LIBVALEUR);
            P_FINANCED_AMOUNT (P_ACTID,
                               P_DOSID,
                               P_DPRVERSION,
                               P_EXERCICE,
                               P_TEFCLASSE,
                               P_LANCODE,
                               P_ANAID,
                               O_VALEUR_FINANCED_AMOUNT,
                               O_LIBVALEUR);
            --O_VALEUR := to_char(to_number( O_VALEUR, '999999999D99', 'NLS_NUMERIC_CHARACTERS='', ''' )  + to_number( O_VALEUR_FINANCED_AMOUNT, '999999999D99', 'NLS_NUMERIC_CHARACTERS='', ''' ) );
            O_VALEUR := O_VALEUR + O_VALEUR_FINANCED_AMOUNT;
        END IF;
    END P_GETENCOURSAPRES;

    PROCEDURE P_EXISTIMPAYE90_EMP (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_EXISTIMPAYE90 (P_ACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE90_EMP;

    PROCEDURE P_NIVEAU_RISK (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    --TO DO
    BEGIN
        NULL;
    END P_NIVEAU_RISK;

    PROCEDURE P_INVESTISSEMENT_HT (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE             VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ROUND (DPMMTINVEST)
          INTO O_VALEUR
          FROM DPRMATERIEL
         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_INVESTISSEMENT_HT;

    PROCEDURE P_FORMJURI (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT CJULIBELLE
          INTO O_VALEUR
          FROM LANCATJURIDIQUE
         WHERE     CJUCODE IN (SELECT CJUCODE
                                 FROM ACTEUR
                                WHERE ACTID = P_ACTID)
               AND LANCODE = NVL (P_LANCODE, 'FR');
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_FORMJURI;

    PROCEDURE P_SIREN (P_ACTID               NUMBER,
                       P_DOSID               NUMBER,
                       P_DPRVERSION          VARCHAR2,
                       P_EXERCICE            NUMBER,
                       P_TEFCLASSE           VARCHAR2,
                       P_LANCODE             VARCHAR2,
                       P_ANAID               NUMBER,
                       O_VALEUR       IN OUT VARCHAR2,
                       O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT SUBSTR (ACTSIRET, 1, 9)
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_SIREN;

    PROCEDURE P_ACTNOM (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT ACTLIBCOURT
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ACTNOM;

    PROCEDURE P_NAF (P_ACTID               NUMBER,
                     P_DOSID               NUMBER,
                     P_DPRVERSION          VARCHAR2,
                     P_EXERCICE            NUMBER,
                     P_TEFCLASSE           VARCHAR2,
                     P_LANCODE             VARCHAR2,
                     P_ANAID               NUMBER,
                     O_VALEUR       IN OUT VARCHAR2,
                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT NAFCODE
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NAF;

    PROCEDURE P_NAFLIBELLE (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT NAFLIBELLE
          INTO O_VALEUR
          FROM ACTEUR A, LANNAF L
         WHERE     A.ACTID = P_ACTID
               AND A.NAFCODE = L.NAFCODE
               AND L.LANCODE = P_LANCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NAFLIBELLE;

    PROCEDURE P_CATJURINSEE3 (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CREID)
          INTO NCREID
          FROM CREVT
         WHERE TMFFONCTION = 'BDF27' AND ACTID = P_ACTID;

        SELECT SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID = NCREID
               AND CDADATASTRING LIKE
                       'PERSONNE/MODULE27/PM27/INFOJURIDIQUES/CODECAT@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_CATJURINSEE3;

    PROCEDURE P_CATJURINSEE3_LIB (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CREID)
          INTO NCREID
          FROM CREVT
         WHERE TMFFONCTION = 'BDF27' AND ACTID = P_ACTID;

        SELECT SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID = NCREID
               AND CDADATASTRING LIKE
                       'PERSONNE/MODULE27/PM27/INFOJURIDIQUES/CATJUR@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_CATJURINSEE3_LIB;

    PROCEDURE P_DATECREAT (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (ACTDTIMMATRICULATION, 'dd/MM/YYYY')
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_DATECREAT;

    PROCEDURE P_AGEENT (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT   TO_CHAR (SYSDATE, 'YYYY')
               - TO_CHAR (ACTDTIMMATRICULATION, 'YYYY')
          INTO O_VALEUR
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_AGEENT;

    PROCEDURE P_REGIMIMPO (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TUPLIBELLE
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC, LANTUSPARAM LAN
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'REGIMP'
               AND CV.ACTID = P_ACTID
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2
                     WHERE CV2.CCHSID = CV.CCHSID AND CV2.ACTID = P_ACTID)
               AND LAN.LANCODE = P_LANCODE
               AND LAN.TUPCODE = CVASTRINGVALUE;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_REGIMIMPO;

    PROCEDURE P_ROLEENTREPRISE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := '';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_ROLEENTREPRISE;

    PROCEDURE P_POUVOIRSIGN (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT    NUMBER := 0;
        NCLIENT   ACTRELATION.ACTID%TYPE;
    BEGIN
        SELECT ACTID
          INTO NCLIENT
          FROM DPRACTEUR
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND ROLCODE IN ('EMPRUNT', 'CLIENT');

        SELECT DECODE (NVL (CVABOOLEANVALUE, 0), 0, 'Non', 'Oui')
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'PVRSIGN'
               AND CV.ACTID = NCLIENT
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2
                     WHERE     CV2.CCHSID = CV.CCHSID
                           AND CV2.ACTID = NCLIENT
                           AND CVAPKEYVALUE LIKE
                                   '%Actidrelation-' || P_ACTID || '|%');
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_POUVOIRSIGN;

    --TRA 23/01 : valeur null si pas de dirigeant
    PROCEDURE P_DIRIGEFFECTIF (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT    NUMBER;
        NCLIENT   ACTRELATION.ACTID%TYPE;
    BEGIN
        SELECT ACTID
          INTO NCLIENT
          FROM DPRACTEUR
         WHERE     DOSID = P_DOSID
               AND DPRVERSION = P_DPRVERSION
               AND ROLCODE IN ('EMPRUNT', 'CLIENT');

        -- select Count(1) Into nCount  From Actrelation where Actid = P_ACTID and TRECODE ='DIRIG';
        --IF nCount >0 THEN
        SELECT DECODE (CVABOOLEANVALUE, 1, 'Oui', 'Non')
          INTO O_VALEUR
          FROM CCHVALUE CV, CUSTOMCHARACTERISTIC CC
         WHERE     CV.CCHSID = CC.CCHSID
               AND CC.CCHVALUECODE = 'DIRIGEFF'
               AND CV.ACTID = NCLIENT
               AND CV.CVAID =
                   (SELECT MAX (CV2.CVAID)
                      FROM CCHVALUE CV2
                     WHERE     CV2.CCHSID = CV.CCHSID
                           AND CV2.ACTID = NCLIENT
                           AND CVAPKEYVALUE LIKE
                                   '%Actidrelation-' || P_ACTID || '|%');
    --ELSE
    -- O_VALEUR := '';
    -- ENd IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_DIRIGEFFECTIF;

    --TRA 23/01 : si pas de beneficiaire, valeur null
    PROCEDURE P_BENEFEFFECTIF (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCOUNT    NUMBER := 0;
        NCLIENT   ACTRELATION.ACTID%TYPE;
    BEGIN
        IF P_ACTID IS NULL
        THEN
            O_VALEUR := '';
        ELSE
            SELECT ACTID
              INTO NCLIENT
              FROM DPRACTEUR
             WHERE     DOSID = P_DOSID
                   AND DPRVERSION = P_DPRVERSION
                   AND ROLCODE IN ('EMPRUNT', 'CLIENT');

            SELECT COUNT (1)
              INTO NCOUNT
              FROM ACTRELATION
             WHERE     ACTID = NCLIENT
                   AND ACTIDRELATION = P_ACTID
                   AND TRECODE = 'ACTIONN'
                   AND AREQP >= 25;

            IF NCOUNT > 0
            THEN
                O_VALEUR := 'Oui';
            ELSE
                O_VALEUR := 'Non';
            END IF;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_BENEFEFFECTIF;

    --TRA : 23/10 : valeur null si pas d'acteur
    PROCEDURE P_CAUTIONPERSO (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NPFIID   NUMBER;
        NCOUNT   NUMBER;
    BEGIN
        IF P_ACTID IS NULL
        THEN
            O_VALEUR := '';
        ELSE
            SELECT F_GETPFIID (P_DOSID) INTO NPFIID FROM DUAL;

            SELECT COUNT (*)
              INTO NCOUNT
              FROM PFIGUARANTEE
             WHERE PFIID = NPFIID AND TGACODE IN ('GAR06');

            IF NCOUNT > 0
            THEN
                O_VALEUR := 'YES';
            ELSE
                O_VALEUR := 'NO';
            END IF;
        END IF;
    END P_CAUTIONPERSO;

    PROCEDURE P_NBDIRIGEANT (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT COUNT (1)
          INTO O_VALEUR
          FROM ACTRELATION
         WHERE ACTID = P_ACTID AND TRECODE = 'DIRIG';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NBDIRIGEANT;

    PROCEDURE P_NBACTIONN (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT COUNT (1)
          INTO O_VALEUR
          FROM ACTRELATION
         WHERE ACTID = P_ACTID AND TRECODE = 'ACTIONN';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_NBACTIONN;

    PROCEDURE F_GET_ACTIDGARANT (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_NUMACTEUR (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END F_GET_ACTIDGARANT;

    PROCEDURE P_GET_TELEMPLOYEUR_GAR (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_TELEMPLOYEUR (NACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_TELEMPLOYEUR_GAR;

    PROCEDURE P_TYPEENTREPRISE (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        BILNBEFFECTIF   NUMBER := 0;
        BILMTCA         NUMBER := 0;
        BILMTTOTBIL     NUMBER := 0;
        NCOUNT          NUMBER;
        SACTTYPE        VARCHAR2 (10);
    BEGIN
        SELECT ACTTYPE
          INTO SACTTYPE
          FROM ACTEUR
         WHERE ACTID = P_ACTID;

        IF SACTTYPE <> 'PART'
        THEN
            -- Effectif de l'entreprise
            BEGIN
                SELECT TO_NUMBER (RATVALUE, '999999999999999999999999.99')
                  INTO BILNBEFFECTIF
                  FROM LKANARAT L, RATIO R
                 WHERE     L.RATID = R.RATID
                       AND L.ANAID = P_ANAID
                       AND R.RATCODE = 'BILNBEFFECTIF';
            EXCEPTION
                WHEN OTHERS
                THEN
                    NULL;
            END;

            -- chiffre d'affaires net
            BEGIN
                SELECT TO_NUMBER (RATVALUE, '999999999999999999999999.99')
                  INTO BILMTCA
                  FROM LKANARAT L, RATIO R
                 WHERE     L.RATID = R.RATID
                       AND L.ANAID = P_ANAID
                       AND R.RATCODE = 'BILMTCA';
            EXCEPTION
                WHEN OTHERS
                THEN
                    NULL;
            END;

            -- Total du Bilan
            BEGIN
                SELECT TO_NUMBER (RATVALUE, '999999999999999999999999.99')
                  INTO BILMTTOTBIL
                  FROM LKANARAT L, RATIO R
                 WHERE     L.RATID = R.RATID
                       AND L.ANAID = P_ANAID
                       AND R.RATCODE = 'BILMTTOTBIL';
            EXCEPTION
                WHEN OTHERS
                THEN
                    NULL;
            END;

            -- Micro entreprise : effectif de moins de 10 personnes ET ( chiffre d'affaires net ou total bilan de moins de 2 millions d'euros )
            IF (    BILNBEFFECTIF < 10
                AND (BILMTCA < 2000000 OR BILMTTOTBIL < 2000000))
            THEN
                O_VALEUR := 'Micro entreprise';
            -- PME : effectif de moins de 250 personnes ET (chiffre d'affaires net de moins de 50 millions d'euros ou un total de bilan de moins de 43 millions d'euros)
            ELSIF (    BILNBEFFECTIF < 250
                   AND (BILMTCA < 50000000 OR BILMTTOTBIL < 48000000))
            THEN
                O_VALEUR := 'PME';
            -- grande entreprise dans les autres cas
            ELSE
                O_VALEUR := 'Grande entreprise';
            END IF;
        ELSE
            O_VALEUR := '';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_TYPEENTREPRISE;

    PROCEDURE P_GET_BUSINESS_SCORE (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE      IN     VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    IS
        VAL1     VARCHAR2 (100);
        VAL2     VARCHAR2 (100);
        VAL3     VARCHAR2 (100);
        VAL4     VARCHAR2 (100);
        SCORE1   NUMBER;
        SCORE2   NUMBER;
        SCORE3   NUMBER;
        SCORE4   NUMBER;
    BEGIN
        BEGIN
            O_LIBVALEUR := '';
            O_VALEUR := '';
            SCORE1 := 0;
            SCORE2 := 0;
            SCORE3 := 0;
            SCORE4 := 0;

            --VAL1
            SELECT RATVALUE
              INTO VAL1
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40000;

            --VAL2
            SELECT RATVALUE
              INTO VAL2
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40001;

            --VAL3
            SELECT RATVALUE
              INTO VAL3
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40002;

            --VAL4
            SELECT RATVALUE
              INTO VAL4
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40003;

            IF VAL1 = 'LA'
            THEN
                SCORE1 := 10;
            ELSIF VAL1 = 'LB'
            THEN
                SCORE1 := 7;
            ELSIF VAL1 = 'LC'
            THEN
                SCORE1 := 3;
            ELSE
                SCORE1 := 0;
            END IF;

            IF VAL2 = 'OA'
            THEN
                SCORE2 := 10;
            ELSIF VAL2 = 'OB'
            THEN
                SCORE2 := 8;
            ELSIF VAL2 = 'OC'
            THEN
                SCORE2 := 4;
            ELSE
                SCORE2 := 0;
            END IF;

            IF VAL3 = 'RA'
            THEN
                SCORE3 := 10;
            ELSIF VAL3 = 'RB'
            THEN
                SCORE3 := 8;
            ELSIF VAL3 = 'RC'
            THEN
                SCORE3 := 6;
            ELSIF VAL3 = 'RD'
            THEN
                SCORE3 := 4;
            ELSIF VAL3 = 'RE'
            THEN
                SCORE3 := 2;
            ELSE
                SCORE3 := 0;
            END IF;

            IF VAL4 = 'FA'
            THEN
                SCORE4 := 10;
            ELSIF VAL4 = 'FB'
            THEN
                SCORE4 := 8;
            ELSIF VAL4 = 'FC'
            THEN
                SCORE4 := 6;
            ELSIF VAL4 = 'FD'
            THEN
                SCORE4 := 4;
            ELSIF VAL4 = 'FE'
            THEN
                SCORE4 := 2;
            ELSE
                SCORE4 := 0;
            END IF;

            O_VALEUR :=
                TO_CHAR (
                      (0.4 * SCORE1)
                    + (0.1 * SCORE2)
                    + (0.2 * SCORE3)
                    + (0.3 * SCORE4));
            O_LIBVALEUR := '20% Of the final Score';
        END;
    END P_GET_BUSINESS_SCORE;

    PROCEDURE P_GET_FINANCIAL_SCORE (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE      IN     VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    IS
        VAL1     VARCHAR2 (100);
        VAL2     VARCHAR2 (100);
        VAL3     VARCHAR2 (100);
        SCORE1   NUMBER;
        SCORE2   NUMBER;
        SCORE3   NUMBER;
    BEGIN
        BEGIN
            O_LIBVALEUR := '';
            O_VALEUR := '';
            SCORE1 := 0;
            SCORE2 := 0;
            SCORE3 := 0;

            --VAL1
            SELECT RATVALUE
              INTO VAL1
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40005;

            --VAL2
            SELECT RATVALUE
              INTO VAL2
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40006;

            --VAL3
            SELECT RATVALUE
              INTO VAL3
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40007;

            IF VAL1 = 'RCA'
            THEN
                SCORE1 := 10;
            ELSIF VAL1 = 'RCB'
            THEN
                SCORE1 := 7;
            ELSIF VAL1 = 'RCC'
            THEN
                SCORE1 := 4;
            ELSIF VAL1 = 'RCD'
            THEN
                SCORE1 := 1;
            ELSE
                SCORE1 := 0;
            END IF;

            IF VAL2 = 'DSRA'
            THEN
                SCORE2 := 10;
            ELSIF VAL2 = 'DSRB'
            THEN
                SCORE2 := 8;
            ELSIF VAL2 = 'DSRC'
            THEN
                SCORE2 := 6;
            ELSIF VAL2 = 'DSRD'
            THEN
                SCORE2 := 4;
            ELSIF VAL2 = 'DSRE'
            THEN
                SCORE2 := 2;
            ELSE
                SCORE2 := 0;
            END IF;

            IF VAL3 = 'ARA'
            THEN
                SCORE3 := 10;
            ELSIF VAL3 = 'ARB'
            THEN
                SCORE3 := 6;
            ELSIF VAL3 = 'ARC'
            THEN
                SCORE3 := 2;
            ELSE
                SCORE3 := 0;
            END IF;

            O_VALEUR :=
                TO_CHAR ((0.2 * SCORE1) + (0.6 * SCORE2) + (0.2 * SCORE3));
            O_LIBVALEUR := '40% Of the final Score';
        END;
    END P_GET_FINANCIAL_SCORE;

    PROCEDURE P_GET_PAY_REC_SCORE (P_ACTID               NUMBER,
                                   P_DOSID               NUMBER,
                                   P_DPRVERSION          VARCHAR2,
                                   P_EXERCICE            NUMBER,
                                   P_TEFCLASSE           VARCHAR2,
                                   P_LANCODE      IN     VARCHAR2,
                                   P_ANAID               NUMBER,
                                   O_VALEUR       IN OUT VARCHAR2,
                                   O_LIBVALEUR    IN OUT VARCHAR2)
    IS
        VAL1     VARCHAR2 (100);
        VAL2     VARCHAR2 (100);
        SCORE1   NUMBER;
        SCORE2   NUMBER;
    BEGIN
        BEGIN
            O_LIBVALEUR := '';
            O_VALEUR := '';
            SCORE1 := 0;
            SCORE2 := 0;

            --VAL1
            SELECT RATVALUE
              INTO VAL1
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40010;

            --VAL2
            SELECT RATVALUE
              INTO VAL2
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40011;

            IF VAL1 = 'PA'
            THEN
                SCORE1 := 10;
            ELSIF VAL1 = 'PB'
            THEN
                SCORE1 := 6;
            ELSIF VAL1 = 'PC'
            THEN
                SCORE1 := 4;
            ELSIF VAL1 = 'PD'
            THEN
                SCORE1 := 2;
            ELSE
                SCORE1 := 0;
            END IF;

            IF VAL2 = 'CA'
            THEN
                SCORE2 := 10;
            ELSIF VAL2 = 'CB'
            THEN
                SCORE2 := 5;
            ELSE
                SCORE2 := 0;
            END IF;

            O_VALEUR := TO_CHAR ((0.4 * SCORE1) + (0.6 * SCORE2));
            O_LIBVALEUR := '60% Of the Management Risk';
        END;
    END P_GET_PAY_REC_SCORE;

    PROCEDURE P_GET_QLT_MANAG_SCORE (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE      IN     VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    IS
        VAL1     VARCHAR2 (100);
        VAL2     VARCHAR2 (100);
        VAL3     VARCHAR2 (100);
        VAL4     VARCHAR2 (100);
        SCORE1   NUMBER;
        SCORE2   NUMBER;
        SCORE3   NUMBER;
        SCORE4   NUMBER;
    BEGIN
        BEGIN
            O_LIBVALEUR := '';
            O_VALEUR := '';
            SCORE1 := 0;
            SCORE2 := 0;
            SCORE3 := 0;
            SCORE4 := 0;

            --VAL1
            SELECT RATVALUE
              INTO VAL1
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40014;

            --VAL2
            SELECT RATVALUE
              INTO VAL2
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40015;

            --VAL3
            SELECT RATVALUE
              INTO VAL3
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40016;

            --VAL4
            SELECT RATVALUE
              INTO VAL4
              FROM LKANARAT
             WHERE     ANAID = (SELECT MAX (ANAID)
                                  FROM ANALYSIS
                                 WHERE DOSID = P_DOSID AND ANMID = 4000)
                   AND RATID = 40017;

            IF VAL1 = 'PA'
            THEN
                SCORE1 := 10;
            ELSIF VAL1 = 'PB'
            THEN
                SCORE1 := 8;
            ELSIF VAL1 = 'PC'
            THEN
                SCORE1 := 6;
            ELSIF VAL1 = 'PD'
            THEN
                SCORE1 := 4;
            ELSIF VAL1 = 'PE'
            THEN
                SCORE1 := 2;
            ELSE
                SCORE1 := 0;
            END IF;

            IF VAL2 = 'AGA'
            THEN
                SCORE2 := 10;
            ELSIF VAL2 = 'AGB'
            THEN
                SCORE2 := 7;
            ELSIF VAL2 = 'AGC'
            THEN
                SCORE2 := 3;
            ELSE
                SCORE2 := 0;
            END IF;

            IF VAL3 = 'EXA'
            THEN
                SCORE3 := 10;
            ELSIF VAL3 = 'EXB'
            THEN
                SCORE3 := 8;
            ELSIF VAL3 = 'EXC'
            THEN
                SCORE3 := 6;
            ELSIF VAL3 = 'EXD'
            THEN
                SCORE3 := 4;
            ELSIF VAL3 = 'EXF'
            THEN
                SCORE3 := 2;
            ELSE
                SCORE3 := 0;
            END IF;

            IF VAL4 = 'PLA'
            THEN
                SCORE4 := 10;
            ELSIF VAL4 = 'PLB'
            THEN
                SCORE4 := 8;
            ELSIF VAL4 = 'PLC'
            THEN
                SCORE4 := 5;
            ELSE
                SCORE4 := 0;
            END IF;

            O_VALEUR :=
                TO_CHAR (
                      (0.2 * SCORE1)
                    + (0.2 * SCORE2)
                    + (0.4 * SCORE3)
                    + (0.2 * SCORE4));
            O_LIBVALEUR := '40% Of the Management Risk';
        END;
    END P_GET_QLT_MANAG_SCORE;

    PROCEDURE P_BILNBDUREE (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILNBDUREE', P_EXERCICE);
    END P_BILNBDUREE;

    PROCEDURE P_BILFLCONSO (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT DECODE (F_GETACTRATVALUE (P_ACTID, 'BILFLCONSO', P_EXERCICE),
                       '0', 'Non',
                       'Oui')
          INTO O_VALEUR
          FROM DUAL;
    END P_BILFLCONSO;

    PROCEDURE P_BILMTCA (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTCA', P_EXERCICE);
    END P_BILMTCA;

    PROCEDURE P_BILMTFP (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTFP', P_EXERCICE);
    END P_BILMTFP;

    PROCEDURE P_BILMTDETFIN (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTDETFIN', P_EXERCICE);
    END P_BILMTDETFIN;

    PROCEDURE P_BILMTDETTEMLT (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTDETTEMLT', P_EXERCICE);
    END P_BILMTDETTEMLT;

    PROCEDURE P_BILMTIMMO (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTIMMO', P_EXERCICE);
    END P_BILMTIMMO;

    PROCEDURE P_BILMTRESULT (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTRESULT', P_EXERCICE);
    END P_BILMTRESULT;

    PROCEDURE P_BILMTCAF (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTCAF', P_EXERCICE);
    END P_BILMTCAF;

    PROCEDURE P_BILNBEFFECTIF (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILNBEFFECTIF', P_EXERCICE);
    END P_BILNBEFFECTIF;

    PROCEDURE P_BILMTEBE (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTEBE', P_EXERCICE);
    END P_BILMTEBE;

    PROCEDURE P_BILMTRESEXP (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTRESEXP', P_EXERCICE);
    END P_BILMTRESEXP;

    PROCEDURE P_BILPRODSTOCKIMM (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILPRODSTOCKIMM', P_EXERCICE);
    END P_BILPRODSTOCKIMM;

    PROCEDURE P_BILACHATS (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILACHATS', P_EXERCICE);
    END P_BILACHATS;

    PROCEDURE P_BILAUTRACHATS (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILAUTRACHATS', P_EXERCICE);
    END P_BILAUTRACHATS;

    PROCEDURE P_BILSUBVEXPLOIT (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILSUBVEXPLOIT', P_EXERCICE);
    END P_BILSUBVEXPLOIT;

    PROCEDURE P_BILIMPOTSTAXES (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILIMPOTSTAXES', P_EXERCICE);
    END P_BILIMPOTSTAXES;

    PROCEDURE P_BILCHARGPERS (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILCHARGPERS', P_EXERCICE);
    END P_BILCHARGPERS;

    PROCEDURE P_BILMTFRAISFIN (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTFRAISFIN', P_EXERCICE);
    END P_BILMTFRAISFIN;

    PROCEDURE P_BILMTSTOCK (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTSTOCK', P_EXERCICE);
    END P_BILMTSTOCK;

    PROCEDURE P_BILMTCREANCL (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTCREANCL', P_EXERCICE);
    END P_BILMTCREANCL;

    PROCEDURE P_BILAUTRCREANC (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILAUTRCREANC', P_EXERCICE);
    END P_BILAUTRCREANC;

    PROCEDURE P_BILMTDETFOURN (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTDETFOURN', P_EXERCICE);
    END P_BILMTDETFOURN;

    PROCEDURE P_BILDETFISETAUTR (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILDETFISETAUTR', P_EXERCICE);
    END P_BILDETFISETAUTR;

    PROCEDURE P_BILDELPAYCLIENT (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILDELPAYCLIENT', P_EXERCICE);
    END P_BILDELPAYCLIENT;

    PROCEDURE P_BILDELPAYFOURN (P_ACTID               NUMBER,
                                P_DOSID               NUMBER,
                                P_DPRVERSION          VARCHAR2,
                                P_EXERCICE            NUMBER,
                                P_TEFCLASSE           VARCHAR2,
                                P_LANCODE             VARCHAR2,
                                P_ANAID               NUMBER,
                                O_VALEUR       IN OUT VARCHAR2,
                                O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILDELPAYFOURN', P_EXERCICE);
    END P_BILDELPAYFOURN;

    PROCEDURE P_BILROTSTOCK (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILROTSTOCK', P_EXERCICE);
    END P_BILROTSTOCK;

    --TRA 18/01 P_ANNEEREF
    PROCEDURE P_ANNEEREF (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := TO_CHAR (P_EXERCICE);
    END P_ANNEEREF;

    /* added on August 17th by MAB to calculate  total de bien */
    PROCEDURE P_BILMTTOTBIL (P_ACTID               NUMBER,
                             P_DOSID               NUMBER,
                             P_DPRVERSION          VARCHAR2,
                             P_EXERCICE            NUMBER,
                             P_TEFCLASSE           VARCHAR2,
                             P_LANCODE             VARCHAR2,
                             P_ANAID               NUMBER,
                             O_VALEUR       IN OUT VARCHAR2,
                             O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_VALEUR := F_GETACTRATVALUE (P_ACTID, 'BILMTTOTBIL', P_EXERCICE);
    END P_BILMTTOTBIL;

    FUNCTION F_GETDIRIGEANT1 (P_ACTID         NUMBER,
                              P_DOSID         NUMBER,
                              P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'DIRIG'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        /**IF NACTID IS NULL THEN  --TRA : 0501 : pas de garant si pas de dirigeant
        BEGIN
        SELECT
        ACTID
        INTO
        NACTID
        FROM
        (
        SELECT
        ACTID
        , ROW_NUMBER() OVER(PARTITION BY ACTID ORDER BY ACTID) ORDRE
        FROM
        DPRACTEUR
        WHERE
        DOSID          = P_DOSID
        AND DPRVERSION = P_DPRVERSION
        --AND ROLCODE    = 'GARANT'
        AND ROLCODE    = 'GARANT' and ACTID <>(select actid from dpracteur where DOSID  = P_DOSID AND DPRVERSION = P_DPRVERSION  AND rolcode = 'APPORT')
        ORDER BY
        ACTID
        )
        WHERE
        ORDRE = 1;
        EXCEPTION
        WHEN OTHERS THEN
        NACTID := '';
        END ;
        END IF;*/
        RETURN NACTID;
    END F_GETDIRIGEANT1;

    FUNCTION F_GETDIRIGEANT2 (P_ACTID         NUMBER,
                              P_DOSID         NUMBER,
                              P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'DIRIG'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 2;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        /**IF NACTID IS NULL THEN --TRA 0501 : pas de garant a la place des dirigeants
        BEGIN
        SELECT
        ACTID
        INTO
        NACTID
        FROM
        (
        SELECT
        ACTID
        , ROW_NUMBER() OVER(PARTITION BY ACTID ORDER BY ACTID) ORDRE
        FROM
        DPRACTEUR
        WHERE
        DOSID          = P_DOSID
        AND DPRVERSION = P_DPRVERSION
        --AND ROLCODE    = 'GARANT'
        AND ROLCODE    = 'GARANT' and ACTID <>(select actid from dpracteur where DOSID  = P_DOSID AND DPRVERSION = P_DPRVERSION  AND rolcode = 'APPORT')
        ORDER BY
        ACTID
        )
        WHERE
        ORDRE = 1;
        EXCEPTION
        WHEN OTHERS THEN
        NACTID := '';
        END ;
        END IF; **/
        RETURN NACTID;
    END F_GETDIRIGEANT2;

    FUNCTION F_GETDIRIGEANT3 (P_ACTID         NUMBER,
                              P_DOSID         NUMBER,
                              P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'DIRIG'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 3;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETDIRIGEANT3;

    FUNCTION F_GETDIRIGEANT4 (P_ACTID         NUMBER,
                              P_DOSID         NUMBER,
                              P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'DIRIG'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 4;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETDIRIGEANT4;

    FUNCTION F_GETDIRIGEANT5 (P_ACTID         NUMBER,
                              P_DOSID         NUMBER,
                              P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'DIRIG'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 5;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETDIRIGEANT5;

    FUNCTION F_GETGARANT1 (P_ACTID         NUMBER,
                           P_DOSID         NUMBER,
                           P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
        NPFIID   NUMBER;
    BEGIN
        BEGIN
            NPFIID := F_GETPFIID (P_DOSID);

            SELECT MAX (ACTID)
              INTO NACTID
              FROM (SELECT ACTID
                      FROM (  SELECT DISTINCT ACTID
                                FROM PFIGUARANTEE
                               WHERE PFIID = NPFIID AND TGACODE <> 'BUYBACK'
                            ORDER BY ACTID)
                     WHERE ROWNUM <= 1);
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETGARANT1;

    FUNCTION F_GETGARANT2 (P_ACTID         NUMBER,
                           P_DOSID         NUMBER,
                           P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM (  SELECT ACTID,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ROLCODE ORDER BY ROLCODE)
                                 ORDRE
                        FROM DPRACTEUR
                       WHERE     DOSID = P_DOSID
                             AND DPRVERSION = P_DPRVERSION
                             --AND ROLCODE    = 'GARANT'
                             AND ROLCODE = 'GARANT'
                             AND ACTID <>
                                 (SELECT actid
                                    FROM dpracteur
                                   WHERE     DOSID = P_DOSID
                                         AND DPRVERSION = P_DPRVERSION
                                         AND rolcode = 'APPORT')
                    ORDER BY ACTID)
             WHERE ORDRE = 2;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETGARANT2;

    FUNCTION F_GETGARANT3 (P_ACTID         NUMBER,
                           P_DOSID         NUMBER,
                           P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM (  SELECT ACTID,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ROLCODE ORDER BY ROLCODE)
                                 ORDRE
                        FROM DPRACTEUR
                       WHERE     DOSID = P_DOSID
                             AND DPRVERSION = P_DPRVERSION
                             --AND ROLCODE    = 'GARANT'
                             AND ROLCODE = 'GARANT'
                             AND ACTID <>
                                 (SELECT actid
                                    FROM dpracteur
                                   WHERE     DOSID = P_DOSID
                                         AND DPRVERSION = P_DPRVERSION
                                         AND rolcode = 'APPORT')
                    ORDER BY ACTID)
             WHERE ORDRE = 3;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETGARANT3;

    FUNCTION F_GETGARANT4 (P_ACTID         NUMBER,
                           P_DOSID         NUMBER,
                           P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM (  SELECT ACTID,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ROLCODE ORDER BY ROLCODE)
                                 ORDRE
                        FROM DPRACTEUR
                       WHERE     DOSID = P_DOSID
                             AND DPRVERSION = P_DPRVERSION
                             --AND ROLCODE    = 'GARANT'
                             AND ROLCODE = 'GARANT'
                             AND ACTID <>
                                 (SELECT actid
                                    FROM dpracteur
                                   WHERE     DOSID = P_DOSID
                                         AND DPRVERSION = P_DPRVERSION
                                         AND rolcode = 'APPORT')
                    ORDER BY ACTID)
             WHERE ORDRE = 4;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETGARANT4;

    FUNCTION F_GETGARANT5 (P_ACTID         NUMBER,
                           P_DOSID         NUMBER,
                           P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTID
              INTO NACTID
              FROM (  SELECT ACTID,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ROLCODE ORDER BY ROLCODE)
                                 ORDRE
                        FROM DPRACTEUR
                       WHERE     DOSID = P_DOSID
                             AND DPRVERSION = P_DPRVERSION
                             --AND ROLCODE    = 'GARANT'
                             AND ROLCODE = 'GARANT'
                             AND ACTID <>
                                 (SELECT actid
                                    FROM dpracteur
                                   WHERE     DOSID = P_DOSID
                                         AND DPRVERSION = P_DPRVERSION
                                         AND rolcode = 'APPORT')
                    ORDER BY ACTID)
             WHERE ORDRE = 5;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETGARANT5;

    --
    FUNCTION F_GETACTIONN1 (P_ACTID         NUMBER,
                            P_DOSID         NUMBER,
                            P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'ACTIONN'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETACTIONN1;

    FUNCTION F_GETACTIONN2 (P_ACTID         NUMBER,
                            P_DOSID         NUMBER,
                            P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'ACTIONN'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 2;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETACTIONN2;

    FUNCTION F_GETACTIONN3 (P_ACTID         NUMBER,
                            P_DOSID         NUMBER,
                            P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'ACTIONN'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 3;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETACTIONN3;

    FUNCTION F_GETACTIONN4 (P_ACTID         NUMBER,
                            P_DOSID         NUMBER,
                            P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'ACTIONN'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 4;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETACTIONN4;

    FUNCTION F_GETACTIONN5 (P_ACTID         NUMBER,
                            P_DOSID         NUMBER,
                            P_DPRVERSION    VARCHAR2)
        RETURN NUMBER
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            SELECT ACTIDRELATION
              INTO NACTID
              FROM (  SELECT ACTIDRELATION,
                             ROW_NUMBER ()
                                 OVER (PARTITION BY ACTID ORDER BY ACTID)
                                 ORDRE
                        FROM ACTRELATION
                       WHERE     ACTID = P_ACTID
                             AND TRECODE = 'ACTIONN'
                             AND AREDTFIN IS NULL
                    ORDER BY ACTIDRELATION)
             WHERE ORDRE = 5;
        EXCEPTION
            WHEN OTHERS
            THEN
                NACTID := '';
        END;

        RETURN NACTID;
    END F_GETACTIONN5;

    FUNCTION F_GETAADORDRE (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    AS
        NRETURN   NUMBER;
    BEGIN
        BEGIN
            SELECT MAX (AADORDRE)
              INTO NRETURN
              FROM ACTADRESSE
             WHERE     ACTID = NACTID
                   AND AADORDREREMPLACE IS NULL
                   AND (AADFLAGCOURRIER = 1 OR AADFLAGSIEGE = 1);
        EXCEPTION
            WHEN OTHERS
            THEN
                NRETURN := 0;
        END;

        RETURN NRETURN;
    END F_GETAADORDRE;

    FUNCTION P_GETINFOACCORDENCOURS (P_ACTID NUMBER, P_TYPE VARCHAR2)
        RETURN VARCHAR2
    IS
        CURSOR C1
        IS
            SELECT DOSID
              FROM DPRACTEUR
             WHERE     ACTID = P_ACTID
                   AND ROLCODE IN ('EMPRUNT',
                                   'COEMPRU',
                                   'CLIENT',
                                   'COCLIEN');

        SADECODE            DPRDECISION.ADECODE%TYPE;
        DTDDEDT             DPRDECISION.DDEDT%TYPE;
        L_COUNT             NUMBER;
        NFINANCEDVALUE_HT   NUMBER := 0;
        NMT                 NUMBER := 0;
        NBDOS               NUMBER := 0;
        SRETOUR             VARCHAR2 (50);
    BEGIN
        FOR RC1 IN C1
        LOOP
            BEGIN
                SELECT ADECODE, DDEDT
                  INTO SADECODE, DTDDEDT
                  FROM (  SELECT ADECODE, DDEDT
                            FROM DPRDECISION DPR
                           WHERE     DPR.ADECODE IN
                                         (SELECT REVFILLECODE
                                            FROM RELATIONVALEURPROFIL
                                           WHERE RECCODE = 'DECICONTEXAVT')
                                 AND DOSID = RC1.DOSID
                                 AND DPR.DPRVERSION = 'FIN'
                                 AND PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                         RC1.DOSID) =
                                     'FIN'
                        ORDER BY DDEORDRE DESC)
                 WHERE ROWNUM = 1;
            EXCEPTION
                WHEN OTHERS
                THEN
                    SADECODE := NULL;
                    DTDDEDT := NULL;
            END;

            IF SADECODE IS NOT NULL
            THEN
                BEGIN
                    SELECT COUNT (1)
                      INTO L_COUNT
                      FROM RELATIONVALEURPROFIL RVE
                     WHERE     RVE.RECCODE = 'DECICONTEXAVT'
                           AND RVE.REVMERECODE = 'POSITIF'
                           AND REVFILLECODE = SADECODE;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        L_COUNT := 0;
                END;

                IF     L_COUNT != 0
                   AND DTDDEDT >= ADD_MONTHS (TRUNC (SYSDATE), -3)
                THEN
                    NBDOS := NBDOS + 1;

                    SELECT MAX (PFADOUBLE)
                      INTO NMT
                      FROM DPRPROPFINANCE DPF, PFIATTRIBUT PFA
                     WHERE     DPF.DOSID = RC1.DOSID
                           AND DPF.DPRVERSION = 'FIN'
                           AND DPFFLAGRETENUE = 1
                           AND DPF.PFIID = PFA.PFIID
                           AND PFACODE = 'FINANCEDVALUE';

                    NFINANCEDVALUE_HT := NFINANCEDVALUE_HT + NMT;
                END IF;
            END IF;
        END LOOP;

        IF P_TYPE IN ('NBDOS')
        THEN
            SRETOUR := NBDOS;
        ELSIF P_TYPE IN ('MT_FIN')
        THEN
            SRETOUR := NFINANCEDVALUE_HT;
        END IF;

        RETURN SRETOUR;
    END P_GETINFOACCORDENCOURS;

    PROCEDURE P_GET_TLMATMTHT (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        l_sql :=
               'select ROUND (nvl(dpmmtinvest,-5)) from dprmateriel where dosid='
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || '';

        -- TEFCLASSE POUR PRENDRE EN COMPTE  LES AUTRES CRITERES UTILISES DANS LA TABLE DE PARAMETRAGE RATIO
        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLMATMTHT;

    PROCEDURE P_GET_TLMATFOUR (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        l_sql :=
               'select actnom from acteur a, lkarodpm l where l.dosid='
            || p_dosid
            || ' and l.dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || ' and l.actid=a.actid';

        BEGIN
            BEGIN
                EXECUTE IMMEDIATE l_sql INTO o_valeur;
           -- select dpmlibelle,dpmmtinvest,round((sysdate-nvl(to_date('01/01/2010','dd/mm/yyyy'),sysdate))/30,0)mois from dprmateriel
            EXCEPTION
                WHEN OTHERS
                THEN
                    -- verif si existe le materiel :
                    l_sql :=
                           'select dpmlibelle from dprmateriel where dosid='
                        || p_dosid
                        || ' and dprversion='''
                        || p_dprversion
                        || ''' and '
                        || p_tefclasse
                        || '';

                    BEGIN
                        EXECUTE IMMEDIATE l_sql INTO o_valeur;

                        BEGIN
                            SELECT '** FRS CT : ' || actnom
                              INTO o_valeur
                              FROM acteur a, dpracteur d
                             WHERE     d.dosid = p_dosid
                                   AND d.dprversion = p_dprversion
                                   AND d.actid = a.actid
                                   AND d.rolcode = 'FOURN'
                                   AND ROWNUM = 1;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                o_valeur := 'Pas de fournisseur renseigne';
                        END;
                    END;
            END;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLMATFOUR;

    --MTR 02/05/2018
    PROCEDURE P_GET_TLMATDESC (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        /* l_sql :=
               'select substr(d.dpmlibelle,0,79),l.acadescription from dprmateriel d,lanassetcategory l where d.dosid='--
            || p_dosid
            || ' and d.dprversion='''
            || p_dprversion
            || '''  and d.acacode=l.acacode and l.lancode='''
            || p_lancode
            || '''';*/
        l_sql :=
               'select substr(d.dpmlibelle,0,79),l.acadescription from dprmateriel d,lanassetcategory l where d.dosid='
            || p_dosid
            || ' and d.dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || ' and d.acacode=l.acacode and l.lancode='''
            || p_lancode
            || '''';                                          --MTR 03/07/2018

        DBMS_OUTPUT.put_line ('l_sql' || l_sql);

        BEGIN
            EXECUTE IMMEDIATE l_sql INTO o_valeur, o_libvaleur;

            o_valeur := o_valeur || '(' || o_libvaleur || ')';
            o_libvaleur := '';
        --select dpmlibelle,dpmmtinvest,round((sysdate-nvl(to_date('01/01/2010','dd/mm/yyyy'),sysdate))/30,0)mois from dprmateriel
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLMATDESC;


    PROCEDURE P_REFERENCEN (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    AS
    BEGIN
        BEGIN
            o_libvaleur := '';


            SELECT ANAREFERENCEYEAR
              INTO o_valeur
              FROM analysis
             WHERE dosid = p_dosid AND ANAID = p_anaid;      ---MTR 10/04/2018
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := -10000;
        END;
    END P_REFERENCEN;


    ---

    PROCEDURE P_REFERENCEN1 (p_actid               NUMBER,
                             p_dosid               NUMBER,
                             p_dprversion          VARCHAR2,
                             p_exercice            NUMBER,
                             p_tefclasse           VARCHAR2,
                             p_lancode      IN     VARCHAR2,
                             p_anaid               NUMBER,
                             o_valeur       IN OUT VARCHAR2,
                             o_libvaleur    IN OUT VARCHAR2)
    AS
    BEGIN
        BEGIN
            o_libvaleur := '';


            SELECT ANAREFERENCEYEAR - 1
              INTO o_valeur
              FROM analysis
             WHERE dosid = p_dosid AND ANAID = p_anaid;      ---MTR 10/04/2018
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := -10000;
        END;
    END P_REFERENCEN1;

    PROCEDURE P_REFERENCEN2 (p_actid               NUMBER,
                             p_dosid               NUMBER,
                             p_dprversion          VARCHAR2,
                             p_exercice            NUMBER,
                             p_tefclasse           VARCHAR2,
                             p_lancode      IN     VARCHAR2,
                             p_anaid               NUMBER,
                             o_valeur       IN OUT VARCHAR2,
                             o_libvaleur    IN OUT VARCHAR2)
    AS
    BEGIN
        BEGIN
            o_libvaleur := '';


            SELECT ANAREFERENCEYEAR - 2
              INTO o_valeur
              FROM analysis
             WHERE dosid = p_dosid AND ANAID = p_anaid;      ---MTR 10/04/2018
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := -1;
        END;
    END P_REFERENCEN2;


    PROCEDURE P_GET_TLMATAGE (p_actid               NUMBER,
                              p_dosid               NUMBER,
                              p_dprversion          VARCHAR2,
                              p_exercice            NUMBER,
                              p_tefclasse           VARCHAR2,
                              p_lancode      IN     VARCHAR2,
                              p_anaid               NUMBER,
                              o_valeur       IN OUT VARCHAR2,
                              o_libvaleur    IN OUT VARCHAR2)
    IS
        l_sql   VARCHAR2 (1000);
    BEGIN
        l_sql :=
               'select ROUND ((SYSDATE - NVL(dpmdtpremiercirculation,SYSDATE) ) / 30, 0) from dprmateriel where dosid='
            || p_dosid
            || ' and dprversion='''
            || p_dprversion
            || ''' and '
            || p_tefclasse
            || '';

        BEGIN
            DBMS_OUTPUT.put_line (l_sql);

            EXECUTE IMMEDIATE l_sql INTO o_valeur;
        --select dpmlibelle,dpmmtinvest,round((sysdate-nvl(to_date('01/01/2010','dd/mm/yyyy'),sysdate))/30,0)mois from dprmateriel
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;
    END P_GET_TLMATAGE;

    PROCEDURE P_GET_TVA (p_actid               NUMBER,
                         p_dosid               NUMBER,
                         p_dprversion          VARCHAR2,
                         p_exercice            NUMBER,
                         p_tefclasse           VARCHAR2,
                         p_lancode      IN     VARCHAR2,
                         p_anaid               NUMBER,
                         o_valeur       IN OUT VARCHAR2,
                         o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT tt.ttaval, l.taxlibelle
              INTO o_valeur, o_libvaleur
              FROM lantaxe l, dossierprospect d, taxtaux tt
             WHERE     d.taxcode = l.taxcode
                   AND d.taxcode = tt.taxcode
                   AND lancode = 'FR'
                   AND d.dosid = p_dosid
                   AND d.dprversion = p_dprversion
                   AND tt.TTADTFIN IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TVA;


    PROCEDURE P_GET_MTVRHT (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            P_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (pfi.pfrvr, 0), ROUND (pfi.pfrpctvr, 0) || '%'
              INTO o_valeur, o_libvaleur
              FROM dprpropfinance dpf, pfirubrique pfi
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND pfi.pfrordre = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_MTVRHT;

    PROCEDURE P_GET_DUREE (p_actid               NUMBER,
                           p_dosid               NUMBER,
                           p_dprversion          VARCHAR2,
                           p_exercice            NUMBER,
                           p_tefclasse           VARCHAR2,
                           p_lancode      IN     VARCHAR2,
                           P_anaid               NUMBER,
                           o_valeur       IN OUT VARCHAR2,
                           o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (pfi.pfinbperiodes, 0),
                   'Periodicite : ' || pfi.pfiperiodicite
              INTO o_valeur, o_libvaleur
              FROM dprpropfinance dpf, propositionfinanciere pfi
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid;
        --AND pfi.pfrordre = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_DUREE;

    PROCEDURE P_GET_MTDG (p_actid               NUMBER,
                          p_dosid               NUMBER,
                          p_dprversion          VARCHAR2,
                          p_exercice            NUMBER,
                          p_tefclasse           VARCHAR2,
                          p_lancode      IN     VARCHAR2,
                          p_anaid               NUMBER,
                          o_valeur       IN OUT VARCHAR2,
                          o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (NVL (pfi.pfimtdeposit, 0)),
                      NVL (pfi.pfipctdeposit, 0)
                   || '% - Tx remun : '
                   || ROUND (NVL (pfi.pfidepositremunerationrate, 0), 2)
              INTO o_valeur, o_libvaleur
              FROM dprpropfinance dpf, propositionfinanciere pfi
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid;
        --AND pfi.pfrordre = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END;


    PROCEDURE P_GETNBACCORDENCOURS (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDSERVICING (P_ACTID);
        O_VALEUR := P_GETINFOACCORDENCOURS (NACTID, 'NBDOS');
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GETNBACCORDENCOURS;

    PROCEDURE P_GETMTACCORDENCOURS (P_ACTID               NUMBER,
                                    P_DOSID               NUMBER,
                                    P_DPRVERSION          VARCHAR2,
                                    P_EXERCICE            NUMBER,
                                    P_TEFCLASSE           VARCHAR2,
                                    P_LANCODE             VARCHAR2,
                                    P_ANAID               NUMBER,
                                    O_VALEUR       IN OUT VARCHAR2,
                                    O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDSERVICING (P_ACTID);
        O_VALEUR := ROUND (P_GETINFOACCORDENCOURS (NACTID, 'MT_FIN'));
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_GETMTACCORDENCOURS;

    -- JLE 30/06/2016 - edit procedure
    --TRA 1301 -- prendre en compte tous les dossiers dans lesquels n a l'acteur servicing
    PROCEDURE P_IMPAYE (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDSERVICING (P_ACTID);

        SELECT ROUND (NVL (SUM (F_PLFACTIMP (FACTURE.FACID, SYSDATE)), 0))
          INTO O_VALEUR
          FROM FACTURE
         WHERE ACTIDCLIENT IN (SELECT DISTINCT actid
                                 FROM dosacteur
                                WHERE dosid IN (SELECT dosid
                                                  FROM dosacteur
                                                 WHERE actid = NACTID));
    END P_IMPAYE;

    -- JLE 30/06/2016 - edit procedure
    PROCEDURE P_EXISTIMPAYE30 (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDSERVICING (P_ACTID);
        O_VALEUR := F_ARREARSSTATUS30 (NACTID);
    END P_EXISTIMPAYE30;

    -- JLE 30/06/2016 - edit procedure
    PROCEDURE P_EXISTIMPAYE90 (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS                                        --TRA 23/01 - pas d'acteur --> 0
    BEGIN
        --  O_VALEUR := NVL(PA_PLUGIN_SCFSCORING.F_ARREARSSTATUS(P_ACTID), 0) ;
        O_VALEUR := NVL (NULL, 0);
    END P_EXISTIMPAYE90;

    -- JLE 30/06/2016 Copy of F_ARREARSSTATUS but with 30 days in the place of 90 days
    FUNCTION F_ARREARSSTATUS30 (NACTID IN FACTURE.ACTIDCLIENT%TYPE)
        RETURN NUMBER
    AS
        NRETURN   NUMBER;
    BEGIN
        SELECT COUNT (*)
          INTO NRETURN
          FROM FACTURE
               LEFT JOIN FACECHEANCE ON FACTURE.FACID = FACECHEANCE.FACID
         WHERE     facture.ACTIDCLIENT = NACTID
               AND (   (    F_PLFACTIMP (FACTURE.FACID, SYSDATE) != 0
                        AND FACECHEANCE.FECDTPMT - FACECHEANCE.FECDTEXIGIBLE >=
                            30)
                    OR (    FACECHEANCE.FECDTPMT - FACECHEANCE.FECDTEXIGIBLE >=
                            30
                        AND FACECHEANCE.FECDTEXIGIBLE >= SYSDATE - 5 * 360));

        RETURN NVL (NRETURN, 0);
    END F_ARREARSSTATUS30;

    -- JLE 30/06/2016 - edit procedure
    PROCEDURE P_GETMTREFUS18MTH (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    --nACTID ACTEUR.ACTID%TYPE;
    BEGIN
        --nACTID := F_GET_ACTIDSERVICING (P_ACTID);
        O_VALEUR := TO_CHAR (F_GETMRECENTEJECT (P_ACTID));
    END P_GETMTREFUS18MTH;

    -- JLE 30/06/2016 - edit procedure
    PROCEDURE P_GETNBREFUS18MTH (P_ACTID               NUMBER,
                                 P_DOSID               NUMBER,
                                 P_DPRVERSION          VARCHAR2,
                                 P_EXERCICE            NUMBER,
                                 P_TEFCLASSE           VARCHAR2,
                                 P_LANCODE             VARCHAR2,
                                 P_ANAID               NUMBER,
                                 O_VALEUR       IN OUT VARCHAR2,
                                 O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    --nACTID ACTEUR.ACTID%TYPE;
    BEGIN
        --nACTID := F_GET_ACTIDSERVICING (P_ACTID);
        O_VALEUR := F_GETNBREJECT18M (P_ACTID);
    END P_GETNBREFUS18MTH;

    -- JLE 30/06/2016 - copy of F_GETNBREJECT3M but with 18 month
    FUNCTION F_GETNBREJECT18M (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    AS
        NCOUNT   NUMBER;
    BEGIN
        BEGIN
            SELECT COUNT (DISTINCT DPA.DOSID)
              INTO NCOUNT
              FROM DPRACTEUR DPA, DPRDECISION DDE, DPRPHASE DPH
             WHERE     DPA.ACTID = NACTID
                   AND ROLCODE IN ('EMPRUNT',
                                   'COEMPRU',
                                   'CLIENT',
                                   'COCLIEN')
                   AND DPA.DPRVERSION =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DPA.DOSID)
                   -- AND PAV4_CRITERIA.F_GET_DPR_DERNIERE_DECISION(DPA.DOSID, NULL) <> 'REPRESENTATION'
                   AND (   (    DDE.DOSID = DPA.DOSID
                            AND DDE.ADECODE IN ('RISKKO')
                            --And  Dde.Dprversion = Dpa.Dprversion
                            --LYA le 01/09/16 les decisions des deals represent?sont toutes stock? dans le nouveau deal. mais en phase nego. Il ne faut pas les prendre en compte
                            AND Dde.Dprversion = 'FIN'
                            AND dde.ddeordre >
                                (SELECT MAX (ddeordre)
                                   FROM dprdecision
                                  WHERE     dosid = Dde.Dosid
                                        AND Dprversion = 'NEGO') --LYA Decision  non copiee de la phase NEGO
                            AND DDE.DDEDT > SYSDATE - 30 * 18)
                        OR (    DPH.DOSID = DPA.DOSID
                            AND DPH.DPRVERSION = DPA.DPRVERSION
                            AND DPH.JALCODE IN ('PEPKO', 'FRAUD')
                            AND DPH.DPHDTEFFECT > SYSDATE - 30 * 18));
        EXCEPTION
            WHEN OTHERS
            THEN
                NCOUNT := 0;
        END;

        RETURN NVL (NCOUNT, 0);
    END F_GETNBREJECT18M;

    -- JLE 30/06/2016 - Adapted from F_GETMOSTRECENTEJECT
    FUNCTION F_GETMRECENTEJECT (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    AS
        NFINANCEDVALUE_HT   NUMBER;
        SRETURN             NUMBER := 0;
    BEGIN
        BEGIN
            FOR REC_DOSSIER
                IN (SELECT DISTINCT DPA.DOSID, DPA.DPRVERSION
                      FROM DPRACTEUR DPA, DPRDECISION DDE, DPRPHASE DPH
                     WHERE     DPA.ACTID = NACTID
                           AND ROLCODE IN ('EMPRUNT',
                                           'COEMPRU',
                                           'CLIENT',
                                           'COCLIEN')
                           --  AND PAV4_CRITERIA.F_GET_DPR_DERNIERE_DECISION(DPA.DOSID, NULL) <> 'REPRESENTATION'
                           AND DPA.DPRVERSION =
                               PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                   DPA.DOSID)
                           AND (   (    DDE.DOSID = DPA.DOSID
                                    AND DDE.ADECODE IN ('RISKKO')
                                    --And  Dde.Dprversion = Dpa.Dprversion
                                    --LYA le 01/09/16 les decisions des deals represent?sont toutes stock? dans le nouveau deal. mais en phase nego. Il ne faut pas les prendre en compte
                                    AND Dde.Dprversion = 'FIN'
                                    AND dde.ddeordre >
                                        (SELECT MAX (ddeordre)
                                           FROM dprdecision
                                          WHERE     dosid = Dde.Dosid
                                                AND Dprversion = 'NEGO') --LYA Decision  non copiee de la phase NEGO
                                    AND DDE.DDEDT > SYSDATE - 30 * 18)
                                OR (    DPH.DOSID = DPA.DOSID
                                    AND DPH.DPRVERSION = DPA.DPRVERSION
                                    AND DPH.JALCODE IN ('PEPKO', 'FRAUD')
                                    AND DPH.DPHDTEFFECT > SYSDATE - 30 * 18)))
            LOOP
                SELECT NVL (MAX (PFADOUBLE), 0)
                  INTO NFINANCEDVALUE_HT
                  FROM DPRPROPFINANCE DPF, PFIATTRIBUT PFA
                 WHERE     DPF.DOSID = REC_DOSSIER.DOSID
                       AND DPF.DPRVERSION = REC_DOSSIER.DPRVERSION
                       AND DPFFLAGRETENUE = 1
                       AND DPF.PFIID = PFA.PFIID
                       AND PFACODE = 'FINANCEDVALUE';

                SRETURN := SRETURN + NFINANCEDVALUE_HT;
            END LOOP;
        EXCEPTION
            WHEN OTHERS
            THEN
                SRETURN := 0;
        END;

        RETURN SRETURN;
    END F_GETMRECENTEJECT;



    PROCEDURE P_COTBDF27 (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID =
                   (SELECT MAX (CREID)
                      FROM CREVT
                     WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF27')
               AND CDADATASTRING LIKE
                       'PERSONNE/MODULE27/DIFF27/COTATION/VALCOT@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_COTBDF27;

    PROCEDURE P_DTCOTBDF27 (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (
                   TO_DATE (
                       SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)),
                   'DD/MM/YYYY')
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID =
                   (SELECT MAX (CREID)
                      FROM CREVT
                     WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF27')
               AND CDADATASTRING LIKE
                       'PERSONNE/MODULE27/DIFF27/COTATION/DATCOT@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_DTCOTBDF27;

    PROCEDURE P_ACTUBDF27 (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT TO_CHAR (
                   TO_DATE (
                       SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)),
                   'DD/MM/YYYY')
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID =
                   (SELECT MAX (CREID)
                      FROM CREVT
                     WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF27')
               AND CDADATASTRING LIKE 'PERSONNE/MODULE27/XX@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 'Aucun';
    END P_ACTUBDF27;

    PROCEDURE P_IMPBDF27 (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID =
                   (SELECT MAX (CREID)
                      FROM CREVT
                     WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF27')
               AND CDADATASTRING LIKE 'INFOIMPAYES@%';
    /* <INFOIMPAYES> <CIP02 ISCODE="1"/> </INFOIMPAYES> */
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 'Aucun';
    END P_IMPBDF27;

    --Cotation BDF dirigeant
    --TRA 2001
    PROCEDURE P_COTBDF56 (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   CREVT.CREID%TYPE;
    BEGIN
        SELECT MAX (CREID)
          INTO NCREID
          FROM CREVT
         WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF56';

        /*
        SELECT
        SUBSTR(CDADATASTRING, INSTR(CDADATASTRING, '@') + 1)
        INTO
        O_VALEUR
        FROM
        CREDATA
        WHERE
        CREID =
        (
        SELECT MAX(CREID) FROM CREVT WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF56'
        )
        AND CDADATASTRING LIKE 'PERSONNE/MODULE56/DIFF56/VALCOT@%';
        */
        --TRA 2001 : on utilise la fonction du package EXTERNALDATA_INET
        --O_VALEUR := PAV4_EXTERNALDATA_INET.F_GET_CODE_BDF56(NCREID);
        O_VALEUR := '';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_COTBDF56;

    --Autres fonctions de direction OUI/NOM
    PROCEDURE P_FCT_DIR56 (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 1)
          INTO O_VALEUR
          FROM CREDATA
         WHERE     CREID =
                   (SELECT MAX (CREID)
                      FROM CREVT
                     WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF27')
               AND CDADATASTRING LIKE
                       'PERSONNE/MODULE27/DIFF27/COTATION/VALCOT@%';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_FCT_DIR56;

    --Cote de credit toutes societes confondues P est la pire,  ensuite 0, 1, 1+, 1++, 2 ...
    PROCEDURE P_COTE_CREDIT56 (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NCREID   NUMBER;
        NCOUNT   NUMBER := 0;
    BEGIN
        SELECT MAX (CREID)
          INTO NCREID
          FROM CREVT
         WHERE ACTID = P_ACTID AND TMFFONCTION LIKE 'BDF56';

        SELECT COUNT (1)
          INTO NCOUNT
          FROM CREDATA
         WHERE CREID = NCREID AND CDADATASTRING LIKE '%FONCTCOT/VALCOT@P%';

        IF NCOUNT > 0
        THEN
            O_VALEUR := 'P';
        ELSE
            SELECT MIN (
                       SUBSTR (CDADATASTRING, INSTR (CDADATASTRING, '@') + 2))
              INTO O_VALEUR
              FROM CREDATA
             WHERE CREID = NCREID AND CDADATASTRING LIKE '%FONCTCOT/VALCOT@%';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := '';
    END P_COTE_CREDIT56;

    -- P_SCORE_FINAL
    PROCEDURE P_LOYER_TTC (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NTAXE   NUMBER;
    BEGIN
        BEGIN
            SELECT F_TAXE (P_DOSID) INTO NTAXE FROM DUAL;

            SELECT                            --ROUND(PFRMTLOYER *(1 + NTAXE))
                   --TRA 04012017 : on arrondi a la 2eme decimale
                   ROUND (PFRMTLOYER * (1 + NTAXE), 2)
              INTO O_VALEUR
              FROM PFIRUBRIQUE PFR, DPRPROPFINANCE DPF
             WHERE     PFR.PFIID = DPF.PFIID
                   AND DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION = P_DPRVERSION
                   AND DPFFLAGRETENUE = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 0;
        END;
    END P_LOYER_TTC;

    PROCEDURE P_EXISTIMPAYE90_COEMP (P_ACTID               NUMBER,
                                     P_DOSID               NUMBER,
                                     P_DPRVERSION          VARCHAR2,
                                     P_EXERCICE            NUMBER,
                                     P_TEFCLASSE           VARCHAR2,
                                     P_LANCODE             VARCHAR2,
                                     P_ANAID               NUMBER,
                                     O_VALEUR       IN OUT VARCHAR2,
                                     O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        NACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        NACTID := F_GET_ACTIDCOCLIENT (P_DOSID, P_DPRVERSION);
        P_EXISTIMPAYE90 (NACTID,
                         P_DOSID,
                         P_DPRVERSION,
                         P_EXERCICE,
                         P_TEFCLASSE,
                         P_LANCODE,
                         P_ANAID,
                         O_VALEUR,
                         O_LIBVALEUR);
    END P_EXISTIMPAYE90_COEMP;

    PROCEDURE P_GET_SECTACTIVITE (P_ACTID               NUMBER,
                                  P_DOSID               NUMBER,
                                  P_DPRVERSION          VARCHAR2,
                                  P_EXERCICE            NUMBER,
                                  P_TEFCLASSE           VARCHAR2,
                                  P_LANCODE             VARCHAR2,
                                  P_ANAID               NUMBER,
                                  O_VALEUR       IN OUT VARCHAR2,
                                  O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT tuplibelle
          INTO O_VALEUR
          FROM lantusparam
         WHERE     lancode = P_LANCODE
               AND tupcode IN
                       (SELECT NVL (MAX (cvastringvalue), ' ')
                          FROM cchvalue cva, customcharacteristic cch
                         WHERE     cch.cchvaluecode = 'SECTEURACT'
                               AND cva.cchsid = cch.cchsid
                               AND cva.actid = P_ACTID);
    END P_GET_SECTACTIVITE;

    PROCEDURE P_GET_SECTACTIVITE_GAR (P_ACTID               NUMBER,
                                      P_DOSID               NUMBER,
                                      P_DPRVERSION          VARCHAR2,
                                      P_EXERCICE            NUMBER,
                                      P_TEFCLASSE           VARCHAR2,
                                      P_LANCODE             VARCHAR2,
                                      P_ANAID               NUMBER,
                                      O_VALEUR       IN OUT VARCHAR2,
                                      O_LIBVALEUR    IN OUT VARCHAR2)
    AS
        nACTID   ACTEUR.ACTID%TYPE;
    BEGIN
        nACTID := F_GET_ACTIDGARANT (P_DOSID, P_DPRVERSION);
        P_GET_SECTACTIVITE (nACTID,
                            P_DOSID,
                            P_DPRVERSION,
                            P_EXERCICE,
                            P_TEFCLASSE,
                            P_LANCODE,
                            P_ANAID,
                            O_VALEUR,
                            O_LIBVALEUR);
    END P_GET_SECTACTIVITE_GAR;

    PROCEDURE P_GET_LIENEMPRUNTEUR_EMP (P_ACTID               NUMBER,
                                        P_DOSID               NUMBER,
                                        P_DPRVERSION          VARCHAR2,
                                        P_EXERCICE            NUMBER,
                                        P_TEFCLASSE           VARCHAR2,
                                        P_LANCODE             VARCHAR2,
                                        P_ANAID               NUMBER,
                                        O_VALEUR       IN OUT VARCHAR2,
                                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        P_GET_LIENEMPRUNTEUR (P_ACTID,
                              P_DOSID,
                              P_DPRVERSION,
                              P_EXERCICE,
                              P_TEFCLASSE,
                              P_LANCODE,
                              P_ANAID,
                              O_VALEUR,
                              O_LIBVALEUR);
    END P_GET_LIENEMPRUNTEUR_EMP;

    PROCEDURE P_LIST_OKKO (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_LIST FOR SELECT TUPCODE AS CODE_VALEUR, TUPLIBELLE AS LIBELLE
                           FROM LANTUSPARAM
                          WHERE TUSNOM = 'OKKO' AND LANCODE = P_LANCODE;
    END P_LIST_OKKO;

    PROCEDURE P_LIST_YESNO (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_LIST FOR SELECT TUPCODE AS CODE_VALEUR, TUPLIBELLE AS LIBELLE
                           FROM LANTUSPARAM
                          WHERE TUSNOM = 'YESNO' AND LANCODE = P_LANCODE;
    END P_LIST_YESNO;

  PROCEDURE P_LIST_LADSGM (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
   IS
   BEGIN
      OPEN pc_list FOR
         SELECT tupcode AS code_valeur, tuplibelle AS libelle
           FROM lantusparam
          WHERE tusnom = 'LADSGM' AND lancode = p_lancode;
   END P_LIST_LADSGM;


  PROCEDURE P_LIST_KYCEFF (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
   IS
   BEGIN
      OPEN pc_list FOR
         SELECT tupcode AS code_valeur, tuplibelle AS libelle
           FROM lantusparam
          WHERE tusnom = 'KYCEFF' AND lancode = p_lancode;

   END P_LIST_KYCEFF;

    	 PROCEDURE P_LIST_GARANTIE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
   IS
   BEGIN
      OPEN pc_list FOR
         SELECT tupcode AS code_valeur, tuplibelle AS libelle
           FROM lantusparam
          WHERE tusnom = 'TYPGAR' AND lancode = p_lancode;

   END P_LIST_GARANTIE;


    PROCEDURE P_LIST_TUSPARAM (P_LANCODE          VARCHAR2,
                               P_TUSNOM           VARCHAR2,
                               PC_LIST     IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_LIST FOR SELECT TUPCODE AS CODE_VALEUR, TUPLIBELLE AS LIBELLE
                           FROM LANTUSPARAM
                          WHERE TUSNOM = P_TUSNOM AND LANCODE = P_LANCODE;
    END P_LIST_TUSPARAM;

    PROCEDURE P_LIST_DECISION (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_LIST FOR SELECT TUPCODE AS CODE_VALEUR, TUPLIBELLE AS LIBELLE
                           FROM LANTUSPARAM
                          WHERE TUSNOM = 'DECISONSF' AND LANCODE = P_LANCODE;
    END P_LIST_DECISION;

    PROCEDURE P_LIST_ROLE (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_LIST FOR SELECT TUPCODE AS CODE_VALEUR, TUPLIBELLE AS LIBELLE
                           FROM LANTUSPARAM
                          WHERE TUSNOM = 'ROLESF' AND LANCODE = P_LANCODE;
    END P_LIST_ROLE;

    PROCEDURE P_DEPCONCL (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            EMPR_ADR      VARCHAR2 (10);
            DISTRIB_ADR   VARCHAR2 (10);
            EMPR_ID       dpracteur.actid%TYPE;
            DISTRIB_ID    dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO EMPR_ID
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT actid
              INTO DISTRIB_ID
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode = 'FOURN';

              SELECT NVL (SUBSTR (ad.ADRCODEPOST, 1, 2), 'A')
                INTO EMPR_ADR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGFACTURATION = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = EMPR_ID
            ORDER BY adr.AADORDRE DESC;

              SELECT NVL (SUBSTR (ad.ADRCODEPOST, 1, 2), 'B')
                INTO DISTRIB_ADR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGSIEGE = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = DISTRIB_ID
            ORDER BY adr.AADORDRE DESC;

            IF (EMPR_ID <> DISTRIB_ID)
            THEN
                O_LIBVALEUR := 'YES';
                O_VALEUR := 1;
            ELSE
                O_LIBVALEUR := 'NO';
                O_VALEUR := 0;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                BEGIN
                    DBMS_OUTPUT.put_line (SQLERRM);
                    O_LIBVALEUR := 'YES';
                    O_VALEUR := 1;
                END;
        END;
    END P_DEPCONCL;

    PROCEDURE P_MARITAL (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT APASITFAM
              INTO O_VALEUR
              FROM ACTEURPARTICULIER
             WHERE actid = nclient;


            SELECT TUPLIBELLE
              INTO O_LIBVALEUR
              FROM lantusparam
             WHERE     tusnom = 'SITFAM'
                   AND lancode = P_LANCODE
                   AND tupcode = O_VALEUR;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_MARITAL;

    PROCEDURE P_CUSDEP (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

              SELECT NVL (SUBSTR (ad.ADRCODEPOST, 1, 2), 'NODATA')
                INTO O_VALEUR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGFACTURATION = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = nclient
            ORDER BY adr.AADORDRE DESC;


              SELECT NVL (ad.adrville, 'NODATA')
                INTO O_LIBVALEUR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGFACTURATION = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = nclient
            ORDER BY adr.AADORDRE DESC;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'NODATA';
                O_LIBVALEUR := 'NODATA';
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_CUSDEP;

    PROCEDURE P_APPORT (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            mtapport       propositionfinanciere.PFIPCTDOWNPAYMENT%TYPE;
            finacedvalue   PFIATTRIBUT.PFADOUBLE%TYPE;
            pfi_id         NUMBER;
            nttval         NUMBER;
            ntxc           VARCHAR2(20);
        BEGIN
        select taxcode into ntxc from dossierprospect where dosid=P_DOSID 
        AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID);
        select TTAVAL into nttval from taxtaux where taxcode=ntxc;

            SELECT pfiid
              INTO pfi_id
              FROM DPRPROPFINANCE
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID);

            SELECT NVL (PFIMTPREMIERLOYER, 0)
              INTO mtapport
              FROM propositionfinanciere
             WHERE pfiid = pfi_id;



            O_VALEUR :=
                NVL (
                    ((mtapport*nttval/100)+mtapport),
                    0);
            --O_LIBVALEUR := '%';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_APPORT;

    PROCEDURE P_GET_AGENCE (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            SELECT act.actnom
              INTO o_valeur
              FROM dpracteur dpract, acteur act
             WHERE     dpract.dosid = p_dosid
                   AND dpract.dprversion = p_dprversion
                   AND dpract.actid = act.actid
                   AND dpract.rolcode = 'AGENCE';
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        END;
    END P_GET_AGENCE;

		   -----sysdate 

	PROCEDURE P_GET_SYSDATE (  p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    AS
    BEGIN
		select TO_CHAR(SYSDATE) INTO o_valeur FROM DUAL;

	EXCEPTION
        WHEN OTHERS
        THEN o_valeur := 'NULL';

    END P_GET_SYSDATE;			   
----HME GET BANQUE
PROCEDURE P_GET_DR (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN

		    select land.DPTLABEL INTO o_valeur from department dep, landepartment land where actid in (select actcode from acteur 
            where actid in (select actid from dpracteur where dosid=p_dosid and rolcode='AGENCE'))
            and dep.DPTCODE=land.DPTCODE;


        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        END;
    END P_GET_DR;

   PROCEDURE P_GET_UTICREAT (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
	NUTC VARCHAR2(20);

    BEGIN
        BEGIN
         select  uticodecreation INTO NUTC from dossierprospect where DOSID=p_dosid;
		 SELECT UTINOM||' '|| UTIPRENOM INTO o_valeur from utilisateur where uticode=NUTC;


        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        END;
    END P_GET_UTICREAT;

    PROCEDURE P_GET_PAT (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS


    BEGIN
        BEGIN
         select CVASTRINGVALUE into o_valeur from cchvalue where actid=p_actid and cchsid='TFDCCHSID197';



        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        END;
    END P_GET_PAT;


 PROCEDURE P_GET_NCOMPTE (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS
        CURSOR C_RIB
        IS
            select RIBCOMPTE FROM RIB RIB,actrib actri where 
             Actid= p_actid
            AND RIB.ribid=actri.RIBID; 

        l_valeur      VARCHAR2 (2000);

    BEGIN
        BEGIN
            o_libvaleur := '';

            OPEN C_RIB;

            LOOP
                FETCH C_RIB INTO l_valeur;

                EXIT WHEN C_RIB%NOTFOUND;
                o_valeur := o_valeur || CHR (10) || l_valeur;

            END LOOP;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';

        END;
    END P_GET_NCOMPTE;
    ----HME GET GARANTIE
    PROCEDURE P_GET_GAR (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS
        CURSOR C_GAR
        IS
           select pfiig.TGACODE 
           from dprpropfinance dprf, pfiguarantee pfiig where 
           dprf.dosid=p_dosid  
           AND dprf.dprversion = p_dprversion
           AND dprf.pfiid=pfiig.pfiid;

        l_valeur      VARCHAR2 (2000);
        l_val         VARCHAR2 (2000);
    BEGIN
        BEGIN
            o_libvaleur := '';

            OPEN C_GAR;

            LOOP
                FETCH C_GAR INTO l_valeur;

                EXIT WHEN C_GAR%NOTFOUND;
				SELECT TGALIBELLE into l_val from lantgarantie where tgacode= l_valeur and lancode='FR';
                o_valeur := o_valeur || CHR (10) || l_val;

            END LOOP;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';

        END;
    END P_GET_GAR;




 PROCEDURE P_GET_LIBTACCODE (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                P_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT D.TACCODE, L.TACLIBELLE
              INTO o_valeur, o_libvaleur
              FROM lantactivite L, DOSSIERPROSPECT D
             WHERE     D.dosid = p_dosid
                   AND D.dprversion = p_dprversion
                   AND D.TACCODE = L.TACCODE
                   AND L.LANCODE = p_lancode;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               
        END;
    END;

PROCEDURE P_GET_FOURN (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode             VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            SELECT act.actnom
              INTO o_valeur
              FROM dpracteur dpract, acteur act
             WHERE     dpract.dosid = p_dosid
                   AND dpract.dprversion = p_dprversion
                   AND dpract.actid = act.actid
                   AND dpract.rolcode = 'FOURN';

        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        END;
    END P_GET_FOURN;

------



    PROCEDURE P_GET_ACTADRESSE (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            IF p_dosid IS NULL OR p_dosid = '' OR p_dosid = 0
            THEN
                  SELECT adrvoie || ' ' || adrlieudit,
                         adrcodepost || ' - ' || adrville
                    INTO o_valeur, o_libvaleur
                    FROM adresse adr, actadresse a
                   WHERE     a.actid = p_actid
                         AND a.adrid = adr.adrid
                         AND ROWNUM = 1
                ORDER BY ROWNUM DESC;
            ELSE
                SELECT adrvoie || ' ' || adrlieudit,
                       adrcodepost || ' - ' || adrville
                  INTO o_valeur, o_libvaleur
                  FROM adresse adr, actadresse a, dpracteur dp
                 WHERE     dp.dosid = p_dosid
                       AND dp.dprversion = p_dprversion -- and dp.rolcode='CLIENT'
                       AND dp.actid = p_actid
                       AND dp.actid = a.actid --  AND dp.aadordrecourrier = a.aadordre
                       AND a.adrid = adr.adrid
                       AND ROWNUM = 1;
            END IF;

            DBMS_OUTPUT.PUT_LINE (
                'o_valeur||o_libvaleur :' || o_valeur || o_libvaleur);
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_ACTADRESSE;



    PROCEDURE P_GET_ACTTELEPHONE (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';


            SELECT ATENUM
              INTO o_valeur
              FROM ACTTELECOM
             WHERE     actid = p_actid
                   AND ATETYPE = 'TEL'
                   AND ATEORDRE =
                       (SELECT MAX (ateordre)
                          FROM acttelecom
                         WHERE actid = p_actid AND atetype = 'TEL');
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_ACTTELEPHONE;

    PROCEDURE P_GET_COMMERCIAL (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            IF p_dosid IS NULL OR p_dosid = '' OR p_dosid = 0
            THEN
                SELECT U.UTICODE, U.UTIPRENOM || ' ' || U.UTINOM
                  INTO o_valeur , o_libvaleur
                  FROM UTILISATEUR U, lkactutitsm lk
                 WHERE     lk.actid = p_actid
                       AND lk.tsmmetier = 'Sales'
                       AND lk.UTICODE = U.UTICODE
                       AND lk.tsmsectgestion = 'SGMLEA';
            ELSE
                SELECT U.UTICODE, U.UTIPRENOM || ' ' || U.UTINOM
                  INTO o_valeur , o_libvaleur
                  FROM UTILISATEUR U, DPRINTERVENANT D
                 WHERE     D.dosid = p_dosid
                       AND d.dprversion = p_dprversion
                       AND d.DINMETIER in ('COMCOM','COMPRO', 'CEST' ,'CESTGE','CNORD','CSUD'  )   --('CCOM','CCOMEST','CCOMGE','CCOMN','CCOMS')
                       AND d.UTICODE = U.UTICODE
                       AND DINORDRE =
                           (SELECT MIN (dinordre)
                              FROM dprintervenant
                             WHERE     dosid = p_dosid
                                   AND dprversion = p_dprversion
                                   AND dinmetier in ( 'COMCOM','COMPRO','CEST' ,'CESTGE','CNORD','CSUD' ));
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_COMMERCIAL;

    ------MTR 31/05/2018
    PROCEDURE P_GET_ACTCODESAP (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur           NUMBER)
    IS
    BEGIN
        BEGIN
            --  o_libvaleur := '';

            SELECT S.ASTLIBRE
              INTO o_valeur
              FROM ACTSTATISTIQUE S
             WHERE s.actid = p_actid AND S.SACCODE = 'RAF';
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
        --  o_libvaleur := 'Non renseigne';
        END;
    END;

    --------MTR 16-05-2018
    PROCEDURE P_GET_ACTFJUR (p_actid               NUMBER,
                             p_dosid               NUMBER,
                             p_dprversion          VARCHAR2,
                             p_exercice            NUMBER,
                             p_tefclasse           VARCHAR2,
                             p_lancode      IN     VARCHAR2,
                             p_anaid               NUMBER,
                             o_valeur       IN OUT VARCHAR2,
                             o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT C.CJULIBELLE
              INTO o_valeur
              FROM LANCATJURIDIQUE C, ACTEUR A
             WHERE     a.actid = p_actid
                   AND a.CJUCODE = c.CJUCODE
                   AND c.lancode = p_lancode
                   AND c.paycode = 'TN';
        EXCEPTION
            WHEN OTHERS
            THEN
                o_libvaleur := 'Non renseigne';
        END;
    END;

-----

----
    PROCEDURE P_GET_RECOUVREUR (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
        l_actid   ACTEUR.ACTID%TYPE;
    BEGIN
        BEGIN
            o_libvaleur := '';

            IF p_dosid IS NULL OR p_dosid = '' OR p_dosid = 0
            THEN
                SELECT U.UTICODE, U.UTIPRENOM || ' ' || U.UTINOM
                  INTO o_valeur, o_libvaleur
                  FROM UTILISATEUR U, lkactutitsm lk
                 WHERE     lk.actid = p_actid
                       AND lk.tsmmetier = 'PORTFL'
                       AND lk.UTICODE = U.UTICODE
                       AND lk.tsmsectgestion = 'PROD';
            ELSE
                SELECT MAX (ACTID)
                  INTO l_ACTId
                  FROM DPRACTEUR
                 WHERE     DOSID = p_DOSID
                       AND DPRVERSION = p_dprversion
                       AND ROLCODE IN ('EMPRUNT', 'CLIENT');

                SELECT MAX (U.UTICODE), MAX (U.UTIPRENOM || ' ' || U.UTINOM)
                  INTO o_valeur, o_libvaleur
                  FROM UTILISATEUR U, lkactutitsm D
                 WHERE     D.actid = l_ACTId
                       AND d.TSMMETIER IN ('RAM', 'RCT')
                       AND d.UTICODE = U.UTICODE;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_RECOUVREUR;


    PROCEDURE P_GET_CLIENT_EN_ACTIVITE (p_actid               NUMBER,
                                        p_dosid               NUMBER,
                                        p_dprversion          VARCHAR2,
                                        p_exercice            NUMBER,
                                        p_tefclasse           VARCHAR2,
                                        p_lancode      IN     VARCHAR2,
                                        p_anaid               NUMBER,
                                        o_valeur       IN OUT VARCHAR2,
                                        o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT DECODE (COUNT (client_id), 0, 'NON', 'OUI')
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE     c.mef > 0
                   AND c.client_id = a.actcode
                   AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := -10000;
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_CLIENT_EN_ACTIVITE;


    PROCEDURE P_GET_LIBTPGCODE (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                P_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT D.TPGCODE, L.TPGLIBELLE
              INTO o_valeur, o_libvaleur
              FROM LANTPROFILGESTION L, DOSSIERPROSPECT D
             WHERE     D.dosid = p_dosid
                   AND D.dprversion = p_dprversion
                   AND D.TPGCODE = L.TPGCODE
                   AND L.LANCODE = p_lancode;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END;

    PROCEDURE P_ANCIEN (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT FLOOR (NVL ((  MONTHS_BETWEEN (SYSDATE,
                                                  (SELECT ACTDTEXTHIRING
                                                     FROM acteur
                                                    WHERE actid = nclient))
                                / 12),
                               0))
              INTO O_VALEUR
              FROM DUAL;

            O_LIBVALEUR := 'ans';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_ANCIEN;

    PROCEDURE P_CATJUR (P_ACTID               NUMBER,
                        P_DOSID               NUMBER,
                        P_DPRVERSION          VARCHAR2,
                        P_EXERCICE            NUMBER,
                        P_TEFCLASSE           VARCHAR2,
                        P_LANCODE             VARCHAR2,
                        P_ANAID               NUMBER,
                        O_VALEUR       IN OUT VARCHAR2,
                        O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
            scat      acteur.cjucode%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT MAX (cjucode)
              INTO O_VALEUR
              FROM acteur
             WHERE actid = nclient;

            SELECT CJULIBELLE
              INTO O_LIBVALEUR
              FROM lancatjuridique
             WHERE cjucode = O_VALEUR AND lancode = P_LANCODE;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_CATJUR;

    PROCEDURE P_VS_TEDT (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

              SELECT   100
                     * (  (  NVL (AFDMTRENTPAID, 0)
                           + NVL (AFDMTLOANPAID, 0)
                           + NVL (AFDMTALIMONYPAID, 0)
                           + NVL (AFDMTOTHERPAID, 0)
                           + NVL (AFDMTPAIDMONTHTAX, 0))
                        / (NVL (
                               NULLIF (
                                   (  NVL (AFDMTWAGEINCOME, 0)
                                    + NVL (AFDMTREALESTATEINCOME, 0)
                                    + NVL (AFDMTALIMONYINCOME, 0)
                                    + NVL (AFDMTOTHERINCOME, 0)),
                                   0),
                               1)))
                INTO O_VALEUR
                FROM apafinancialdata
               WHERE actid = nclient AND ROWNUM = 1
            ORDER BY AFDORDER DESC;

            O_LIBVALEUR := '%';

            IF O_VALEUR > 100
            THEN
                O_VALEUR := 100;
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 100;
                O_LIBVALEUR := '%';
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_TEDT;

    PROCEDURE P_VS_ABAN (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT MAX (FLOOR (NVL (MONTHS_BETWEEN (SYSDATE, ARIDTDEB), 0)))
              INTO O_VALEUR
              FROM ACTRIB
             WHERE actid = nclient;

            O_LIBVALEUR := 'mois';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_ABAN;

    PROCEDURE P_VS_COAN (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient         dpracteur.actid%TYPE;
            P_count_tbaid   NUMBER;
            P_tbaid         NUMBER;
            Ant_cont        NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

              SELECT COUNT (*)
                INTO P_count_tbaid
                FROM tbadetail td INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
               WHERE     tb.TBACODE = 'ANTERIO'
                     AND td.actid = nclient
                     AND ROWNUM = 1
            ORDER BY td.tbaid DESC;

            IF P_count_tbaid != 0
            THEN
                  SELECT td.tbaid
                    INTO P_tbaid
                    FROM tbadetail td
                         INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
                   WHERE     tb.TBACODE = 'ANTERIO'
                         AND td.actid = nclient
                         AND ROWNUM = 1
                ORDER BY td.tbaid DESC;

                SELECT COUNT (*)
                  INTO Ant_cont
                  FROM tbadetdata
                 WHERE     tbaid = P_tbaid
                       AND tddcolumn = 'FILENAME'
                       AND tdddatastring NOT IN ('0',
                                                 '1',
                                                 '7',
                                                 'FIN',
                                                 'IMP1',
                                                 'LOY');

                IF Ant_cont != 0
                THEN
                    O_VALEUR := 1;
                    O_LIBVALEUR := 'Classe 1';
                ELSE
                    O_VALEUR := 2;
                    O_LIBVALEUR := 'Classe 2';
                END IF;
            ELSE
                O_VALEUR := 1;
                O_LIBVALEUR := 'Classe 1';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_COAN;

    PROCEDURE P_VS_REPR (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient      dpracteur.actid%TYPE;
            P_AADORDRE   NUMBER;
        BEGIN
            SELECT actid, ACOORDREPRINCIPAL
              INTO nclient, P_AADORDRE
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            IF NVL (P_AADORDRE, 0) != 0
            THEN
                SELECT AADTYPE
                  INTO O_VALEUR
                  FROM actadresse
                 WHERE actid = nclient AND AADORDRE = P_AADORDRE;

                SELECT TUPLIBELLE
                  INTO O_LIBVALEUR
                  FROM lantusparam
                 WHERE     tusnom = 'AADTYPE'
                       AND TUPCODE = O_VALEUR
                       AND lancode = P_LANCODE;
            ELSE
                  SELECT AADTYPE
                    INTO O_VALEUR
                    FROM ACTADRESSE adr
                   WHERE     adr.AADFLAGSIEGE = 1
                         AND adr.AADDTREMPLACE IS NULL
                         AND ROWNUM = 1
                         AND adr.actid = nclient
                ORDER BY adr.AADORDRE DESC;

                SELECT TUPLIBELLE
                  INTO O_LIBVALEUR
                  FROM lantusparam
                 WHERE     tusnom = 'AADTYPE'
                       AND TUPCODE = O_VALEUR
                       AND lancode = P_LANCODE;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_REPR;

    PROCEDURE P_VarS_TXEF (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient           dpracteur.actid%TYPE;
            ncoempru          dpracteur.actid%TYPE;
            row_count         NUMBER;
            row_count_coemp   NUMBER;
            P_Revenu_Emp      NUMBER;
            P_Revenu_CoEmp    NUMBER;
            P_Mnt_Bien        NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 1
            THEN
                SELECT actid
                  INTO ncoempru
                  FROM dpracteur
                 WHERE     dosid = P_DOSID
                       AND dprversion =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                       AND rolcode IN ('COEMPRU', 'COCLIEN');
            ELSE
                ncoempru := NULL;
            END IF;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTWAGEINCOME, 0)
                                      + NVL (AFDMTREALESTATEINCOME, 0)
                                      + NVL (AFDMTALIMONYINCOME, 0)
                                      + NVL (AFDMTOTHERINCOME, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_Emp
                FROM apafinancialdata
               WHERE actid = nclient AND ROWNUM = 1
            ORDER BY AFDORDER DESC;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTWAGEINCOME, 0)
                                      + NVL (AFDMTREALESTATEINCOME, 0)
                                      + NVL (AFDMTALIMONYINCOME, 0)
                                      + NVL (AFDMTOTHERINCOME, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_CoEmp
                FROM apafinancialdata
               WHERE actid = ncoempru AND ROWNUM = 1
            ORDER BY AFDORDER DESC;

            SELECT NVL (SUM (NVL (DPMMTINVEST, 0)), 0)
              INTO P_Mnt_Bien
              FROM dprmateriel
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID);

            SELECT COUNT (*)
              INTO row_count
              FROM actrelation
             WHERE    (actid = nclient AND actidrelation = ncoempru)
                   OR     (actid = ncoempru AND actidrelation = nclient)
                      AND trecode IN ('CONB', 'MARIE');

            IF row_count = 0
            THEN
                O_VALEUR :=
                    100 * (NVL (P_Mnt_Bien, 0) / NVL (P_Revenu_Emp, 1));
                O_LIBVALEUR := '%';
            ELSE
                O_VALEUR :=
                      100
                    * (  NVL (P_Mnt_Bien, 0)
                       / NVL (P_Revenu_Emp + P_Revenu_CoEmp, 1));
                O_LIBVALEUR := '%';
            END IF;

            IF O_VALEUR > 100
            THEN
                O_VALEUR := 100;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VarS_TXEF;

    PROCEDURE P_VS_PRES (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
        BEGIN
            SELECT COUNT (*)
              INTO row_count
              FROM dpracteur
             WHERE rolcode IN ('COEMPRU', 'COCLIEN') AND dosid = P_DOSID;

            IF row_count = 0
            THEN
                O_VALEUR := 0;
                O_LIBVALEUR := 'NO';
            ELSE
                O_VALEUR := 1;
                O_LIBVALEUR := 'YES';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_PRES;

    PROCEDURE P_VS_NALI (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient    dpracteur.actid%TYPE;
            DEP_CODE   VARCHAR2 (10);
            DEP_LEB    VARCHAR2 (10);
        BEGIN
            BEGIN
                pav4_ratio.P_CUSDEP ('',
                                     P_DOSID,
                                     '',
                                     '',
                                     '',
                                     P_LANCODE,
                                     '',
                                     DEP_CODE,
                                     DEP_LEB);
            END;

            IF DEP_CODE = 99
            THEN
                O_VALEUR := 0;
                O_LIBVALEUR := 'YES';
            ELSE
                O_VALEUR := 1;
                O_LIBVALEUR := 'NO';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_VS_NALI;

    PROCEDURE P_LOAB_LODU (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            pfi_id   NUMBER;
        BEGIN
            SELECT pfiid
              INTO pfi_id
              FROM DPRPROPFINANCE
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID);

            SELECT NVL (PFAENTIER, 0)
              INTO O_VALEUR
              FROM PFIATTRIBUT
             WHERE PFACODE = 'NBTERM' AND pfiid = pfi_id;

            O_LIBVALEUR := 'mois';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_LOAB_LODU;

    PROCEDURE P_FLT_AGEM (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT FLOOR (NVL (MONTHS_BETWEEN (SYSDATE, APADTNAISS) / 12, 0))
              INTO O_VALEUR
              FROM acteurparticulier
             WHERE actid = nclient;

            O_LIBVALEUR := 'mois';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_AGEM;

    PROCEDURE P_FLT_CTEN (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
            ret_count   NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT COUNT (*)
              INTO ret_count
              FROM acteur
             WHERE actid = nclient AND CJUCODE = '70';

            IF (ret_count = 0)
            THEN
                O_VALEUR := 70;

                SELECT CJULIBELLE
                  INTO O_LIBVALEUR
                  FROM lancatjuridique
                 WHERE cjucode IN ('70');
            ELSE
                SELECT NVL (AJHBUSCAREERCODE, 'NODATA')
                  INTO O_VALEUR
                  FROM actjobhistory
                 WHERE AJHDTEND IS NULL AND actid = nclient;

                SELECT TUPLIBELLE
                  INTO O_LIBVALEUR
                  FROM lantusparam
                 WHERE     tusnom = 'CAREERCODE'
                       AND lancode = 'FR'
                       AND tusnom = O_VALEUR;
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'NODATA';
                O_LIBVALEUR := 'NODATA';
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_CTEN;

    PROCEDURE P_FLT_FEMP (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient               dpracteur.actid%TYPE;
            P_FLT_FEMP_FCC        VARCHAR2 (10);
            P_FLT_FEMP_FICP       VARCHAR2 (10);
            P_FLT_FEMP_FCC_LIB    VARCHAR2 (100);
            P_FLT_FEMP_FICP_LIB   VARCHAR2 (100);
        BEGIN
            BEGIN
                pav4_ratio.P_FLT_FEMP_FCC ('',
                                           P_DOSID,
                                           '',
                                           '',
                                           '',
                                           P_LANCODE,
                                           '',
                                           P_FLT_FEMP_FCC,
                                           P_FLT_FEMP_FCC_LIB);
            END;

            BEGIN
                pav4_ratio.P_FLT_FEMP_FICP ('',
                                            P_DOSID,
                                            '',
                                            '',
                                            '',
                                            P_LANCODE,
                                            '',
                                            P_FLT_FEMP_FICP,
                                            P_FLT_FEMP_FICP_LIB);
            END;

            IF (P_FLT_FEMP_FCC = 'NOFCC' AND P_FLT_FEMP_FICP = 'NOFICP')
            THEN
                O_VALEUR := 0;
                O_LIBVALEUR :=
                    CONCAT (CONCAT (P_FLT_FEMP_FCC_LIB, ' '),
                            P_FLT_FEMP_FICP_LIB);
            ELSE
                O_VALEUR := 1;
                O_LIBVALEUR :=
                    CONCAT (CONCAT (P_FLT_FEMP_FCC_LIB, ' '),
                            P_FLT_FEMP_FICP_LIB);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FEMP;

    PROCEDURE P_FLT_FEMP_FCC (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
            P_FCC       NUMBER;
            P_FICP      NUMBER;
        BEGIN
            SELECT TCOID
              INTO P_FCC
              FROM TCOTATION
             WHERE TCOCODE = 'FCC';

            --   SELECT TCOID into P_FICP FROM TCOTATION WHERE TCOCODE = 'FICP';

            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');


            SELECT TVACODE
              INTO O_VALEUR
              FROM ACTEUR ACT, ACTTCOVALEUR ACV
             WHERE     ACT.ACTID = ACV.ACTID
                   AND ACV.TCOID = P_FCC
                   AND ACV.ATVDTFIN IS NULL
                   AND ACT.ACTID = nclient;


            SELECT TVALIBELLE
              INTO O_LIBVALEUR
              FROM lantcovaleur
             WHERE     TVACODE = O_VALEUR
                   AND TCOID = P_FCC
                   AND lancode = P_LANCODE;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'SUSPI';

                SELECT TVALIBELLE
                  INTO O_LIBVALEUR
                  FROM lantcovaleur
                 WHERE     TVACODE = O_VALEUR
                       AND TCOID = P_FCC
                       AND lancode = P_LANCODE;
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FEMP_FCC;

    PROCEDURE P_FLT_FEMP_FICP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
            P_FCC       NUMBER;
            P_FICP      NUMBER;
        BEGIN
            --SELECT TCOID into P_FCC FROM TCOTATION WHERE TCOCODE = 'FCC';
            SELECT TCOID
              INTO P_FICP
              FROM TCOTATION
             WHERE TCOCODE = 'FICP';

            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');


            SELECT TVACODE
              INTO O_VALEUR
              FROM ACTEUR ACT, ACTTCOVALEUR ACV
             WHERE     ACT.ACTID = ACV.ACTID
                   AND ACV.TCOID = P_FICP
                   AND ACV.ATVDTFIN IS NULL
                   AND ACT.ACTID = nclient;


            SELECT TVALIBELLE
              INTO O_LIBVALEUR
              FROM lantcovaleur
             WHERE     TVACODE = O_VALEUR
                   AND TCOID = P_FICP
                   AND lancode = P_LANCODE;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'SUSPI';

                SELECT TVALIBELLE
                  INTO O_LIBVALEUR
                  FROM lantcovaleur
                 WHERE     TVACODE = O_VALEUR
                       AND TCOID = P_FICP
                       AND lancode = P_LANCODE;
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FEMP_FICP;

    PROCEDURE P_FLT_FCOE (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient               dpracteur.actid%TYPE;
            P_FLT_FCOE_FCC        VARCHAR2 (10);
            P_FLT_FCOE_FICP       VARCHAR2 (10);
            P_FLT_FCOE_FCC_LIB    VARCHAR2 (100);
            P_FLT_FCOE_FICP_LIB   VARCHAR2 (100);
            row_count_coemp       NUMBER;
        BEGIN
            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 0
            THEN
                O_VALEUR := 0;
                O_LIBVALEUR := 'Pas de coemprunteur';
            ELSE
                BEGIN
                    pav4_ratio.P_FLT_FCOE_FCC ('',
                                               P_DOSID,
                                               '',
                                               '',
                                               '',
                                               P_LANCODE,
                                               '',
                                               P_FLT_FCOE_FCC,
                                               P_FLT_FCOE_FCC_LIB);
                END;

                BEGIN
                    pav4_ratio.P_FLT_FCOE_FICP ('',
                                                P_DOSID,
                                                '',
                                                '',
                                                '',
                                                P_LANCODE,
                                                '',
                                                P_FLT_FCOE_FICP,
                                                P_FLT_FCOE_FICP_LIB);
                END;

                IF (P_FLT_FCOE_FCC = 'NOFCC' AND P_FLT_FCOE_FICP = 'NOFICP')
                THEN
                    O_VALEUR := 2;
                    O_LIBVALEUR :=
                        CONCAT (CONCAT (P_FLT_FCOE_FCC_LIB, ' '),
                                P_FLT_FCOE_FICP_LIB);
                ELSE
                    O_VALEUR := 1;
                    O_LIBVALEUR :=
                        CONCAT (CONCAT (P_FLT_FCOE_FCC_LIB, ' '),
                                P_FLT_FCOE_FICP_LIB);
                END IF;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FCOE;

    PROCEDURE P_FLT_FCOE_FCC (P_ACTID               NUMBER,
                              P_DOSID               NUMBER,
                              P_DPRVERSION          VARCHAR2,
                              P_EXERCICE            NUMBER,
                              P_TEFCLASSE           VARCHAR2,
                              P_LANCODE             VARCHAR2,
                              P_ANAID               NUMBER,
                              O_VALEUR       IN OUT VARCHAR2,
                              O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
            P_FCC       NUMBER;
            P_FICP      NUMBER;
        BEGIN
            SELECT TCOID
              INTO P_FCC
              FROM TCOTATION
             WHERE TCOCODE = 'FCC';

            --SELECT TCOID into P_FICP FROM TCOTATION WHERE TCOCODE = 'FICP';

            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            SELECT TVACODE
              INTO O_VALEUR
              FROM ACTEUR ACT, ACTTCOVALEUR ACV
             WHERE     ACT.ACTID = ACV.ACTID
                   AND ACV.TCOID = P_FCC
                   AND ACV.ATVDTFIN IS NULL
                   AND ACT.ACTID = nclient;

            SELECT TVALIBELLE
              INTO O_LIBVALEUR
              FROM lantcovaleur
             WHERE     TVACODE = O_VALEUR
                   AND TCOID = P_FCC
                   AND lancode = P_LANCODE;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'SUSPI';

                SELECT TVALIBELLE
                  INTO O_LIBVALEUR
                  FROM lantcovaleur
                 WHERE     TVACODE = O_VALEUR
                       AND TCOID = P_FCC
                       AND lancode = P_LANCODE;
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FCOE_FCC;

    PROCEDURE P_FLT_FCOE_FICP (P_ACTID               NUMBER,
                               P_DOSID               NUMBER,
                               P_DPRVERSION          VARCHAR2,
                               P_EXERCICE            NUMBER,
                               P_TEFCLASSE           VARCHAR2,
                               P_LANCODE             VARCHAR2,
                               P_ANAID               NUMBER,
                               O_VALEUR       IN OUT VARCHAR2,
                               O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient     dpracteur.actid%TYPE;
            row_count   NUMBER;
            P_FCC       NUMBER;
            P_FICP      NUMBER;
        BEGIN
            -- SELECT TCOID into P_FCC FROM TCOTATION WHERE TCOCODE = 'FCC';
            SELECT TCOID
              INTO P_FICP
              FROM TCOTATION
             WHERE TCOCODE = 'FICP';

            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            SELECT TVACODE
              INTO O_VALEUR
              FROM ACTEUR ACT, ACTTCOVALEUR ACV
             WHERE     ACT.ACTID = ACV.ACTID
                   AND ACV.TCOID = P_FICP
                   AND ACV.ATVDTFIN IS NULL
                   AND ACT.ACTID = nclient;

            SELECT TVALIBELLE
              INTO O_LIBVALEUR
              FROM lantcovaleur
             WHERE     TVACODE = O_VALEUR
                   AND TCOID = P_FICP
                   AND lancode = P_LANCODE;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'SUSPI';

                SELECT TVALIBELLE
                  INTO O_LIBVALEUR
                  FROM lantcovaleur
                 WHERE     TVACODE = O_VALEUR
                       AND TCOID = P_FICP
                       AND lancode = P_LANCODE;
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_FCOE_FICP;

    PROCEDURE P_FLT_ADUR (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient       dpracteur.actid%TYPE;
            P_FLT_AGEM    VARCHAR2 (10);
            P_LOAB_LODU   VARCHAR2 (10);
            P_UNITY       VARCHAR2 (10);
        BEGIN
            BEGIN
                pav4_ratio.P_FLT_AGEM ('',
                                       P_DOSID,
                                       '',
                                       '',
                                       '',
                                       P_LANCODE,
                                       '',
                                       P_FLT_AGEM,
                                       P_UNITY);
            END;

            BEGIN
                pav4_ratio.P_LOAB_LODU ('',
                                        P_DOSID,
                                        '',
                                        '',
                                        '',
                                        P_LANCODE,
                                        '',
                                        P_LOAB_LODU,
                                        P_UNITY);
            END;

            O_VALEUR := NVL (FLOOR ((P_FLT_AGEM + P_LOAB_LODU) / 12), 0);
            O_LIBVALEUR := 'mois';
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_ADUR;

    PROCEDURE P_FLT_ANTO4 (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient         dpracteur.actid%TYPE;
            row_count_emp   NUMBER;
            P_tbaid         NUMBER;
            Ant_cont        NUMBER;
            CTX3_Count      NUMBER;
            Ant_hors_cont   NUMBER;
            P_count_tbaid   NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

              SELECT COUNT (*)
                INTO P_count_tbaid
                FROM tbadetail td INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
               WHERE     tb.TBACODE = 'ANTERIO'
                     AND td.actid = nclient
                     AND ROWNUM = 1
            ORDER BY td.tbaid DESC;

            IF P_count_tbaid != 0
            THEN
                  SELECT td.tbaid
                    INTO P_tbaid
                    FROM tbadetail td
                         INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
                   WHERE     tb.TBACODE = 'ANTERIO'
                         AND td.actid = nclient
                         AND ROWNUM = 1
                ORDER BY td.tbaid DESC;

                SELECT COUNT (*)
                  INTO Ant_cont
                  FROM tbadetdata
                 WHERE     tbaid = P_tbaid
                       AND tddcolumn = 'FILENAME'
                       AND tdddatastring = 'CTX3';

                IF Ant_cont > 0
                THEN
                    O_VALEUR := 2;
                    O_LIBVALEUR := '';
                ELSE
                    SELECT COUNT (*)
                      INTO Ant_hors_cont
                      FROM tbadetdata
                     WHERE     tbaid = P_tbaid
                           AND tddcolumn = 'FILENAME'
                           AND tdddatastring IN ('CTX1',
                                                 'CTX2',
                                                 '6',
                                                 '4',
                                                 '3',
                                                 'DBT',
                                                 'IMP2',
                                                 'IMP3',
                                                 'MEM1',
                                                 'SUDT',
                                                 'MEM2',
                                                 'REFR');

                    IF Ant_hors_cont > 0
                    THEN
                        O_VALEUR := 1;
                        O_LIBVALEUR := '';
                    ELSE
                        O_VALEUR := 3;
                        O_LIBVALEUR := '';
                    END IF;
                END IF;
            ELSE
                O_VALEUR := 3;
                O_LIBVALEUR := '';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_ANTO4;

    PROCEDURE P_LIST_BILAN (
        P_DOSID               DOSSIERPROSPECT.DOSID%TYPE,
        P_DPRVERSION          DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_ACTID               DPRACTEUR.ACTID%TYPE,
        P_LANCODE             LANGUE.LANCODE%TYPE,
        PC_RETURN      IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN PC_RETURN FOR
            WITH
                LIB_BILAN
                AS
                    (SELECT 1                               INDICE,
                            'II'                            CODE_ACTIF,
                            'Immobilisations incorporelles' LIB_ACIF,
                            'DA'                            CODE_PASSIF,
                            'Capital'                       LIB_PASSIF
                       FROM DUAL
                     UNION
                     SELECT 2,
                            '',
                            '',
                            'DB',
                            'Prime d''emission et ecart de '
                       FROM DUAL
                     UNION
                     SELECT 3,
                            'AH3',
                            'Fonds de commerce',
                            'DD',
                            'Reserves'
                       FROM DUAL
                     UNION
                     SELECT 4, '', '', 'DH', 'Report a nouveau' FROM DUAL
                     UNION
                     SELECT 5, '', '', 'DI', 'Resultat' FROM DUAL
                     UNION
                     SELECT 6,
                            '',
                            '',
                            'DJ',
                            'Subventions d''investissement'
                       FROM DUAL
                     UNION
                     SELECT 7,
                            'IC',
                            'Immobilisations corporelles',
                            '',
                            ''
                       FROM DUAL
                     UNION
                     SELECT 8, '', '', 'DL', 'Total Fond propres' FROM DUAL
                     UNION
                     SELECT 9, 'AN3', 'Terrains', '', '' FROM DUAL
                     UNION
                     SELECT 10,
                            'AP3',
                            'Constructions',
                            'DP',
                            'Provisions pour risques '
                       FROM DUAL
                     UNION
                     SELECT 11,
                            'AR3',
                            'Materiel et autre',
                            'DN',
                            'Avances conditionnees '
                       FROM DUAL
                     UNION
                     SELECT 12,
                            'IF',
                            'Immobilisations financieres',
                            'RP',
                            'Ressources propres'
                       FROM DUAL
                     UNION
                     SELECT 13,
                            'PA',
                            'Participations',
                            'DMLT',
                            'Dmlt'
                       FROM DUAL
                     UNION
                     SELECT 14, 'CS3', 'Creances groupe', '', '' FROM DUAL
                     UNION
                     SELECT 15,
                            'BJ3',
                            'Actif Immobilise',
                            'RS',
                            'Ressources Stables'
                       FROM DUAL
                     UNION
                     SELECT 16,
                            'FR',
                            'Fonds de roulement',
                            'FR',
                            'Fonds de roulement'
                       FROM DUAL
                     UNION
                     SELECT 17,
                            'ST',
                            'Stocks',
                            'DX',
                            'Fournisseurs'
                       FROM DUAL
                     UNION
                     SELECT 18,
                            'CC',
                            'Creances clients',
                            'DY',
                            'Dettes fiscales'
                       FROM DUAL
                     UNION
                     SELECT 19,
                            'BZ3',
                            'Autres creances',
                            'EA',
                            'Autres dettes'
                       FROM DUAL
                     UNION
                     SELECT 20,
                            'CJ3',
                            'Actif Circulant',
                            'EC',
                            'Passif Circulant'
                       FROM DUAL
                     UNION
                     SELECT 21,
                            'CD3',
                            'Besoin en fonds de roulement',
                            'RFR',
                            'Ressource en fonds de roulement'
                       FROM DUAL
                     UNION
                     SELECT 22,
                            'TA',
                            'Tresorerie active',
                            'TP',
                            'Tresorerie passive'
                       FROM DUAL
                     UNION
                     SELECT 23,
                            'CD3',
                            'Vmp',
                            '?',
                            'Decouvert bancaire'
                       FROM DUAL
                     UNION
                     SELECT 19,
                            'CF3',
                            'Disponibilites',
                            'VZ',
                            'Dettes financieres CT'
                       FROM DUAL
                     UNION
                     SELECT 25, '', '', 'CC', 'Comptes courants' FROM DUAL
                     UNION
                     SELECT 26,
                            'TTN',
                            'Total tresorerie nette',
                            'TTN',
                            'Total tresorerie nette'
                       FROM DUAL
                     UNION
                     SELECT 27,
                            'CR',
                            'Comptes de regulation',
                            'EB',
                            'Compte de regulation'
                       FROM DUAL
                     UNION
                     SELECT 22,
                            'CO3',
                            'Total Actif',
                            'EE',
                            'Total passif'
                       FROM DUAL)
            SELECT LIB_ACIF,
                   1 ActifAnneeN,
                   1 ActifAnneeN1,
                   1 ActifAnneeN2,
                   1 ActifAnneeN3,
                   1 ActifAnneeN4,
                   LIB_PASSIF,
                   1 PassifAnneeN,
                   1 PassifAnneeN1,
                   1 PassifAnneeN2,
                   1 PassifAnneeN3,
                   1 PassifAnneeN4
              FROM LIB_BILAN;
    /*FROM TBATCH AA,TBADETDATA BB,TBADETDATA CC
    WHERE AA.TBAID=P_ACTID
    AND   CC.TBAID=AA.TBAID
    AND   CC.TBAID=AA.TBAID;*/

    END P_LIST_BILAN;

    PROCEDURE P_FLT_ANT4 (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient           dpracteur.actid%TYPE;
            row_count_coemp   NUMBER;
            P_tbaid           NUMBER;
            Ant_cont          NUMBER;
            CTX3_Count        NUMBER;
            Ant_hors_cont     NUMBER;
            P_count_tbaid     NUMBER;
        BEGIN
            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 0
            THEN
                O_VALEUR := 1;
                O_LIBVALEUR := '';
            ELSE
                SELECT actid
                  INTO nclient
                  FROM dpracteur
                 WHERE     dosid = P_DOSID
                       AND dprversion =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                       AND rolcode IN ('COEMPRU', 'COCLIEN');

                  SELECT COUNT (*)
                    INTO P_count_tbaid
                    FROM tbadetail td
                         INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
                   WHERE     tb.TBACODE = 'ANTERIO'
                         AND td.actid = nclient
                         AND ROWNUM = 1
                ORDER BY td.tbaid DESC;

                IF P_count_tbaid != 0
                THEN
                      SELECT td.tbaid
                        INTO P_tbaid
                        FROM tbadetail td
                             INNER JOIN tbatch tb ON tb.tbaid = td.tbaid
                       WHERE     tb.TBACODE = 'ANTERIO'
                             AND td.actid = nclient
                             AND ROWNUM = 1
                    ORDER BY td.tbaid DESC;

                    SELECT COUNT (*)
                      INTO Ant_cont
                      FROM tbadetdata
                     WHERE     tbaid = P_tbaid
                           AND tddcolumn = 'FILENAME'
                           AND tdddatastring = 'CTX3';

                    IF Ant_cont > 0
                    THEN
                        O_VALEUR := 2;
                        O_LIBVALEUR := '';
                    ELSE
                        SELECT COUNT (*)
                          INTO Ant_hors_cont
                          FROM tbadetdata
                         WHERE     tbaid = P_tbaid
                               AND tddcolumn = 'FILENAME'
                               AND tdddatastring IN ('CTX1',
                                                     'CTX2',
                                                     '6',
                                                     '4',
                                                     '3',
                                                     'DBT',
                                                     'IMP2',
                                                     'IMP3',
                                                     'MEM1',
                                                     'SUDT',
                                                     'MEM2',
                                                     'REFR');

                        IF Ant_hors_cont > 0
                        THEN
                            O_VALEUR := 3;
                            O_LIBVALEUR := '';
                        ELSE
                            O_VALEUR := 4;
                            O_LIBVALEUR := '';
                        END IF;
                    END IF;
                ELSE
                    O_VALEUR := 4;
                    O_LIBVALEUR := '';
                END IF;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_FLT_ANT4;

    PROCEDURE P_DEP_CONC (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            ncoempru   dpracteur.actid%TYPE;
        BEGIN
            SELECT actid
              INTO ncoempru
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

              SELECT NVL (SUBSTR (ad.ADRCODEPOST, 1, 2), 'NODATA')
                INTO O_VALEUR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGFACTURATION = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = ncoempru
            ORDER BY adr.AADORDRE DESC;


              SELECT NVL (ad.adrville, 'NODATA')
                INTO O_LIBVALEUR
                FROM ACTADRESSE adr
                     LEFT JOIN ADRESSE ad ON ad.adrid = adr.adrid
               WHERE     adr.AADFLAGFACTURATION = 1
                     AND adr.AADDTREMPLACE IS NULL
                     AND ROWNUM = 1
                     AND adr.actid = ncoempru
            ORDER BY adr.AADORDRE DESC;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                O_VALEUR := 'NODATA';
                O_LIBVALEUR := 'NODATA';
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_DEP_CONC;

    PROCEDURE P_MQFINANCED (P_ACTID               NUMBER,
                            P_DOSID               NUMBER,
                            P_DPRVERSION          VARCHAR2,
                            P_EXERCICE            NUMBER,
                            P_TEFCLASSE           VARCHAR2,
                            P_LANCODE             VARCHAR2,
                            P_ANAID               NUMBER,
                            O_VALEUR       IN OUT VARCHAR2,
                            O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';


        SELECT ROUND (MAX (PFADOUBLE))
          INTO O_VALEUR
          FROM DPRPROPFINANCE DPF, PFIATTRIBUT PFA
         WHERE     DPF.DOSID = P_DOSID
               AND DPF.DPRVERSION = P_DPRVERSION
               AND DPFFLAGRETENUE = 1
               AND DPF.PFIID = PFA.PFIID
               AND PFACODE = 'FINANCEDVALUE';
    EXCEPTION
        WHEN OTHERS
        THEN
            O_VALEUR := 0;
    END P_MQFINANCED;

    PROCEDURE P_APPORTMT (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            mtapport   propositionfinanciere.PFIMTDOWNPAYMENT%TYPE;
            pfi_id     NUMBER;
        BEGIN
            SELECT pfiid
              INTO pfi_id
              FROM DPRPROPFINANCE
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID);

            SELECT NVL (PFIMTDOWNPAYMENT, 0)
              INTO mtapport
              FROM propositionfinanciere
             WHERE pfiid = pfi_id;

            IF NVL (mtapport, 0) > 0
            THEN
                O_VALEUR := mtapport;
            ELSE
                O_VALEUR := 0;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_APPORTMT;

    PROCEDURE P_RELACOEMP (P_ACTID               NUMBER,
                           P_DOSID               NUMBER,
                           P_DPRVERSION          VARCHAR2,
                           P_EXERCICE            NUMBER,
                           P_TEFCLASSE           VARCHAR2,
                           P_LANCODE             VARCHAR2,
                           P_ANAID               NUMBER,
                           O_VALEUR       IN OUT VARCHAR2,
                           O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient           dpracteur.actid%TYPE;
            ncoempru          dpracteur.actid%TYPE;
            row_count         NUMBER;
            row_count_coemp   NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 1
            THEN
                SELECT actid
                  INTO ncoempru
                  FROM dpracteur
                 WHERE     dosid = P_DOSID
                       AND dprversion =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                       AND rolcode IN ('COEMPRU', 'COCLIEN');
            ELSE
                ncoempru := NULL;
            END IF;



            SELECT COUNT (*)
              INTO row_count
              FROM actrelation
             WHERE    (actid = nclient AND actidrelation = ncoempru)
                   OR     (actid = ncoempru AND actidrelation = nclient)
                      AND trecode IN ('CONB', 'MARIE');

            IF row_count = 0
            THEN
                O_VALEUR := NULL;
                O_LIBVALEUR := 'Aucun coemprunteur';
            ELSE
                SELECT tre.TRELIBELLEASC
                  INTO O_VALEUR
                  FROM actrelation act, lantrelation tre
                 WHERE    (actid = nclient AND actidrelation = ncoempru)
                       OR     (actid = ncoempru AND actidrelation = nclient)
                          AND tre.trecode IN ('CONB', 'MARIE')
                          AND act.trecode = tre.trecode
                          AND tre.lancode = P_LANCODE
                          AND AREDTFIN = NULL
                          AND ROWNUM = 1;
            END IF;

            IF O_VALEUR > 100
            THEN
                O_VALEUR := 100;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_RELACOEMP;

    PROCEDURE P_REVENUES (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient           dpracteur.actid%TYPE;
            ncoempru          dpracteur.actid%TYPE;
            row_count         NUMBER;
            row_count_coemp   NUMBER;
            P_Revenu_Emp      NUMBER;
            P_Revenu_CoEmp    NUMBER;
            P_Mnt_Bien        NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 1
            THEN
                SELECT actid
                  INTO ncoempru
                  FROM dpracteur
                 WHERE     dosid = P_DOSID
                       AND dprversion =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                       AND rolcode IN ('COEMPRU', 'COCLIEN');
            ELSE
                ncoempru := NULL;
            END IF;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTWAGEINCOME, 0)
                                      + NVL (AFDMTREALESTATEINCOME, 0)
                                      + NVL (AFDMTALIMONYINCOME, 0)
                                      + NVL (AFDMTOTHERINCOME, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_Emp
                FROM apafinancialdata
               WHERE actid = nclient AND ROWNUM = 1
            ORDER BY AFDORDER DESC;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTWAGEINCOME, 0)
                                      + NVL (AFDMTREALESTATEINCOME, 0)
                                      + NVL (AFDMTALIMONYINCOME, 0)
                                      + NVL (AFDMTOTHERINCOME, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_CoEmp
                FROM apafinancialdata
               WHERE actid = ncoempru AND ROWNUM = 1
            ORDER BY AFDORDER DESC;



            O_VALEUR := NVL (P_Revenu_Emp + P_Revenu_CoEmp, 0);
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_REVENUES;

    PROCEDURE P_CHARGES (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            nclient           dpracteur.actid%TYPE;
            ncoempru          dpracteur.actid%TYPE;
            row_count         NUMBER;
            row_count_coemp   NUMBER;
            P_Revenu_Emp      NUMBER;
            P_Revenu_CoEmp    NUMBER;
            P_Mnt_Bien        NUMBER;
        BEGIN
            SELECT actid
              INTO nclient
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('CLIENT', 'EMPRUNT');

            SELECT COUNT (*)
              INTO row_count_coemp
              FROM dpracteur
             WHERE     dosid = P_DOSID
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                   AND rolcode IN ('COEMPRU', 'COCLIEN');

            IF row_count_coemp = 1
            THEN
                SELECT actid
                  INTO ncoempru
                  FROM dpracteur
                 WHERE     dosid = P_DOSID
                       AND dprversion =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
                       AND rolcode IN ('COEMPRU', 'COCLIEN');
            ELSE
                ncoempru := NULL;
            END IF;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTRENTPAID, 0)
                                      + NVL (AFDMTLOANPAID, 0)
                                      + NVL (AFDMTALIMONYPAID, 0)
                                      + NVL (AFDMTOTHERPAID, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_Emp
                FROM apafinancialdata
               WHERE actid = nclient AND ROWNUM = 1
            ORDER BY AFDORDER DESC;

              SELECT NVL (
                         SUM (
                             NVL (
                                 NULLIF (
                                     (  NVL (AFDMTRENTPAID, 0)
                                      + NVL (AFDMTLOANPAID, 0)
                                      + NVL (AFDMTALIMONYPAID, 0)
                                      + NVL (AFDMTOTHERPAID, 0)),
                                     0),
                                 1)),
                         1)
                INTO P_Revenu_CoEmp
                FROM apafinancialdata
               WHERE actid = ncoempru AND ROWNUM = 1
            ORDER BY AFDORDER DESC;



            O_VALEUR := NVL (P_Revenu_Emp + P_Revenu_CoEmp, 0);
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
        END;
    END P_CHARGES;

    PROCEDURE P_DEALTYPE (P_ACTID               NUMBER,
                          P_DOSID               NUMBER,
                          P_DPRVERSION          VARCHAR2,
                          P_EXERCICE            NUMBER,
                          P_TEFCLASSE           VARCHAR2,
                          P_LANCODE             VARCHAR2,
                          P_ANAID               NUMBER,
                          O_VALEUR       IN OUT VARCHAR2,
                          O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        DECLARE
            ntype   DPRCOMPLEMENT.DCODEALTYPE%TYPE;
        BEGIN
            SELECT DCODEALTYPE
              INTO ntype
              FROM DPRCOMPLEMENT DPR
             WHERE DPR.DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION;

            SELECT TTPLIBELLE
              INTO O_VALEUR
              FROM LANTTRPARAM
             WHERE     TTRNOM = 'DEALTYPE'
                   AND TTPCODE = ntype
                   AND LANCODE = P_LANCODE;
        END;
    END P_DEALTYPE;

    PROCEDURE P_ALERT (P_DOSID               DOSSIERPROSPECT.DOSID%TYPE,
                       P_DPRVERSION          DOSSIERPROSPECT.DPRVERSION%TYPE,
                       P_ACTID               DPRACTEUR.ACTID%TYPE,
                       P_LANCODE             LANGUE.LANCODE%TYPE,
                       PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TO_CHAR (ROWNUM)
                       AS Sequence,
                   RATVALUE || '-' || RATRETURNMSG || '-' || RATCONCLUSION
                       AS Alerte
              FROM lkanarat lk, analysis ana
             WHERE     ana.anaid = lk.anaid
                   AND dosid = P_DOSID
                   AND RATDISPLAYSTYLE = 'ALERT'
                   AND ana.anaid =
                       (SELECT MAX (anaid)
                          FROM analysis
                         WHERE     dosid = P_DOSID
                               AND anmid IN (SELECT anmid
                                               FROM analysismatrix
                                              WHERE anmcode IN ('PARTLOA',
                                                                'PARTCRVN',
                                                                'PARTCRVO',
                                                                'PARTLLD',
                                                                'PARTPPER',
                                                                'ENTRBABIC',
                                                                'ENTRIS',
                                                                'PARTPPER',
                                                                'PARTPPER')))
                   AND ratid NOT IN
                           (SELECT ratid
                              FROM ratio
                             WHERE ratdescription IN
                                       ('CONTROL_GRILLE',
                                        'RISK_GRILLE',
                                        'ACCEPT_GRILLE'));
    END P_ALERT;

    PROCEDURE P_GRILLE1 (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT RATRETURNMSG
          INTO O_VALEUR
          FROM lkanarat lk, analysis ana
         WHERE     ana.anaid = lk.anaid
               AND dosid = P_DOSID
               AND ana.anaid =
                   (SELECT MAX (anaid)
                      FROM analysis
                     WHERE     dosid = P_DOSID
                           AND anmid IN (SELECT anmid
                                           FROM analysismatrix
                                          WHERE anmcode IN ('PARTLOA',
                                                            'PARTCRVN',
                                                            'PARTCRVO',
                                                            'PARTLLD',
                                                            'PARTPPER',
                                                            'ENTRBABIC',
                                                            'ENTRIS',
                                                            'PARTPPER',
                                                            'PARTPPER')))
               AND ratid = (SELECT ratid
                              FROM ratio
                             WHERE ratdescription = 'CONTROL_GRILLE');
    END P_GRILLE1;

    PROCEDURE P_GRILLE2 (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT RATRETURNMSG
          INTO O_VALEUR
          FROM lkanarat lk, analysis ana
         WHERE     ana.anaid = lk.anaid
               AND dosid = P_DOSID
               AND ana.anaid =
                   (SELECT MAX (anaid)
                      FROM analysis
                     WHERE     dosid = P_DOSID
                           AND anmid IN (SELECT anmid
                                           FROM analysismatrix
                                          WHERE anmcode IN ('PARTLOA',
                                                            'PARTCRVN',
                                                            'PARTCRVO',
                                                            'PARTLLD',
                                                            'PARTPPER',
                                                            'ENTRBABIC',
                                                            'ENTRIS',
                                                            'PARTPPER',
                                                            'PARTPPER')))
               AND ratid = (SELECT ratid
                              FROM ratio
                             WHERE ratdescription = 'RISK_GRILLE');
    END P_GRILLE2;

    ---MTR 12/07/2018
    PROCEDURE P_CTRL_TLTOTALPCTACT (p_actid       IN     NUMBER,
                                    p_dosid       IN     NUMBER,
                                    p_exercice    IN     NUMBER,
                                    p_tefclasse   IN     VARCHAR2,
                                    P_ANAID       IN     NUMBER,
                                    o_valeur      IN     VARCHAR2,
                                    o_retour      IN OUT NUMBER,
                                    o_msgretour   IN OUT VARCHAR2)
    IS
        l_valeur   VARCHAR2 (50);
    BEGIN
        l_valeur := o_valeur;
        O_RETOUR := 0;
        O_MSGRETOUR := 'Pas de controle';

        IF l_valeur IS NOT NULL
        THEN
            BEGIN
                IF (l_valeur <> '100' AND l_valeur <> '0')
                THEN
                    o_msgretour :=
                        'Le total repartition capital doit etre egal a 100.';
                    o_retour := '2';  -----MTR 05/09/2018 : Controle d�clench�
                END IF;
            EXCEPTION
                WHEN OTHERS
                THEN
                    o_msgretour := '';
                    o_retour := '2';
            END;
        END IF;
    END;

    ---MTR 12/07/2018
    PROCEDURE P_GET_ATTRETOUR (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               P_ANAID               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_valeur := '-';
        o_libvaleur := 'Attente retour Score';
    END;

    ---MTR 02/05/2018 maj o_retour
    PROCEDURE P_CTRL_TLMTCAPITAL (p_actid       IN     NUMBER,
                                  p_dosid       IN     NUMBER,
                                  p_exercice    IN     NUMBER,
                                  p_tefclasse   IN     VARCHAR2,
                                  P_ANAID       IN     NUMBER,
                                  o_valeur      IN     VARCHAR2,
                                  o_retour      IN OUT NUMBER,
                                  o_msgretour   IN OUT VARCHAR2)
    IS
        l_valeur    VARCHAR2 (50);
        l_cjucode   VARCHAR2 (50);
    BEGIN
        l_valeur := o_valeur;
        o_msgretour := '';
        o_retour := 0;

        IF l_valeur IS NULL
        THEN
            BEGIN
                SELECT cjucode
                  INTO l_cjucode
                  FROM acteur
                 WHERE actid = p_actid;


                IF (l_cjucode <> 1000 AND NVL (l_valeur, '0') = '0')
                THEN
                    o_msgretour :=
                        'Le montant du capital doit etre superieur a zero';

                    o_retour := 2;
                END IF;
            EXCEPTION
                WHEN OTHERS
                THEN
                    o_msgretour := '';
                    o_retour := 2;
            END;
        END IF;
    END P_CTRL_TLMTCAPITAL;

    PROCEDURE P_GRILLE3 (P_ACTID               NUMBER,
                         P_DOSID               NUMBER,
                         P_DPRVERSION          VARCHAR2,
                         P_EXERCICE            NUMBER,
                         P_TEFCLASSE           VARCHAR2,
                         P_LANCODE             VARCHAR2,
                         P_ANAID               NUMBER,
                         O_VALEUR       IN OUT VARCHAR2,
                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        SELECT RATRETURNMSG
          INTO O_VALEUR
          FROM lkanarat lk, analysis ana
         WHERE     ana.anaid = lk.anaid
               AND dosid = P_DOSID
               AND ana.anaid =
                   (SELECT MAX (anaid)
                      FROM analysis
                     WHERE     dosid = P_DOSID
                           AND anmid IN (SELECT anmid
                                           FROM analysismatrix
                                          WHERE anmcode IN ('PARTLOA',
                                                            'PARTCRVN',
                                                            'PARTCRVO',
                                                            'PARTLLD',
                                                            'PARTPPER',
                                                            'ENTRBABIC',
                                                            'ENTRIS',
                                                            'PARTPPER',
                                                            'PARTPPER')))
               AND ratid = (SELECT ratid
                              FROM ratio
                             WHERE ratdescription = 'ACCEPT_GRILLE');
    END P_GRILLE3;

    ----MTR 12/07/2018
    PROCEDURE P_WATCHLIST (p_actid               NUMBER,
                           p_dosid               NUMBER,
                           p_dprversion          VARCHAR2,
                           p_exercice            NUMBER,
                           p_tefclasse           VARCHAR2,
                           p_lancode      IN     VARCHAR2,
                           P_ANAID               NUMBER,
                           o_valeur       IN OUT VARCHAR2,
                           o_libvaleur    IN OUT VARCHAR2)
    IS
        l_atvordre   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT MAX (atvordre)
              INTO l_atvordre
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 11 AND atvdtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := '';
        END;

        IF l_atvordre > 0
        THEN
            -- il existe une cotation alors la comparer :
            SELECT tvacode
              INTO o_valeur
              FROM acttcovaleur
             WHERE actid = p_actid AND tcoid = 11 AND atvordre = l_atvordre;
        ELSE
            o_valeur := '';
            o_libvaleur := 'Non renseigne';
        END IF;
    END P_WATCHLIST;

    ----------MTR 12/07/2018
    PROCEDURE p_set_antetablfin (i_actid       IN     NUMBER,
                                 i_dosid       IN     NUMBER,
                                 i_exercice    IN     NUMBER,
                                 i_tefclasse   IN     VARCHAR2,
                                 P_ANAID              NUMBER,
                                 i_valeur      IN     VARCHAR2,
                                 o_retour         OUT VARCHAR2)
    IS
        l_count   NUMBER;
    BEGIN
        o_retour := '0';

        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM actstatistique
             WHERE actid = i_actid AND saccode = 'ANTETABLFIN';

            IF l_count > 0
            THEN
                UPDATE actstatistique
                   SET astlibre = i_valeur
                 WHERE actid = i_actid AND saccode = 'ANTETABLFIN';
            ELSE
                INSERT INTO actstatistique (actid, saccode, astlibre)
                     VALUES (i_actid, 'ANTETABLFIN', i_valeur);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_retour := 1;
        END;
    END;

    ------MTR 12/07/2018
    PROCEDURE p_set_clientTF (i_actid       IN     NUMBER,
                              i_dosid       IN     NUMBER,
                              i_exercice    IN     NUMBER,
                              i_tefclasse   IN     VARCHAR2,
                              P_ANAID              NUMBER,
                              i_valeur      IN     VARCHAR2,
                              o_retour         OUT VARCHAR2)
    IS
        l_count   NUMBER;
    BEGIN
        o_retour := '0';

        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM actstatistique
             WHERE actid = i_actid AND saccode = 'CLIENTTF';

            IF l_count > 0
            THEN
                UPDATE actstatistique
                   SET astlibre = i_valeur
                 WHERE actid = i_actid AND saccode = 'CLIENTTF';
            ELSE
                INSERT INTO actstatistique (actid, saccode, astlibre)
                     VALUES (i_actid, 'CLIENTTF', i_valeur);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_retour := 1;
        END;
    END;

    -------------MTR 12/07/2018

    PROCEDURE p_set_CLASSE_RISQ_COMM (i_actid       IN     NUMBER,
                                      i_dosid       IN     NUMBER,
                                      i_exercice    IN     NUMBER,
                                      i_tefclasse   IN     VARCHAR2,
                                      P_ANAID              NUMBER,
                                      i_valeur      IN     VARCHAR2,
                                      o_retour         OUT VARCHAR2)
    IS
        l_atvordre   NUMBER;
        l_tvacode    VARCHAR2 (15);
    BEGIN
        -- si derniere cotation est egale a la nouvelle alors pas d'update
        -- sinon, insert la nouvelle et mettre fin a la derniere.

        BEGIN
            BEGIN
                SELECT NVL (MAX (atvordre), 0)
                  INTO l_atvordre
                  FROM acttcovaleur
                 WHERE actid = i_actid AND tcoid = 9 AND atvdtfin IS NULL;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_atvordre := 0;
            END;

            IF l_atvordre > 0
            THEN
                -- il existe une cotation alors la comparer :
                SELECT tvacode
                  INTO l_tvacode
                  FROM acttcovaleur
                 WHERE     actid = i_actid
                       AND tcoid = 9
                       AND atvordre = l_atvordre;

                IF l_tvacode <> i_valeur
                THEN
                    -- insert nouvelle valeur et fermeture ancienne.
                    INSERT INTO acttcovaleur (actid,
                                              atvordre,
                                              tcoid,
                                              tvacode,
                                              atvdtdeb)
                         VALUES (i_actid,
                                 l_atvordre + 1,
                                 9,
                                 i_valeur,
                                 SYSDATE);

                    UPDATE acttcovaleur
                       SET atvdtfin = SYSDATE
                     WHERE     actid = i_actid
                           AND tcoid = 9
                           AND atvordre = l_atvordre;
                END IF;
            ELSE
                -- insert :
                INSERT INTO acttcovaleur (actid,
                                          atvordre,
                                          tcoid,
                                          tvacode,
                                          atvdtdeb)
                     VALUES (i_actid,
                             l_atvordre + 1,
                             9,
                             i_valeur,
                             SYSDATE);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        o_retour := '0';
    END;

    ---------MTR 12/07/2018
    PROCEDURE p_set_CLASSE_RISQUE (i_actid       IN     NUMBER,
                                   i_dosid       IN     NUMBER,
                                   i_exercice    IN     NUMBER,
                                   i_tefclasse   IN     VARCHAR2,
                                   P_ANAID              NUMBER,
                                   i_valeur      IN     VARCHAR2,
                                   o_retour         OUT VARCHAR2)
    IS
        l_atvordre   NUMBER;
        l_tvacode    VARCHAR2 (15);
    BEGIN
        -- si derniere cotation est egale a la nouvelle alors pas d'update
        -- sinon, insert la nouvelle et mettre fin a la derniere.

        BEGIN
            BEGIN
                SELECT NVL (MAX (atvordre), 0)
                  INTO l_atvordre
                  FROM acttcovaleur
                 WHERE actid = i_actid AND tcoid = 10 AND atvdtfin IS NULL;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_atvordre := 0;
            END;

            IF l_atvordre > 0
            THEN
                -- il existe une cotation alors la comparer :
                SELECT tvacode
                  INTO l_tvacode
                  FROM acttcovaleur
                 WHERE     actid = i_actid
                       AND tcoid = 10
                       AND atvordre = l_atvordre;

                IF l_tvacode <> i_valeur
                THEN
                    -- insert nouvelle valeur et fermeture ancienne.
                    INSERT INTO acttcovaleur (actid,
                                              atvordre,
                                              tcoid,
                                              tvacode,
                                              atvdtdeb)
                         VALUES (i_actid,
                                 l_atvordre + 1,
                                 10,
                                 i_valeur,
                                 SYSDATE);

                    UPDATE acttcovaleur
                       SET atvdtfin = SYSDATE
                     WHERE     actid = i_actid
                           AND tcoid = 10
                           AND atvordre = l_atvordre;
                END IF;
            ELSE
                -- insert :
                INSERT INTO acttcovaleur (actid,
                                          atvordre,
                                          tcoid,
                                          tvacode,
                                          atvdtdeb)
                     VALUES (i_actid,
                             l_atvordre + 1,
                             10,
                             i_valeur,
                             SYSDATE);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        o_retour := '0';
    END;

    --------------MTR 12/07/2018
    PROCEDURE p_set_CLASSE_RISQ_BCT (i_actid       IN     NUMBER,
                                     i_dosid       IN     NUMBER,
                                     i_exercice    IN     NUMBER,
                                     i_tefclasse   IN     VARCHAR2,
                                     P_ANAID              NUMBER,
                                     i_valeur      IN     VARCHAR2,
                                     o_retour         OUT VARCHAR2)
    IS
        l_atvordre   NUMBER;
        l_tvacode    VARCHAR2 (15);
    BEGIN
        -- si derniere cotation est egale a la nouvelle alors pas d'update
        -- sinon, insert la nouvelle et mettre fin a la derniere.

        BEGIN
            BEGIN
                SELECT NVL (MAX (atvordre), 0)
                  INTO l_atvordre
                  FROM acttcovaleur
                 WHERE actid = i_actid AND tcoid = 8 AND atvdtfin IS NULL;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_atvordre := 0;
            END;

            IF l_atvordre > 0
            THEN
                -- il existe une cotation alors la comparer :
                SELECT tvacode
                  INTO l_tvacode
                  FROM acttcovaleur
                 WHERE     actid = i_actid
                       AND tcoid = 8
                       AND atvordre = l_atvordre;

                IF l_tvacode <> i_valeur
                THEN
                    -- insert nouvelle valeur et fermeture ancienne.
                    INSERT INTO acttcovaleur (actid,
                                              atvordre,
                                              tcoid,
                                              tvacode,
                                              atvdtdeb)
                         VALUES (i_actid,
                                 l_atvordre + 1,
                                 8,
                                 i_valeur,
                                 SYSDATE);

                    UPDATE acttcovaleur
                       SET atvdtfin = SYSDATE
                     WHERE     actid = i_actid
                           AND tcoid = 8
                           AND atvordre = l_atvordre;
                END IF;
            ELSE
                -- insert :
                INSERT INTO acttcovaleur (actid,
                                          atvordre,
                                          tcoid,
                                          tvacode,
                                          atvdtdeb)
                     VALUES (i_actid,
                             l_atvordre + 1,
                             8,
                             i_valeur,
                             SYSDATE);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        o_retour := '0';
    END;

    /*
     PROCEDURE P_LIST_HAS_AUTO (p_lancode          VARCHAR2,
                                  p_actid            NUMBER,
                                  pc_list     IN OUT t_cursor)
       IS
          l_actortype   VARCHAR2 (7);
       BEGIN
          BEGIN
             SELECT NVL (MAX (ACTTYPE), 'PM')
               INTO l_actortype
               FROM ACTEUR
              WHERE actid = p_actid;

             IF (l_actortype = 'PM')
             THEN
                OPEN pc_list FOR SELECT 'NA' AS code, 'N/A' AS libelle FROM DUAL;
             ELSE
                OPEN pc_list FOR
                   SELECT tupcode AS code, tuplibelle AS libelle
                     FROM lantusparam
                    WHERE tusnom = 'OUINONNA' AND lancode = p_lancode AND TUPCODE<>'NA';
             END IF;
          EXCEPTION
             WHEN OTHERS
             THEN
                OPEN pc_list FOR
                   SELECT 'NA' AS code,
                          'Erreur : Acteur inexistant dans AVACTEUR' AS libelle
                     FROM DUAL;
          END;
       END P_LIST_HAS_AUTO;
    */
    PROCEDURE P_LIST_FICP (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 37 AND lancode = p_lancode;
    END P_LIST_FICP;



    PROCEDURE P_LIST_TYPECLIENT (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tclcode AS Code_valeur, tcllibelle AS libelle
                           FROM lantclientele
                          WHERE     tclcode IN ('MGRCPT',
                                                'MPARTI',
                                                'MPMEMI',
                                                'MPROLI',
                                                'MARTCO')
                                AND lancode = p_lancode;
    END P_LIST_TYPECLIENT;

    /*  PROCEDURE P_LIST_PRESLIASSE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)---MBN 07112017  lanavvaleurparam ??
      IS
      BEGIN
         OPEN pc_list FOR
            SELECT avpcode AS code, avplibelle AS libelle
              FROM lanavvaleurparam
             WHERE atpcode = 'PRESLIASSE' AND lancode = p_lancode;
      END P_LIST_PRESLIASSE;*/

    PROCEDURE P_LIST_ANTETABLFIN (p_lancode          VARCHAR2,
                                  pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR
            SELECT tupcode AS Code_valeur, TUPLIBELLE AS libelle
              FROM lantusparam
             WHERE tusnom = 'ANTETABLFIN' AND lancode = p_lancode;
    END P_LIST_ANTETABLFIN;

    PROCEDURE P_LIST_ANTBANQUE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'ANTBANQUE' AND lancode = p_lancode;
    END P_LIST_ANTBANQUE;

    PROCEDURE P_LIST_FIBEN (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 37 AND lancode = p_lancode;
    END P_LIST_FIBEN;

    PROCEDURE P_LIST_TYPECTTRAVAIL (p_lancode          VARCHAR2,
                                    pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR
            SELECT TUPCODE AS code_valeur, TUplibelle AS libelle
              FROM LANTUSPARAM
             WHERE TUSNOM = 'TYPECTTRAVAIL' AND lancode = p_lancode;
    END P_LIST_TYPECTTRAVAIL;

    PROCEDURE P_LIST_STATUT (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'QUALIT' AND lancode = p_lancode;
    END P_LIST_STATUT;

    PROCEDURE P_LIST_CLASSE_RISQ_BCT (p_lancode          VARCHAR2,
                                      pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 8 AND lancode = p_lancode;
    END P_LIST_CLASSE_RISQ_BCT;

    PROCEDURE P_LIST_OUINON (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'OUINON' AND lancode = p_lancode;
    END P_LIST_OUINON;

     PROCEDURE P_LIST_SCORECALIBRE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'SCORCAL' AND lancode = p_lancode;
    END P_LIST_SCORECALIBRE;

     PROCEDURE P_LIST_SOGEACT (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'SOGEACT' AND lancode = p_lancode;
    END P_LIST_SOGEACT;

    PROCEDURE P_LIST_SOGEPROF (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'SOGEPROF' AND lancode = p_lancode;
    END P_LIST_SOGEPROF;

    PROCEDURE P_LIST_SOGERELATION (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'SOGERELATION' AND lancode = p_lancode;
    END P_LIST_SOGERELATION;



    PROCEDURE P_LIST_HAS_AUTO (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'OUINONNA' AND lancode = p_lancode;
    END P_LIST_HAS_AUTO;

    PROCEDURE P_LIST_CLASSE_RISQ_COMM (p_lancode          VARCHAR2,
                                       pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 9 AND lancode = p_lancode;
    END P_LIST_CLASSE_RISQ_COMM;

    PROCEDURE P_LIST_TYPECONCENTRA (p_lancode          VARCHAR2,
                                    pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR
            SELECT tupcode AS Code_valeur, tuplibelle AS libelle
              FROM lantusparam
             WHERE tusnom = 'TYPECONCENTRA' AND lancode = p_lancode;
    END P_LIST_TYPECONCENTRA;

    /*    PROCEDURE P_LIST_STATUTCC (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
      IS
      BEGIN
         OPEN pc_list FOR
            SELECT tupcode AS code, tuplibelle AS libelle
              FROM lantusparam
             WHERE tusnom = 'STATUTCC' AND lancode = p_lancode;
      END P_LIST_STATUTCC;*/

    PROCEDURE P_LIST_AVISSOURCE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'AVISOUR' AND lancode = p_lancode;
    END P_LIST_AVISSOURCE;

    PROCEDURE P_LIST_TYPESOURCE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'LISTSOUR' AND lancode = p_lancode;
    END P_LIST_TYPESOURCE;

    /*     PROCEDURE P_LIST_BANK (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
      IS
      BEGIN
         OPEN pc_list FOR
            SELECT bancode AS code , banlibelle AS libelle
              FROM banque;

      END P_LIST_BANK;*/


    PROCEDURE P_LIST_ACCESS (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'ACCESS' AND lancode = p_lancode;
    END P_LIST_ACCESS;

    /*   PROCEDURE P_LIST_USAGE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
      IS
      BEGIN
         OPEN pc_list FOR
            SELECT tupcode AS code, tuplibelle AS libelle
              FROM lantusparam
             WHERE tusnom = 'USAGE' AND lancode = p_lancode;
      END P_LIST_USAGE;*/

    PROCEDURE P_LIST_AGENCE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT ACTCODE AS Code_valeur, ACTNOM AS libelle
                           FROM ACTEUR
                          WHERE ACTID BETWEEN 20 AND 25 OR ACTID = 29;
    END P_LIST_AGENCE;

    /*   PROCEDURE P_LIST_MODEPAIMENET (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
       OPEN pc_list FOR
          SELECT tupcode AS code, tuplibelle AS libelle
            FROM lantusparam
           WHERE tusnom = 'MODAQUI' AND lancode = p_lancode;
       END P_LIST_MODEPAIMENET ;*/

    PROCEDURE P_LIST_TYPE (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'TYPEB' AND lancode = p_lancode;
    END P_LIST_TYPE;


    PROCEDURE P_LIST_STATUTCC (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'STATUTCC' AND lancode = p_lancode;
    END P_LIST_STATUTCC;

    /* PROCEDURE P_LIST_AVISSOURCE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
       OPEN pc_list FOR
          SELECT tupcode AS code, tuplibelle AS libelle
            FROM lantusparam
           WHERE tusnom = 'AVISOUR' AND lancode = p_lancode;
    END P_LIST_AVISSOURCE;*/

    PROCEDURE P_LIST_BANK (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR
            SELECT bancode AS Code_valeur, banlibelle AS libelle FROM banque;
    END P_LIST_BANK;

    /* PROCEDURE P_LIST_ACCESS (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
       OPEN pc_list FOR
          SELECT tupcode AS code, tuplibelle AS libelle
            FROM lantusparam
           WHERE tusnom = 'ACCESS' AND lancode = p_lancode;
    END P_LIST_ACCESS;*/

    PROCEDURE P_LIST_USAGE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'USAGE' AND lancode = p_lancode;
    END P_LIST_USAGE;

    /*   PROCEDURE P_LIST_AGENCE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
     IS
     BEGIN
        OPEN pc_list FOR
           SELECT ACTCODE AS code , ACTNOM AS libelle
             FROM ACTEUR
             WHERE ACTID BETWEEN 20 AND 25 OR ACTID=29;

     END P_LIST_AGENCE;*/

    PROCEDURE P_LIST_MODEPAIMENET (p_lancode          VARCHAR2,
                                   pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'MODAQUI' AND lancode = p_lancode;
    END P_LIST_MODEPAIMENET;

    /*    PROCEDURE P_LIST_TYPE (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)IS
      BEGIN
         OPEN pc_list FOR
            SELECT tupcode AS code, tuplibelle AS libelle
              FROM lantusparam
             WHERE tusnom='TYPEB' AND lancode = p_lancode;
      END P_LIST_TYPE;*/

    PROCEDURE P_LIST_CLASS (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'CLASS' AND lancode = p_lancode;
    END P_LIST_CLASS;

    PROCEDURE P_LIST_ETAT (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'ETATB' AND lancode = p_lancode;
    END P_LIST_ETAT;

    PROCEDURE P_LIST_OCCUPY (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'OCCUPIED' AND lancode = p_lancode;
    END P_LIST_OCCUPY;

    PROCEDURE P_LIST_AQUISITION (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'MODAQUI' AND lancode = p_lancode;
    END P_LIST_AQUISITION;

    PROCEDURE P_LIST_PERIODICITE (p_lancode          VARCHAR2,
                                  pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'PERIOD' AND lancode = p_lancode;
    END P_LIST_PERIODICITE;

    /*   PROCEDURE P_LIST_PERIODICITE (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
       OPEN pc_list FOR
          SELECT tupcode AS code, tuplibelle AS libelle
            FROM lantusparam
           WHERE tusnom = 'PERIOD' AND lancode = p_lancode;
    END P_LIST_PERIODICITE;*/

    PROCEDURE P_LIST_PAYS (P_LANCODE VARCHAR2, PC_LIST IN OUT T_CURSOR)
    IS
    BEGIN
        OPEN pc_list FOR SELECT paycode AS Code_valeur, paylibelle AS libelle
                           FROM lanpays
                          WHERE lancode = 'FR';
    END P_LIST_PAYS;

    PROCEDURE P_LIST_STATUTMATERIEL (p_lancode          VARCHAR2,
                                     pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tupcode AS Code_valeur, tuplibelle AS libelle
                           FROM lantusparam
                          WHERE tusnom = 'STATUTMA' AND lancode = p_lancode;
    END P_LIST_STATUTMATERIEL;

    PROCEDURE P_LIST_WATCHLIST (p_lancode VARCHAR2, pc_list IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 11 AND lancode = p_lancode;
    END P_LIST_WATCHLIST;

    ---------MTR 10/2/07/2018

    PROCEDURE p_set_watchlist (i_actid       IN     NUMBER,
                               i_dosid       IN     NUMBER,
                               i_exercice    IN     NUMBER,
                               i_tefclasse   IN     VARCHAR2,
                               i_valeur      IN     VARCHAR2,
                               o_retour         OUT VARCHAR2)
    IS
        l_atvordre   NUMBER;
        l_tvacode    VARCHAR2 (15);
    BEGIN
        -- si derniere cotation est egale a la nouvelle alors pas d'update
        -- sinon, insert la nouvelle et mettre fin a la derniere.

        BEGIN
            BEGIN
                SELECT NVL (MAX (atvordre), 0)
                  INTO l_atvordre
                  FROM acttcovaleur
                 WHERE actid = i_actid AND tcoid = 11 AND atvdtfin IS NULL;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_atvordre := 0;
            END;

            IF l_atvordre > 0
            THEN
                -- il existe une cotation alors la comparer :
                SELECT tvacode
                  INTO l_tvacode
                  FROM acttcovaleur
                 WHERE     actid = i_actid
                       AND tcoid = 11
                       AND atvordre = l_atvordre;

                IF l_tvacode <> i_valeur
                THEN
                    -- insert nouvelle valeur et fermeture ancienne.
                    INSERT INTO acttcovaleur (actid,
                                              atvordre,
                                              tcoid,
                                              tvacode,
                                              atvdtdeb)
                         VALUES (i_actid,
                                 l_atvordre + 1,
                                 11,
                                 i_valeur,
                                 SYSDATE);

                    UPDATE acttcovaleur
                       SET atvdtfin = SYSDATE
                     WHERE     actid = i_actid
                           AND tcoid = 11
                           AND atvordre = l_atvordre;
                END IF;
            ELSE
                -- insert :
                INSERT INTO acttcovaleur (actid,
                                          atvordre,
                                          tcoid,
                                          tvacode,
                                          atvdtdeb)
                     VALUES (i_actid,
                             l_atvordre + 1,
                             11,
                             i_valeur,
                             SYSDATE);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        o_retour := '0';
    END;

    PROCEDURE P_LIST_CLASSE_RISQUE (p_lancode          VARCHAR2,
                                    pc_list     IN OUT t_cursor)
    IS
    BEGIN
        OPEN pc_list FOR SELECT tvacode AS Code_valeur, tvalibelle AS libelle
                           FROM lantcovaleur
                          WHERE tcoid = 10 AND lancode = p_lancode;
    END P_LIST_CLASSE_RISQUE;



    PROCEDURE P_GET_TLCLITOTALMEF (p_actid               NUMBER,
                                   p_dosid               NUMBER,
                                   p_dprversion          VARCHAR2,
                                   p_exercice            NUMBER,
                                   p_tefclasse           VARCHAR2,
                                   p_lancode      IN     VARCHAR2,
                                   p_anaid               NUMBER,
                                   o_valeur       IN OUT VARCHAR2,
                                   o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.MEF_groupe / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_TLCLITOTALMEF;

    PROCEDURE P_GET_TLCLIMEFEC (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (C.MEF_ENCOURS_groupe / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_TLCLIMEFEC;

    PROCEDURE P_GET_TLCLICRDMAX (p_actid               NUMBER,
                                 p_dosid               NUMBER,
                                 p_dprversion          VARCHAR2,
                                 p_exercice            NUMBER,
                                 p_tefclasse           VARCHAR2,
                                 p_lancode      IN     VARCHAR2,
                                 p_anaid               NUMBER,
                                 o_valeur       IN OUT VARCHAR2,
                                 o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.CRDMAXGROUPE / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLICRDMAX;

    PROCEDURE P_GET_TLCLICRDACTUEL (p_actid               NUMBER,
                                    p_dosid               NUMBER,
                                    p_dprversion          VARCHAR2,
                                    p_exercice            NUMBER,
                                    p_tefclasse           VARCHAR2,
                                    p_lancode      IN     VARCHAR2,
                                    p_anaid               NUMBER,
                                    o_valeur       IN OUT VARCHAR2,
                                    o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.CRD_GROUPE / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLICRDACTUEL;

    PROCEDURE P_GET_TLCLIIMPAYE (p_actid               NUMBER,
                                 p_dosid               NUMBER,
                                 p_dprversion          VARCHAR2,
                                 p_exercice            NUMBER,
                                 p_tefclasse           VARCHAR2,
                                 p_lancode      IN     VARCHAR2,
                                 p_anaid               NUMBER,
                                 o_valeur       IN OUT VARCHAR2,
                                 o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.impayes_groupe / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLIIMPAYE;

    PROCEDURE P_GET_TLCLPCTLA (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.pourcentfinancementla, 0), 2)
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLPCTLA;

    PROCEDURE P_GET_TLCLIPCTLE (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.pourcentfinancementle, 0), 2)
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLIPCTLE;

    PROCEDURE P_GET_1LOYER (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (
                       pfi.pfimtpremierloyer * (100 + NVL (ttaval, 0)) / 100),
                      ROUND (pfimtpremierloyer * 100 / pfiinvestissement, 2)
                   || '%'
              INTO o_valeur, o_libvaleur
              FROM dprpropfinance         dpf,
                   propositionfinanciere  pfi,
                   dossierprospect        d,
                   taxtaux                tx
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND dpf.dosid = d.dosid
                   AND dpf.dprversion = d.dprversion
                   AND d.taxcode = tx.taxcode(+)
                   AND tx.ttadtfin IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_1LOYER;

    PROCEDURE p_get_taux (p_actid               NUMBER,
                          p_dosid               NUMBER,
                          p_dprversion          VARCHAR2,
                          p_exercice            NUMBER,
                          p_tefclasse           VARCHAR2,
                          p_lancode      IN     VARCHAR2,
                          p_anaid               NUMBER,
                          o_valeur       IN OUT VARCHAR2,
                          o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (pfi.pfrtotalnominal, 2)
              INTO o_valeur
              FROM dprpropfinance dpf, pfirubrique pfi
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND pfi.pfrordre = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END p_get_taux;

    PROCEDURE p_get_invest (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT ROUND (pfi.pfrmontant)
              INTO o_valeur
              FROM dprpropfinance dpf, pfirubrique pfi
             WHERE     dpf.dosid = p_dosid
                   AND dpf.dprversion = p_dprversion
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND pfi.pfrordre = 1;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END p_get_invest;

    PROCEDURE P_GET_TLCLIPCTLI (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (c.pourcentfinancementlI, 0), 2)
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLCLIPCTLI;

    PROCEDURE P_GET_TLLOYERGPE (p_actid               NUMBER,
                                p_dosid               NUMBER,
                                p_dprversion          VARCHAR2,
                                p_exercice            NUMBER,
                                p_tefclasse           VARCHAR2,
                                p_lancode      IN     VARCHAR2,
                                p_anaid               NUMBER,
                                o_valeur       IN OUT VARCHAR2,
                                o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (C.LOYER_TTC_GROUPE / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLLOYERGPE;

    ------------MTR 12/07/2018
    PROCEDURE P_GET_NOMGPE (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            /* SELECT NOM_GPE INTO  o_libvaleur
               FROM PERIGROUPE P , dossierexterne D
              WHERE p.tbaid=d.tbaid
              and d.dosid=p_dosid
           ;*/

            SELECT aggname
              INTO o_valeur
              FROM aggregate ag, actrelation ar
             WHERE     ar.actid = p_actid
                   AND ar.aggid = ag.aggid
                   AND ag.aggtype = 'REF'
                   AND AR.AREDTFIN IS NULL;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                BEGIN
                    SELECT MAX (aggname)
                      INTO o_valeur
                      FROM aggregate
                     WHERE actid = p_actid;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        o_libvaleur := 'Non renseigne';
                END;
            WHEN OTHERS
            THEN
                o_libvaleur := 'Non renseigne';
        END;

        IF o_valeur IS NOT NULL
        THEN
            o_libvaleur := '';
        END IF;
    END;

    -----------MTR 20/09/2018
    PROCEDURE P_GET_LIBTOTAL (p_actid               NUMBER,
                              p_dosid               NUMBER,
                              p_dprversion          VARCHAR2,
                              p_exercice            NUMBER,
                              p_tefclasse           VARCHAR2,
                              p_lancode             VARCHAR2,
                              p_anaid               NUMBER,
                              o_valeur       IN OUT VARCHAR2,
                              o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            o_valeur := 'TOTAL';
        EXCEPTION
            WHEN OTHERS
            THEN
                o_libvaleur := '';
        END;
    END;

    PROCEDURE P_GET_CLIENTTLDEPUIS (p_actid               NUMBER,
                                    p_dosid               NUMBER,
                                    p_dprversion          VARCHAR2,
                                    p_exercice            NUMBER,
                                    p_tefclasse           VARCHAR2,
                                    p_lancode             VARCHAR2,
                                    p_anaid               NUMBER,
                                    o_valeur       IN OUT VARCHAR2,
                                    o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT TO_CHAR (DTPREMIERCONTRAT, 'DD/MM/YYYY')
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
            WHEN OTHERS
            THEN
                o_valeur := '';
                DBMS_OUTPUT.put_line (
                    SUBSTR ('GET CLIENTTLDEPUIS ' || SQLERRM, 1, 250));
        END;
    END p_get_clientTLDEPUIS;



    PROCEDURE P_GET_NAF (p_actid               NUMBER,
                         p_dosid               NUMBER,
                         p_dprversion          VARCHAR2,
                         p_exercice            NUMBER,
                         p_tefclasse           VARCHAR2,
                         p_lancode      IN     VARCHAR2,
                         p_anaid               NUMBER,
                         o_valeur       IN OUT VARCHAR2,
                         o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT NAFCODE, NAFLIBELLE
              INTO o_valeur, o_libvaleur
              FROM LANNAF
             WHERE     NAFCODE = (SELECT nafcode
                                    FROM acteur
                                   WHERE ACTID = p_ACTID)
                   AND LANCODE = p_lancode;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';               --MTR 16/05/2018
        END;
    END P_GET_NAF;

    PROCEDURE P_GET_NOMSOURCE (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT cvastringvalue
              INTO o_libvaleur
              FROM cchvalue
             WHERE     cchsid = 'TLNOMSOURCE'
                   AND actid = p_actid
                   AND dosid = p_dosid
                   AND dprversion = p_dprversion;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_NOMSOURCE;

    PROCEDURE P_GET_SOURCE (p_actid               NUMBER,
                            p_dosid               NUMBER,
                            p_dprversion          VARCHAR2,
                            p_exercice            NUMBER,
                            p_tefclasse           VARCHAR2,
                            p_lancode      IN     VARCHAR2,
                            p_anaid               NUMBER,
                            o_valeur       IN OUT VARCHAR2,
                            o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        BEGIN
            o_libvaleur := '';

            SELECT c.cvastringvalue, t.tuplibelle
              INTO o_valeur, o_libvaleur
              FROM cchvalue c, lantusparam t
             WHERE     C.CCHSID = 'CMBCCHSID421'
                   AND C.actid = p_actid
                   AND t.tupcode = c.cvastringvalue
                   AND c.dosid = p_dosid
                   AND T.LANCODE = 'FR'
                   AND T.TUSNOM = 'SOURCEAFF';
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_SOURCE;

    PROCEDURE P_GET_TLENGAGPE (p_actid               NUMBER,
                               p_dosid               NUMBER,
                               p_dprversion          VARCHAR2,
                               p_exercice            NUMBER,
                               p_tefclasse           VARCHAR2,
                               p_lancode      IN     VARCHAR2,
                               p_anaid               NUMBER,
                               o_valeur       IN OUT VARCHAR2,
                               o_libvaleur    IN OUT VARCHAR2)
    IS
    BEGIN
        o_libvaleur := '';

        BEGIN
            SELECT ROUND (NVL (C.ENGAGEMENTGROUPE / 1000, 0))
              INTO o_valeur
              FROM clientcrm c, acteur a
             WHERE c.client_id = a.actcode AND a.actid = p_actid;
        EXCEPTION
            WHEN OTHERS
            THEN
                o_valeur := '';
                o_libvaleur := 'Non renseigne';
        END;
    END P_GET_TLENGAGPE;

--------------CHI---SGML
 PROCEDURE P_GET_ACTFAX (p_actid               NUMBER,
                                 p_dosid               NUMBER,
                                 p_dprversion          VARCHAR2,
                                 p_exercice            NUMBER,
                                 p_tefclasse           VARCHAR2,
                                 p_lancode      IN     VARCHAR2,
                                 p_anaid               NUMBER,
                                 o_valeur       IN OUT VARCHAR2,
                                 o_libvaleur    IN OUT VARCHAR2)
   IS
   BEGIN
      BEGIN
         o_libvaleur := '';


         SELECT ATENUM
           INTO o_valeur
           FROM ACTTELECOM
          WHERE     actid = p_actid
                AND ATETYPE = 'PRO'
                AND ATEORDRE = (SELECT MAX (ateordre)
                                  FROM acttelecom
                                 WHERE actid = p_actid AND atetype = 'PRO');
      EXCEPTION
         WHEN OTHERS
         THEN
            o_valeur := '';
            o_libvaleur := 'Non renseigne';
      END;
   END P_GET_ACTFAX;


    PROCEDURE P_LIST_PFIGUARANTEE (P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT (SELECT MAX (TGALIBELLE)
                      FROM LANTGARANTIE
                     WHERE     TGACODE = PFIGUARANTEE.TGACODE
                           AND LANCODE = P_LANCODE)
                       AS GARANTIE,
                   PFGMTGUARANTEE
                       AS MONTANT,
                   PFGDESCRIPTION
                       AS DESCRIPTION,
                   (SELECT MAX (ACTNOM)
                      FROM ACTEUR
                     WHERE ACTID = PFIGUARANTEE.ACTID)
                       AS GARANT
              FROM PFIGUARANTEE
             WHERE PFIID IN
                       (SELECT PFIID
                          FROM DPRPROPFINANCE
                         WHERE DOSID = P_DOSID AND DPRVERSION = P_DPRVERSION);
    EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_PFIGUARANTEE;

PROCEDURE P_LIST_GEOGPART (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_ACTID        IN  NUMBER,   
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
          SELECT (select actnom from acteur where actid=AR.actid) AS NOMAC,AR.AREQP AS MNTCAP from ACTEUR A,ACTRELATION AR
		  where A.actid=AR.actidrelation
		  and AR.actidrelation=P_ACTID
		  and ar.TRECODE='SHAREHOLDER';

          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_GEOGPART;

PROCEDURE P_LIST_ENGSGLM (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
								   P_ACTID               DPRACTEUR.ACTID%TYPE,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
               SELECT PAV4_JASPER_FO.F_GET_COUNT_CTR (ACTID) AS Nbrc ,
               PAV4_JASPER_FO.F_GET_MT_INV_ACT (ACTID) AS Mtcr ,
			   PAV4_JASPER_FO.F_GET_ENCOURS_ACTEUR_CTRACTIF (ACTID) AS Enc,
			   PAV4_JASPER_FO.F_GET_ENCOURS_ACTEUR_CTRENG(ACTID) AS Scdb,
			   PAV4_JASPER_FO.F_GET_RILIQ_ENV_ACT(ACTID) AS Renv,
			   PAV4_JASPER_FO.F_GET_IMPAYE_ACTEUR(ACTID,SYSDATE) AS Imp

			FROM ACTEUR WHERE ACTID=P_ACTID;
          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_ENGSGLM;




    -------------------HME ENGA AUTRE AFFAIRE DE GROUPE
 PROCEDURE P_LIST_ENGSGLMAUTAFFG (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
								   P_ACTID               DPRACTEUR.ACTID%TYPE,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR

	  select "Code Affaire GRP" AS Codc ,"Nom_Affaire_Groupe" AS Nomc ,"NB_CONTRAT_Affaire_Group" AS Nbrcon,"MT_CREDIT_AFFAIRE_GROUP" AS Mtcre,
      "ENCOURS_AFFAIRE_GROUP" AS Encr,"CTR_DEBLOC_AFFAIRE_GROUP" AS Scecdb,"RELIQ_ENV_AFFAIRE_GROUP" AS Relenv,"IMPAYE_AFFAIRE_GROUP" AS Impa
             from ENG_SGLM where id_client=P_ACTID;


      ---  FROM ACTEUR ,DOSSIER@SGM_LINK DOSLINK WHERE DOSALINK.actid=P_ACTID and rolcode='CLIENT' and DOSLINK.tpgcode='AUT02'
    -- and DOSALINK.DOSID=DOSLINK.dosid;

          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_ENGSGLMAUTAFFG; 
    ---------------------
   PROCEDURE P_LIST_AUT (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
								   P_ACTID               DPRACTEUR.ACTID%TYPE,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN

        OPEN PC_RETURN FOR

    select PAV4_JASPER_FO.F_GET_MT_AUT_BO (DOSLINK.dosid) AS Mtaut,F_PLGETDOSQPAVAILABLE@SGM_LINK(DOSLINK.dosid,'FR','ORFI') AS Mtdisp,
	TO_CHAR(PAV4_JASPER_FO.F_GET_DT_FIN_BO (DOSLINK.dosid))AS Dtfin
     FROM DOSACTEUR@SGM_LINK DOSALINK ,DOSSIER@SGM_LINK DOSLINK WHERE DOSALINK.actid=P_ACTID and rolcode='CLIENT' and DOSLINK.tpgcode='AUT02'
     and DOSALINK.DOSID=DOSLINK.dosid;


          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_AUT;
    ----------------
  PROCEDURE P_LIST_ATAUTO (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
								   P_ACTID               DPRACTEUR.ACTID%TYPE,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN


        OPEN PC_RETURN FOR

    select PAV4_JASPER_FO.F_GET_MT_AUT_BO (DOSLINK.dosid) AS Mtaut ,
	F_PLGETDOSQPAVAILABLE@SGM_LINK(DOSLINK.dosid,'FR','ORFI') AS Mtdisp,
	TO_CHAR(PAV4_JASPER_FO.F_GET_DT_FIN_BO (DOSLINK.dosid))AS Dtf

	 FROM DOSACTEUR@SGM_LINK DOSALINK ,DOSSIER@SGM_LINK DOSLINK WHERE DOSALINK.actid=P_ACTID and rolcode='CLIENT' and DOSLINK.tpgcode='AUT02'
     and DOSALINK.DOSID=DOSLINK.dosid;


          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_ATAUTO;
    ------------------

    PROCEDURE P_LISTLEASA (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
								   P_ACTID               DPRACTEUR.ACTID%TYPE,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN

        OPEN PC_RETURN FOR

			SELECT SUM(pfir.PECMTBASIC) AS Prl ,TO_CHAR(EXTRACT(YEAR from pfir.PECDTDUE)) AS An from dossierprospect dosp , dpracteur dprac ,dprpropfinance dprpop , pfirubecheancier pfir
		where dosp.dosid=dprac.dosid
		AND dprpop.dosid=dosp.dosid
		and dprpop.pfiid=pfir.pfiid
		and pfir.PECTYPEELEMENT='LOYER'
		and dprac.actid=P_ACTID
		and dosp.dosid =P_DOSID
		and dosp.dprversion=P_DPRVERSION
		and dprac.rolcode='CLIENT'
		GROUP BY EXTRACT(YEAR from pfir.PECDTDUE) ORDER by EXTRACT(YEAR from pfir.PECDTDUE);  

          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LISTLEASA;

---------P_Representant legaux				
PROCEDURE P_LIST_REPRLEG (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_ACTID        IN     NUMBER,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
          SELECT (select actnom from acteur where actid=ar.actidrelation) AS Nomdr,AR.AREQP AS Qp from ACTEUR A,ACTRELATION AR
		  where A.actid=AR.actid
		  and ar.actid=P_ACTID
		  and ar.TRECODE='DIRIGEANT';
          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_REPRLEG;					  
-------AUtres affaire du groupe 
PROCEDURE P_LIST_AUAFFGR (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_ACTID        IN     NUMBER,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR

          select ac.actnom AS Nom,ac.actcode AS Code  from actrelation acr ,acteur ac where acr.actid in (select dprac.actid from dpracteur dprac,acteur ac where dosid=P_DOSID and rolcode in ('CLIENT','GARANT')
          and ac.Actid=dprac.actid)
          AND acr.actidrelation=ac.actid
          AND acr.TRECODE IN ('DIRIGEANT','GROUPE','SHAREHOLDER');

          EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_AUAFFGR;	



PROCEDURE P_LIST_PROPOSITION (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR

                 SELECT PFRVR AS Vr ,PFRMTLOYER AS Loyht,pfinbperiodes AS Nbm

              FROM dprpropfinance dpf, pfirubrique pfi ,propositionfinanciere pro
             WHERE     dpf.dosid = P_DOSID
                   AND dpf.dprversion =P_DPRVERSION
                   AND dpf.dpfflagretenue = 1
                   AND dpf.pfiid = pfi.pfiid
                   AND pfi.pfiid=pro.pfiid
                   AND pfi.pfrordre = 1;

		EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_PROPOSITION;	

---------P_LIST_FRAICOMM
PROCEDURE P_LIST_FRAICOMM  (  P_DOSID        IN     NUMBER,
                                   P_DPRVERSION   IN     VARCHAR2,
                                   P_LANCODE      IN     VARCHAR2,
                                   PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR

              SELECT  LPRE.TPRLIBELLE AS Nomf ,ROUND((PFI.PFPMT*NVL(txt.TTAVAL,0)/100 +PFI.PFPMT),0) AS  Mtf
           FROM PFIPRESTATION PFI, DPRPROPFINANCE DPF, LANAVTPRESTATION LPRE ,taxtaux txt
             WHERE     PFI.PFIID = DPF.PFIID
                   AND DPF.DOSID = P_DOSID
                   AND DPF.DPRVERSION  =(SELECT DPRVERSION FROM V_DEAL WHERE  dosid= P_DOSID)
                   AND PFI.TPRCODE=LPRE.TPRCODE
                   AND pfi.taxcode=txt.taxcode(+);


		EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_FRAICOMM;



    PROCEDURE P_TOTAL_STRUCTURE_CAPITAL (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    AS
    BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := NULL;

        BEGIN
            SELECT SUM (TO_NUMBER (REPLACE (agrvalue, '.', ',')))
              INTO O_VALEUR
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'STRUCAPITAL')
                   AND anaid = P_ANAID
                   AND agrgcocode = 'Detention';
        EXCEPTION
            WHEN OTHERS
            THEN
                O_VALEUR := 'N/A';
        END;
    END P_TOTAL_STRUCTURE_CAPITAL;

 ------------
   PROCEDURE P_LIST_OBJET_FINANCE (P_DOSID        IN     NUMBER,
                                    P_DPRVERSION   IN     VARCHAR2,
                                    P_LANCODE      IN     VARCHAR2,
                                    PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT dpm.dpmlibelle
                       AS Description,
                  (SELECT a.actnom
                    FROM lkarodpm lka, acteur a
                     WHERE     dpm.dosid = lka.dosid
                           AND dpm.dprversion = LKA.dprversion
                           AND dpm.dpmordre = lka.dpmordre
                           AND lka.rolcode = 'FOURN'
                           AND a.Actid = lka.actid)
                       AS Founisseur

              FROM DPRMATERIEL            dpm 

             WHERE     dpm.DOSID = P_DOSID
                   AND dpm.DPRVERSION = P_DPRVERSION ;

    EXCEPTION
        WHEN OTHERS
        THEN
            OPEN PC_RETURN FOR SELECT NULL
                                 FROM DUAL;
    END P_LIST_OBJET_FINANCE;

------RBE

PROCEDURE P_GET_CONTENTIEUX(
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
  VAL2    NUMBER;
  RES VARCHAR2(100);
  RESL VARCHAR2(100);
  ncount  NUMBER;
  nactid NUMBER;

     BEGIN
      BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
 -- RESL := '';

 --Select CCH vale
         SELECT count(*) INTO ncount

          FROM cchvalue@SGM_LINK cva,customcharacteristic@SGM_LINK cch
           WHERE cch.cchsid = 'CMBCCHSID219'
                               AND cva.cchsid = cch.cchsid
                               AND cva.actid = p_actid;

              select actid into nactid from dpracteur where dosid=p_dosid
            and rolcode='CLIENT'
            and DPRVERSION =pa_avcommun.f_derniereversiondossier (p_dosid);


		IF (ncount>0) THEN 

		   SELECT  NVL(MAX(CVASTRINGVALUE), 0) INTO VAL1
           FROM cchvalue@SGM_LINK cva,customcharacteristic@SGM_LINK cch
           WHERE cch.cchsid = 'CMBCCHSID219'
                               AND cva.cchsid = cch.cchsid
                               AND cva.actid = p_actid;

               ELSIF (nactid IS NOT NULL )
            THEN                 
              select  NVL(MAX(CVASTRINGVALUE), 0) 
			 INTO VAL2 from cchvalue where actid =nactid
                  and CCHSID='TFDCCHSID70903';  

		  IF ( VAL1='CTX') OR  ( VAL2 ='5' ) THEN 
				RES:='OUI';

			ELSE
				RES:='NON';

		  End If;
		ELSE 
		  RES:='NON';--old value  =  OUI
		END IF;  
   --  END IF; 
			o_valeur:= RES;

   END;
 END P_GET_CONTENTIEUX;



 PROCEDURE P_GET_DOUTEUX(
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
      nDosid    VARCHAR2(100);
      nactid    VARCHAR2(100);
       VAL1    VARCHAR2(100);
      RES VARCHAR2(100);
      RESL VARCHAR2(100);
      ncount  NUMBER;

     BEGIN
      BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
 -- RESL := '';

 --Select CCH vale


     SELECT actid 
         INTO nactid
         FROM dpracteur
         WHERE     dosid = P_DOSID
         AND dprversion = PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
          AND rolcode IN ('CLIENT');

        SELECT count(*) INTO ncount

          FROM cchvalue@SGM_LINK cva,customcharacteristic@SGM_LINK cch
           WHERE cch.cchsid = 'TFDCCHSID70903' --'CMBCCHSID219'
                               AND cva.cchsid = cch.cchsid
                               AND cva.actid = p_actid;

		IF (ncount>0) THEN 

		   SELECT NVL(CVASTRINGVALUE,0) INTO VAL1
           FROM cchvalue@SGM_LINK cva,customcharacteristic@SGM_LINK cch
           WHERE cch.cchsid = 'TFDCCHSID70903' ---'CMBCCHSID219'
                               AND cva.cchsid = cch.cchsid
                               AND cva.actid = p_actid;

		  IF ( VAL1 ='4' ) --( VAL1 ='DTX' OR VAL1='PREDTX') 
          THEN 
				RES:='OUI';

			ELSE
				RES:='NON';

		  End If;
		ELSE 
		  RES:='NON';-- old value = OUI
		END IF;  
			o_valeur:= RES;


   END;     

 END P_GET_DOUTEUX;


    PROCEDURE P_GET_RAPPCTB(
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

      ncount  NUMBER;
      VAL1    NUMBER;
      RES VARCHAR2(100);    
     nactid    VARCHAR2(100);

     BEGIN
      BEGIN
    O_LIBVALEUR := '';
	O_VALEUR := '';
	RES := '';


	/*	SELECT actid 
         INTO nactid
         FROM dpracteur
         WHERE    dosid = P_DOSID
         AND dprversion = PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (P_DOSID)
          AND rolcode IN ('CLIENT');
		  
		 
		     SELECT NVL(CVASTRINGVALUE,0) 
			  INTO  VAL1
			  FROM cchvalue 
			  WHERE cchsid = 'TFDCCHSID5005' 
			   --AND anaid  = (select anaid from analysis where dosid=p_dosid and ANMID=5) ;
				AND actid = nactid;*/

          SELECT count(agrvalue)
              INTO ncount
              FROM aragrid
              WHERE     ratid IN (SELECT ratid
              FROM ratio
              WHERE ratcode = 'INFNEG')
             AND anaid  = (select anaid from analysis where dosid=P_DOSID and ANMID=5) ;

	   IF (ncount > 0 )  --OR  (VAL1 ='03' ) 
		THEN    

  		RES:='OUI';

			ELSE
				RES:='NON';

		 -- End If;
	--	ELSE 
		 -- RES:='NON';-- old value = OUI
		END IF;  
			o_valeur:= RES;


   END;     
 END P_GET_RAPPCTB;


    PROCEDURE P_GET_DATECREA(
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

      nactid  NUMBER;
      l_taccode    VARCHAR2(50);
      DCADT     DATE;
      RES     VARCHAR2(100);


     BEGIN
      BEGIN
    O_LIBVALEUR := '';
	O_VALEUR := '';
	RES := '';


                   select taccode 
                   INTO l_taccode from dossierprospect 
                   where dosid=p_dosid      
                   and DPRVERSION =pa_avcommun.f_derniereversiondossier (p_dosid);

                   select actid 
                   INTO  nactid from dpracteur where dosid=p_dosid 
                   and rolcode='CLIENT'
                  and DPRVERSION =pa_avcommun.f_derniereversiondossier (p_dosid);

                  IF (nactid IS NOT NULL ) THEN 
                      select actdtimmatriculation 
                      INTO DCADT  from acteur 
                      where actid = nactid;

	   IF ((l_taccode ='CBM') AND (sysdate-DCADT > 360)) OR 
		    ((l_taccode ='CBI') AND (sysdate-DCADT > 720)) 
        THEN 
				RES:='OUI';

			ELSE
				RES:='NON';

		  End If;
		ELSE 
		  RES:='NON';
		END IF;  
			o_valeur:= RES;


   END;     

 END P_GET_DATECREA;


  PROCEDURE P_GET_CA(
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
  nDosid    VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
--
 --Select CCH vale
select max (dosid) into nDosid from dpracteur where rolcode='CLIENT' and actid=p_actid ;


     select CVANUMERICVALUE into o_valeur from cchvalue where DOSIDPROSPECT=nDosid and  cchsid='TFDCCHSID91' ;

   END;
 END P_GET_CA;  

    PROCEDURE P_GET_ANCACTIV(
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
  RES VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

 --Select anc en activité
   SELECT (NVL (MONTHS_BETWEEN (SYSDATE, ACTDTIMMATRICULATION) / 12, 0))
        --SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (ACTDTIMMATRICULATION, 'YYYY')
          INTO VAL1
          FROM ACTEUR
         WHERE ACTID = P_ACTID;
 --Activité produit
 SELECT TACCODE into VAL2 from dossierprospect where dosid=p_dosid;

 -- 
  IF (((VAL1>1) AND (VAL2='CBM')) OR ((VAL1>2) AND (VAL2='CBI'))) THEN RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_GET_ANCACTIV;

  PROCEDURE P_GET_DUREESOG(
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
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

 --Select durée
   SELECT PFINBPERIODES
        --SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (ACTDTIMMATRICULATION, 'YYYY')
          INTO VAL1
          FROM propositionfinanciere
         WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1);

 -- --PTANBPERIODMIN PTANBPERIODMAX
    select PTANBPERIODMIN
    INTO VAL2
    from pvetable where pelid in 
    (select pelid from pcrpel where pcrid in (SELECT PCRID  FROM propositionfinanciere
         WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1))
    and pelid in (select pelid from pricingelement where PELFRONTFILTER='PAYMENTPROFILE'));

 -- -- PTANBPERIODMAX
  select PTANBPERIODMAX
    INTO VAL3
    from pvetable where pelid in 
    (select pelid from pcrpel where pcrid in (SELECT PCRID  FROM propositionfinanciere
         WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1)) 
    and pelid in (select pelid from pricingelement where PELFRONTFILTER='PAYMENTPROFILE'));


  IF ((VAL1>= VAL2) AND (VAL1 <= VAL3)) THEN RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_GET_DUREESOG;
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
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

 --Select premier loyer HT
 /*  SELECT nvl(PFIMTPREMIERLOYER,0) 
        --SELECT TO_CHAR (SYSDATE, 'YYYY') - TO_CHAR (ACTDTIMMATRICULATION, 'YYYY')
          INTO VAL1
          FROM propositionfinanciere
         WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1);*/

  --Select premier loyer TTC        
select max((nvl(PFIMTPREMIERLOYER,0) * txt.TTAVAL/100)+PFIMTPREMIERLOYER)
INTO VAL1
from propositionfinanciere prop , dossierprospect dospr, taxtaux txt
where txt.TAXCODE=dospr.taxcode
and dospr.dosid=p_dosid
and prop.PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1); 

 -- -- mnt investissement TTC 
       SELECT max((nvl(PFIINVESTISSEMENT,0) * txt.TTAVAL/100)+PFIINVESTISSEMENT) INTO VAL2
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

---HME CHECK MATERIEL EXCLU OU NON
 PROCEDURE P_GET_EXCLU(
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

  VAL1    NUMBER;
  RES VARCHAR2(100);
  NBEURMT  VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
  NBEURMT  := '';

 --Select Ceckbox exclu
 SELECT max (nvl(CVABOOLEANVALUE,0))
          INTO  VAL1
          FROM cchvalue cva,customcharacteristic cch
           WHERE cch.cchvaluecode = 'CHK452'
                               AND cva.cchsid = cch.cchsid
                               AND cva.dosidprospect = p_dosid ;

 -- IF Exclu then 
  IF VAL1=1 THEN 
   RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_GET_EXCLU;


  PROCEDURE P_GET_OBSERVATION(
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

 l_ANCIEN    VARCHAR2(100);
 l_DUREM  NUMBER;
 l_taccode VARCHAR2(100);
 NNAPCODE VARCHAR2(100);
 l_QTFIN  VARCHAR2(100);
  l_exclu  Number;
 l_ANFIN  VARCHAR2(100);
 l_SECTAC  VARCHAR2(100);
 l_CAFF   VARCHAR2(100);
 l_MTCREDIT NUMBER;
  RES VARCHAR2(100);


  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';


select taccode INTO l_taccode from dossierprospect where dosid=p_dosid and DPRVERSION =pa_avcommun.f_derniereversiondossier (p_dosid);
SELECT max (NAPCODE)INTO NNAPCODE FROM dprmateriel WHERE    DOSID = p_dosid  AND DPRVERSION = pa_avcommun.f_derniereversiondossier (p_dosid);
SELECT nvl(MAX(ratvalue),0) INTO l_ANCIEN FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71008;  
SELECT NVL (SUM (NVL ( ROUND (DPMMTINVEST), 0)), 0) INTO l_MTCREDIT  FROM DPRMATERIEL WHERE DOSID = p_dosid AND DPRVERSION = pa_avcommun.f_derniereversiondossier (p_dosid);
SELECT nvl(MAX(ratvalue),0) INTO l_DUREM FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=5) AND ratid=1008;   
SELECT nvl(MAX(ratvalue),0) INTO l_QTFIN FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71010;   
--SELECT nvl(MAX(ratvalue),0) INTO l_exclu FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71012;   
SELECT  NVL(MAX(CVABOOLEANVALUE), 0) INTO  l_exclu FROM cchvalue cva,customcharacteristic cch WHERE cch.cchvaluecode = 'CHK452'AND cva.cchsid = cch.cchsid AND cva.dosidprospect = p_dosid ;
SELECT nvl(MAX(ratvalue),0) INTO l_ANFIN FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71014;   
SELECT nvl(MAX(ratvalue),0) INTO l_SECTAC FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71025;   
--SELECT nvl(MAX(ratvalue),0) INTO l_CAFF FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71016;   
SELECT  NVL(MAX(CVABOOLEANVALUE), 0) INTO l_CAFF from cchvalue where DOSIDPROSPECT=p_dosid and  cchsid='TFDCCHSID91' ;




IF ((l_taccode='CBM')   AND ( NNAPCODE in ('1201','1401','1402','1403','1404','1405','1406','1407','1408','1409','3101','3102','3103','3104','3105'))
 AND (l_ANCIEN='OUI') AND (l_MTCREDIT >=70000 ) AND (l_DUREM BETWEEN 36 and 60) AND (l_QTFIN='OUI') AND (l_exclu=1)
 AND (l_ANFIN='OUI') AND ((l_SECTAC='11' and to_number(l_CAFF)>=1000000) OR l_SECTAC <>'11' ) )
 THEN RES:='NON';


ELSIF ( (l_taccode='CBM') AND (NNAPCODE in ('1601','1603','1604','2103'))
 AND (l_ANCIEN='OUI') AND (l_MTCREDIT >=70000) AND (l_DUREM BETWEEN 36 and 60) AND (l_QTFIN='OUI')  AND (l_exclu=1) 
 AND (l_ANFIN='OUI') AND ((l_SECTAC='11' and to_number(l_CAFF)>=1000000) OR l_SECTAC <>'11' ) )
 THEN RES:='NON' ;

ELSIF ( (l_taccode='CBM') AND ( NNAPCODE in ('1202','1203','1300','1301','1302','1303','1304','1305','1306','1307','1308','1309','1310','1311','1312','1313','1314','1315','1316','1317','1318','1319','1320','1501','1502','1602','1701','1702','2101','2102','2104','3201','3202','3301'))
AND (l_ANCIEN='OUI') AND (l_MTCREDIT >=70000) AND (l_DUREM BETWEEN 36 and 60) AND (l_QTFIN='OUI') AND (l_exclu=1)
 AND (l_ANFIN='OUI')  AND  ((l_SECTAC='11' and to_number(l_CAFF)>=1000000) OR l_SECTAC <>'11' ) )
 THEN RES:='NON' ;


ELSIF ( (l_taccode='CBM') AND (NNAPCODE in ('2183','2184','4101','4102','4201','4202','5101','5102','5103'))
AND (l_ANCIEN='OUI') AND (l_MTCREDIT >=70000) AND (l_DUREM BETWEEN 30 and 48) AND (l_QTFIN='OUI') AND (l_exclu=1)
 AND (l_ANFIN='OUI')  AND ((l_SECTAC='11' and to_number(l_CAFF)>=1000000) OR l_SECTAC <>'11' ) )
 THEN RES:='NON' ;

ELSIF ( (l_taccode='CBI') AND (NNAPCODE in ('6000','6001','8001','8002','8003'))
AND (l_ANCIEN='OUI') AND (l_MTCREDIT >=600000) AND (l_DUREM BETWEEN 96 and 144) AND (l_QTFIN='OUI') AND (l_exclu=1)
 AND (l_ANFIN='OUI') AND ((l_SECTAC='11' and to_number(l_CAFF)>=1000000) OR l_SECTAC <>'11' ) )
 THEN RES:='NON' ;


ELSE RES:='OUI';
END IF;

    o_valeur:= RES;
   END;
 END P_GET_OBSERVATION;


  PROCEDURE P_GET_RATIOCAL(
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
  NBEURMT  VARCHAR2(100);
  NBEURMT2  VARCHAR2(100);
  NBEURMT3  VARCHAR2(100);
  RES VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
  NBEURMT:= '';
  NBEURMT2:= '';
  NBEURMT3:= '';

 --71015
 SELECT COUNT(ratvalue)
            into NBEURMT 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71015; 
        IF NBEURMT>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL1 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71015;   
        ELSE VAL1:=0;
        END IF;
  --71016
 SELECT
        COUNT(ratvalue)
            into NBEURMT2 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71016; 
        IF NBEURMT2>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL2 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71016;   
        ELSE VAL2:=0;
        END IF;

 --71017
  SELECT
        COUNT(ratvalue)
            into NBEURMT3 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017; 
        IF NBEURMT3>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL3 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017;   
        ELSE VAL3:=0;
        END IF;

  IF VAL1>0  THEN RES:= VAL1/VAL3;
  ELSIF VAL2>0 THEN RES:= VAL2/VAL3;
    ELSE RES:=0;
  End If;
    o_valeur:= RES;
   END;
 END P_GET_RATIOCAL;
 PROCEDURE P_GET_CRCB(
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
  NBEURMT2  VARCHAR2(100);
  NBEURMT3  VARCHAR2(100);
  RES VARCHAR2(100);
  RESL VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
  RESL := '';
  NBEURMT2:= '';
  NBEURMT3:= '';

  --71016
 SELECT
        COUNT(ratvalue)
            into NBEURMT2 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71016; 
        IF NBEURMT2>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL1 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71016;   
        ELSE VAL2:=0;
        END IF;

 --71017
  SELECT
        COUNT(ratvalue)
            into NBEURMT3 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017; 
        IF NBEURMT3>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL2 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017;   
        ELSE VAL2:=0;
        END IF;

 -- select round(to_number(VAL2)/to_number(VAL1)) into RES from dual;
    select (VAL2/VAL1) into RES from dual;

  IF (RES*100) <10 THEN RESL:= 'CR < 10%';
  ELSE RESL:= 'CR > 10%';
  End If;
  o_valeur:=RES;
  O_LIBVALEUR :=RESL;
   END;
 END P_GET_CRCB;

  PROCEDURE P_GET_CRCF(
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
  NBEURMT  VARCHAR2(100);
  NBEURMT3  VARCHAR2(100);
  RES VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';
  NBEURMT:= '';
  NBEURMT3:= '';

 --71015
 SELECT COUNT(ratvalue)
            into NBEURMT 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71015; 
        IF NBEURMT>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL1 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71015;   
        ELSE VAL1:=0;
        END IF;

 --71017
  SELECT
        COUNT(ratvalue)
            into NBEURMT3 
                FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017; 
        IF NBEURMT3>0 THEN 
            SELECT nvl(ratvalue,0) INTO VAL2 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71017;   
        ELSE VAL2:=0;
        END IF;
  --VAL3 Activité
  SELECT nvl(MAX(ratvalue),0) INTO VAL3 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71025;   

  --select round( to_number(VAL2)/to_number(VAL1)) into o_valeur from dual;
    select (VAL2/VAL1) into o_valeur from dual;

  --Professions médicales : CR ≤ 28%
  IF VAL3='1' and (o_valeur*100)<28 THEN O_LIBVALEUR:= 'CR < 28%';
  ELSIF  VAL3='1' and (o_valeur*100)>28 THEN O_LIBVALEUR:= 'CR > 28%';
  --Professions paramédicales : CR ≤ 20%
  ELSIF VAL3='2' and (o_valeur*100)<20 THEN O_LIBVALEUR:= 'CR < 20%';
  ELSIF VAL3='2' and (o_valeur*100)>20 THEN O_LIBVALEUR:= 'CR > 20%';
  --Pharmaciens : CR ≤ 16%
    ELSIF VAL3='3' and (o_valeur*100)<16 THEN O_LIBVALEUR:= 'CR < 16%';
  ELSIF VAL3='3' and (o_valeur*100)>16 THEN O_LIBVALEUR:= 'CR > 16%';
  --Professions libérales, juridiques et fiscales (hors avocats et notaires) : CR ≤ 16%
    ELSIF VAL3='4' and (o_valeur*100)<16 THEN O_LIBVALEUR:= 'CR < 16%';
  ELSIF VAL3='4' and (o_valeur*100)>16 THEN O_LIBVALEUR:= 'CR > 16%';
  --Activités d’assurance et d’intermédiation + avocats : CR ≤ 6%
    ELSIF VAL3='5' and (o_valeur*100)<6 THEN O_LIBVALEUR:= 'CR < 6%';
  ELSIF VAL3='5' and (o_valeur*100)>6 THEN O_LIBVALEUR:= 'CR > 6%';
--Autres professions libérales : CR ≤ 8%
  ELSIF VAL3='6' and (o_valeur*100)<8 THEN O_LIBVALEUR:= 'CR < 8%';
  ELSIF VAL3='6' and (o_valeur*100)>8 THEN O_LIBVALEUR:= 'CR > 8%';
--Artisans : CR ≤ 6%
  ELSIF VAL3='7' and (o_valeur*100)<6 THEN O_LIBVALEUR:= 'CR < 6%';
  ELSIF VAL3='7' and (o_valeur*100)>6 THEN O_LIBVALEUR:= 'CR > 6%';
--Commerce de gros : CR ≤ 4.5%
  ELSIF VAL3='8' and (o_valeur*100)<4.5 THEN O_LIBVALEUR:= 'CR < 4,5%';
  ELSIF VAL3='8' and (o_valeur*100)>4.5 THEN O_LIBVALEUR:= 'CR > 4,5%';
--Petit commerce de détail: CR ≤ 4.5%
  ELSIF VAL3='9' and (o_valeur*100)<4.5 THEN O_LIBVALEUR:= 'CR < 4,5%';
  ELSIF VAL3='9' and (o_valeur*100)>4.5 THEN O_LIBVALEUR:= 'CR > 4,5%';
--Restauration: CR ≤ 6%
  ELSIF VAL3='10' and (o_valeur*100)<6 THEN O_LIBVALEUR:= 'CR < 6%';
  ELSIF VAL3='10' and (o_valeur*100)>6 THEN O_LIBVALEUR:= 'CR > 6%';

  ELSE O_LIBVALEUR:= 'ND%';
  End If;
   END;
 END P_GET_CRCF;
  PROCEDURE P_GET_DECISIONSOGE(
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
  VAL4    VARCHAR2(100);
  VAL5    VARCHAR2(100);
  VAL6    VARCHAR2(100);
  VAL7    VARCHAR2(100);
  VAL8    VARCHAR2(100);
  VAL9    VARCHAR2(100);
  VAL10    VARCHAR2(100);
  VAL11   VARCHAR2(100);
  VAL12   VARCHAR2(100);
  VAL13   VARCHAR2(100);
  VAL14   VARCHAR2(100);
  VAL15   VARCHAR2(100);
  RES   VARCHAR2(100);

  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

SELECT nvl(MAX(ratvalue),0) INTO VAL1 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71001;   
SELECT nvl(MAX(ratvalue),0) INTO VAL2 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71002;   
SELECT nvl(MAX(ratvalue),0) INTO VAL3 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71003;   
SELECT nvl(MAX(ratvalue),0) INTO VAL4 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71004;   
SELECT nvl(MAX(ratvalue),0) INTO VAL5 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71005;   
SELECT nvl(MAX(ratvalue),0) INTO VAL6 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71006;   
SELECT nvl(MAX(ratvalue),0) INTO VAL7 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71007;   
SELECT nvl(MAX(ratvalue),0) INTO VAL8 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71008;   
SELECT nvl(MAX(ratvalue),0) INTO VAL9 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71009;   
SELECT nvl(MAX(ratvalue),0) INTO VAL10 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71010;   
SELECT nvl(MAX(ratvalue),0) INTO VAL11 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71011;   
SELECT nvl(MAX(ratvalue),0) INTO VAL12 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71012;   
SELECT nvl(MAX(ratvalue),0) INTO VAL13 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71013;   
SELECT nvl(MAX(ratvalue),0) INTO VAL14 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71014;
SELECT nvl(MAX(ratvalue),0) INTO VAL15 FROM lkanarat WHERE anaid=(SELECT Max(anaid) FROM analysis WHERE dosid=p_dosid AND anmid=7000) AND ratid=71019;


IF (VAL1='NON' AND VAL2='NON' AND VAL3='NON' AND VAL4='NON' AND VAL5='NON' AND VAL6='NON' AND VAL7='NON' AND VAL11='NON' AND VAL13='NON' AND VAL14='NON'
AND VAL8='OUI' AND VAL9='OUI' AND VAL10='OUI' AND VAL12='OUI' AND VAL15='OUI') THEN RES:='SCORE VERT' ;
ELSIF (VAL1='OUI' OR VAL2='OUI' OR VAL3='OUI' OR VAL4='OUI' OR VAL5='OUI' OR VAL6='OUI' OR VAL7='OUI' OR VAL14='OUI') THEN RES:='SCORE ROUGE' ;
ELSIF ( VAL8='NON' OR VAL9='NON' OR VAL10='NON' OR VAL12='NON' OR VAL11='OUI' OR VAL13='OUI' OR VAL15='NON') THEN  RES:='SCORE ORANGE' ;
ELSE RES:='ND';
END IF;

    o_valeur:= RES;
   END;
 END P_GET_DECISIONSOGE;

FUNCTION F_NAP_OK (P_DOSID NUMBER)
    RETURN VARCHAR2
IS
    SRESULT    VARCHAR2(10):=null;
    SNAPCODE   VARCHAR2(10):=null;



BEGIN
FOR C IN (SELECT DISTINCT DPMORDRE FROM DPRMATERIEL WHERE DOSID=P_DOSID)
        LOOP
                  SELECT NAPCODE
                    INTO SNAPCODE
                    FROM DPRMATERIEL
                    WHERE DOSID = P_DOSID
                        AND DPRVERSION = (select DPRVERSION FROM V_DEAL where dosid= P_dosid)
                        AND DPMORDRE= C.DPMORDRE ;



                            IF (SNAPCODE  in ('1201','1401','1402','1403','1404','1405','1406','1407',
                                                '1408','1409','3101','3102','3103','3104','3105')) THEN
                                    SRESULT:='TRUE';
                                ELSE
                                    SRESULT:='FALSE';
                                END IF;
    EXIT WHEN     SRESULT='FALSE';



        END LOOP;    
RETURN SRESULT;



END F_NAP_OK;


PROCEDURE P_GET_GARSOGE (p_actid               NUMBER,
                                  p_dosid               NUMBER,
                                  p_dprversion          VARCHAR2,
                                  p_exercice            NUMBER,
                                  p_tefclasse           VARCHAR2,
                                  p_lancode      IN     VARCHAR2,
                                  p_anaid               NUMBER,
                                  o_valeur       IN OUT VARCHAR2,
                                  o_libvaleur    IN OUT VARCHAR2)
    IS



        l_valeur      VARCHAR2 (2000);
        NCOUNT  NUMBER;
        RES VARCHAR2(100);
        o_libval  VARCHAR2(100);
        sRESULT  VARCHAR2(100);



    BEGIN
        BEGIN
            o_libvaleur := '';



        select F_NAP_OK (P_dosid)
             into sRESULT
             FROM DUAL;




        SELECT COUNT (*)
          INTO NCOUNT
          FROM PFIGUARANTEE
          WHERE PFIID in (select max(PFIID) from dprpropfinance where dosid=p_dosid and DPFFLAGRETENUE=1)
          and TGACODE='CCG';



        IF NCOUNT>0 
            THEN  RES:='OUI'; 

            ELSIF ( NCOUNT=0 AND sRESULT='TRUE') 
                THEN  RES:='OUI';

            ELSIF ( NCOUNT=0 AND sRESULT='FALSE') 
                THEN RES:='NON';
            ELSE 
                    RES :='NON';
        END IF;





          o_valeur:=RES;
      END;
 END P_GET_GARSOGE;


PROCEDURE P_TOTAL_IMPAYE (P_ACTID               NUMBER,
                                         P_DOSID               NUMBER,
                                         P_DPRVERSION          VARCHAR2,
                                         P_EXERCICE            NUMBER,
                                         P_TEFCLASSE           VARCHAR2,
                                         P_LANCODE             VARCHAR2,
                                         P_ANAID               NUMBER,
                                         O_VALEUR       IN OUT VARCHAR2,
                                         O_LIBVALEUR    IN OUT VARCHAR2)
    IS
    O_VALEUR1   NUMBER;
    O_VALEUR2   NUMBER;
    O_VALEUR3   NUMBER;
    O_VALEUR4    NUMBER;
    VAL   NUMBER;
    RES VARCHAR2(100);

    BEGIN
     BEGIN
        O_LIBVALEUR := '';
        O_VALEUR := '';
        VAL := 0;
        O_VALEUR1 := 0;
        O_VALEUR2 := 0;
        O_VALEUR3 := 0;
        O_VALEUR4 := 0; 

            --Ratid=5000
            SELECT nvl(SUM (TO_NUMBER (REPLACE (agrvalue, '.', ','))),0)
              INTO O_VALEUR1
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'R5000')
                   --AND anaid = P_ANAID
                   --Récupérer depuis la dernière version d'un autre DE dont le anmid=5
                   AND anaid = (select max(anaid) from analysis where dosid=P_DOSID and ANMID=5)
                   AND agrgcocode = 'Impa';
             dbms_output.put_line(O_VALEUR1);
            --Ratid=5002
            SELECT nvl(SUM (TO_NUMBER (REPLACE (agrvalue, '.', ','))),0)
              INTO O_VALEUR2
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'R5002')
                   --AND anaid = P_ANAID
                   --Récupérer depuis la dernière version d'un autre DE dont le anmid=5
                   AND anaid = (select max(anaid) from analysis where dosid=P_DOSID and ANMID=5)
                   AND agrgcocode = 'Impa';
                   dbms_output.put_line(O_VALEUR2);   
            --Ratid=6000
           SELECT nvl(SUM (TO_NUMBER (REPLACE (agrvalue, '.', ','))),0)
              INTO O_VALEUR3
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'ENGALEAS')

                   AND anaid = (select max(anaid) from analysis where dosid=P_DOSID and ANMID=5)
                   AND agrgcocode = 'Imp';
                                dbms_output.put_line(O_VALEUR3);   

            --Ratid=6001
            SELECT nvl(SUM (TO_NUMBER (REPLACE (agrvalue, '.', ','))),0)
              INTO O_VALEUR4
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'ENGALEASAG')
                   --AND anaid = P_ANAID
                   --Récupérer depuis la dernière version d'un autre DE dont le anmid=5
                   AND anaid = (select max(anaid) from analysis where dosid=P_DOSID and ANMID=5)
                   AND agrgcocode = 'Impa';
              dbms_output.put_line(O_VALEUR4);         

            VAL :=O_VALEUR1+O_VALEUR2+O_VALEUR3+O_VALEUR4;
            dbms_output.put_line(VAL);         

            IF (VAL > 0) THEN 
            RES:='OUI';
            ELSE RES:='NON';
            END IF;
            O_VALEUR:=RES;        
        END;
    END P_TOTAL_IMPAYE;
----HME-----
PROCEDURE P_GET_IMP(
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

  VAL1    NUMBER;
  VAL2    NUMBER;
  VAL3    NUMBER;
  VAL4    NUMBER;
  SVAL    NUMBER;
  RES VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL1
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'ENGALEAS')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Imp' or agrgcocode IS NULL);

    SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL2
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'ENGALEASAG')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);


	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL3
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'R5002')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);


	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL4
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'R5000')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);

    SELECT  (VAL1+VAL2+VAL3+VAL4) INTO SVAL FROM DUAL;

  IF (SVAL>0)THEN 
    RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_GET_IMP;
 ---HME RAPPORT CREDIT BUREAU 
PROCEDURE P_RAPPORT(
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

  VAL1    NUMBER;
  VAL2    NUMBER;
  VAL3    NUMBER;
  VAL4    NUMBER;
  SVAL    NUMBER;
  ncount  NUMBER;
  RES   VARCHAR2(100);
  NRES  VARCHAR2(100);
  BEGIN
     BEGIN
  O_LIBVALEUR := '';
  O_VALEUR := '';
  RES := '';

	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL1
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'ENGALEAS')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Imp' or agrgcocode IS NULL);

    SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL2
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'ENGALEASAG')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);


	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL3
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'R5002')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);


	SELECT MAX(nvl(agrvalue,0))
				  INTO  VAL4
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'R5000')

					   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5)
					   AND (agrgcocode = 'Impa' or agrgcocode IS NULL);

    SELECT  (VAL1+VAL2+VAL3+VAL4) INTO SVAL FROM DUAL;

	SELECT count(*) INTO  ncount
				  FROM aragrid
				 WHERE     ratid IN (SELECT ratid
									   FROM ratio
									  WHERE ratcode = 'INFNEG')
                 AND anaid = (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=5);


	select RATVALUE into NRES from lkanarat where ratid=71011 
	and anaid= (select max(anaid) from analysis where dosid=p_dosid and dprversion=p_dprversion and ANMID=7000);


  IF (SVAL>0 OR ncount>0 OR NRES='OUI')THEN 
    RES:='OUI';
    ELSE RES:='NON';
  End If;
    o_valeur:= RES;
   END;
 END P_RAPPORT;

 PROCEDURE P_TOTAL_ECHEANCE(
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

 o_val NUMBER :=0;
  BEGIN
     BEGIN




	SELECT nvl(SUM (TO_NUMBER (REPLACE (agrvalue, ',', '.'))),0)
              INTO   o_val
              FROM aragrid
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'LEASANE')
                   AND anaid = (select max(anaid) from analysis where dosid=p_dosid and ANMID=5)
                   AND agrgcocode IN ('A1','A2','A3','A4','A5','A6');


      o_valeur:=TO_CHAR(o_val);


   END;
 END P_TOTAL_ECHEANCE;

END PAV4_RATIO ;