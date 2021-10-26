create or replace PACKAGE BODY pav4_com_general
AS
    PROCEDURE S_GET_USER_INFO (SUSER         IN     UTILISATEUR.UTICODE%TYPE,
                               SUGECODE         OUT UTILISATEUR.UGECODE%TYPE,
                               SUSERGROUPE      OUT UTILISATEUR.GROCODE%TYPE,
                               SUSERNAME        OUT VARCHAR2)
    AS
    BEGIN
        SELECT UGECODE, GROCODE, UTIPRENOM || ' ' || UTINOM
          INTO SUGECODE, SUSERGROUPE, SUSERNAME
          FROM UTILISATEUR
         WHERE UTICODE = SUSER;
    END S_GET_USER_INFO;

    PROCEDURE S_TTR_PARAM (P_LISTTRAITNOM          LANTTRPARAM.TTRNOM%TYPE,
                           P_LISTACTIVITE          LKTTPTACTPG.TACCODE%TYPE,
                           P_LISTPROFIL            LKTTPTACTPG.TPGCODE%TYPE,
                           P_LANGUE                LANTTRPARAM.LANCODE%TYPE,
                           P_PREF                  LKTTPTACTPG.TACCODE%TYPE,
                           PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT A.TTPCODE
                       CODE,
                   A.TTPLIBELLE
                       DISPLAYVALUE,
                   B.TTGFLAGDEFAUT
                       DEFAULTFLAG,
                   DECODE (B.TACCODE || B.TPGCODE, P_PREF, 1, 0)
                       PREFERENCEFLAG,
                   B.TTGORDRE
                       OTHERINFO
              FROM LANTTRPARAM A, LKTTPTACTPG B
             WHERE     B.TTRNOM = P_LISTTRAITNOM
                   AND B.TACCODE = P_LISTACTIVITE
                   AND B.TPGCODE = P_LISTPROFIL
                   AND A.TTRNOM = P_LISTTRAITNOM
                   AND B.TTPCODE = A.TTPCODE
                   AND A.LANCODE = P_LANGUE
            UNION
            SELECT DISTINCT TTPCODE,
                            TTPLIBELLE,
                            0,
                            0,
                            0
              FROM LANTTRPARAM
             WHERE     TTRNOM = P_LISTTRAITNOM
                   AND LANCODE = P_LANGUE
                   AND TTPCODE NOT IN
                           (SELECT DISTINCT TTPCODE
                              FROM LKTTPTACTPG
                             WHERE     TTRNOM = P_LISTTRAITNOM
                                   AND TACCODE = P_LISTACTIVITE
                                   AND TPGCODE = P_LISTPROFIL)
            ORDER BY 5, 2;
    END S_TTR_PARAM;

    PROCEDURE S_TTR_PARAM_NONE (
        P_LISTTRAITNOM          LANTTRPARAM.TTRNOM%TYPE,
        P_LANGUE                LANTTRPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TTPCODE  CODE,
                     TTPCODE  CODEINTEGER,
                     TTPLIBELLE DISPLAYVALUE,
                     0        DEFAULTFLAG,
                     0        PREFERENCEFLAG
                FROM LANTTRPARAM
               WHERE TTRNOM = P_LISTTRAITNOM AND LANCODE = P_LANGUE
            ORDER BY TTPLIBELLE;
    END S_TTR_PARAM_NONE;

    PROCEDURE S_FORMALITIES_COM (
        P_FORCODE                  FORMALITE.FORCODE%TYPE,
        SADDRESSTYPE               VARCHAR2,
        SUTICODE                   UTILISATEUR.UTICODE%TYPE,
        SDOSID                     DPRINTERVENANT.DOSID%TYPE,
        SGROCODERECEIVER           VARCHAR2,
        SMETIERRECEIVER            VARCHAR2,
        SSECTGESTRECEIVER          VARCHAR2,
        P_LANGUE                   LANTUSPARAM.LANCODE%TYPE,
        SUTICODER                  UTIUSERLINK.UTICODE%TYPE,
        SUGECODE                   UTILISATEUR.UGECODE%TYPE,
        PC_RETURN           IN OUT T_CURSOR)
    AS
        SDPRVERSION   DOSSIERPROSPECT.DPRVERSION%TYPE := NULL;
        STPGCODE      TPROFILGESTION.TPGCODE%TYPE;
        NFORID        FORMALITE.FORID%TYPE;
        SGROCODE      UTILISATEUR.GROCODE%TYPE;
        SWORCODE      WORSTEP.WORCODE%TYPE;
        NWSTORDER     WORSTEP.WSTORDER%TYPE;
    BEGIN
        DECLARE
            NDPRVERSIONCNT     NUMBER;
            nCompteur          NUMBER := 0;
            SMETIERRECEIVER1   VARCHAR2 (50);
        BEGIN
            NDPRVERSIONCNT := 0;

            IF SDOSID IS NOT NULL
            THEN
                SELECT COUNT (DPRVERSION)
                  INTO NDPRVERSIONCNT
                  FROM V_DEAL
                 WHERE DOSID = SDOSID;

                IF NDPRVERSIONCNT > 0
                THEN
                    SELECT DPRVERSION
                      INTO SDPRVERSION
                      FROM V_DEAL
                     WHERE DOSID = SDOSID;
                END IF;
            END IF;

            SELECT MAX (TPGCODE)
              INTO STPGCODE
              FROM DOSSIERPROSPECT
             WHERE DOSID = SDOSID AND DPRVERSION = SDPRVERSION;

            SELECT MAX (FORID)
              INTO NFORID
              FROM FORMALITE
             WHERE UGECODE = SUGECODE AND FORCODE = P_FORCODE;

            SELECT MAX (FDEMETIERRECEIVER) -----MTR 04/05/2018  FODMETIERRECEIVER -> FDEMETIERRECEIVER
              INTO SMETIERRECEIVER1
              FROM fordestination
             WHERE     forid = NFORID
                   AND (TPGCODE = STPGCODE OR TPGCODE = 'TOUT');

            -----MTR 04/05/2018   forid = NFORID AND   TPGCODE=STPGCODE
            --> forid = NFORID AND  (TPGCODE='TOUT' OR TPGCODE=STPGCODE)
            --EMI 25/01/2018 besoin TLG



            IF UPPER (SADDRESSTYPE) = 'GROUP'
            THEN
                SELECT GROCODE
                  INTO SGROCODE
                  FROM UTILISATEUR
                 WHERE UTICODE = SUTICODER and UTILOCKED='N';

                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE  UTILOCKED='N' AND GROCODE = SGROCODE;
            ELSIF UPPER (SADDRESSTYPE) IN ('AUTODERO',
                                           'RESPHIERAR',
                                           'ANALYSTE',
                                           'BACKOFFICE',
                                           'CORRENGA',
                                           'ASSCOMM',
                                           'ADJOINT',
                                           'CORRCOM')
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, UTIUSERLINK ULI
                     WHERE  UTI.UTILOCKED='N' and  UTICODELINKED = SUTICODER
                           AND ULITYPE = SADDRESSTYPE
                           AND ULIDTEND IS NULL
                           AND ULI.UTICODE = UTI.UTICODE
                           AND UTI.UTICODE IN
                                   (SELECT UTICODE
                                      FROM LKUTIARO
                                     WHERE ACTID IN
                                               (SELECT DISTINCT ACTID
                                                  FROM LKUTIARO
                                                 WHERE     UTICODE =
                                                           SUTICODER
                                                       AND ROLCODE = 'AGENCE'));
            ELSIF UPPER (SADDRESSTYPE) = 'DEALAGENCE'
            THEN
                OPEN PC_RETURN FOR
                    SELECT uti.UTICODE                        CODE,
                           uti.UTINOM || ' ' || uti.UTIPRENOM DISPLAYVALUE
                      FROM lkutiaro     lk,
                           utitsm       u,
                           dpracteur    d,
                           utilisateur  uti
                     WHERE  uti.UTILOCKED='N' and   d.dosid = SDOSID
                           AND d.rolcode = 'AGENCE'
                           AND d.actid = lk.actid
                           AND lk.rolcode = d.rolcode
                           AND lk.uticode = u.uticode
                           AND u.tsmmetier = SMETIERRECEIVER1
                           AND d.dprversion IN (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE dosid = SDOSID)
                           --AND u.tsmflagdefaut = 1 -----MTR 19/07/2018
                           AND lk.DEFAULTBRANCH = 1 --Supression cloisonnement CA
                           AND uti.uticode = u.uticode
                           AND NVL (utilocked, 'N') = 'N';
            ELSIF UPPER (SADDRESSTYPE) = 'DEALINVUSER'
            THEN
                BEGIN
                    --EMI 25/01/2018 besin TLG
                    SELECT COUNT (DIN.UTICODE)
                      INTO nCompteur
                      FROM DPRINTERVENANT DIN
                     WHERE     DOSID = SDOSID
                           AND DPRVERSION = SDPRVERSION
                           AND DINMETIER = SMETIERRECEIVER1;

                    IF (nCompteur <> 0)
                    THEN
                        OPEN PC_RETURN FOR
                            SELECT UTI.UTICODE                CODE,
                                   UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                              FROM UTILISATEUR UTI, DPRINTERVENANT DIN
                             WHERE UTI.UTILOCKED='N' AND    DOSID = SDOSID
                                   AND DPRVERSION = SDPRVERSION
                                   AND (   DINMETIER = SMETIERRECEIVER
                                        OR NVL (SMETIERRECEIVER, '') = '')
                                   AND UTI.UTICODE = DIN.UTICODE;
                    ELSIF (nCompteur = 0)
                    THEN
                        OPEN PC_RETURN FOR
                            SELECT UTICODE                    CODE,
                                   UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                              FROM UTILISATEUR
                             WHERE UTILOCKED='N' and UTICODE IN (SELECT DISTINCT (UTICODE)
                                                 FROM DPRINTERVENANT
                                                WHERE DOSID = SDOSID);
                    END IF;
                END;
            ELSIF UPPER (SADDRESSTYPE) = 'ACTOR'
            THEN
                OPEN PC_RETURN FOR
                    SELECT ACTCODE CODE, ACTLIBCOURT DISPLAYVALUE
                      FROM ACTEUR ACT, DPRACTEUR DAC
                     WHERE DOSID = SDOSID AND ACT.ACTID = DAC.ACTID;
            ELSIF UPPER (SADDRESSTYPE) = 'RESPHIERAR'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, UTIUSERLINK ULI
                     WHERE  UTI.UTILOCKED='N' AND  UTICODELINKED = SUTICODER
                           AND ULIDTEND IS NULL
                           AND ULI.UTICODE = UTI.UTICODE;
            ELSIF UPPER (SADDRESSTYPE) = 'OTHERS'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, UTIUSERLINK USL
                     WHERE UTI.UTILOCKED='N' and   UTI.UTICODE = SUTICODER
                           AND UTI.UTICODE = UTICODELINKED;
            ELSIF UPPER (SADDRESSTYPE) = 'GRORECEIVE'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE UTILOCKED='N' and    UGECODE = SUGECODE
                           AND (   (    SGROCODERECEIVER IS NOT NULL
                                    AND GROCODE = SGROCODERECEIVER)
                                OR (    SGROCODERECEIVER IS NULL
                                    AND GROCODE IN
                                            (SELECT DISTINCT
                                                    (FDEGROCODERECEIVER)
                                               FROM FORDESTINATION
                                              WHERE FORID = NFORID)));
            ELSIF UPPER (SADDRESSTYPE) = 'METIER'
            THEN
                OPEN PC_RETURN FOR
                    SELECT DISTINCT
                           UTILISATEUR.UTICODE
                               CODE,
                           UTILISATEUR.UTINOM || ' ' || UTILISATEUR.UTIPRENOM
                               DISPLAYVALUE
                      FROM UTILISATEUR, UTITSM
                     WHERE UTILOCKED='N' and    UTILISATEUR.UTICODE = UTITSM.UTICODE
                           AND UTITSM.TSMMETIER = SMETIERRECEIVER
                           AND (TSMDTEND IS NULL OR TSMDTEND > SYSDATE)
                           AND (   SSECTGESTRECEIVER IS NULL
                                OR UTITSM.TSMSECTGESTION = SSECTGESTRECEIVER);
            ELSIF UPPER (SADDRESSTYPE) = 'GROCODE'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE  UTILOCKED='N' and   GROCODE IN
                                   NVL (SGROCODERECEIVER,
                                        (SELECT DISTINCT (FDEGROCODERECEIVER)
                                           FROM FORDESTINATION
                                          WHERE FORID = NFORID))
                           AND UGECODE = SUGECODE;
            ELSIF UPPER (SADDRESSTYPE) = 'JOB'
            THEN
                IF     SMETIERRECEIVER IS NOT NULL
                   AND SSECTGESTRECEIVER IS NOT NULL
                THEN
                    OPEN PC_RETURN FOR
                        SELECT DISTINCT
                               FDEMETIERRECEIVER
                                   CODE,
                               PA_AVCOMMUN.F_LANTUSPARAM (P_LANGUE,
                                                          'METIER',
                                                          FDEMETIERRECEIVER)
                                   DISPLAYVALUE
                          FROM FORDESTINATION
                         WHERE     FDESECTGESTIONRECEIVER = SSECTGESTRECEIVER
                               AND FDEULYRECEIVER = UPPER (SADDRESSTYPE)
                               AND FORID = NFORID
                               AND EXISTS
                                       (SELECT 1
                                          FROM FILTREPARAMPROFIL
                                         WHERE     TPGCODE = STPGCODE
                                               AND FPPCODE =
                                                   FDEMETIERRECEIVER);
                ELSE
                    OPEN PC_RETURN FOR
                        SELECT DISTINCT
                               FDEMETIERRECEIVER
                                   CODE,
                               PA_AVCOMMUN.F_LANTUSPARAM (P_LANGUE,
                                                          'METIER',
                                                          FDEMETIERRECEIVER)
                                   DISPLAYVALUE
                          FROM FORDESTINATION FDT, DOSSIERPROSPECT DPR
                         WHERE     DOSID = SDOSID
                               AND DPRVERSION = SDPRVERSION
                               AND FDESECTGESTIONRECEIVER = DPRSECTGESTION
                               AND FDT.FORID = NFORID
                               AND EXISTS
                                       (SELECT 1
                                          FROM FILTREPARAMPROFIL
                                         WHERE     TPGCODE = DPR.TPGCODE
                                               AND FPPCODE =
                                                   FDEMETIERRECEIVER);
                END IF;
            ELSIF UPPER (SADDRESSTYPE) = 'POSITION'
            THEN
                IF     SMETIERRECEIVER IS NOT NULL
                   AND SSECTGESTRECEIVER IS NOT NULL
                THEN
                    OPEN PC_RETURN FOR
                        SELECT DISTINCT
                               FDEMETIERRECEIVER
                                   CODE,
                               PA_AVCOMMUN.F_LANTUSPARAM (P_LANGUE,
                                                          'METIER',
                                                          FDEMETIERRECEIVER)
                                   DISPLAYVALUE
                          FROM FORDESTINATION
                         WHERE     FDESECTGESTIONRECEIVER = SSECTGESTRECEIVER
                               AND FDEULYRECEIVER = UPPER (SADDRESSTYPE)
                               AND FORID = NFORID
                               AND EXISTS
                                       (SELECT 1
                                          FROM FILTREPARAMPROFIL
                                         WHERE     TPGCODE = STPGCODE
                                               AND FPPCODE =
                                                   FDEMETIERRECEIVER);
                ELSE
                    OPEN PC_RETURN FOR
                        SELECT DISTINCT
                               FDEMETIERRECEIVER
                                   CODE,
                               PA_AVCOMMUN.F_LANTUSPARAM (P_LANGUE,
                                                          'METIER',
                                                          FDEMETIERRECEIVER)
                                   DISPLAYVALUE
                          FROM FORDESTINATION FDT, DOSSIERPROSPECT DPR
                         WHERE     DOSID = SDOSID
                               AND DPRVERSION = SDPRVERSION
                               AND FDT.FORID = NFORID;
                END IF;
            ELSIF UPPER (SADDRESSTYPE) = 'ALLOCATE'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTIACTDEFAULT  UAD,
                           DPRACTEUR      DAC,
                           UTITSM         UTS,
                           UTILISATEUR    UTI
                     WHERE  UTI.UTILOCKED='N' and   UAD.UTICODE = UTI.UTICODE
                           AND UAD.ROLCODE = 'AGENCE'
                           AND UAD.ACTID = DAC.ACTID
                           AND UAD.UTICODE = UTS.UTICODE(+)
                           AND DAC.ROLCODE = UAD.ROLCODE
                           AND DAC.DOSID = SDOSID
                           AND DAC.DPRVERSION = SDPRVERSION
                           AND (   SMETIERRECEIVER IS NULL
                                OR UTS.TSMMETIER = SMETIERRECEIVER)
                           AND (   SSECTGESTRECEIVER IS NULL
                                OR UTS.TSMSECTGESTION = SSECTGESTRECEIVER)
                           AND SYSDATE >= UAD.UADDTSTART
                           AND SYSDATE <= UAD.UADDTEND;
            ELSIF UPPER (SADDRESSTYPE) = 'ACTINVUSER'
            THEN
                OPEN PC_RETURN FOR
                    SELECT DISTINCT
                           UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, LKACTUTITSM DIN
                     WHERE  UTI.UTILOCKED='N' and   DIN.ACTID IN
                                   (SELECT MAX (ACTIDRELATION)
                                      FROM ACTRELATION
                                     WHERE     TRECODE = 'NETWORK'
                                           AND ACTID IN
                                                   (SELECT ACTID
                                                      FROM DPRACTEUR
                                                     WHERE     DOSID = SDOSID
                                                           AND ROLCODE =
                                                               'EMPRUNT'))
                           AND UTI.UTICODE = DIN.UTICODE
                    UNION
                    SELECT DISTINCT
                           UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, LKACTUTITSM DIN
                     WHERE  UTI.UTILOCKED='N' and   DIN.ACTID IN
                                   (SELECT ACTID
                                      FROM DPRACTEUR
                                     WHERE     DOSID = SDOSID
                                           AND ROLCODE = 'EMPRUNT')
                           AND UTI.UTICODE = DIN.UTICODE
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM LKACTUTITSM
                                     WHERE ACTID IN
                                               (SELECT ACTIDRELATION
                                                  FROM ACTRELATION
                                                 WHERE     TRECODE =
                                                           'NETWORK'
                                                       AND ACTID IN
                                                               (SELECT ACTID
                                                                  FROM DPRACTEUR
                                                                 WHERE     DOSID =
                                                                           SDOSID
                                                                       AND ROLCODE =
                                                                           'EMPRUNT')));
            ELSIF UPPER (SADDRESSTYPE) = 'INTERVEXEC'
            THEN
                SELECT FDEWORCODE, FDEWSTORDER
                  INTO SWORCODE, NWSTORDER
                  FROM FORDESTINATION
                 WHERE FORID = NFORID AND ROWNUM = 1;

                OPEN PC_RETURN FOR
                    SELECT DISTINCT
                           UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE UTILOCKED='N' and UTICODE =
                           PAV4_WORKFLOW_2.F_GET_INTERVENANT_EXECUTANT (
                               SDOSID,
                               NWSTORDER,
                               SWORCODE);
            -----------20180129 MTR besoin TLG
            ELSIF UPPER (sAddressType) = 'TOUTAGENCEMO'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE UTILOCKED='N' and UTICODE IN
                               (SELECT DISTINCT (UTICODE)
                                  FROM utitsm
                                 WHERE     TSMMETIER = 'AGMO'
                                       AND TSMFLAGDEFAUT = '1'
                                       AND TSMDTEND IS NULL);
            ELSIF UPPER (sAddressType) = 'TOUTAGENCEBO'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE UTILOCKED='N' and UTICODE IN (SELECT DISTINCT (UTICODE)
                                         FROM utitsm
                                        WHERE TSMMETIER = 'AGBO');
            ----------END 20180129 MTR
            ELSE
                OPEN PC_RETURN FOR
                    SELECT TO_CHAR (NULL) CODE, TO_CHAR (NULL) DISPLAYVALUE
                      FROM DUAL
                     WHERE 1 = 2;
            END IF;
        END;
    END S_FORMALITIES_COM;

    PROCEDURE S_FORMALITIES_COM (
        SADDRESSTYPE          VARCHAR2,
        SUTICODE              UTILISATEUR.UTICODE%TYPE,
        SDOSID                DPRINTERVENANT.DOSID%TYPE,
        SUTICODER             UTIUSERLINK.UTICODE%TYPE,
        PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        IF UPPER (SADDRESSTYPE) = 'GROUP'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE GROCODE = (SELECT GROCODE
                                    FROM UTILISATEUR
                                   WHERE UTICODE = SUTICODER);
        ELSIF UPPER (SADDRESSTYPE) = 'DEALINVUSER'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE UTICODE IN (SELECT DISTINCT (UTICODE)
                                     FROM DPRINTERVENANT
                                    WHERE DOSID = SDOSID);
        ELSIF UPPER (SADDRESSTYPE) = 'INTERVEXEC'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE UTICODE IN (SELECT DISTINCT (UTICODEEXEC)
                                     FROM DPRWORSTEP
                                    WHERE DOSID = SDOSID);
        ELSIF UPPER (SADDRESSTYPE) = 'MYSELF'
        THEN
            OPEN PC_RETURN FOR
                SELECT SUTICODER                  CODE,
                       UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE UTICODE = SUTICODER;
        ELSIF UPPER (SADDRESSTYPE) = 'ACTOR'
        THEN
            OPEN PC_RETURN FOR SELECT ACTCODE CODE, ACTLIBCOURT DISPLAYVALUE
                                 FROM ACTEUR
                                WHERE ACTID IN (SELECT DISTINCT (ACTID)
                                                  FROM DPRACTEUR
                                                 WHERE DOSID = SDOSID);
        ELSIF UPPER (SADDRESSTYPE) IN ('CORRCOM',
                                       'INTERVEXEC',
                                       'RESPHIERAR',
                                       'ANALYSTE',
                                       'BACKOFFICE',
                                       'CORRENGA',
                                       'ASSCOMM',
                                       'ADJOINT')
        THEN
            OPEN PC_RETURN FOR
                SELECT DISTINCT
                       UTI.UTICODE                CODE,
                       UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR UTI, UTIUSERLINK ULI
                 WHERE     UTICODELINKED = SUTICODER
                       AND ULITYPE = SADDRESSTYPE
                       AND ULIDTEND IS NULL
                       AND ULI.UTICODE = UTI.UTICODE;
        ELSIF UPPER (SADDRESSTYPE) = 'OTHERS'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE UTICODE IN (SELECT DISTINCT (UTICODELINKED)
                                     FROM UTIUSERLINK
                                    WHERE UTICODE = SUTICODER);
        ELSE
            OPEN PC_RETURN FOR
                SELECT TO_CHAR (NULL) CODE, TO_CHAR (NULL) DISPLAYVALUE
                  FROM DUAL
                 WHERE 1 = 2;
        END IF;
    END S_FORMALITIES_COM;

    /*
       PROCEDURE S_FORM_USER_BY_AGENCE (
          SADDRESSTYPE          VARCHAR2,
          SUTICODE              UTILISATEUR.UTICODE%TYPE,
          SDOSID                DPRINTERVENANT.DOSID%TYPE,
          SUTICODER             UTIUSERLINK.UTICODE%TYPE,
          PC_RETURN      IN OUT T_CURSOR)
       AS
       BEGIN
          IF UPPER (SADDRESSTYPE) = 'GROUP'
          THEN
             OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     GROCODE = (SELECT GROCODE
                                        FROM UTILISATEUR
                                       WHERE UTICODE = SUTICODER)
                       AND UTICODE IN
                              (SELECT UTICODE
                                 FROM LKUTIARO
                                WHERE ACTID IN
                                         (SELECT DISTINCT ACTID
                                            FROM LKUTIARO
                                           WHERE     UTICODE = SUTICODER
                                                 AND ROLCODE = 'AGENCE'));
          ELSIF UPPER (SADDRESSTYPE) = 'DEALINVUSER'
          THEN
             OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     UTICODE IN (SELECT DISTINCT (UTICODE)
                                         FROM DPRINTERVENANT
                                        WHERE DOSID = SDOSID)
                       AND UTICODE IN
                              (SELECT UTICODE
                                 FROM LKUTIARO
                                WHERE ACTID IN
                                         (SELECT DISTINCT ACTID
                                            FROM LKUTIARO
                                           WHERE     UTICODE = SUTICODER
                                                 AND ROLCODE = 'AGENCE'));
          /*ELSIF UPPER(sAddressType) = 'INTERVEXEC' THEN
          OPEN PC_RETURN FOR
          SELECT  UTICODE      CODE,
          UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
          FROM UTILISATEUR
          WHERE UTICODE IN (SELECT distinct(UTICODEEXEC)FROM DPRWORSTEP  WHERE DOSID = sDosid)
          AND UTICODE IN (SELECT UTICODE FROM LKUTIARO WHERE ACTID IN (SELECT DISTINCT ACTID
          FROM LKUTIARO
          WHERE UTICODE = sUticoder
          AND ROLCODE = 'AGENCE'));*/
    /*
    ELSIF UPPER (SADDRESSTYPE) = 'ACTOR'
    THEN
       OPEN PC_RETURN FOR
          SELECT ACTCODE CODE, ACTLIBCOURT DISPLAYVALUE
            FROM ACTEUR
           WHERE ACTID IN (SELECT DISTINCT (ACTID)
                             FROM DPRACTEUR
                            WHERE DOSID = SDOSID);
    ELSIF UPPER (SADDRESSTYPE) IN ('AUTODERO',
                                   'RESPHIERAR',
                                   'ANALYSTE',
                                   'BACKOFFICE',
                                   'CORRENGA',
                                   'ASSCOMM',
                                   'ADJOINT',
                                   'CORRCOM')
    THEN
       OPEN PC_RETURN FOR
          SELECT UTI.UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
            FROM UTILISATEUR UTI, UTIUSERLINK ULI
           WHERE     UTICODELINKED = SUTICODER
                 AND ULITYPE = SADDRESSTYPE
                 AND ULIDTEND IS NULL
                 AND ULI.UTICODE = UTI.UTICODE
                 AND UTI.UTICODE IN
                        (SELECT UTICODE
                           FROM LKUTIARO
                          WHERE ACTID IN
                                   (SELECT DISTINCT ACTID
                                      FROM LKUTIARO
                                     WHERE     UTICODE = SUTICODER
                                           AND ROLCODE = 'AGENCE'));
    ELSIF UPPER (SADDRESSTYPE) = 'OTHERS'
    THEN
       OPEN PC_RETURN FOR
          SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
            FROM UTILISATEUR
           WHERE     UTICODE IN (SELECT DISTINCT (UTICODELINKED)
                                   FROM UTIUSERLINK
                                  WHERE UTICODE = SUTICODER)
                 AND UTICODE IN
                        (SELECT UTICODE
                           FROM LKUTIARO
                          WHERE ACTID IN
                                   (SELECT DISTINCT ACTID
                                      FROM LKUTIARO
                                     WHERE     UTICODE = SUTICODER
                                           AND ROLCODE = 'AGENCE'));
    ELSE
       OPEN PC_RETURN FOR
          SELECT TO_CHAR (NULL) CODE, TO_CHAR (NULL) DISPLAYVALUE
            FROM DUAL
           WHERE 1 = 2;
    END IF;
 END S_FORM_USER_BY_AGENCE;
 */
    -- Modified For TLG Evol Gest Absence

    PROCEDURE S_FORM_USER_BY_AGENCE (
        sAddressType          VARCHAR2,
        sUticode              UTILISATEUR.UTICODE%TYPE,
        sDosid                DPRINTERVENANT.DOSID%TYPE,
        sUticoder             UTIUSERLINK.UTICODE%TYPE,
        sMetier               UTITSM.TSMMETIER%TYPE,
        PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        IF UPPER (sAddressType) = 'GROUP'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     GROCODE = (SELECT GROCODE
                                        FROM UTILISATEUR
                                       WHERE UTICODE = sUticoder)
                       AND UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'DEALINVUSER'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     UTICODE IN (SELECT DISTINCT (UTICODE)
                                         FROM DPRINTERVENANT
                                        WHERE DOSID = sDosid)
                       AND UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'INTERVEXEC'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     UTICODE IN (SELECT DISTINCT (UTICODEEXEC)
                                         FROM DPRWORSTEP
                                        WHERE DOSID = sDosid)
                       AND UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'ACTOR'
        THEN
            OPEN PC_RETURN FOR SELECT ACTCODE CODE, ACTLIBCOURT DISPLAYVALUE
                                 FROM ACTEUR
                                WHERE ACTID IN (SELECT DISTINCT (ACTID)
                                                  FROM DPRACTEUR
                                                 WHERE DOSID = sDosid);
        ELSIF UPPER (sAddressType) IN ('AUTODERO',
                                       'RESPHIERAR',
                                       'ANALYSTE',
                                       'BACKOFFICE',
                                       'CORRENGA',
                                       'ASSCOMM',
                                       'ADJOINT',
                                       'CORRCOM')
        THEN
            OPEN PC_RETURN FOR
                SELECT UTI.UTICODE                CODE,
                       UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR UTI, UTIUSERLINK ULI
                 WHERE     UTICODELINKED = sUticoder
                       AND ULITYPE = sAddressType
                       AND ULIDTEND IS NULL
                       AND ULI.UTICODE = UTI.UTICODE
                       AND UTI.UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'OTHERS'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     UTICODE IN (SELECT DISTINCT (UTICODELINKED)
                                         FROM UTIUSERLINK
                                        WHERE UTICODE = sUticoder)
                       AND UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'METIER'
        THEN
            OPEN PC_RETURN FOR
                SELECT UTICODE CODE, UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                  FROM UTILISATEUR
                 WHERE     UTICODE IN (SELECT DISTINCT UTICODE
                                         FROM utitsm
                                        WHERE TSMMETIER = sMetier)
                       AND UTICODE IN
                               (SELECT UTICODE
                                  FROM LKUTIARO
                                 WHERE ACTID IN
                                           (SELECT DISTINCT ACTID
                                              FROM LKUTIARO
                                             WHERE     UTICODE = sUticoder
                                                   AND ROLCODE = 'AGENCE'));
        ELSIF UPPER (sAddressType) = 'AGENCETL'
        THEN
            OPEN PC_RETURN FOR
                SELECT a.UTICODE CODE, UTINOM || ' ' || UTIPRENOM
                  FROM utilisateur a, lkutiaro b
                 WHERE     a.uticode = b.uticode
                       AND b.actid =
                           (SELECT MAX (actid)
                              FROM dpracteur
                             WHERE     dosid = sDOSID
                                   AND rolcode = 'AGENCE'
                                   AND dprversion =
                                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                           sDOSID))
                       AND grocode = 'CC'
                       AND defaultbranch = 1
                       AND utilocked = 'N'
                UNION
                SELECT a.UTICODE CODE, UTINOM || ' ' || UTIPRENOM
                  FROM utitsm a, lkutiaro b, utilisateur c
                 WHERE     a.uticode = b.uticode
                       AND a.uticode = c.uticode
                       AND b.uticode = c.uticode
                       AND b.actid =
                           (SELECT MAX (actid)
                              FROM dpracteur
                             WHERE     dosid = sDOSID
                                   AND rolcode = 'AGENCE'
                                   AND dprversion =
                                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                           sDOSID))
                       AND tsmmetier = 'COMSE'
                       AND defaultbranch = 1;
        ELSE
            OPEN PC_RETURN FOR
                SELECT TO_CHAR (NULL) CODE, TO_CHAR (NULL) DISPLAYVALUE
                  FROM DUAL
                 WHERE 1 = 2;
        END IF;
    END S_FORM_USER_BY_AGENCE;

    -- Modified For TLG Evol Gest Absence


    PROCEDURE S_UTICODE_BY_GROUP (PUTICODE    IN     UTILISATEUR.UTICODE%TYPE,
                                  PGROCODE    IN     UTILISATEUR.GROCODE%TYPE,
                                  PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT UTICODE                      AS CODE,
                   UTINOM || ' - ' || UTIPRENOM AS DISPLAYVALUE
              FROM UTILISATEUR
             WHERE UTICODE <> PUTICODE AND GROCODE = PGROCODE;
    END S_UTICODE_BY_GROUP;
END PAV4_COM_GENERAL;