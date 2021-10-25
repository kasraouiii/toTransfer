create or replace PACKAGE BODY PA_COM_GENERAL
AS
    PROCEDURE S_TMOLIBELLE (SNOMMODULE           LANTMODULE.TMOMODULE%TYPE,
                            SLANGUE              LANTMODULE.LANCODE%TYPE,
                            STMOLIBELLE   IN OUT LANTMODULE.TMOLIBELLE%TYPE)
    AS
    BEGIN
        SELECT TMOLIBELLE
          INTO STMOLIBELLE
          FROM LANTMODULE
         WHERE TMOMODULE = SNOMMODULE AND LANCODE = SLANGUE;
    EXCEPTION
        WHEN OTHERS
        THEN
            STMOLIBELLE := NULL;
    END S_TMOLIBELLE;

    PROCEDURE FCPAYSSELECTATTRIBUTES (SLANGUE            LANROLE.LANCODE%TYPE,
                                      SPAYCODE           PAYS.PAYCODE%TYPE,
                                      PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LPA.PAYLIBELLE,
                   PAY.PAYCODE,
                   PAY.DEVCODE,
                   PAY.PAYFLAGREGION,
                   PAY.PAYTEL,
                   PAY.PAYFORMATPOST,
                   PAY.PAYNBCARIBAN,
                   PAY.PAYFLAGBANQUE,
                   PAY.PAYFORMATBANQUE,
                   PAY.PAYFLAGGUICHET,
                   PAY.PAYFORMATGUICHET,
                   PAY.PAYFORMATCOMPTE,
                   PAY.PAYFLAGCLERIB,
                   PAY.PAYFORMATCLERIB,
                   PAY.PAYFLAGSIRET,
                   PAY.PAYFORMATSIRET,
                   PAY.PAYREGLEIBAN,
                   LDE.DEVNOM
              FROM PAYS PAY, LANPAYS LPA, LANDEVISE LDE
             WHERE     PAY.PAYCODE = SPAYCODE
                   AND LPA.PAYCODE(+) = PAY.PAYCODE
                   AND LPA.LANCODE(+) = SLANGUE
                   AND LDE.DEVCODE(+) = PAY.DEVCODE
                   AND LDE.LANCODE(+) = SLANGUE;
    END FCPAYSSELECTATTRIBUTES;

    PROCEDURE GETAFFECTATIONGREFFE (
        LS_CODEPOSTAL   IN     ADRESSE.ADRCODEPOST%TYPE,
        LS_VILLE        IN     ADRESSE.ADRVILLE%TYPE,
        S_PAYCODE       IN     ADRESSE.PAYCODE%TYPE,
        LS_PAYCODE      IN     UNITEGESTION.UGECODE%TYPE,
        LI_ACTID           OUT NUMBER)
    AS
    BEGIN
        SELECT F_GETLKACTCPO (LS_CODEPOSTAL,
                              LS_VILLE,
                              LS_PAYCODE,
                              LS_PAYCODE)
          INTO LI_ACTID
          FROM DUAL;
    EXCEPTION
        WHEN OTHERS
        THEN
            LI_ACTID := NULL;
    END GETAFFECTATIONGREFFE;

    PROCEDURE GETQUOTEPART (SPCODE   IN     TRELATION.TRECODE%TYPE,
                            NFLAG       OUT NUMBER)
    AS
    BEGIN
        SELECT TREFLAGQP
          INTO NFLAG
          FROM TRELATION
         WHERE TRECODE = SPCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            NFLAG := NULL;
    END GETQUOTEPART;

    PROCEDURE ISAFFECTATIONGREFFE (
        PS_CODEPOSTAL   IN     ADRESSE.ADRCODEPOST%TYPE,
        PS_VILLE        IN     ADRESSE.ADRVILLE%TYPE,
        PS_PAYCODE      IN     ADRESSE.PAYCODE%TYPE,
        PI_ACTID        IN     ACTEUR.ACTID%TYPE,
        LI_OK              OUT NUMBER)
    AS
    BEGIN
        SELECT F_ISLKACTCPO (PS_CODEPOSTAL,
                             PS_VILLE,
                             PS_PAYCODE,
                             PI_ACTID)
          INTO LI_OK
          FROM DUAL;
    EXCEPTION
        WHEN OTHERS
        THEN
            LI_OK := NULL;
    END ISAFFECTATIONGREFFE;

    PROCEDURE S_DUAL_DATE_F_CLOISONISACTIF (SRETVAL     OUT NUMBER,
                                            SDATE       OUT DATE,
                                            SCURRDATE   OUT DATE)
    AS
    BEGIN
        SRETVAL := 0;
        SDATE := TRUNC (SYSDATE);
        SCURRDATE := SYSDATE;
    END S_DUAL_DATE_F_CLOISONISACTIF;

    PROCEDURE S_GETRUBCODEWITHRUBID (NRUBID         RUBRIQUE.RUBID%TYPE,
                                     SRUBCODE   OUT RUBRIQUE.RUBCODE%TYPE)
    AS
    BEGIN
        SELECT RUBCODE
          INTO SRUBCODE
          FROM RUBRIQUE
         WHERE RUBID = NRUBID;
    EXCEPTION
        WHEN OTHERS
        THEN
            SRUBCODE := NULL;
    END S_GETRUBCODEWITHRUBID;

    PROCEDURE S_ISRUBIDONFILTRE (NRUBID    IN     RUBACCES.RUBID%TYPE,
                                 SFILTRE   IN     RUBACCES.RACACCES%TYPE,
                                 NOK       IN OUT NUMBER)
    AS
    BEGIN
        NOK := F_ISRUBIDONFILTRE (NRUBID, SFILTRE);
    END S_ISRUBIDONFILTRE;

    PROCEDURE S_RUBLIBELLE (NRUBID               RUBRIQUE.RUBID%TYPE,
                            SLANCODE             LANGUE.LANCODE%TYPE,
                            SRUBLIBELLE   IN OUT LANRUBRIQUE.RUBLIBELLE%TYPE,
                            NFOUND        IN OUT NUMBER)
    AS
    BEGIN
        BEGIN
            NFOUND := 1;

            SELECT RUBLIBELLE
              INTO SRUBLIBELLE
              FROM LANRUBRIQUE
             WHERE RUBID = NRUBID AND LANCODE = SLANCODE;
        EXCEPTION
            WHEN OTHERS
            THEN
                NFOUND := 0;                   -- pas trouve ou autre incident
        END;
    END S_RUBLIBELLE;

    PROCEDURE S_GETRUBIDFROMFILTER (SPARAM   IN     RUBACCES.RACACCES%TYPE,
                                    NRUBID   IN OUT RUBRIQUE.RUBID%TYPE)
    AS
    BEGIN
        BEGIN
            SELECT MIN (RUBID)
              INTO NRUBID
              FROM RUBACCES
             WHERE RACACCES = SPARAM;
        EXCEPTION
            WHEN OTHERS
            THEN
                NRUBID := NULL;
        END;
    END S_GETRUBIDFROMFILTER;

    PROCEDURE S_GETRUBIDWITHRUBCODE (
        PS_RUBCODE          RUBRIQUE.RUBCODE%TYPE,
        SUGECODE            RUBRIQUE.UGECODE%TYPE,
        LI_RUBID     IN OUT RUBRIQUE.RUBID%TYPE)
    AS
    BEGIN
        BEGIN
            SELECT RUBID
              INTO LI_RUBID
              FROM RUBRIQUE
             WHERE RUBCODE = PS_RUBCODE AND UGECODE = SUGECODE;
        EXCEPTION
            WHEN OTHERS
            THEN
                LI_RUBID := NULL;
        END;
    END S_GETRUBIDWITHRUBCODE;

    PROCEDURE S_GET_USER_DETAILS (SUSERCODE          UTITSM.UTICODE%TYPE,
                                  PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TSMSECTGESTION AS TSMSECTGESTION, TSMMETIER AS TSMMETIER
              FROM UTITSM
             WHERE     UTITSM.UTICODE = SUSERCODE
                   AND (UTITSM.TSMDTEND IS NULL OR UTITSM.TSMDTEND > SYSDATE);
    END S_GET_USER_DETAILS;

    PROCEDURE S_LIBPHASE (SPHACODE             LANPHASE.PHACODE%TYPE,
                          SPHADEST             LANPHASE.PHADEST%TYPE,
                          SLANGUE       IN     LANPHASE.LANCODE%TYPE,
                          SPHALIBELLE   IN OUT LANPHASE.PHALIBELLE%TYPE)
    AS
    BEGIN
        SELECT PHALIBELLE
          INTO SPHALIBELLE
          FROM LANPHASE
         WHERE     PHACODE = SPHACODE
               AND PHADEST = SPHADEST
               AND LANCODE(+) = SLANGUE;
    EXCEPTION
        WHEN OTHERS
        THEN
            SPHALIBELLE := NULL;
    END S_LIBPHASE;

    PROCEDURE S_LV_COMMON (SSQLSTRING IN VARCHAR, PC_RETURN IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SSQLSTRING;
    END S_LV_COMMON;

    PROCEDURE S_MAX_SELECT (NPSYMAXSELECT OUT PARSYSTEME.PSYMAXSELECT%TYPE)
    AS
    BEGIN
        SELECT NVL (PSYMAXSELECT, 100000000)
          INTO NPSYMAXSELECT
          FROM PARSYSTEME;
    EXCEPTION
        WHEN OTHERS
        THEN
            NPSYMAXSELECT := NULL;
    END S_MAX_SELECT;

    PROCEDURE S_OGETSOCIETEFILIALE (
        NSOCID      IN     ACTRELATION.ACTIDRELATION%TYPE,
        SUGECODE    IN     ACTEUR.UGECODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT AGE.ACTID, ACT.ACTLIBCOURT
                FROM ACTRELATION ARE, ACTEURGESTION AGE, ACTEUR ACT
               WHERE     ARE.ACTIDRELATION = NSOCID
                     AND ARE.TRECODE = 'GROUPE'
                     AND AGE.ACTID = ARE.ACTID
                     AND ACT.ACTID = ARE.ACTID
                     AND ACT.UGECODE = SUGECODE
            ORDER BY ACT.ACTLIBCOURT;
    END S_OGETSOCIETEFILIALE;

    PROCEDURE U_UTIPREF_VALSTRING (
        PS_VALUESTRING          UTIPREFERENCE.UPRSTRINGVALUE%TYPE,
        PS_USER                 UTIPREFERENCE.UTICODE%TYPE,
        PS_CODE                 UTIPREFERENCE.UPRCODE%TYPE,
        NRETURNCODE      IN OUT NUMBER)
    AS
        COUNT1   NUMBER;
        COUNT2   NUMBER;
    BEGIN
        SELECT COUNT (*)
          INTO COUNT1
          FROM UTIPREFERENCE
         WHERE     UTICODE = PS_USER
               AND UPRSTRINGVALUE = PS_VALUESTRING
               AND UPRCODE = PS_CODE;

        IF COUNT1 <= 0
        THEN
            SELECT COUNT (*)
              INTO COUNT2
              FROM UTIPREFERENCE
             WHERE UTICODE = PS_USER AND UPRCODE = PS_CODE;

            IF COUNT2 <= 0
            THEN
                INSERT INTO UTIPREFERENCE (UTICODE, UPRCODE, UPRSTRINGVALUE)
                     VALUES (PS_USER, PS_CODE, PS_VALUESTRING);
            ELSE
                UPDATE UTIPREFERENCE
                   SET UPRSTRINGVALUE = PS_VALUESTRING
                 WHERE UTICODE = PS_USER AND UPRCODE = PS_CODE;
            END IF;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            NRETURNCODE := -1 * SQLCODE;
    END U_UTIPREF_VALSTRING;



    PROCEDURE D_UTIPREF_VALSTRING (
        PS_VALUESTRING          UTIPREFERENCE.UPRSTRINGVALUE%TYPE,
        PS_USER                 UTIPREFERENCE.UTICODE%TYPE,
        PS_CODE                 UTIPREFERENCE.UPRCODE%TYPE,
        NRETURNCODE      IN OUT NUMBER)
    AS
        nCOUNT   NUMBER;
    BEGIN
        SELECT COUNT (*)
          INTO nCOUNT
          FROM UTIPREFERENCE
         WHERE UTICODE = PS_USER AND UPRCODE = PS_CODE;

        IF nCOUNT > 0
        THEN
            DELETE FROM UTIPREFERENCE
                  WHERE UTICODE = PS_USER AND UPRCODE = PS_CODE;
        END IF;

        NRETURNCODE := 0;
    EXCEPTION
        WHEN OTHERS
        THEN
            NRETURNCODE := -1 * SQLCODE;
    END D_UTIPREF_VALSTRING;

    PROCEDURE S_PREPARE_TTR_PARAM (
        P_LISTTRAITNOM          LKTTPTACTPG.TTRNOM%TYPE,
        P_LISTACTIVITE          LKTTPTACTPG.TACCODE%TYPE,
        P_LISTPROFIL            LKTTPTACTPG.TPGCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TO_CHAR (COUNT (*)) CODE
              FROM LKTTPTACTPG
             WHERE     TTRNOM = P_LISTTRAITNOM
                   AND TACCODE = P_LISTACTIVITE
                   AND TPGCODE = P_LISTPROFIL;
    END S_PREPARE_TTR_PARAM;

    PROCEDURE S_LANGUAGE (SLANGUE            LANLANGUE.LANCODETRAD%TYPE,
                          PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT A.LANCODE AS CODE, LANLIBELLE AS DISPLAYVALUE
                FROM LANLANGUE A, LANGUE B
               WHERE     A.LANCODETRAD = SLANGUE
                     AND LANCODETRAD = B.LANCODE
                     AND EXISTS
                             (SELECT 1
                                FROM ACTEUR
                               WHERE LANCODE = A.LANCODE)
            ORDER BY 2;
    END S_LANGUAGE;

    PROCEDURE S_EDITION_LANGUAGE (
        SLANGUE            LANLANGUE.LANCODETRAD%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT A.LANCODE AS CODE, LANLIBELLE AS DISPLAYVALUE
                FROM LANLANGUE A, LANGUE B
               WHERE     A.LANCODETRAD = SLANGUE
                     AND LANCODETRAD = B.LANCODE
                     AND EXISTS
                             (SELECT 1
                                FROM ACTEUR
                               WHERE LANCODE = A.LANCODE)
            ORDER BY 2;
    END S_EDITION_LANGUAGE;

    PROCEDURE S_PREPARE_TUS_PARAM (
        P_LISTTRAITNOM          LKTUPTACTPG.TUSNOM%TYPE,
        P_LISTACTIVITE          LKTUPTACTPG.TACCODE%TYPE,
        P_LISTPROFIL            LKTUPTACTPG.TPGCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT COUNT (*) CODE
              FROM LKTUPTACTPG
             WHERE     TUSNOM = P_LISTTRAITNOM
                   AND TACCODE = P_LISTACTIVITE
                   AND TPGCODE = P_LISTPROFIL;
    END S_PREPARE_TUS_PARAM;

    PROCEDURE S_PAYS (P_LANCODE          LANPAYS.LANCODE%TYPE,
                      PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR   SELECT PAYCODE CODE, PAYLIBELLE DISPLAYVALUE
                               FROM LANPAYS
                              WHERE LANCODE = P_LANCODE
                           ORDER BY PAYLIBELLE;
    END S_PAYS;

    PROCEDURE S_TMOYENPMT (P_LANCODE          LANPAYS.LANCODE%TYPE,
                           PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT A.TMPCODE CODE, TMPLIBELLE DISPLAYVALUE
                FROM TMOYENPMT A, LANTMOYENPMT B
               WHERE     B.TMPCODE(+) = A.TMPCODE
                     AND LANCODE(+) = P_LANCODE
                     AND A.TMPCODE != 'ENCMDT'
            ORDER BY TMPLIBELLE;
    END S_TMOYENPMT;

    PROCEDURE S_TMOYENPMT_NO_FILTER (P_LANCODE          LANPAYS.LANCODE%TYPE,
                                     PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT A.TMPCODE CODE, TMPLIBELLE DISPLAYVALUE
                FROM TMOYENPMT A, LANTMOYENPMT B
               WHERE B.TMPCODE(+) = A.TMPCODE AND LANCODE(+) = P_LANCODE
            ORDER BY TMPLIBELLE;
    END S_TMOYENPMT_NO_FILTER;

    PROCEDURE S_DEVISE (P_LANCODE          LANPAYS.LANCODE%TYPE,
                        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT D.DEVCODE   CODE,
                     LD.DEVNOM   DISPLAYVALUE,
                     D.DEVDECIMALE OTHERINFO,
                     D.PAYCODE   OTHERINFO2
                FROM DEVISE D, LANDEVISE LD
               WHERE D.DEVCODE = LD.DEVCODE AND LANCODE = P_LANCODE
            ORDER BY 2;
    END S_DEVISE;

    PROCEDURE S_VARCODE (SLANCODE           LANGUE.LANCODE%TYPE,
                         PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT
                   ROL.RVACODE AS CODE, LAN.RVALIBELLE AS DISPLAYVALUE
              FROM ROLVARIABLE ROL, LANROLVARIABLE LAN
             WHERE ROL.RVACODE = LAN.RVACODE AND LAN.LANCODE = SLANCODE;
    END S_VARCODE;

    PROCEDURE S_GET_DPTCODE (SGUSER      IN     UTITSM.UTICODE%TYPE,
                             SLANCODE    IN     LANGUE.LANCODE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT D.DPTCODE CODE, LD.DPTLABEL DISPLAYVALUE
                FROM DEPARTMENT D, LANDEPARTMENT LD
               WHERE     D.DPTCODE = LD.DPTCODE
                     AND LD.LANCODE = SLANCODE
                     AND D.DPTCODE IN (SELECT DISTINCT (TSMSECTGESTION)
                                         FROM UTITSM
                                        WHERE UTICODE = SGUSER)
            ORDER BY 1;
    END S_GET_DPTCODE;

    PROCEDURE F_DEVISEIN (SDEVCODE DEVISE.DEVCODE%TYPE, NDEVIN IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            DDEBINTER   DEVISE.DEVDTDEBINTER%TYPE;
        BEGIN
            SELECT DEVDTDEBINTER
              INTO DDEBINTER
              FROM DEVISE
             WHERE DEVCODE = SDEVCODE;

            IF (DDEBINTER IS NOT NULL)
            THEN
                NDEVIN := 1;
            ELSE
                NDEVIN := 0;
            END IF;
        END;
    END F_DEVISEIN;

    PROCEDURE S_DEVSORTANT_NOTINSERT (
        P_LANCODE          LANPAYS.LANCODE%TYPE,
        P_DATE             DEVISE.DEVDTREMPLACE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT D.DEVCODE   CODE,
                     LD.DEVNOM   DISPLAYVALUE,
                     D.DEVDECIMALE OTHERINFO
                FROM DEVISE D, LANDEVISE LD
               WHERE     D.DEVCODE = LD.DEVCODE
                     AND LD.LANCODE = P_LANCODE
                     AND (   D.DEVDTREMPLACE IS NULL
                          OR TRUNC (D.DEVDTREMPLACE) > TRUNC (P_DATE))
            ORDER BY 2;
    END S_DEVSORTANT_NOTINSERT;

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
                       OTHERINFO,
                   1
                       OTHERINFO2
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
                            0,
                            1
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

    PROCEDURE S_TUS_PARAM (P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
                           P_LISTACTIVITE          LKTUPTACTPG.TACCODE%TYPE,
                           P_LISTPROFIL            LKTUPTACTPG.TPGCODE%TYPE,
                           P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
                           P_PREF                  LKTUPTACTPG.TACCODE%TYPE,
                           PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT A.TUPCODE       CODE,
                   A.TUPLIBELLE    DISPLAYVALUE,
                   B.TUGFLAGDEFAUT DEFAULTFLAG,
                   B.TUGORDRE      OTHERINFO,
                   1               PREFERENCEFLAG,
                   1               OTHERINFO2
              FROM LANTUSPARAM A, LKTUPTACTPG B
             WHERE     B.TUSNOM = P_LISTTRAITNOM
                   AND B.TACCODE = P_LISTACTIVITE
                   AND B.TPGCODE = P_LISTPROFIL
                   AND A.TUSNOM = P_LISTTRAITNOM
                   AND B.TUPCODE = A.TUPCODE
                   AND A.LANCODE = P_LANGUE;
    END S_TUS_PARAM;

    PROCEDURE S_TTR_PARAM_NONE (
        P_LISTTRAITNOM          LANTTRPARAM.TTRNOM%TYPE,
        P_LANGUE                LANTTRPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT DISTINCT TO_CHAR (TTPCODE) CODE,
                              TTPLIBELLE      DISPLAYVALUE,
                              0               DEFAULTFLAG,
                              0               PREFERENCEFLAG,
                              0               OTHERINFO2
                FROM LANTTRPARAM
               WHERE TTRNOM = P_LISTTRAITNOM AND LANCODE = P_LANGUE
            ORDER BY TTPLIBELLE;
    END S_TTR_PARAM_NONE;

    PROCEDURE S_TUS_PARAM_NONE (
        P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
        P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TUPCODE   CODE,
                     TUPLIBELLE DISPLAYVALUE,
                     0         DEFAULTFLAG,
                     0         PREFERENCEFLAG,
                     TUPHELPTEXT OTHERINFO
                FROM LANTUSPARAM
               WHERE TUSNOM = P_LISTTRAITNOM AND LANCODE = P_LANGUE
            ORDER BY TUPLIBELLE;
    END S_TUS_PARAM_NONE;

    PROCEDURE S_TUS_PARAM2 (P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
                            P_LISTACTIVITE          LKTUPTACTPG.TACCODE%TYPE,
                            P_LISTPROFIL            LKTUPTACTPG.TPGCODE%TYPE,
                            P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
                            P_PREF                  LKTUPTACTPG.TACCODE%TYPE,
                            PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT A.TUPCODE       CODE,
                   A.TUPLIBELLE    DISPLAYVALUE,
                   B.TUGFLAGDEFAUT DEFAULTFLAG,
                   B.TUGORDRE      OTHERINFO,
                   1               PREFERENCEFLAG,
                   1               OTHERINFO2
              FROM LANTUSPARAM A, LKTUPTACTPG B
             WHERE     B.TUSNOM = P_LISTTRAITNOM
                   AND B.TACCODE = P_LISTACTIVITE
                   AND B.TPGCODE = P_LISTPROFIL
                   AND A.TUSNOM = P_LISTTRAITNOM
                   AND B.TUPCODE = A.TUPCODE
                   AND A.LANCODE = P_LANGUE
                   AND EXISTS
                           (SELECT 1
                              FROM TUSPARAM T
                             WHERE     T.TUSNOM = P_LISTTRAITNOM
                                   AND T.TUPCODE = A.TUPCODE
                                   AND NVL (T.TUPFLAGORFI, 0) = 0)
            UNION
            SELECT DISTINCT B.TUPCODE,
                            B.TUPLIBELLE,
                            0,
                            0,
                            0,
                            1
              FROM LANTUSPARAM B
             WHERE     TUSNOM = P_LISTTRAITNOM
                   AND EXISTS
                           (SELECT 1
                              FROM TUSPARAM T
                             WHERE     T.TUSNOM = P_LISTTRAITNOM
                                   AND NVL (T.TUPFLAGORFI, 0) = 0)
                   AND LANCODE = P_LANGUE
                   AND TUPCODE NOT IN
                           (SELECT DISTINCT TUPCODE
                              FROM LKTUPTACTPG
                             WHERE     TUSNOM = P_LISTTRAITNOM
                                   AND TACCODE = P_LISTACTIVITE
                                   AND TPGCODE = P_LISTPROFIL)
            ORDER BY 5, 2;
    END S_TUS_PARAM2;

    PROCEDURE S_TUS_PARAM_NONE2 (
        P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
        P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TUPCODE  CODE,
                     TUPLIBELLE DISPLAYVALUE,
                     0        DEFAULTFLAG,
                     0        PREFERENCEFLAG,
                     0        OTHERINFO2
                FROM LANTUSPARAM
               WHERE     TUSNOM = P_LISTTRAITNOM
                     AND LANCODE = P_LANGUE
                     AND EXISTS
                             (SELECT 1
                                FROM TUSPARAM T
                               WHERE     T.TUSNOM = P_LISTTRAITNOM
                                     AND T.TUPCODE = LANTUSPARAM.TUPCODE
                                     AND NVL (T.TUPFLAGORFI, 0) = 0)
            ORDER BY TUPLIBELLE;
    END S_TUS_PARAM_NONE2;

    --Ajout Antoine
    --Date February 15 2006
    -- Window: NONE
    --
    PROCEDURE S_TAXE_CODE_LIBELLE_PAYS (SLANGUE            VARCHAR2,
                                        SPAYSCODE          VARCHAR2,
                                        STAXTYPE           TAXE.TAXTYPE%TYPE,
                                        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        IF (STAXTYPE IS NOT NULL)
        THEN
            OPEN PC_RETURN FOR
                  SELECT A.TAXCODE CODE, A.TAXLIBELLE DISPLAYVALUE
                    FROM LANTAXE A, TAXE B
                   WHERE     LANCODE = SLANGUE
                         AND A.TAXCODE = B.TAXCODE
                         AND (B.PAYCODE IS NULL OR B.PAYCODE = SPAYSCODE)
                         AND B.TAXTYPE = STAXTYPE
                ORDER BY B.TAXORDRE, A.TAXLIBELLE;
        ELSE
            OPEN PC_RETURN FOR
                  SELECT A.TAXCODE CODE, A.TAXLIBELLE DISPLAYVALUE
                    FROM LANTAXE A, TAXE B
                   WHERE     LANCODE = SLANGUE
                         AND A.TAXCODE = B.TAXCODE
                         AND (B.PAYCODE IS NULL OR B.PAYCODE = SPAYSCODE)
                ORDER BY B.TAXORDRE, A.TAXLIBELLE;
        END IF;
    END S_TAXE_CODE_LIBELLE_PAYS;

    PROCEDURE S_PARAM_DATE (PS_TABLE            TOPPARAM.TOPTABLE%TYPE,
                            PS_PARAM            TOPPARAM.TPAPARAM%TYPE,
                            PS_UGECODE          TOPPARAM.UGECODE%TYPE,
                            PC_RETURN    IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TPADATE CODE
              FROM TOPPARAM
             WHERE     TOPTABLE = PS_TABLE
                   AND TPAPARAM = PS_PARAM
                   AND UGECODE = PS_UGECODE;
    END S_PARAM_DATE;

    PROCEDURE S_DEV_ENTRANT (P_LANCODE          LANPAYS.LANCODE%TYPE,
                             P_DATE             DEVISE.DEVDTREMPLACE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT D.DEVCODE     CODE,
                   LD.DEVNOM     DISPLAYVALUE,
                   D.DEVDECIMALE OTHERINFO
              FROM DEVISE D, LANDEVISE LD
             WHERE     D.DEVCODE = LD.DEVCODE
                   AND LD.LANCODE = P_LANCODE
                   AND (D.DEVDTREMPLACE IS NULL OR D.DEVDTREMPLACE > P_DATE);
    END S_DEV_ENTRANT;

    PROCEDURE S_DLGACTFDGNEWDOSAVNT (NACTID             ACTPROCEDURE.ACTID%TYPE,
                                     SDOSNUM            DOSSIER.DOSNUM%TYPE,
                                     PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT
                   DOSID AS CODE, TO_CHAR (DOSAVENANT, '00') AS DISPLAYVALUE
              FROM DOSSIER
             WHERE DOSNUM = SDOSNUM AND ACTID = NACTID;
    END S_DLGACTFDGNEWDOSAVNT;

    PROCEDURE S_SECTEURGESTION (
        SLISTACTIVITE   IN     VARCHAR2,
        SLISTPROFIL     IN     LKTUPTACTPG.TPGCODE%TYPE,
        SGUSER          IN     UTITSM.UTICODE%TYPE,
        SLANGUE                LANROLE.LANCODE%TYPE,
        PC_RETURN       IN OUT T_CURSOR)
    AS
        NCOUNT   NUMBER := 0;
    BEGIN
        /*
        IF sListActivite IS NOT NULL AND sListActivite != '' THEN
        IF sListProfil IS NOT NULL AND sListProfil != '' THEN
        SELECT  COUNT( DISTINCT LTP.TUPCODE )
        INTO nCount
        FROM UTITSM USM,
        LANTUSPARAM LTP,
        LKTUPTACTPG LKT
        WHERE USM.UTICODE = sgUser
        AND  LTP.TUSNOM = 'SECTEURGEST'
        AND  LTP.LANCODE = sLangue
        AND  USM.TSMSECTGESTION = LTP.TUPCODE
        AND  TACCODE IN (  sListActivite  )
        AND  LKT.TUPCODE = LTP.TUPCODE
        AND  LKT.TUSNOM = LTP.TUSNOM
        AND LKT.TPGCODE IN ( sListProfil  );
        ELSE
        SELECT  COUNT( DISTINCT LTP.TUPCODE )
        INTO nCount
        FROM UTITSM USM,
        LANTUSPARAM LTP,
        LKTUPTACTPG LKT
        WHERE USM.UTICODE = sgUser
        AND  LTP.TUSNOM = 'SECTEURGEST'
        AND  LTP.LANCODE = sLangue
        AND  USM.TSMSECTGESTION = LTP.TUPCODE
        AND  TACCODE IN (  sListActivite  )
        AND  LKT.TUPCODE = LTP.TUPCODE
        AND  LKT.TUSNOM = LTP.TUSNOM ;
        END IF;
        END IF;
        IF nCount IS NULL OR  nCount = 0 THEN
        OPEN PC_RETURN FOR
        SELECT DISTINCT LTP.TUPLIBELLE AS Displayvalue,
        LTP.TUPCODE AS Code,
        0 AS Defaultflag,
        0 AS Ord
        FROM UTITSM USM,
        LANTUSPARAM LTP
        WHERE USM.UTICODE = sgUser
        AND  LTP.TUSNOM = 'SECTEURGEST'
        AND  LTP.LANCODE = sLangue
        AND  USM.TSMSECTGESTION = LTP.TUPCODE
        ORDER BY 1;
        ELSE
        IF nCount > 0 THEN
        IF sListProfil IS NOT NULL AND sListProfil != '' THEN
        OPEN PC_RETURN FOR
        SELECT  DISTINCT LTP.TUPLIBELLE  AS Displayvalue,
        LTP.TUPCODE AS Code,
        LKT.TUGFLAGDEFAUT AS Defaultflag,
        LKT.TUGORDRE AS Ord
        FROM UTITSM USM,
        LANTUSPARAM LTP,
        LKTUPTACTPG LKT
        WHERE USM.UTICODE = sgUser
        AND  LTP.TUSNOM = 'SECTEURGEST'
        AND  LTP.LANCODE = sLangue
        AND  USM.TSMSECTGESTION = LTP.TUPCODE
        AND  TACCODE IN (  sListActivite  )
        AND  LKT.TUPCODE = LTP.TUPCODE
        AND  LKT.TUSNOM = LTP.TUSNOM
        AND  LKT.TPGCODE IN (  sListProfil  )
        AND  ( LKT.TUGFLAGDEFAUT, LKT.TUGORDRE ) = (  SELECT  MAX( TUGFLAGDEFAUT ), MIN ( TUGORDRE )
        FROM LKTUPTACTPG
        WHERE  TUSNOM = LKT.TUSNOM
        AND TACCODE IN (  sListActivite  )
        AND  TUPCODE = LKT.TUPCODE
        AND TPGCODE IN (  sListProfil  )  )
        ORDER BY 3, 4,1 ;
        ELSE
        OPEN PC_RETURN FOR
        SELECT  DISTINCT LTP.TUPLIBELLE  AS Displayvalue,
        LTP.TUPCODE AS Code,
        LKT.TUGFLAGDEFAUT AS Defaultflag,
        LKT.TUGORDRE AS Ord
        FROM UTITSM USM,
        LANTUSPARAM LTP,
        LKTUPTACTPG LKT
        WHERE USM.UTICODE = sgUser
        AND  LTP.TUSNOM = 'SECTEURGEST'
        AND  LTP.LANCODE = sLangue
        AND  USM.TSMSECTGESTION = LTP.TUPCODE
        AND  TACCODE IN (  sListActivite  )
        AND  LKT.TUPCODE = LTP.TUPCODE
        AND  LKT.TUSNOM = LTP.TUSNOM
        AND  ( LKT.TUGFLAGDEFAUT, LKT.TUGORDRE ) = (  SELECT  MAX( TUGFLAGDEFAUT ), MIN ( TUGORDRE )
        FROM LKTUPTACTPG
        WHERE  TUSNOM = LKT.TUSNOM
        AND TACCODE IN ( sListActivite  )
        AND  TUPCODE = LKT.TUPCODE )
        ORDER BY 3, 4,1;
        END IF;
        END IF;
        END IF;*/
        OPEN PC_RETURN FOR   SELECT DPTCODE AS CODE, DPTLABEL AS DISPLAYVALUE
                               FROM LANDEPARTMENT
                              WHERE LANCODE = SLANGUE
                           ORDER BY 2;
    END S_SECTEURGESTION;

    PROCEDURE S_TAUX (P_LANCODE          LANTAUX.LANCODE%TYPE,
                      PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TAULIBELLE                AS DISPLAYVALUE,
                     TAU.TAUCODE               AS CODE,
                     TAUTYPE                   AS TAUTYPE,
                     TAUFLAGMATIF              AS MAT,
                     LPAD (TAUFREQUNITE, 3, '0') AS REFER
                FROM TAUX TAU, LANTAUX LTA
               WHERE TAU.TAUCODE = LTA.TAUCODE AND LTA.LANCODE = P_LANCODE
            ORDER BY TAULIBELLE;
    END S_TAUX;

    PROCEDURE S_INSERTPROTECT (SFONCTION    TSECURITE.TSEFONCTION%TYPE,
                               SCONCEPT     TSECURITE.TSECONCEPT%TYPE,
                               NSECLE       TSECURITE.TSECLE%TYPE)
    AS
    BEGIN
        INSERT INTO TSECURITE (TSEFONCTION, TSECONCEPT, TSECLE)
             VALUES (SFONCTION, SCONCEPT, NSECLE);
    END S_INSERTPROTECT;

    PROCEDURE S_SELECTPROTECT (SFONCTION          TSECURITE.TSEFONCTION%TYPE,
                               SCONCEPT           TSECURITE.TSECONCEPT%TYPE,
                               NSECLE             TSECURITE.TSECLE%TYPE,
                               NCOUNT      IN OUT NUMBER)
    AS
    BEGIN
        IF NSECLE IS NULL
        THEN
            SELECT COUNT (*)
              INTO NCOUNT
              FROM TSECURITE
             WHERE     TSEFONCTION = SFONCTION
                   AND TSECONCEPT = SCONCEPT
                   AND TSECLE IS NULL;
        ELSE
            SELECT COUNT (*)
              INTO NCOUNT
              FROM TSECURITE
             WHERE     TSEFONCTION = SFONCTION
                   AND TSECONCEPT = SCONCEPT
                   AND TSECLE = NSECLE;
        END IF;
    END S_SELECTPROTECT;

    PROCEDURE S_CAL (SLANGUE LANGUE.LANCODE%TYPE, PC_RETURN IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT CALID AS CODEINTEGER, CALNAME AS DISPLAYVALUE
                FROM LANCALENDAR
               WHERE LANCODE = SLANGUE
            ORDER BY CALNAME;
    END S_CAL;

    PROCEDURE S_GET_USRERIGHTS_FORMODULE (
        SPMODULE           TMODULE.TMOMODULE%TYPE,
        SUSER              LKUTITMFDROIT.UTICODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT UFD.TMFFONCTION         FONC,
                     NVL (UFD.UFDCREER, 0)   CRE,
                     NVL (UFD.UFDMODIFIER, 0) MODIF,
                     NVL (UFD.UFDVOIR, 0)    VISU,
                     NVL (UFD.UFDSUPPRIMER, 0) SUPP
                FROM LKUTITMFDROIT UFD, TMOFONCTION TMF
               WHERE     UFD.UTICODE = SUSER
                     AND UFD.TMOMODULE = SPMODULE
                     AND TMF.TMOMODULE = UFD.TMOMODULE
                     AND TMF.TMFFONCTION = UFD.TMFFONCTION
                     AND NVL (TMF.TMFFLAGEVT, 0) = 0
            ORDER BY UFD.TMFFONCTION;
    END S_GET_USRERIGHTS_FORMODULE;

    PROCEDURE S_GET_GROUPRIGHTS_FORMODULE (
        SPMODULE             TMODULE.TMOMODULE%TYPE,
        SUSERGROUPE          LKGROTMFDROIT.GROCODE%TYPE,
        PC_RETURN     IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT GFD.TMFFONCTION         FONC,
                     NVL (GFD.GFDCREER, 0)   CRE,
                     NVL (GFD.GFDMODIFIER, 0) MODIF,
                     NVL (GFD.GFDVOIR, 0)    VISU,
                     NVL (GFD.GFDSUPPRIMER, 0) SUPP
                FROM LKGROTMFDROIT GFD, TMOFONCTION TMF
               WHERE     GFD.GROCODE = SUSERGROUPE
                     AND GFD.TMOMODULE = SPMODULE
                     AND TMF.TMOMODULE = GFD.TMOMODULE
                     AND TMF.TMFFONCTION = GFD.TMFFONCTION
                     AND NVL (TMF.TMFFLAGEVT, 0) = 0
            ORDER BY GFD.TMFFONCTION;
    END S_GET_GROUPRIGHTS_FORMODULE;

    PROCEDURE S_GETMODULE_EXE (SPMODULE       TMODULE.TMOMODULE%TYPE,
                               SEXE       OUT TMODULE.TMOMODULE%TYPE)
    AS
    BEGIN
        SELECT TMOEXE
          INTO SEXE
          FROM TMODULE
         WHERE TMOMODULE = SPMODULE;
    EXCEPTION
        WHEN OTHERS
        THEN
            SEXE := NULL;
    END S_GETMODULE_EXE;

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
    EXCEPTION
        WHEN OTHERS
        THEN
            SUGECODE := NULL;
            SUSERGROUPE := NULL;
            SUSERNAME := NULL;
    END S_GET_USER_INFO;

    PROCEDURE S_GET_SCHEMA_DB_NAME (SSCHEMA   OUT VARCHAR2,
                                    SDBNAME   OUT VARCHAR2)
    AS
    BEGIN
        SELECT USER, SYS_CONTEXT ('USERENV', 'DB_NAME')
          INTO SSCHEMA, SDBNAME
          FROM DUAL;
    EXCEPTION
        WHEN OTHERS
        THEN
            SSCHEMA := NULL;
            SDBNAME := NULL;
    END S_GET_SCHEMA_DB_NAME;

    PROCEDURE S_GET_ACTIF_USER_COUNT (NUSERCOUNT OUT NUMBER)
    AS
    BEGIN
        SELECT COUNT (*)
          INTO NUSERCOUNT
          FROM UTILISATEUR
         WHERE NVL (UTIFLAGINACTIF, 0) != 1;
    END S_GET_ACTIF_USER_COUNT;

    PROCEDURE S_PENALITE_CMBARATACCODECREATE (
        NACTID             TACACTGESTION.ACTID%TYPE,
        SLANGUE     IN     VARCHAR2,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LTA.TACCODE AS CODE, LTA.TACLIBELLE AS DISPLAYVALUE
              FROM LANTACTIVITE LTA, TACTIVITE TAC, TACACTGESTION TAG
             WHERE     TAG.ACTID = NACTID
                   AND TAC.TACCODE = TAG.TACCODE
                   AND LTA.LANCODE = SLANGUE
                   AND LTA.TACCODE = TAC.TACCODE;
    END S_PENALITE_CMBARATACCODECREATE;

    PROCEDURE S_PENALITE_CMBARATPGCODECREATE (
        SARATACCODE          TPROFILGESTION.TACCODE%TYPE,
        SLANGUE       IN     VARCHAR2,
        PC_RETURN     IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LTP.TPGCODE AS CODE, LTP.TPGLIBELLE AS DISPLAYVALUE
              FROM LANTPROFILGESTION LTP, TPROFILGESTION TPG
             WHERE     LTP.LANCODE = SLANGUE
                   AND TPG.TPGCODE = LTP.TPGCODE
                   AND TPG.TACCODE = SARATACCODE;
    END S_PENALITE_CMBARATPGCODECREATE;

    PROCEDURE S_UTIPREF_VALSTRING (SUSER              UTIPREFERENCE.UTICODE%TYPE,
                                   SCODE              UTIPREFERENCE.UPRCODE%TYPE,
                                   PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT UTICODE,
                                  UPRCODE,
                                  UPRSTRINGVALUE,
                                  UPRBOOLEANVALUE,
                                  UPRDATEVALUE
                             FROM UTIPREFERENCE
                            WHERE UTICODE = SUSER AND UPRCODE = SCODE;
    END S_UTIPREF_VALSTRING;

    -- Not used
    /*
    PROCEDURE S_COMBOBOXRIBTRESOINTITULE(
    nActId ACTRIB.ACTID%TYPE,
    dtRibRemplace  UTIPREFERENCE.UPRCODE%TYPE,
    sRibBanque VARCHAR2,
    dtRemplace DATE,
    bRibAcces NUMBER,
    nRibManId NUMBER,
    PC_RETURN IN OUT T_CURSOR) AS
    BEGIN
    IF ( nActId IS NOT NULL ) THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM   ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    ORDER BY Displayvalue;
    ELSIF ( dtRibRemplace IS NOT NULL ) THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM   ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    AND     ARI.ARIDTDEB <= dtRemplace
    AND     ( ( ARI.ARIDTREMPLACE IS NULL ) OR ( dtRemplace <= ARI.ARIDTREMPLACE ) )
    ORDER BY Displayvalue;
    ELSIF (sRibBanque IS NOT NULL) THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM   ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    AND RIB.BGUBANQUE =  sRibBanque
    ORDER BY Displayvalue;
    ELSIF (bRibAcces = 1) THEN
    IF nRibManId IS NOT NULL THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    UNION
    SELECT TO_CHAR( ARI.RIBID ), TO_CHAR( ARI.ACTID ), RIB.RIBINTITULE
    FROM    ACTRIBACCES ARA,
    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARA.ACTIDSIGNATURE = nActId
    AND     ARI.ACTID = ARA.ACTID
    AND     ARI.RIBID = ARA.RIBID
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    AND  ARA.ACTID = F_GetMandataire( nRibManId )
    ORDER BY Displayvalue;
    ELSIF ( dtRibRemplace IS NOT NULL ) THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    UNION
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM    ACTRIBACCES ARA,
    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARA.ACTIDSIGNATURE = nActId
    AND     ARI.ACTID = ARA.ACTID
    AND     ARI.RIBID = ARA.RIBID
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    AND     NVL( ARA.ARADTBEG, dtRemplace ) <= dtRemplace
    AND     ( ( ARA.ARADTEND IS NULL ) OR ( dtRemplace <= ARA.ARADTEND ) )
    AND     ARI.ARIDTDEB <= dtRemplace
    AND     ( ( ARI.ARIDTREMPLACE IS NULL ) OR ( dtRemplace <= ARI.ARIDTREMPLACE ) )
    ORDER BY Displayvalue;
    ELSIF (sRibBanque IS NOT NULL) THEN
    OPEN PC_RETURN FOR
    SELECT DISTINCT TO_CHAR( ARI.RIBID ) AS Code, TO_CHAR( ARI.ACTID ) AS otherinfo, RIB.RIBINTITULE AS Displayvalue
    FROM    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARI.ACTID =  nActId
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    UNION
    SELECT TO_CHAR( ARI.RIBID ), TO_CHAR( ARI.ACTID ), RIB.RIBINTITULE
    FROM    ACTRIBACCES ARA,
    ACTRIB ARI,
    RIB,
    BANQUEGUICHET BGU
    WHERE   ARA.ACTIDSIGNATURE = nActId
    AND     ARI.ACTID = ARA.ACTID
    AND     ARI.RIBID = ARA.RIBID
    AND     RIB.RIBID =  ARI.RIBID
    AND     BGU.BGUBANQUE = RIB.BGUBANQUE
    AND     BGU.BGUGUICHET = RIB.BGUGUICHET
    AND  RIB.BGUBANQUE = sRibBanque
    ORDER BY Displayvalue;
    END IF;
    END IF;
    END S_COMBOBOXRIBTRESOINTITULE;*/
    PROCEDURE S_GETDOSSIER_1 (NDOSID               DOSSIER.DOSID%TYPE,
                              SDOSNUM       IN OUT DOSSIER.DOSNUM%TYPE,
                              NDOSAVENANT   IN OUT DOSSIER.DOSAVENANT%TYPE,
                              SDOSNOM       IN OUT DOSSIER.DOSNOM%TYPE)
    AS
    BEGIN
        SELECT DOSNUM, DOSAVENANT, DOSNOM
          INTO SDOSNUM, NDOSAVENANT, SDOSNOM
          FROM DOSSIER
         WHERE DOSID = NDOSID;
    EXCEPTION
        WHEN OTHERS
        THEN
            SDOSNUM := NULL;
            NDOSAVENANT := NULL;
            SDOSNOM := NULL;
    END S_GETDOSSIER_1;

    PROCEDURE S_GETDOSSIER_2 (SDOSNUM              DOSSIER.DOSNUM%TYPE,
                              NDOSAVENANT          DOSSIER.DOSAVENANT%TYPE,
                              SDOSNOM       IN OUT DOSSIER.DOSNOM%TYPE)
    AS
    BEGIN
        SELECT DOSNOM
          INTO SDOSNOM
          FROM DOSSIER
         WHERE DOSNUM = SDOSNUM AND DOSAVENANT = NDOSAVENANT;
    EXCEPTION
        WHEN OTHERS
        THEN
            SDOSNOM := NULL;
    END S_GETDOSSIER_2;

    PROCEDURE S_OLOIAMOGETTYPE (STLFCODE          TAMOLOIFISC.TLFCODE%TYPE,
                                STLFTYPE   IN OUT TAMOLOIFISC.TLFTYPE%TYPE)
    AS
    BEGIN
        SELECT TLFTYPE
          INTO STLFTYPE
          FROM TAMOLOIFISC
         WHERE TLFCODE = STLFCODE;
    EXCEPTION
        WHEN OTHERS
        THEN
            STLFTYPE := NULL;
    END S_OLOIAMOGETTYPE;

    PROCEDURE S_CONFIGURATIONKEY (SENTCODE           LKESUCKE.ENTCODE%TYPE,
                                  PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT CKEID,
                                  TPGCODE,
                                  UGECODE,
                                  JALCODE,
                                  UTICODE,
                                  PHACODE,
                                  PHADEST,
                                  GROCODE,
                                  TACCODE,
                                  ACTID,
                                  CKETYPE,
                                  CKECONTEXT,
                                  CKENAME,
                                  CKEWEIGHT,
                                  TBNCODE,
                                  TBTCODE,
                                  PAYCODE,
                                  NAPCODE,
                                  TCACODE,
                                  CKEACTTYPE
                             FROM CONFIGURATIONKEY
                            WHERE CKEID IN (SELECT CKEID
                                              FROM LKESUCKE
                                             WHERE ENTCODE = SENTCODE);
    END S_CONFIGURATIONKEY;

    PROCEDURE S_TTR_PARAMBIS (
        SLISTTRAITNOM          LANTTRPARAM.TTRNOM%TYPE,
        SLISTACTIVITE          VARCHAR2,
        SLISTPROFIL            VARCHAR2,
        SLANGUE                LANTTRPARAM.LANCODE%TYPE,
        PC_RETURN       IN OUT T_CURSOR)
    AS
        V_VAR         VARCHAR2 (4000);
        SLISTSELECT   VARCHAR2 (4000);
        NCOUNT        NUMBER := 0;
    BEGIN
        IF (SLISTPROFIL IS NOT NULL AND SLISTPROFIL = 'TOUT')
        THEN
            OPEN PC_RETURN FOR
                  SELECT DISTINCT A.TTPCODE     AS COde,
                                  A.TTPLIBELLE  AS Displayvalue,
                                  B.TTGFLAGDEFAUT AS DefaultFlag,
                                  B.TTGORDRE    AS Ordre
                    FROM LANTTRPARAM A, LKTTPTACTPG B
                   WHERE     B.TTRNOM = SLISTTRAITNOM
                         AND B.TPGCODE = SLISTPROFIL
                         AND A.TTRNOM = B.TTRNOM
                         AND A.TTPCODE = B.TTPCODE
                         AND A.LANCODE = SLANGUE
                         AND (B.TTGFLAGDEFAUT, B.TTGORDRE) =
                             (SELECT MAX (TTGFLAGDEFAUT), MIN (TTGORDRE)
                                FROM LKTTPTACTPG
                               WHERE     TTPCODE = B.TTPCODE
                                     AND TTRNOM = B.TTRNOM
                                     AND TPGCODE = SLISTPROFIL)
                ORDER BY 3, 4, 2;
        ELSE
            BEGIN
                IF SLISTACTIVITE IS NOT NULL
                THEN
                    V_VAR := 'SELECT             COUNT(*)';
                    V_VAR := V_VAR || ' FROM             LKTTPTACTPG';
                    V_VAR :=
                        V_VAR || ' WHERE              TTRNOM = sListTraitNom';
                    V_VAR :=
                           V_VAR
                        || ' AND             TACCODE IN ( '
                        || SLISTACTIVITE
                        || ' )';

                    EXECUTE IMMEDIATE V_VAR INTO NCOUNT;
                END IF;

                IF SLISTPROFIL IS NOT NULL
                THEN
                    V_VAR := 'SELECT             COUNT(*)';
                    V_VAR := V_VAR || ' FROM             LKTTPTACTPG';
                    V_VAR :=
                        V_VAR || ' WHERE              TTRNOM = sListTraitNom';
                    V_VAR :=
                           V_VAR
                        || ' AND             TACCODE IN ( '
                        || SLISTACTIVITE
                        || ' )';
                    V_VAR :=
                           V_VAR
                        || ' AND             TPGCODE IN ( '
                        || SLISTPROFIL
                        || ' ) ';

                    EXECUTE IMMEDIATE V_VAR INTO NCOUNT;
                END IF;

                IF NCOUNT > 0
                THEN
                    IF SLISTPROFIL IS NOT NULL
                    THEN
                        SLISTSELECT :=
                               'SELECT   DISTINCT A.TTPCODE AS COde,
A.TTPLIBELLE AS Displayvalue,
B.TTGFLAGDEFAUT AS DefaultFlag,
B.TTGORDRE AS Ordre
FROM    LANTTRPARAM A,
LKTTPTACTPG B
WHERE  B.TTRNOM = '''
                            || SLISTTRAITNOM
                            || '''
AND     B.TACCODE IN ( '
                            || SLISTACTIVITE
                            || ' )
AND     B.TPGCODE IN ( '
                            || SLISTPROFIL
                            || ' )
AND     A.TTRNOM = B.TTRNOM
AND     A.TTPCODE = B.TTPCODE
AND     A.LANCODE = '''
                            || SLANGUE
                            || '''
AND     ( B.TTGFLAGDEFAUT, B.TTGORDRE  ) = (SELECT  MAX( TTGFLAGDEFAUT ), MIN( TTGORDRE )
FROM    LKTTPTACTPG
WHERE  TTPCODE = B.TTPCODE
AND     TTRNOM = B.TTRNOM
AND     TACCODE IN ( '
                            || SLISTACTIVITE
                            || ' )
AND     TPGCODE IN ( '
                            || SLISTPROFIL
                            || ' ))
ORDER BY 3, 4, 2';
                    ELSE
                        SLISTSELECT :=
                               'SELECT DISTINCT A.TTPCODE AS Code,
A.TTPLIBELLE AS Displayvalue,
B.TTGFLAGDEFAUT AS DefaultFlag,
B.TTGORDRE AS Ordre
FROM   LANTTRPARAM A,
LKTTPTACTPG B
WHERE B.TTRNOM = '''
                            || SLISTTRAITNOM
                            || '''
AND      B.TACCODE IN ( '
                            || SLISTACTIVITE
                            || ' )
AND      A.TTRNOM = B.TTRNOM
AND      A.TTPCODE = B.TTPCODE
AND      A.LANCODE = '''
                            || SLANGUE
                            || '''
AND      ( B.TTGFLAGDEFAUT, B.TTGORDRE  ) = (  SELECT   MAX( TTGFLAGDEFAUT ), MIN( TTGORDRE )
FROM    LKTTPTACTPG
WHERE TTPCODE = B.TTPCODE
AND      TTRNOM = B.TTRNOM
AND      TACCODE IN ( '
                            || SLISTACTIVITE
                            || ' ))
ORDER BY 3, 4, 2 ';
                    END IF;
                ELSE
                    SLISTSELECT := 'SELECT   DISTINCT A.TTPCODE AS Code,
A.TTPLIBELLE AS Displayvalue,
0 AS DefaultFlag,
0 AS Ordre
FROM      LANTTRPARAM A
WHERE     A.TTRNOM = ''' || SLISTTRAITNOM || '''
AND       A.LANCODE = ''' || SLANGUE || '''
ORDER BY 4, 2';
                END IF;

                OPEN PC_RETURN FOR SLISTSELECT;
            END;
        END IF;
    END S_TTR_PARAMBIS;

    PROCEDURE S_UPDATECREVTUTICODES (
        STMOMODULE     IN     CREVT.TMOMODULE%TYPE,
        STEVDEST       IN     CREVT.TEVDEST%TYPE,
        STMFFONCTION   IN     CREVT.TMFFONCTION%TYPE,
        SUSER          IN     CREVT.UTICODECREAT%TYPE,
        NENTITYID             NUMBER,
        SENTPK                VARCHAR2,
        NRETURNCODE    IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            SQUERY   VARCHAR2 (400);
            NCREID   NUMBER;
        BEGIN
            SQUERY :=
                   'SELECT CREID FROM CREVT CRE
WHERE CRE.TMFFONCTION ='''
                || STMFFONCTION
                || '''
AND CRE.TEVDEST ='''
                || STEVDEST
                || '''
AND CRE.TMOMODULE ='''
                || STMOMODULE
                || '''
AND CRE.'
                || SENTPK
                || ' = '
                || NENTITYID
                || '';

            EXECUTE IMMEDIATE SQUERY INTO NCREID;

            PACOM_EVT.U_UTICODECREVT (NCREID,
                                      SUSER,
                                      SUSER,
                                      NULL,
                                      NRETURNCODE);
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            NRETURNCODE := 0;
    END;

    PROCEDURE S_CONTACTNOTEMEMO (NACTID      IN     ACTCONTACT.ACTID%TYPE,
                                 NACTORDRE   IN     ACTCONTACT.ACNORDRE%TYPE,
                                 SUTICODE    IN     UTILISATEUR.UTICODE%TYPE,
                                 SLANGUE     IN     LANGUE.LANCODE%TYPE,
                                 PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT UTI.UTICODE,
                   UTI.UTIPRENOM,
                   UTI.UTINOM,
                   ACT.ACNDTDEB,
                   ACT.ACTID,
                   ACT.ACNORDRE,
                   ACT.ACTIDPARENT,
                   ACT.ACNORDREPARENT,
                   ACT.ACNMEMO
              FROM ACTCONTACT ACT, UTILISATEUR UTI
             WHERE     ACT.ACTID = NACTID
                   AND ACT.ACNORDRE = NACTORDRE
                   AND UTI.UTICODE = ACT.UTICODE
            UNION
            SELECT UTI.UTICODE,
                   UTI.UTIPRENOM,
                   UTI.UTINOM,
                   ACT.ACNDTDEB,
                   ACT.ACTID,
                   ACT.ACNORDRE,
                   ACT.ACTIDPARENT,
                   ACT.ACNORDREPARENT,
                   ACT.ACNMEMO
              FROM ACTCONTACT ACT, UTILISATEUR UTI
             WHERE     ACT.ACTIDPARENT = NACTID
                   AND ACT.ACNORDREPARENT = NACTORDRE
                   AND UTI.UTICODE = ACT.UTICODE
            ORDER BY 4 DESC;
    END S_CONTACTNOTEMEMO;

    PROCEDURE S_CONTACTNOTETOPIC (
        NACTID      IN     ACTCONTACT.ACTID%TYPE,
        NACNODRE    IN     ACTCONTOPIC.ACNORDRE%TYPE,
        SLANGUE     IN     LANGUE.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
        NCOUNT     NUMBER;
        SACNTYPE   ACTCONTACT.ACNTYPE%TYPE;
    BEGIN
        DECLARE
            MAXCOUNT   NUMBER;
        BEGIN
            SELECT PSYMAXSELECT INTO MAXCOUNT FROM PARSYSTEME;

            SELECT MAX (ACNTYPE)
              INTO SACNTYPE
              FROM ACTCONTACT
             WHERE ACTID = NACTID AND ACNORDRE = NACNODRE;

            SELECT COUNT (*)
              INTO NCOUNT
              FROM RELATIONVALEURPROFIL
             WHERE RECCODE = 'CONTACTNOTE' AND REVMERECODE = SACNTYPE;

            OPEN PC_RETURN FOR
                SELECT SEL AS SEL, TOPICLIB
                  FROM (SELECT 1 AS SEL, LAN.TUPLIBELLE AS TOPICLIB
                          FROM ACTCONTOPIC ATO, LANTUSPARAM LAN
                         WHERE     ATO.ACTID = NACTID
                               AND ATO.ACNORDRE = NACNODRE
                               AND LAN.LANCODE = SLANGUE
                               AND LAN.TUSNOM = 'NOTETOPIC'
                               AND LAN.TUPCODE = ATO.ATOTOPIC
                        UNION
                        SELECT 0, LAN1.TUPLIBELLE
                          FROM LANTUSPARAM LAN1
                         WHERE     LAN1.LANCODE = SLANGUE
                               AND LAN1.TUSNOM = 'NOTETOPIC'
                               AND NOT EXISTS
                                       (SELECT 1
                                          FROM ACTCONTOPIC ATO
                                         WHERE     ATO.ACTID = NACTID
                                               AND ATO.ACNORDRE = NACNODRE
                                               AND ATO.ATOTOPIC =
                                                   LAN1.TUPCODE)
                               AND (   NCOUNT = 0
                                    OR (    NCOUNT != 0
                                        AND (EXISTS
                                                 (SELECT 1
                                                    FROM RELATIONVALEURPROFIL
                                                   WHERE     RECCODE =
                                                             'CONTACTNOTE'
                                                         AND REVMERECODE =
                                                             SACNTYPE
                                                         AND REVFILLECODE =
                                                             LAN1.TUSNOM))))
                        ORDER BY SEL DESC)
                 WHERE ROWNUM < MAXCOUNT;
        END;
    END S_CONTACTNOTETOPIC;

    PROCEDURE S_CONTACTNOTE (SENTITYNAME   IN     VARCHAR2,
                             NENTITYID     IN     NUMBER,
                             SUTICODE      IN     UTILISATEUR.UTICODE%TYPE,
                             SLANGUE       IN     LANGUE.LANCODE%TYPE,
                             PC_RETURN     IN OUT T_CURSOR)
    AS
    BEGIN
        PACOM_GENERAL_SHARED.S_CONTACTNOTE (SENTITYNAME,
                                            NENTITYID,
                                            SUTICODE,
                                            SLANGUE,
                                            PC_RETURN);
    END S_CONTACTNOTE;

    PROCEDURE S_GENRETREE (SLANGUE     IN     LANGUE.LANCODE%TYPE,
                           PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT A.TBTCODE TBNPARENT, B.TBNCODE TBNCODE, C.TBNLIBELLE
              FROM TBIENTYPE A, TBTNATURE B, LANTBTNATURE C
             WHERE     A.TBTCODE = B.TBTCODE
                   AND B.TBNCODE = C.TBNCODE
                   AND C.LANCODE = 'EN'
            UNION
            SELECT 'ROOT_TREE_NODE' TBNPARENT,
                   A.TBTCODE        TBNCODE,
                   B.TBTLIBELLE
              FROM TBIENTYPE A, LANTBIENTYPE B
             WHERE     A.TBTCODE = B.TBTCODE
                   AND B.LANCODE = 'EN'
                   AND A.TBTCODE IN
                           (SELECT DISTINCT A.TBTCODE
                              FROM TBIENTYPE A, TBTNATURE B, LANTBTNATURE C
                             WHERE     A.TBTCODE = B.TBTCODE
                                   AND B.TBNCODE = C.TBNCODE
                                   AND C.LANCODE = 'EN')
            ORDER BY TBNPARENT;
    END S_GENRETREE;

    PROCEDURE S_TAXPARENT (P_LANCODE          LANTAXE.LANCODE%TYPE,
                           PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT TAX.TAXCODE CODE, LTAX.TAXLIBELLE DISPLAYVALUE
                  FROM TAXE TAX, LANTAXE LTAX
                 WHERE     TAX.TAXCODE = LTAX.TAXCODE
                       AND LTAX.LANCODE = P_LANCODE;
        END;
    END S_TAXPARENT;

    PROCEDURE S_CMBTBTCODEFEDERATEUR (SLANGUE     IN     LANGUE.LANCODE%TYPE,
                                      PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TBT.TBTCODE AS CODE, TBTLIBELLE AS DISPLAYVALUE
                FROM TBIENTYPE TBT, LANTBIENTYPE LTBT
               WHERE     TBT.TBTCODE = LTBT.TBTCODE
                     AND LTBT.LANCODE = SLANGUE
                     AND TBT.TBTFLAGFEDERATEUR = 1
            ORDER BY TBT.TBTCODE;
    END S_CMBTBTCODEFEDERATEUR;

    PROCEDURE S_CMBTBTCODEFEDERE (
        SGLANGUE          IN     LANGUE.LANCODE%TYPE,
        NNIVHCODEFEDERE   IN     TBIENTYPE.TBTNIVH%TYPE,
        PC_RETURN         IN OUT T_CURSOR)
    AS
    BEGIN
        IF NNIVHCODEFEDERE IS NOT NULL
        THEN
            OPEN PC_RETURN FOR
                  SELECT TBT.TBTCODE AS CODE, TBTLIBELLE AS DISPLAYVALUE
                    FROM TBIENTYPE TBT, LANTBIENTYPE LTBT
                   WHERE     TBT.TBTCODE = LTBT.TBTCODE
                         AND LTBT.LANCODE = SGLANGUE
                         AND TBT.TBTNIVH = NNIVHCODEFEDERE
                ORDER BY TBT.TBTCODE;
        ELSE
            OPEN PC_RETURN FOR
                  SELECT TBT.TBTCODE AS CODE, TBTLIBELLE AS DISPLAYVALUE
                    FROM TBIENTYPE TBT, LANTBIENTYPE LTBT
                   WHERE TBT.TBTCODE = LTBT.TBTCODE AND LTBT.LANCODE = SGLANGUE
                ORDER BY TBT.TBTCODE;
        END IF;
    END S_CMBTBTCODEFEDERE;

    PROCEDURE S_COUNTREGLEFEDERATION (
        SCODEFEDERE       IN     REGLEFEDERATION.TBTCODEFEDERE%TYPE,
        SCODEFEDERATEUR   IN     REGLEFEDERATION.TBTCODEFEDERATEUR%TYPE,
        NCPT              IN OUT NUMBER)
    AS
    BEGIN
        IF SCODEFEDERATEUR IS NOT NULL
        THEN
            SELECT COUNT (*)
              INTO NCPT
              FROM REGLEFEDERATION
             WHERE     TBTCODEFEDERATEUR = SCODEFEDERATEUR
                   AND TBTCODEFEDERE = SCODEFEDERE;
        ELSE
            SELECT COUNT (*)
              INTO NCPT
              FROM REGLEFEDERATION
             WHERE TBTCODEFEDERE = SCODEFEDERE AND TBTCODEFEDERATEUR IS NULL;
        END IF;
    END S_COUNTREGLEFEDERATION;

    PROCEDURE S_METIER (SLISTACTIVITE            LKTUPTACTPG.TACCODE%TYPE,
                        SLISTPROFIL              LKTUPTACTPG.TPGCODE%TYPE,
                        SDOSSECTGESTION          UTITSM.TSMSECTGESTION%TYPE,
                        SLANGUE                  LANTUSPARAM.LANCODE%TYPE,
                        PC_RETURN         IN OUT T_CURSOR)
    AS
    BEGIN
        DECLARE
            NCOUNT   NUMBER := 0;
        BEGIN
            IF (SLISTACTIVITE IS NOT NULL)
            THEN
                IF (SLISTPROFIL IS NOT NULL)
                THEN
                    SELECT COUNT (DISTINCT LTP.TUPCODE)
                      INTO NCOUNT
                      FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                     WHERE     USM.TSMMETIER = LTP.TUPCODE
                           AND LTP.TUPCODE = LKT.TUPCODE
                           AND LTP.TUSNOM = LKT.TUSNOM
                           AND LTP.LANCODE = SLANGUE
                           AND LTP.TUSNOM = 'METIER'
                           AND USM.TSMSECTGESTION = SDOSSECTGESTION
                           AND LKT.TACCODE = SLISTACTIVITE
                           AND LKT.TPGCODE = SLISTPROFIL;
                ELSE
                    SELECT COUNT (DISTINCT LTP.TUPCODE)
                      INTO NCOUNT
                      FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                     WHERE     USM.TSMMETIER = LTP.TUPCODE
                           AND LTP.TUPCODE = LKT.TUPCODE
                           AND LTP.TUSNOM = LKT.TUSNOM
                           AND LTP.LANCODE = SLANGUE
                           AND LTP.TUSNOM = 'METIER'
                           AND USM.TSMSECTGESTION = SDOSSECTGESTION
                           AND LKT.TACCODE = SLISTACTIVITE;
                END IF;
            END IF;

            IF (NCOUNT = 0)
            THEN
                OPEN PC_RETURN FOR
                      SELECT DISTINCT LTP.TUPLIBELLE AS DISPLAYVALUE,
                                      LTP.TUPCODE  AS CODE,
                                      0            AS DEFAULTFLAG,
                                      0            AS OTHERINFO,
                                      0            AS PREFERENCEFLAG
                        FROM UTITSM USM, LANTUSPARAM LTP
                       WHERE     USM.TSMMETIER = LTP.TUPCODE
                             AND LTP.TUSNOM = 'METIER'
                             AND LTP.LANCODE = SLANGUE
                             AND USM.TSMSECTGESTION = SDOSSECTGESTION
                    ORDER BY 1;
            ELSE
                IF (NCOUNT > 0)
                THEN
                    IF (SLISTPROFIL IS NOT NULL)
                    THEN
                        OPEN PC_RETURN FOR
                              SELECT DISTINCT
                                     LTP.TUPLIBELLE  AS DISPLAYVALUE,
                                     LTP.TUPCODE     AS CODE,
                                     LKT.TUGFLAGDEFAUT AS DEFAULTFLAG,
                                     LKT.TUGORDRE    AS OTHERINFO,
                                     1               AS PREFERENCEFLAG
                                FROM UTITSM     USM,
                                     LANTUSPARAM LTP,
                                     LKTUPTACTPG LKT
                               WHERE     USM.TSMMETIER = LTP.TUPCODE
                                     AND LTP.TUPCODE = LKT.TUPCODE
                                     AND LTP.TUSNOM = LKT.TUSNOM
                                     AND LTP.LANCODE = SLANGUE
                                     AND LTP.TUSNOM = 'METIER'
                                     AND USM.TSMSECTGESTION = SDOSSECTGESTION
                                     AND LKT.TACCODE = SLISTACTIVITE
                                     AND LKT.TPGCODE = SLISTPROFIL
                                     AND (LKT.TUGFLAGDEFAUT, LKT.TUGORDRE) =
                                         (SELECT MAX (TUGFLAGDEFAUT),
                                                 MIN (TUGORDRE)
                                            FROM LKTUPTACTPG
                                           WHERE     TUSNOM = LKT.TUSNOM
                                                 AND TACCODE = SLISTACTIVITE
                                                 AND TPGCODE = SLISTPROFIL
                                                 AND TUPCODE = LKT.TUPCODE)
                            ORDER BY 3, 4, 1;
                    ELSE
                        OPEN PC_RETURN FOR
                              SELECT DISTINCT
                                     LTP.TUPLIBELLE  AS DISPLAYVALUE,
                                     LTP.TUPCODE     AS CODE,
                                     LKT.TUGFLAGDEFAUT AS DEFAULTFLAG,
                                     LKT.TUGORDRE    AS OTHERINFO,
                                     1               AS PREFERENCEFLAG
                                FROM UTITSM     USM,
                                     LANTUSPARAM LTP,
                                     LKTUPTACTPG LKT
                               WHERE     USM.TSMMETIER = LTP.TUPCODE
                                     AND LTP.TUPCODE = LKT.TUPCODE
                                     AND LTP.TUSNOM = LKT.TUSNOM
                                     AND LTP.LANCODE = SLANGUE
                                     AND LTP.TUSNOM = 'METIER'
                                     AND USM.TSMSECTGESTION = SDOSSECTGESTION
                                     AND LKT.TACCODE = SLISTACTIVITE
                                     AND (LKT.TUGFLAGDEFAUT, LKT.TUGORDRE) =
                                         (SELECT MAX (TUGFLAGDEFAUT),
                                                 MIN (TUGORDRE)
                                            FROM LKTUPTACTPG
                                           WHERE     TUSNOM = LKT.TUSNOM
                                                 AND TACCODE = SLISTACTIVITE
                                                 AND TUPCODE = LKT.TUPCODE)
                            ORDER BY 3, 4, 1;
                    END IF;
                END IF;
            END IF;
        END;
    END S_METIER;

    PROCEDURE S_METIER_DETAILS (
        SSECTEURGESTION          UTITSM.TSMSECTGESTION%TYPE,
        STSMMETIER               UTITSM.TSMMETIER%TYPE,
        SLANGUE                  LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN         IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT DISTINCT LTP.TUPLIBELLE AS DISPLAYVALUE,
                              LTP.TUPCODE  AS CODE,
                              0            AS DEFAULTFLAG,
                              0            AS OTHERINFO,
                              0            AS PREFERENCEFLAG
                FROM UTITSM USM, LANTUSPARAM LTP
               WHERE     USM.TSMMETIER = LTP.TUPCODE
                     AND LTP.TUSNOM = 'METIER'
                     AND LTP.LANCODE = SLANGUE
                     AND USM.TSMSECTGESTION = SSECTEURGESTION
                     AND USM.TSMMETIER = STSMMETIER
            ORDER BY 1;
    END S_METIER_DETAILS;

    PROCEDURE S_TTR_PARAM_COMBOBOX (
        P_LISTTRAITNOM          LANTTRPARAM.TTRNOM%TYPE,
        P_LISTACTIVITE          LKTTPTACTPG.TACCODE%TYPE,
        P_LISTPROFIL            LKTTPTACTPG.TPGCODE%TYPE,
        P_LANGUE                LANTTRPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        DECLARE
            NCOUNT   NUMBER;
        BEGIN
            IF (P_LISTPROFIL IS NULL)
            THEN
                S_TTR_PARAM_NONE (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
            ELSE
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM LKTTPTACTPG
                 WHERE     TTRNOM = P_LISTTRAITNOM
                       AND TACCODE = P_LISTACTIVITE
                       AND TPGCODE = P_LISTPROFIL;

                IF (NCOUNT > 0)
                THEN
                    S_TTR_PARAM (P_LISTTRAITNOM,
                                 P_LISTACTIVITE,
                                 P_LISTPROFIL,
                                 P_LANGUE,
                                 P_LISTACTIVITE || P_LISTPROFIL,
                                 PC_RETURN);
                ELSE
                    S_TTR_PARAM_NONE (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
                END IF;
            END IF;
        END;
    END S_TTR_PARAM_COMBOBOX;

    PROCEDURE S_TUS_PARAM_COMBOBOX (
        P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
        P_LISTACTIVITE          LKTUPTACTPG.TACCODE%TYPE,
        P_LISTPROFIL            LKTUPTACTPG.TPGCODE%TYPE,
        P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        DECLARE
            NCOUNT   NUMBER;
        BEGIN
            IF (P_LISTPROFIL IS NULL)
            THEN
                S_TUS_PARAM_NONE (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
            ELSE
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM LKTUPTACTPG
                 WHERE     TUSNOM = P_LISTTRAITNOM
                       AND TACCODE = P_LISTACTIVITE
                       AND TPGCODE = P_LISTPROFIL;

                IF (NCOUNT > 0)
                THEN
                    S_TUS_PARAM (P_LISTTRAITNOM,
                                 P_LISTACTIVITE,
                                 P_LISTPROFIL,
                                 P_LANGUE,
                                 P_LISTACTIVITE || P_LISTPROFIL,
                                 PC_RETURN);
                ELSE
                    S_TUS_PARAM_NONE (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
                END IF;
            END IF;
        END;
    END S_TUS_PARAM_COMBOBOX;

    PROCEDURE S_TUS_PARAM_COMBOBOX2 (
        P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
        P_LISTACTIVITE          LKTUPTACTPG.TACCODE%TYPE,
        P_LISTPROFIL            LKTUPTACTPG.TPGCODE%TYPE,
        P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        DECLARE
            NCOUNT   NUMBER;
        BEGIN
            IF (P_LISTPROFIL IS NULL)
            THEN
                S_TUS_PARAM_NONE2 (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
            ELSE
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM LKTUPTACTPG
                 WHERE     TUSNOM = P_LISTTRAITNOM
                       AND TACCODE = P_LISTACTIVITE
                       AND TPGCODE = P_LISTPROFIL;

                IF (NCOUNT > 0)
                THEN
                    S_TUS_PARAM2 (P_LISTTRAITNOM,
                                  P_LISTACTIVITE,
                                  P_LISTPROFIL,
                                  P_LANGUE,
                                  P_LISTACTIVITE || P_LISTPROFIL,
                                  PC_RETURN);
                ELSE
                    S_TUS_PARAM_NONE2 (P_LISTTRAITNOM, P_LANGUE, PC_RETURN);
                END IF;
            END IF;
        END;
    END S_TUS_PARAM_COMBOBOX2;

    PROCEDURE S_TUS_PARAM_FLAGORFI (
        P_LISTTRAITNOM          TUSPARAM.TUSNOM%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT TUS.TUPCODE, TUS.TUPFLAGORFI
                             FROM TUSPARAM TUS
                            WHERE TUS.TUSNOM = P_LISTTRAITNOM;
    END S_TUS_PARAM_FLAGORFI;

    PROCEDURE S_GETNAPTYPE (SNAPCODE   IN     NAP.NAPCODE%TYPE,
                            SPAYCODE   IN     NAP.PAYCODE%TYPE,
                            SNAPTYPE      OUT NAP.NAPTYPE%TYPE)
    AS
    BEGIN
        BEGIN
            SELECT NAPTYPE
              INTO SNAPTYPE
              FROM NAP
             WHERE NAPCODE = SNAPCODE AND PAYCODE = SPAYCODE;
        END;
    END S_GETNAPTYPE;

    PROCEDURE S_LISTE_LIMITES (P_LANCODE            LANGUE.LANCODE%TYPE,
                               P_TTRNOM             TTRPARAM.TTRNOM%TYPE,
                               PC_RESULTAT   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RESULTAT FOR
            SELECT 'LIMITE'    CODE_PARAMETRE,
                   TTP.TTPCODE CODE_VALEUR,
                   TTPLIBELLE  LIBELLE,
                   1           FLAG_VISIBLE
              FROM TTRPARAM TTP, LANTTRPARAM LTP
             WHERE     LANCODE = P_LANCODE
                   AND TTP.TTRNOM = P_TTRNOM
                   AND LTP.TTRNOM = TTP.TTRNOM
                   AND LTP.TTPCODE = TTP.TTPCODE;
    END S_LISTE_LIMITES;

    PROCEDURE S_INTERVENANT (P_UTICODE                     UTILISATEUR.UTICODE%TYPE,
                             PC_LISTE_INTERVENANT   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_LISTE_INTERVENANT FOR
            SELECT UTI.UTICODE,
                   UTI.UTINOM,
                   UTI.UTIPRENOM,
                   UTI.UTITELECOM,
                   UTI.GROCODE,
                   --     DECODE( PA_SELECTPARAMPROFIL.F_ESTVISIBLE( 0, 'AVUTILISATEUR', UTI.UTICODE ),
                   --       0,
                   --       1,
                   --       1,
                   --       NVL( UTI.UTIFLAGINACTIF, 0 ) ) UTIFLAGINACTIF,
                   NVL (UTI.UTIFLAGINACTIF, 0) UTIFLAGINACTIF,
                   UTINOM || ' ' || UTIPRENOM  NOM_COMPLET,
                   NULL                        TSMSECTGESTION
              FROM UTILISATEUR UTI
             WHERE UTI.UTICODE = P_UTICODE;
    END S_INTERVENANT;

    PROCEDURE S_DEPARTMENT (P_LANCODE          LANDEPARTMENT.LANCODE%TYPE,
                            PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT DEPT.DPTCODE CODE, LDEPT.DPTLABEL DISPLAYVALUE
                  FROM DEPARTMENT DEPT, LANDEPARTMENT LDEPT
                 WHERE     DEPT.DPTCODE = LDEPT.DPTCODE
                       AND LDEPT.LANCODE = P_LANCODE;
        END;
    END S_DEPARTMENT;

    PROCEDURE S_CMBDPTPOST (PC_RETURN IN OUT T_CURSOR)
    AS
        SUGECODE   UTILISATEUR.UGECODE%TYPE := F_GETCURRENTUGECODE;
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT DPT.DPTCODE AS CODE, LDPT.DPTLABEL AS DISPLAYVALUE
                  FROM LANDEPARTMENT LDPT, DEPARTMENT DPT, TOPPARAM TPA
                 WHERE     DPT.DPTSTATUS = '01'
                       AND TPA.TOPTABLE = 'CASE'
                       AND TPAPARAM = 'COLCENPOST'
                       AND DPT.DPTTYPE = TPA.TPATEXTE
                       AND DPT.DPTCODE = LDPT.DPTCODE
                       AND TPA.UGECODE = SUGECODE;
        END;
    END S_CMBDPTPOST;

    PROCEDURE S_DEPTPOSITIONS (SLANGUE     IN     VARCHAR2,
                               SDPTCODE           DEPARTMENT.DPTCODE%TYPE,
                               PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT TSM.TSMMETIER CODE, LTUS2.TUPLIBELLE DISPLAYVALUE
              FROM TSECTEURMETIER TSM, DEPARTMENT DEP, LANTUSPARAM LTUS2
             WHERE     DEP.DPTCODE = TSM.TSMSECTGESTION
                   AND LTUS2.TUSNOM = 'METIER'
                   AND LTUS2.TUPCODE = TSM.TSMMETIER
                   AND LTUS2.LANCODE = SLANGUE
                   AND (DEP.DPTCODE = SDPTCODE OR SDPTCODE IS NULL);
    END S_DEPTPOSITIONS;

    PROCEDURE S_DEPARTMENT_BY_TYPE (
        P_LANCODE          LANDEPARTMENT.LANCODE%TYPE,
        P_DPTTYPE          DEPARTMENT.DPTTYPE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT DEPT.DPTCODE CODE, LDEPT.DPTLABEL DISPLAYVALUE
                  FROM DEPARTMENT DEPT, LANDEPARTMENT LDEPT
                 WHERE     DEPT.DPTCODE = LDEPT.DPTCODE
                       AND LDEPT.LANCODE = P_LANCODE
                       AND DEPT.DPTTYPE = P_DPTTYPE;
        END;
    END S_DEPARTMENT_BY_TYPE;

    PROCEDURE S_ROLE (P_LANCODE          LANROLE.LANCODE%TYPE,
                      PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT R.ROLCODE CODE, LR.ROLLIBELLE DISPLAYVALUE
                  FROM ROLE R, LANROLE LR
                 WHERE R.ROLCODE = LR.ROLCODE AND LR.LANCODE = P_LANCODE;
        END;
    END S_ROLE;

    PROCEDURE S_LISTE_COUNSTYPE (
        NACTID      IN     ACTEUR.ACTID%TYPE,
        SACOORDRE   IN     LKACTTELCOR.ACOORDRE%TYPE,
        SLANGUE     IN     LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TUPCODE  CODE,
                     TUPLIBELLE DISPLAYVALUE,
                     ATE.ATENUM OTHERINFO
                FROM LKACTTELCOR ATC, ACTTELECOM ATE, LANTUSPARAM
               WHERE     LANCODE = SLANGUE
                     AND ATE.ACTID = NACTID
                     AND ATC.ACOORDRE = SACOORDRE
                     AND ATC.ACTID = ATE.ACTID
                     AND TUSNOM = 'TELTYPE'
                     AND TUPCODE = ATE.ATETYPE
                     AND ATC.ATEORDRE = ATE.ATEORDRE
            ORDER BY ATE.ATETYPE;
    END S_LISTE_COUNSTYPE;

    PROCEDURE S_PARENTCODE (SUGECODE    IN     DOCUMENT.UGECODE%TYPE,
                            PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT DOCID CODEINTEGER, DOCCODE DISPLAYVALUE
                             FROM DOCUMENT
                            WHERE UGECODE = SUGECODE;
    END S_PARENTCODE;

    PROCEDURE S_LANDOCPARAM_NAME (SDOCID      IN     LANDOCPARAM.DOCID%TYPE,
                                  SLANCODE    IN     LANDOCPARAM.LANCODE%TYPE,
                                  PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT DPALIBELLE AS CODE, DPALIBELLE AS DISPLAYVALUE
              FROM LANDOCPARAM
             WHERE DOCID = SDOCID AND LANCODE = SLANCODE;
    END S_LANDOCPARAM_NAME;

    PROCEDURE S_LANDOCPARAM_TYPE (SDOCID      IN     LANDOCPARAM.DOCID%TYPE,
                                  SLANCODE    IN     LANDOCPARAM.LANCODE%TYPE,
                                  PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT
                   DPATYPEPARAM AS CODE, DPATYPEPARAM AS DISPLAYVALUE
              FROM DOCPARAM
             WHERE DOCID = SDOCID;
    END S_LANDOCPARAM_TYPE;

    PROCEDURE S_LANDOCPARAM (SDOCID      IN     DOCPARAM.DOCID%TYPE,
                             SLANCODE    IN     LANDOCPARAM.LANCODE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LDP.DPALIBELLE,
                   DP.DOCID,
                   DP.DPAORDER,
                   DP.DPATYPEPARAM,
                   DP.DPALENGTH,
                   DP.DPAFLAGMANDATORY
              FROM DOCPARAM DP, LANDOCPARAM LDP
             WHERE     LDP.DOCID = DP.DOCID
                   AND LDP.DPAORDER = DP.DPAORDER
                   AND DP.DOCID = SDOCID
                   AND LDP.LANCODE = SLANCODE;
    END S_LANDOCPARAM;

    PROCEDURE S_MASTERLABEL (SLANCODE    IN     LANEDIREQUETE.LANCODE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT EDI.ERECODE || '-' || LED.ERELIBELLE AS DISPLAYVALUE,
                     EDI.ERECODE                        AS CODE
                FROM EDIREQUETE EDI, LANEDIREQUETE LED
               WHERE EDI.ERECODE = LED.ERECODE AND LED.LANCODE = SLANCODE
            ORDER BY DISPLAYVALUE;
    END S_MASTERLABEL;

    PROCEDURE S_GET_ACTIF_APPLICATION (NCOUNT OUT NUMBER)
    AS
    BEGIN
        SELECT COUNT (*)
          INTO NCOUNT
          FROM USER_TABLES
         WHERE TABLE_NAME = 'UTILISATEUR';
    END S_GET_ACTIF_APPLICATION;

    PROCEDURE S_LANDOCUMENT (SLANGUE     IN     LANDOCUMENT.LANCODE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DOC.DOCID          AS CODEDOUBLE,
                   LDO.DOCLIBELLE     AS DISPLAYVALUE,
                   LDO.DOCDESCRIPTION AS OTHERINFO
              FROM DOCUMENT DOC, LANDOCUMENT LDO
             WHERE     DOC.DOCID = LDO.DOCID
                   AND DOC.DOCCONTEXTE = 'TEMPLATE'
                   AND LANCODE = SLANGUE;
    END S_LANDOCUMENT;

    PROCEDURE S_GET_ACTOR_DETAILS (
        NACTID         IN     ACTEUR.ACTID%TYPE,
        SACTCODE       IN OUT ACTEUR.ACTCODE%TYPE,
        SACTLIBCOURT   IN OUT ACTEUR.ACTLIBCOURT%TYPE)
    AS
    BEGIN
        SELECT ACTCODE, ACTLIBCOURT
          INTO SACTCODE, SACTLIBCOURT
          FROM ACTEUR
         WHERE ACTID = NACTID;
    END S_GET_ACTOR_DETAILS;

    PROCEDURE S_TUSPARAM_COMPANY (P_TUSNOM             TUSPARAM.TUSNOM%TYPE,
                                  P_LANCODE            LANGUE.LANCODE%TYPE,
                                  PC_TUSPARAM   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_TUSPARAM FOR
            SELECT DISTINCT
                   LAN.TUPCODE AS CODE, LAN.TUPLIBELLE AS DISPLAYVALUE
              FROM LANTUSPARAM LAN
             WHERE LAN.TUSNOM = P_TUSNOM AND LAN.LANCODE = P_LANCODE;
    END S_TUSPARAM_COMPANY;

    PROCEDURE S_TUS_PARAM_TUPFLAGORFI (
        P_LISTTRAITNOM          LANTUSPARAM.TUSNOM%TYPE,
        P_LANGUE                LANTUSPARAM.LANCODE%TYPE,
        P_FLAGORFI              TUSPARAM.TUPFLAGORFI%TYPE,
        PC_RETURN        IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT LAN.TUPCODE  CODE,
                     LAN.TUPLIBELLE DISPLAYVALUE,
                     0            DEFAULTFLAG,
                     0            PREFERENCEFLAG,
                     0            OTHERINFO2
                FROM LANTUSPARAM LAN, TUSPARAM TUS
               WHERE     TUS.TUSNOM = P_LISTTRAITNOM
                     AND TUS.TUPFLAGORFI = P_FLAGORFI
                     AND LAN.TUSNOM = TUS.TUSNOM
                     AND LAN.TUPCODE = TUS.TUPCODE
                     AND LANCODE = P_LANGUE
            ORDER BY TUPLIBELLE;
    END S_TUS_PARAM_TUPFLAGORFI;

    PROCEDURE S_CMB_PROFIL (P_LANGUE           LANTUSPARAM.LANCODE%TYPE,
                            PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT PGE.TPGCODE AS CODE, LPG.TPGLIBELLE AS DISPLAYVALUE
              FROM TPROFILGESTION PGE, LANTPROFILGESTION LPG
             WHERE     LPG.LANCODE = P_LANGUE
                   AND PGE.TPGCODE = LPG.TPGCODE
                   AND PGE.TPGDTFIN IS NULL;
    END S_CMB_PROFIL;

    PROCEDURE S_CMB_PROFIL_BY_COLID (
        P_COLID            COLTPG.COLID%TYPE,
        P_LANGUE           LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT
                   CTP.COLID      AS OTHERINFO,
                   CTP.TPGCODE    AS CODE,
                   TPE.TPGLIBELLE AS DISPLAYVALUE
              FROM COLTPG CTP, LANTPROFILGESTION TPE
             WHERE CTP.TPGCODE = TPE.TPGCODE AND CTP.COLID = P_COLID;
    END S_CMB_PROFIL_BY_COLID;

    PROCEDURE S_CMB_PROFIL_BY_TCTCODE (
        P_TCTCODE          LKTPGTCT.TCTCODE%TYPE,
        P_LANGUE           LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT
                   LKT.TCTCODE    AS OTHERINFO,
                   LKT.TPGCODE    AS CODE,
                   TPE.TPGLIBELLE AS DISPLAYVALUE
              FROM LKTPGTCT LKT, LANTPROFILGESTION TPE
             WHERE LKT.TPGCODE = TPE.TPGCODE AND LKT.TCTCODE = P_TCTCODE;
    END S_CMB_PROFIL_BY_TCTCODE;

    PROCEDURE S_COL_DLI_IMPUTANA (
        P_UGECODE          RUBRIQUE.UGECODE%TYPE,
        P_LANGUE           LANTUSPARAM.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT RUB.RUBID AS CODEINTEGER, LRB.RUBLIBELLE AS DISPLAYVALUE
                FROM LANRUBRIQUE LRB, RUBRIQUE RUB
               WHERE     LRB.RUBID = RUB.RUBID
                     AND RUB.UGECODE = P_UGECODE
                     AND LANCODE = P_LANGUE
            ORDER BY 2;
    END S_COL_DLI_IMPUTANA;

    PROCEDURE S_CRITERIA_TYPE_AND_ORDER (
        P_TMFFONCTION          TEVCRITERIA.TMOMODULE%TYPE,
        P_CRITERIA             TEVCRITERIA.TCRFILTER%TYPE,
        PC_RETURN       IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TCRORDER, TCRTYPECRI
              FROM TEVCRITERIA
             WHERE     TEVDEST = 'EVTGLOB'
                   AND TACCODE = 'GLOBAL'
                   AND TMOMODULE = 'EVTGLOB'
                   AND TMFFONCTION = P_TMFFONCTION
                   AND TCRFILTER = P_CRITERIA;
    END S_CRITERIA_TYPE_AND_ORDER;

    PROCEDURE S_BIMSECTEURGESTION (
        SLISTACTIVITE   IN     VARCHAR2,
        SLISTPROFIL     IN     LKTUPTACTPG.TPGCODE%TYPE,
        SGUSER          IN     UTITSM.UTICODE%TYPE,
        SLANGUE                LANROLE.LANCODE%TYPE,
        PC_RETURN       IN OUT T_CURSOR)
    AS
        NCOUNT   NUMBER := 0;
    BEGIN
        IF SLISTACTIVITE IS NOT NULL AND SLISTACTIVITE != ''
        THEN
            IF SLISTPROFIL IS NOT NULL AND SLISTPROFIL != ''
            THEN
                SELECT COUNT (DISTINCT LTP.TUPCODE)
                  INTO NCOUNT
                  FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                 WHERE     USM.UTICODE = SGUSER
                       AND LTP.TUSNOM = 'BIMSECTEURGEST'
                       AND LTP.LANCODE = SLANGUE
                       AND USM.TSMSECTGESTION = LTP.TUPCODE
                       AND TACCODE IN (SLISTACTIVITE)
                       AND LKT.TUPCODE = LTP.TUPCODE
                       AND LKT.TUSNOM = LTP.TUSNOM
                       AND LKT.TPGCODE IN (SLISTPROFIL);
            ELSE
                SELECT COUNT (DISTINCT LTP.TUPCODE)
                  INTO NCOUNT
                  FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                 WHERE     USM.UTICODE = SGUSER
                       AND LTP.TUSNOM = 'BIMSECTEURGEST'
                       AND LTP.LANCODE = SLANGUE
                       AND USM.TSMSECTGESTION = LTP.TUPCODE
                       AND TACCODE IN (SLISTACTIVITE)
                       AND LKT.TUPCODE = LTP.TUPCODE
                       AND LKT.TUSNOM = LTP.TUSNOM;
            END IF;
        END IF;

        IF NCOUNT IS NULL OR NCOUNT = 0
        THEN
            OPEN PC_RETURN FOR
                  SELECT DISTINCT LTP.TUPLIBELLE AS DISPLAYVALUE,
                                  LTP.TUPCODE  AS CODE,
                                  0            AS DEFAULTFLAG,
                                  0            AS ORD
                    FROM UTITSM USM, LANTUSPARAM LTP
                   WHERE     USM.UTICODE = SGUSER
                         AND LTP.TUSNOM = 'BIMSECTEURGEST'
                         AND LTP.LANCODE = SLANGUE
                         AND USM.TSMSECTGESTION = LTP.TUPCODE
                ORDER BY 1;
        ELSE
            IF NCOUNT > 0
            THEN
                IF SLISTPROFIL IS NOT NULL AND SLISTPROFIL != ''
                THEN
                    OPEN PC_RETURN FOR
                          SELECT DISTINCT LTP.TUPLIBELLE  AS DISPLAYVALUE,
                                          LTP.TUPCODE     AS CODE,
                                          LKT.TUGFLAGDEFAUT AS DEFAULTFLAG,
                                          LKT.TUGORDRE    AS ORD
                            FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                           WHERE     USM.UTICODE = SGUSER
                                 AND LTP.TUSNOM = 'BIMSECTEURGEST'
                                 AND LTP.LANCODE = SLANGUE
                                 AND USM.TSMSECTGESTION = LTP.TUPCODE
                                 AND TACCODE IN (SLISTACTIVITE)
                                 AND LKT.TUPCODE = LTP.TUPCODE
                                 AND LKT.TUSNOM = LTP.TUSNOM
                                 AND LKT.TPGCODE IN (SLISTPROFIL)
                                 AND (LKT.TUGFLAGDEFAUT, LKT.TUGORDRE) =
                                     (SELECT MAX (TUGFLAGDEFAUT),
                                             MIN (TUGORDRE)
                                        FROM LKTUPTACTPG
                                       WHERE     TUSNOM = LKT.TUSNOM
                                             AND TACCODE IN (SLISTACTIVITE)
                                             AND TUPCODE = LKT.TUPCODE
                                             AND TPGCODE IN (SLISTPROFIL))
                        ORDER BY 3, 4, 1;
                ELSE
                    OPEN PC_RETURN FOR
                          SELECT DISTINCT LTP.TUPLIBELLE  AS DISPLAYVALUE,
                                          LTP.TUPCODE     AS CODE,
                                          LKT.TUGFLAGDEFAUT AS DEFAULTFLAG,
                                          LKT.TUGORDRE    AS ORD
                            FROM UTITSM USM, LANTUSPARAM LTP, LKTUPTACTPG LKT
                           WHERE     USM.UTICODE = SGUSER
                                 AND LTP.TUSNOM = 'BIMSECTEURGEST'
                                 AND LTP.LANCODE = SLANGUE
                                 AND USM.TSMSECTGESTION = LTP.TUPCODE
                                 AND TACCODE IN (SLISTACTIVITE)
                                 AND LKT.TUPCODE = LTP.TUPCODE
                                 AND LKT.TUSNOM = LTP.TUSNOM
                                 AND (LKT.TUGFLAGDEFAUT, LKT.TUGORDRE) =
                                     (SELECT MAX (TUGFLAGDEFAUT),
                                             MIN (TUGORDRE)
                                        FROM LKTUPTACTPG
                                       WHERE     TUSNOM = LKT.TUSNOM
                                             AND TACCODE IN (SLISTACTIVITE)
                                             AND TUPCODE = LKT.TUPCODE)
                        ORDER BY 3, 4, 1;
                END IF;
            END IF;
        END IF;
    END S_BIMSECTEURGESTION;

    PROCEDURE S_LISTE_UTICOORDONNEE (
        P_LANCODE                       LANGUE.LANCODE%TYPE,
        P_UTICODE                       UTICOORDONNEE.UTICODE%TYPE,
        PC_LISTE_UTICOORDONNEE   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_LISTE_UTICOORDONNEE FOR
            SELECT UTICODE,
                   UCOORDRE,
                   UCOTYPE,
                   UCOREFERENCE,
                   1
                       FLAG_MODIFIABLE,
                   PA_AVCOMMUN.F_LANTUSPARAM (P_LANCODE, 'TELTYPE', UCOTYPE)
                       LIBELLE_TYPE
              FROM UTICOORDONNEE
             WHERE UTICODE = P_UTICODE
            UNION
            SELECT UTICODE,
                   0,
                   'TEL',
                   UTITELECOM,
                   0
                       FLAG_MODIFIABLE,
                   PA_AVCOMMUN.F_LANTUSPARAM (P_LANCODE, 'TELTYPE', 'TEL')
                       LIBELLE_TYPE
              FROM UTILISATEUR
             WHERE UTICODE = P_UTICODE AND UTITELECOM IS NOT NULL;
    END S_LISTE_UTICOORDONNEE;

    FUNCTION F_GETFORLIBELLE (NFORID     IN LANFORMALITE.FORID%TYPE,
                              SLANCODE   IN LANFORMALITE.LANCODE%TYPE)
        RETURN LANFORMALITE.FORLIBELLE%TYPE
    AS
    BEGIN
        DECLARE
            SFORLIBELLE   LANFORMALITE.FORLIBELLE%TYPE;
        BEGIN
            IF (NFORID IS NOT NULL) AND (SLANCODE IS NOT NULL)
            THEN
                BEGIN
                    SELECT FORLIBELLE
                      INTO SFORLIBELLE
                      FROM LANFORMALITE
                     WHERE FORID = NFORID AND LANCODE = SLANCODE;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        SFORLIBELLE := NULL;
                END;
            END IF;

            RETURN SFORLIBELLE;
        END;
    END F_GETFORLIBELLE;

    FUNCTION GETFLAG_TMOYENPMTENC (P_LANCODE    LANPAYS.LANCODE%TYPE,
                                   P_TMPCODE    DPRACTEUR.TMPCODEENC%TYPE)
        RETURN NUMBER
    AS
        NFLAG   NUMBER := 0;
    BEGIN
        SELECT MAX (TMPFLAGRIB)
          INTO NFLAG
          FROM TMOYENPMT A, LANTMOYENPMT B
         WHERE     B.TMPCODE(+) = A.TMPCODE
               AND LANCODE(+) = P_LANCODE
               AND A.TMPCODE = P_TMPCODE;

        RETURN NFLAG;
    END GETFLAG_TMOYENPMTENC;

    FUNCTION GETFLAG_TMOYENPMTDEC (P_LANCODE    LANPAYS.LANCODE%TYPE,
                                   P_TMPCODE    DPRACTEUR.TMPCODEDEC%TYPE)
        RETURN NUMBER
    AS
        NFLAG   NUMBER := 0;
    BEGIN
        SELECT MAX (TMPFLAGRIB)
          INTO NFLAG
          FROM TMOYENPMT A, LANTMOYENPMT B
         WHERE     B.TMPCODE(+) = A.TMPCODE
               AND LANCODE(+) = P_LANCODE
               AND A.TMPCODE = P_TMPCODE;

        RETURN NFLAG;
    END GETFLAG_TMOYENPMTDEC;

    PROCEDURE S_CMBITRRUBTITLEPHASE (SPHASEDEST   IN     VARCHAR2,
                                     SLANGUE      IN     VARCHAR2,
                                     PC_RETURN    IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LPH.PHACODE AS CODE, LPH.PHALIBELLE AS DISPLAYVALUE
              FROM PHASE PHA, LANPHASE LPH
             WHERE     PHA.PHADEST = SPHASEDEST
                   AND PHA.PHADEST = LPH.PHADEST
                   AND PHA.PHACODE = LPH.PHACODE
                   AND LPH.LANCODE = SLANGUE;
    END S_CMBITRRUBTITLEPHASE;

    PROCEDURE S_ITRGEN_PHA_CMBITRRUBJALON (
        SPHASEDEST   IN     VARCHAR2,
        SPHACODE     IN     PHAJAL.PHACODE%TYPE,
        SLANGUE      IN     VARCHAR2,
        PC_RETURN    IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LJA.JALCODE AS CODE, LJA.JALLIBELLE AS DISPLAYVALUE
              FROM PHAJAL PHJ, LANJALON LJA
             WHERE     LJA.LANCODE = SLANGUE
                   AND PHJ.JALCODE = LJA.JALCODE
                   AND (PHJ.PHACODE IS NULL OR PHJ.PHACODE = SPHACODE)
                   AND PHJ.PHADEST = SPHASEDEST;
    END S_ITRGEN_PHA_CMBITRRUBJALON;

    PROCEDURE S_ITRGEN_CMBITRRUBJALON (SPHASEDEST   IN     VARCHAR2,
                                       SLANGUE      IN     VARCHAR2,
                                       PC_RETURN    IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT LJA.JALCODE AS CODE, LJA.JALLIBELLE AS DISPLAYVALUE
              FROM PHAJAL PHJ, LANJALON LJA
             WHERE     LJA.LANCODE = SLANGUE
                   AND PHJ.JALCODE = LJA.JALCODE
                   AND PHJ.PHADEST = SPHASEDEST;
    END S_ITRGEN_CMBITRRUBJALON;

    PROCEDURE S_ACTIVITY (SLANGUE     IN     LANGUE.LANCODE%TYPE,
                          PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT TAC.TACCODE AS CODE, LTA.TACLIBELLE AS DISPLAYVALUE
                FROM TACTIVITE TAC
                     INNER JOIN LANTACTIVITE LTA ON TAC.TACCODE = LTA.TACCODE
               WHERE LTA.LANCODE = SLANGUE
            ORDER BY LTA.TACLIBELLE;
    END S_ACTIVITY;

    PROCEDURE S_INVESTCOMPANY (SLANGUE     IN     LANGUE.LANCODE%TYPE,
                               PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT ACT.ACTID AS CODEINTEGER, ACT.ACTLIBCOURT AS DISPLAYVALUE
                FROM ACTEURGESTION AGE
                     INNER JOIN ACTEUR ACT ON ACT.ACTID = AGE.ACTID
               WHERE ACT.UGECODE = '_ORIG_' -- TODO: SYC version provisoire pour test RESTAT-323
            ORDER BY ACT.ACTLIBCOURT;
    END S_INVESTCOMPANY;

    PROCEDURE P_ESUFUNCNAMECONV (nEntityId                   NUMBER,
                                 SLANGUE              IN     LANGUE.LANCODE%TYPE,
                                 SUTICODE             IN     UTILISATEUR.UTICODE%TYPE,
                                 SESUFUNCNAME         IN     VARCHAR2,
                                 SESUFUNCRETURNTYPE   IN     VARCHAR2,
                                 SFUNCCONV               OUT VARCHAR2,
                                 SFUNTYPE                OUT VARCHAR2,
                                 SSTRRETURN              OUT VARCHAR2,
                                 NNUMRETURN              OUT NUMBER,
                                 DDATERETURN             OUT DATE)
    AS
    BEGIN
        DECLARE
            SFUNCCONV   VARCHAR2 (100);
            nInx        NUMBER := 0;
            sQuery      VARCHAR2 (100) := NULL;
        BEGIN
            IF SESUFUNCNAME IS NOT NULL
            THEN
                SFUNTYPE := 'SQL';
                SFUNCCONV := SESUFUNCNAME;
                nInx := INSTR ('SESUFUNCNAME', '.');

                IF nInx > 0
                THEN
                    SFUNCCONV :=
                        SUBSTR (SESUFUNCNAME,
                                nInx + 1,
                                LENGTH (SESUFUNCNAME));
                END IF;

                SFUNCCONV := UPPER (SFUNCCONV);

                IF INSTR (SFUNCCONV, 'F_') <= 0
                THEN
                    SFUNTYPE := 'JAVA';
                    SFUNCCONV := INITCAP (SFUNCCONV);
                END IF;
            END IF;

            IF SFUNTYPE = 'SQL'
            THEN
                IF    SESUFUNCRETURNTYPE = 'STRING'
                   OR SESUFUNCRETURNTYPE = 'VARCHAR2'
                THEN
                    sQuery :=
                           ' SELECT '
                        || SESUFUNCNAME
                        || '( '
                        || nEntityId
                        || ' , '''
                        || SLANGUE
                        || ''' , '''
                        || SUTICODE
                        || ''')  INTO '
                        || SSTRRETURN
                        || ' FROM DUAL ';
                ELSIF    SESUFUNCRETURNTYPE = 'NUMBER'
                      OR SESUFUNCRETURNTYPE = 'INTEGER'
                      OR SESUFUNCRETURNTYPE = 'DOUBLE'
                THEN
                    sQuery :=
                           ' SELECT '
                        || SESUFUNCNAME
                        || '( '
                        || nEntityId
                        || ' , '''
                        || SLANGUE
                        || ''' , '''
                        || SUTICODE
                        || ''')  INTO '
                        || NNUMRETURN
                        || ' FROM DUAL ';
                ELSIF SESUFUNCRETURNTYPE = 'DATE'
                THEN
                    sQuery :=
                           ' SELECT '
                        || SESUFUNCNAME
                        || '( '
                        || nEntityId
                        || ' , '''
                        || SLANGUE
                        || ''' , '''
                        || SUTICODE
                        || ''')  INTO '
                        || DDATERETURN
                        || ' FROM DUAL ';
                END IF;
            END IF;
        END;
    END P_ESUFUNCNAMECONV;

    PROCEDURE S_SUMMARYITEMS (
        NESUFLAGLIST          ENTSUMMARY.ESUFLAGLIST%TYPE,
        NCKEID                LKESUCKE.CKEID%TYPE,
        SENTCODE              ENTSUMMARY.ENTCODE%TYPE,
        PC_RETURN      IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT ES.ESULOCALIZATIONKEY,
                     ES.ESUFUNCNAME,
                     ES.ESUFUNCRETURNTYPE,
                     ES.ESUSECURITYKEY,
                     ES.ESUGROUP,
                     ES.COMTYPE,
                     ES.ESUCONTENTSTYLE,
                     ES.ESULABELSTYLE
                FROM LKESUCKE LKE, ENTSUMMARY ES
               WHERE     LKE.ENTCODE = ES.ENTCODE
                     AND LKE.ESULOCALIZATIONKEY = ES.ESULOCALIZATIONKEY
                    AND LKE.ECKFLAGHIDDEN = '0'
                     AND LKE.CKEID = NCKEID
                     AND ES.ENTCODE = SENTCODE
                     AND (   NESUFLAGLIST IS NULL
                          OR NESUFLAGLIST = 0
                          OR ES.ESUFLAGLIST = NESUFLAGLIST)
            ORDER BY LKE.ECKORDRE;
    END S_SUMMARYITEMS;

    PROCEDURE S_SUMMARYCRITERIA (SKEYS              VARCHAR2,
                                 SENTCODE           ENTSUMMARY.ENTCODE%TYPE,
                                 PC_RETURN   IN OUT T_CURSOR)
    AS
        sQuery   VARCHAR2 (10000);
    BEGIN
        sQuery :=
               'SELECT   ENTCODE, ESULOCALIZATIONKEY, ESCORDER , ESCOPERATOR, ESCLOWERLIMIT , ESCUPPERLIMIT, ESCLABELSTYLE ,ESCCONTENTSTYLE FROM ENTSUMSTYLECRITERIA WHERE ESULOCALIZATIONKEY IN ( '
            || SKEYS
            || ') AND ENTCODE = '''
            || SENTCODE
            || '''';

        OPEN PC_RETURN FOR sQuery;
    END S_SUMMARYCRITERIA;

    FUNCTION F_SUMMARYITEMS_COUNT (
        NESUFLAGLIST    ENTSUMMARY.ESUFLAGLIST%TYPE,
        NCKEID          LKESUCKE.CKEID%TYPE,
        SENTCODE        ENTSUMMARY.ENTCODE%TYPE)
        RETURN NUMBER
    AS
        NCOUNT   NUMBER := 0;
    BEGIN
        SELECT COUNT (*)
          INTO NCOUNT
          FROM LKESUCKE LKE, ENTSUMMARY ES
         WHERE     LKE.ENTCODE = ES.ENTCODE
               AND LKE.ESULOCALIZATIONKEY = ES.ESULOCALIZATIONKEY
               AND LKE.ECKFLAGHIDDEN = '0'
               AND LKE.CKEID = NCKEID
               AND ES.ENTCODE = SENTCODE
               AND (   NESUFLAGLIST IS NULL
                    OR NESUFLAGLIST = 0
                    OR ES.ESUFLAGLIST = NESUFLAGLIST);

        RETURN NCOUNT;
    END F_SUMMARYITEMS_COUNT;

    FUNCTION F_CLIENT_CODE (p_dealId DOSSIERPROSPECT.DOSID%TYPE)
        RETURN ACTEUR.ACTCODE%TYPE
    AS
        l_clientCode   ACTEUR.ACTCODE%TYPE;
    BEGIN
        SELECT MAX (actor.ACTCODE)
          INTO l_clientCode
          FROM ACTEUR  actor
               LEFT JOIN DPRACTEUR client
                   ON     client.ACTID = actor.ACTID
                      AND client.ROLCODE IN ('CLIENT', 'EMPRUNT', 'PARTEN')
         WHERE client.DOSID = p_dealId;

        RETURN l_clientCode;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_CLIENT_CODE;

    FUNCTION F_DEAL_NUMBER (p_dealId DOSSIERPROSPECT.DOSID%TYPE)
        RETURN DOSSIERPROSPECT.DPRNUMERO%TYPE
    AS
        l_dealNum   DOSSIERPROSPECT.DPRNUMERO%TYPE;
    BEGIN
        SELECT DISTINCT DPRNUMERO
          INTO l_dealNum
          FROM DOSSIERPROSPECT
         WHERE DOSID = p_dealId;

        RETURN l_dealNum;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_DEAL_NUMBER;

    FUNCTION CLEANUP_SPECIAL_CHARACTERS (p_source IN VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   VARCHAR2 (2000) := '';
    BEGIN
        IF p_source IS NULL OR p_source = ''
        THEN
            RETURN p_source;
        END IF;

        l_result := p_source;

        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '???', 'A');
        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '??', 'A');
        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '??', 'A');
        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '??', 'A');
        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '??', 'A');
        l_result := REPLACE (l_result, '??', 'a');
        l_result := REPLACE (l_result, '??', 'A');
        l_result := REPLACE (l_result, '??', 'ae');
        l_result := REPLACE (l_result, '??', 'AE');
        l_result := REPLACE (l_result, '??', 'c');
        l_result := REPLACE (l_result, '??', 'C');
        l_result := REPLACE (l_result, '??', 'e');
        l_result := REPLACE (l_result, '??', 'E');
        l_result := REPLACE (l_result, '??', 'e');
        l_result := REPLACE (l_result, '??', 'E');
        l_result := REPLACE (l_result, '??', 'e');
        l_result := REPLACE (l_result, '??', 'E');
        l_result := REPLACE (l_result, '??', 'e');
        l_result := REPLACE (l_result, '??', 'E');
        l_result := REPLACE (l_result, '??', 'i');
        l_result := REPLACE (l_result, '???', 'I');
        l_result := REPLACE (l_result, '??', 'i');
        l_result := REPLACE (l_result, '??', 'I');
        l_result := REPLACE (l_result, '??', 'i');
        l_result := REPLACE (l_result, '??', 'I');
        l_result := REPLACE (l_result, '??', 'i');
        l_result := REPLACE (l_result, '???', 'I');
        l_result := REPLACE (l_result, '??', 'n');
        l_result := REPLACE (l_result, '??', 'N');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'O');
        l_result := REPLACE (l_result, '??', 'oe');
        l_result := REPLACE (l_result, '??', 'OE');
        l_result := REPLACE (l_result, '??', 's');
        l_result := REPLACE (l_result, '??', 'S');
        l_result := REPLACE (l_result, '??', 'ss');
        l_result := REPLACE (l_result, '??', 'o');
        l_result := REPLACE (l_result, '??', 'u');
        l_result := REPLACE (l_result, '??', 'U');
        l_result := REPLACE (l_result, '??', 'u');
        l_result := REPLACE (l_result, '??', 'U');
        l_result := REPLACE (l_result, '??', 'u');
        l_result := REPLACE (l_result, '??', 'U');
        l_result := REPLACE (l_result, '??', 'u');
        l_result := REPLACE (l_result, '??', 'U');
        l_result := REPLACE (l_result, '??', 'y');
        l_result := REPLACE (l_result, '???', 'Y');
        l_result := REPLACE (l_result, '??', 'y');
        l_result := REPLACE (l_result, '??', 'Y');
        l_result := REPLACE (l_result, ' ', '');
        l_result := REPLACE (l_result, '_', '');
        l_result := REPLACE (l_result, '-', '');
        l_result := REPLACE (l_result, '+', '');
        l_result := REPLACE (l_result, '*', '');
        l_result := REPLACE (l_result, ':', '');
        l_result := REPLACE (l_result, ';', '');
        l_result := REPLACE (l_result, ',', '');
        l_result := REPLACE (l_result, '.', '');
        l_result := REPLACE (l_result, '!', '');
        l_result := REPLACE (l_result, '?', '');
        l_result := REPLACE (l_result, '\\', '');
        l_result := REPLACE (l_result, '/', '');
        l_result := REPLACE (l_result, '''', '');
        l_result := REPLACE (l_result, '"', '');
        l_result := REPLACE (l_result, '|', '');
        l_result := REPLACE (l_result, '??', '');
        l_result := REPLACE (l_result, '(', '');
        l_result := REPLACE (l_result, ')', '');
        l_result := REPLACE (l_result, '[', '');
        l_result := REPLACE (l_result, ']', '');
        l_result := REPLACE (l_result, '<', '');
        l_result := REPLACE (l_result, '>', '');
        l_result := REPLACE (l_result, '??', '');
        l_result := REPLACE (l_result, '~', '');
        l_result := REPLACE (l_result, '}', '');
        l_result := REPLACE (l_result, '{', '');
        l_result := REPLACE (l_result, '@', '');
        l_result := REPLACE (l_result, '$', '');
        l_result := REPLACE (l_result, '??', '');
        l_result := REPLACE (l_result, '??', '');
        l_result := REPLACE (l_result, CHR (9), '');
        l_result := REPLACE (l_result, CHR (10), '');
        l_result := REPLACE (l_result, CHR (13), '');
        l_result := REPLACE (l_result, CHR (38), '');


        l_result := UPPER (l_result);

        RETURN l_result;
    END CLEANUP_SPECIAL_CHARACTERS;

    PROCEDURE S_CMBTAXCODETVA (sPaycode    IN     TAXE.PAYCODE%TYPE,
                               sLangue     IN     LANGUE.LANCODE%TYPE,
                               PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
              SELECT LTA.TAXCODE AS Code, LTA.TAXLIBELLE AS Displayvalue
                FROM LANTAXE LTA, TAXE TAX
               WHERE     LTA.TAXCODE = TAX.TAXCODE
                     AND LTA.LANCODE = sLangue
                     AND TAX.TAXTYPE = 'TVA'
                     AND TAX.TAXCODE != 'MIXTE'
                     AND TAX.PAYCODE = sPaycode
            ORDER BY LTA.TAXLIBELLE;
    END S_CMBTAXCODETVA;

    PROCEDURE DEACTIVATEUSER (sUticode      IN     UTILISATEUR.UTICODE%TYPE,
                              nReturncode   IN OUT NUMBER)
    AS
    BEGIN
        NRETURNCODE := 0;

        UPDATE UTILISATEUR
           SET UTIFLAGINACTIF = 1
         WHERE UTICODE = sUticode;
    EXCEPTION
        WHEN OTHERS
        THEN
            NRETURNCODE := -1 * SQLCODE;
    END DEACTIVATEUSER;

    PROCEDURE FETCHUSERS (sUticode    IN     UTILISATEUR.UTICODE%TYPE,
                          PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT UTI.UTICODE,
                   UTI.UTINOM,
                   UTI.UTIPRENOM,
                   UTI.UTIFLAGINACTIF AS UTISTATUS,
                   UTI.UTIFLAGINACTIF
              FROM UTILISATEUR UTI, UTIACTDEFAULT UAD1,       --Broker manager
                                                        UTIACTDEFAULT UAD2 --Employee and the Broker Firm will be taken care by department of user (UTITSM)
             WHERE     UAD1.UTICODE = sUticode
                   AND UTI.UTICODE = UAD2.UTICODE
                   AND UAD2.ACTID = UAD1.ACTID
                   AND UAD1.UADLINKTYPE = 'MYSELF'
                   AND UAD2.UADLINKTYPE != 'MYSELF';
    END FETCHUSERS;

    PROCEDURE U_UTIPREF_VALBOOLEAN (
        PS_VALUEBOOLEAN          UTIPREFERENCE.UPRBOOLEANVALUE%TYPE,
        PS_USER                  UTIPREFERENCE.UTICODE%TYPE,
        NRETURNCODE       IN OUT NUMBER)
    AS
        COUNT1   NUMBER;
    BEGIN
        SELECT COUNT (*)
          INTO COUNT1
          FROM UTIPREFERENCE
         WHERE UTICODE = PS_USER AND UPRCODE = 'HELPENABLED';

        IF COUNT1 <= 0
        THEN
            INSERT INTO UTIPREFERENCE (UTICODE, UPRCODE, UPRBOOLEANVALUE)
                 VALUES (PS_USER, 'HELPENABLED', PS_VALUEBOOLEAN);
        ELSE
            UPDATE UTIPREFERENCE
               SET UPRBOOLEANVALUE = PS_VALUEBOOLEAN
             WHERE UTICODE = PS_USER AND UPRCODE = 'HELPENABLED';
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            NRETURNCODE := -1 * SQLCODE;
    END U_UTIPREF_VALBOOLEAN;

    PROCEDURE S_LISTMODULE_DOCUMENT (
        SLANGUE     IN     LANTMODULE.LANCODE%TYPE,
        PC_RETURN      OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT DISTINCT LTM.TMOMODULE CODE, LTM.TMOLIBELLE DISPLAYVALUE
              FROM DOCUMENT D, LANTMODULE LTM
             WHERE     D.DOCCONTEXTE = LTM.TMOMODULE
                   AND D.DOCTYPEEDINAT = 'JASPER'
                   AND LTM.LANCODE = SLANGUE
                   AND (   D.DOCDTEND IS NULL
                        OR TO_DATE (D.DOCDTEND, 'DD/MM/YY') >=
                           TO_DATE (SYSDATE, 'DD/MM/YY'));
    END S_LISTMODULE_DOCUMENT;

    PROCEDURE S_TUS_PARAM_TUPHELPTEXT (
        P_TTRNOM           LANTTRPARAM.TTRNOM%TYPE,
        P_LANGUE           LANTTRPARAM.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR
            SELECT TO_CHAR (TUPCODE) CODE,
                   TUPLIBELLE        DISPLAYVALUE,
                   0                 DEFAULTFLAG,
                   0                 PREFERENCEFLAG,
                   0                 OTHERINFO2
              FROM LANTUSPARAM
             WHERE     TUSNOM = P_TTRNOM
                   AND LANCODE = P_LANGUE
                   AND TUPHELPTEXT IS NOT NULL;
    END S_TUS_PARAM_TUPHELPTEXT;

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
            NDPRVERSIONCNT   NUMBER;
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

            IF UPPER (SADDRESSTYPE) = 'GROUP'
            THEN
                SELECT GROCODE
                  INTO SGROCODE
                  FROM UTILISATEUR
                 WHERE UTICODE = SUTICODER;

                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE GROCODE = SGROCODE;
            ELSIF UPPER (SADDRESSTYPE) = 'DEALINVUSER'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, DPRINTERVENANT DIN
                     WHERE     DOSID = SDOSID
                           AND DPRVERSION = SDPRVERSION
                           AND (   DINMETIER = SMETIERRECEIVER
                                OR NVL (SMETIERRECEIVER, '') = '')
                           AND UTI.UTICODE = DIN.UTICODE;
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
                     WHERE     UTICODELINKED = SUTICODER
                           AND ULIDTEND IS NULL
                           AND ULI.UTICODE = UTI.UTICODE;
            ELSIF UPPER (SADDRESSTYPE) = 'OTHERS'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTI.UTICODE                CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR UTI, UTIUSERLINK USL
                     WHERE     UTI.UTICODE = SUTICODER
                           AND UTI.UTICODE = UTICODELINKED;
            ELSIF UPPER (SADDRESSTYPE) = 'GRORECEIVE'
            THEN
                OPEN PC_RETURN FOR
                    SELECT UTICODE                    CODE,
                           UTINOM || ' ' || UTIPRENOM DISPLAYVALUE
                      FROM UTILISATEUR
                     WHERE     UGECODE = SUGECODE
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
                     WHERE     UTILISATEUR.UTICODE = UTITSM.UTICODE
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
                     WHERE     GROCODE IN
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
                     WHERE     UAD.UTICODE = UTI.UTICODE
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
                     WHERE     DIN.ACTID IN
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
                     WHERE     DIN.ACTID IN
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
            ELSE
                OPEN PC_RETURN FOR
                    SELECT TO_CHAR (NULL) CODE, TO_CHAR (NULL) DISPLAYVALUE
                      FROM DUAL
                     WHERE 1 = 2;
            END IF;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            NULL;
    END S_FORMALITIES_COM;

    PROCEDURE S_DEFAULTUSER (SUTICODE     IN     UTILISATEUR.UTICODE%TYPE,
                             SUTINOM      IN     UTILISATEUR.UTINOM%TYPE,
                             SUTIPRENOM   IN     UTILISATEUR.UTIPRENOM%TYPE,
                             PC_RETURN    IN OUT T_CURSOR)
    AS
    BEGIN
        DECLARE
            UTICODESQL     VARCHAR2 (2000) := '';
            UTINOMSQL      VARCHAR2 (2000) := '';
            UTIPRENOMSQL   VARCHAR2 (2000) := '';
            SFINALSQL      VARCHAR2 (2000) := NULL;
        BEGIN
            IF SUTICODE IS NOT NULL
            THEN
                UTICODESQL := ' AND UTICODE = ''' || SUTICODE || ''' ';
            END IF;

            IF SUTINOM IS NOT NULL
            THEN
                UTINOMSQL := ' AND UTINOM = ''' || SUTINOM || ''' ';
            END IF;

            IF SUTIPRENOM IS NOT NULL
            THEN
                UTIPRENOMSQL := ' AND UTIPRENOM = ''' || SUTIPRENOM || ''' ';
            END IF;

            SFINALSQL :=
                   'SELECT DISTINCT UTICODE, UTINOM, UTIPRENOM FROM UTILISATEUR WHERE NVL(UTIFLAGINACTIF ,0 ) = 0  '
                || UTICODESQL
                || UTINOMSQL
                || UTIPRENOMSQL;

            OPEN PC_RETURN FOR SFINALSQL;
        EXCEPTION
            WHEN OTHERS
            THEN
                OPEN PC_RETURN FOR SELECT DISTINCT UTICODE, UTINOM, UTIPRENOM
                                     FROM UTILISATEUR;
        END;
    END S_DEFAULTUSER;

    PROCEDURE S_DOUBTFULATTRIBUTE (
        P_LANCODE          LANDEPARTMENT.LANCODE%TYPE,
        PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        BEGIN
            OPEN PC_RETURN FOR
                SELECT DISTINCT
                       ROV.RVVCODE AS CODE, LAV.RVVLIBELLE AS DISPLAYVALUE
                  FROM ROLVARVALEUR ROV, LANROLVARVALEUR LAV
                 WHERE     ROV.ROLCODE = LAV.ROLCODE
                       AND ROV.RVACODE = LAV.RVACODE
                       AND ROV.RVVCODE = LAV.RVVCODE
                       AND ROV.RVACODE = 'DOUTEUX'
                       AND LAV.LANCODE = P_LANCODE;
        END;
    END S_DOUBTFULATTRIBUTE;


    PROCEDURE S_RULCONTEXTBYRULID (SRULID      IN     RULCONTEXT.RULID%TYPE,
                                   SLANCODE           LANRULE.LANCODE%TYPE,
                                   PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT RCOCONTEXT AS Rulcontext
                             FROM RULCONTEXT RCO
                            WHERE RCO.RULID = SRULID;
    END S_RULCONTEXTBYRULID;


    PROCEDURE I_RULCONTEXTANDRULID (
        SRULID        IN     RULCONTEXT.RULID%TYPE,
        SRCOCONTEXT   IN     RULCONTEXT.RCOCONTEXT%TYPE,
        PC_RETURN     IN OUT INTEGER)
    AS
    BEGIN
        INSERT INTO RULCONTEXT (RULID, RCOCONTEXT)
             VALUES (SRULID, SRCOCONTEXT);

        COMMIT;
        PC_RETURN := 0;
    EXCEPTION
        WHEN OTHERS
        THEN
            ROLLBACK;
            RAISE;
    END I_RULCONTEXTANDRULID;


    PROCEDURE D_RULCONTEXTROW (
        SRULID        IN     RULCONTEXT.RULID%TYPE,
        SRCOCONTEXT   IN     RULCONTEXT.RCOCONTEXT%TYPE,
        PC_RETURN     IN OUT INTEGER)
    AS
    BEGIN
        DELETE FROM RULCONTEXT
              WHERE RULID = SRULID AND RCOCONTEXT = SRCOCONTEXT;

        COMMIT;
        PC_RETURN := 0;
    EXCEPTION
        WHEN OTHERS
        THEN
            ROLLBACK;
            RAISE;
    END D_RULCONTEXTROW;
END PA_COM_GENERAL;