create or replace PACKAGE         pav4_ccfunctions
AS
    FUNCTION CRESTATUS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFNEWCAR (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFUSEDCAR (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFHSINS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_CVASTRINGVALUE (P_ENT_ID VARCHAR2, P_CCHVALUECODE VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFDURGOOD (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFLOANS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFLSCNTS (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFLEASES (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFFACTOR (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFLOANCARD (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION FIRSTLOANDT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION FIRSTDELDT (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION LATESTDELDT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION NOOFRECONTRACT (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION LATESTRECONDT (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION TOTALVHCE (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETREGIONALCODE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETPOST (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETHCSCCENTER (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETREPORTEDDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETRECEIVEDUSER (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETRECVMETHOD (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETFRAUDTYPE (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETREGISTEREDDATE (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETFRAUDAMT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETREPORTDATE (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETREASONFORCANCEL (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETNOOFDEFAULTS (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETWRITOFF (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETCONTRACTDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETMATURITYDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETPASTCOMPENSATIONLOSS (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETFINALDECISIONDATE (SENTID      VARCHAR2,
                                   SLANCODE    VARCHAR2,
                                   SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETFINALDECISION (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETFINALDECISIONCATCODE (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETDEBTTRANSFERSTATUS (SENTID      VARCHAR2,
                                    SLANCODE    VARCHAR2,
                                    SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETPOLICEINFOR (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETREPORTCUSTOMER (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETACKNOWLEDGEFRAUD (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

         FUNCTION SGMCINACT (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2; --ABH 20210305

         FUNCTION SGMIDFISC (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
            RETURN VARCHAR2 ; ---ABH 20210318

        FUNCTION LADSGM (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
         RETURN VARCHAR2 ; ---ABH 20210316



         FUNCTION SGMACTCA (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2   ; --abh 31/03/2021

    FUNCTION GETCUSTCATEGORYCODE (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETATTRIBUTEAMOUNTCUST (SENTID      VARCHAR2,
                                     SLANCODE    VARCHAR2,
                                     SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETATTRIBUTERATE (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETRELATIONGUARANTOR (SENTID      VARCHAR2,
                                   SLANCODE    VARCHAR2,
                                   SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETGUARANTORSTARTDATE (SENTID      VARCHAR2,
                                    SLANCODE    VARCHAR2,
                                    SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETGUARANTORENDDATE (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETDELETEGUARANTOR (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETATTRIBUTEAMOUNTAGENCY (SENTID      VARCHAR2,
                                       SLANCODE    VARCHAR2,
                                       SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETCOUNSELLINGNO (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETUWCHECK (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETCONFIRMATION (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETCONFIRMEDBY (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETUNDERWRITER (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETCONFIRMEDYN (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION GETJOURNALNO (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETBRANCHCHIEF (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    --prebooking
    FUNCTION F_CVANUMERICVALUE (P_ENT_ID VARCHAR2, P_CCHVALUECODE VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTBPREBOOKR (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTTPREBOOK (P_ENT_ID     VARCHAR2,
                             P_LANCODE    VARCHAR2,
                             P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTRPREBOOK (P_ENT_ID     VARCHAR2,
                             P_LANCODE    VARCHAR2,
                             P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTTMPPREBOOK (P_ENT_ID     VARCHAR2,
                               P_LANCODE    VARCHAR2,
                               P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTBPREBOOKRU (P_ENT_ID     VARCHAR2,
                               P_LANCODE    VARCHAR2,
                               P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLAPREBOOKU (P_ENT_ID     VARCHAR2,
                           P_LANCODE    VARCHAR2,
                           P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTTPREBOOKU (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION SQLFCTRPREBOOKU (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER;

    --AGENDA
    FUNCTION SQLFCTPARK1 (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION SQLFCTCAR1 (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION SQLFCTVIN1 (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION SQLFCTASSOPTION (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    /*************************** added to test by Park Young min ***********************************/
    FUNCTION F_GETRVACODECNT (P_ACTID NUMBER)
        RETURN NUMBER;

    FUNCTION F_ISRIMORDRECNT (P_REGID REGIMPUTATION.REGID%TYPE)
        RETURN NUMBER;

    FUNCTION F_ISCOMPNEWLOAN (P_DOSID DOSSIER.DOSID%TYPE)
        RETURN NUMBER;

    /*************************** added to test by Park Young min ***********************************/

    FUNCTION GETTAEG (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETTAEGBIS (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETTAEA (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN NUMBER;

    -- JLE 20160210
    FUNCTION F_GETNBRENEWAL (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION GETRISKDECIVALUE (sEntId      VARCHAR2,
                               sLanCode    VARCHAR2,
                               sUtiCode    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_ITR (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION F_PRODUCTCATALOGUE_CODE (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_PRODUCTCATALOGUE_LABEL (SENTID      VARCHAR2,
                                       SLANCODE    VARCHAR2,
                                       SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_CLIENTTYPE_CODE (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_CLIENTTYPE_LABEL (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DURATION (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN NUMBER;

    FUNCTION F_GET_ITR_DURATION (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN NUMBER;


    FUNCTION F_GET_ITRDEP (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2;
        FUNCTION F_GET_USER_CASGMA (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2;
END PAV4_CCFUNCTIONS;
/
create or replace PACKAGE BODY         pav4_ccfunctions
AS
    FUNCTION NOOFNEWCAR (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFUSEDCAR (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFHSINS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFDURGOOD (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFLOANS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFLSCNTS (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFLEASES (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFFACTOR (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFLOANCARD (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION FIRSTLOANDT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION FIRSTDELDT (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION LATESTDELDT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION NOOFRECONTRACT (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION F_CVASTRINGVALUE (P_ENT_ID VARCHAR2, P_CCHVALUECODE VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRETURN   CCHVALUE.CVASTRINGVALUE%TYPE;
        BEGIN
            SELECT CVASTRINGVALUE
              INTO SRETURN
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CCH.CCHSID = CVA.CCHSID
                   AND CCH.CCHVALUECODE = P_CCHVALUECODE
                   AND CVAPKEYVALUE = P_ENT_ID;

            RETURN SRETURN;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END F_CVASTRINGVALUE;

    FUNCTION LATESTRECONDT (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'Test';
    END;

    FUNCTION TOTALVHCE (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NTOTALOWNED   NUMBER := 0;
        BEGIN
            SELECT SUM (NVL (CVANUMERICVALUE, 0))
              INTO NTOTALOWNED
              FROM CCHVALUE
             WHERE     CCHSID IN
                           (SELECT CCHSID
                              FROM CUSTOMCHARACTERISTIC
                             WHERE    CCHVALUECODE = 'CCA_PSNGRVHCE'
                                   OR CCHVALUECODE = 'CCA_CMRCLVHCE')
                   AND ACTID =
                       SUBSTR (
                           'Actid-1258||Ugecode-_ORIG_||',
                           7,
                           INSTR ('Actid-1258||Ugecode-_ORIG_||', '||') - 7);

            RETURN NTOTALOWNED;
        END;
    END;

    FUNCTION CRESTATUS (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN SENTID;
    END;

    FUNCTION GETREGIONALCODE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREGION   LANPAYREGION.PRELIBELLE%TYPE;
        BEGIN
            SELECT LPR.PRELIBELLE
              INTO SREGION
              FROM COLLECTIONCASE  CC,
                   DPTPAYREG       DPR,
                   PAYREGDEPT      PRD,
                   DOSACTEUR       DAC,
                   DOSACTADRESSE   DAA,
                   ACTADRESSE      AAD,
                   ADRESSE         ADR,
                   LANPAYREGION    LPR
             WHERE     CC.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND DAC.ROLCODE = 'EMPRUNT'
                   AND DAC.DACDTFIN IS NULL
                   AND DAA.DAADTFIN IS NULL
                   AND PRD.PAYCODE = 'KR'
                   AND LPR.PAYCODE = 'KR'
                   AND LPR.LANCODE = SLANCODE
                   AND CC.CCADTEND IS NULL
                   AND TRUNC (SYSDATE) BETWEEN DPR.DPRDTBEG
                                           AND NVL (
                                                   DPR.DPRDTEND,
                                                   LAST_DAY (TRUNC (SYSDATE)))
                   AND CC.DPTCODE = DPR.DPTCODE
                   AND DPR.PRECODE = PRD.PRECODE
                   AND DPR.PAYCODE = PRD.PAYCODE
                   AND DPR.PRETYPE = PRD.PRETYPE
                   AND DPR.PRECODE = LPR.PRECODE
                   AND CC.DOSID = DAC.DOSID
                   AND PRD.PRDDEPT =
                       NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
                   AND DAC.DOSID = DAA.DOSID
                   AND DAC.ACTID = AAD.ACTID
                   AND DAA.AADORDRE = AAD.AADORDRE
                   AND AAD.ADRID = ADR.ADRID;

            RETURN SREGION;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETPOST (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SPOST   PAYREGDEPT.PRDDEPT%TYPE;
        BEGIN
            SELECT NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
              INTO SPOST
              FROM COLLECTIONCASE  CC,
                   DPTPAYREG       DPR,
                   PAYREGDEPT      PRD,
                   DOSACTEUR       DAC,
                   DOSACTADRESSE   DAA,
                   ACTADRESSE      AAD,
                   ADRESSE         ADR,
                   LANPAYREGION    LPR
             WHERE     CC.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND DAC.ROLCODE = 'EMPRUNT'
                   AND DAC.DACDTFIN IS NULL
                   AND DAA.DAADTFIN IS NULL
                   AND PRD.PAYCODE = 'KR'
                   AND LPR.PAYCODE = 'KR'
                   AND LPR.LANCODE = SLANCODE
                   AND CC.CCADTEND IS NULL
                   AND TRUNC (SYSDATE) BETWEEN DPR.DPRDTBEG
                                           AND NVL (
                                                   DPR.DPRDTEND,
                                                   LAST_DAY (TRUNC (SYSDATE)))
                   AND CC.DPTCODE = DPR.DPTCODE
                   AND DPR.PRECODE = PRD.PRECODE
                   AND DPR.PAYCODE = PRD.PAYCODE
                   AND DPR.PRETYPE = PRD.PRETYPE
                   AND DPR.PRECODE = LPR.PRECODE
                   AND CC.DOSID = DAC.DOSID
                   AND PRD.PRDDEPT =
                       NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
                   AND DAC.DOSID = DAA.DOSID
                   AND DAC.ACTID = AAD.ACTID
                   AND DAA.AADORDRE = AAD.AADORDRE
                   AND AAD.ADRID = ADR.ADRID;

            RETURN SPOST;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETHCSCCENTER (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCCENTER          DTRDETAIL.DDEHOSTVALUE%TYPE;
            SHOMECODEPOST     ADRESSE.ADRCODEPOST%TYPE;
            SOFFICECODEPOST   ADRESSE.ADRCODEPOST%TYPE;
            SOTHERCODEPOST    ADRESSE.ADRCODEPOST%TYPE;
            SACTTYPE          ACTEUR.ACTTYPE%TYPE;
            SCODEPOST         ADRESSE.ADRCODEPOST%TYPE;
            SAADTYPE          ACTADRESSE.AADTYPE%TYPE;
        BEGIN
            SELECT ACT.ACTTYPE
              INTO SACTTYPE
              FROM ACTEUR ACT, DOSACTEUR DAC
             WHERE     ACT.ACTID = DAC.ACTID
                   AND DAC.DACDTFIN IS NULL
                   AND DAC.ROLCODE = 'EMPRUNT'
                   AND DAC.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7);

            BEGIN
                SELECT NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
                  INTO SHOMECODEPOST
                  FROM ADRESSE ADR, ACTADRESSE AAD, DOSACTEUR DAC
                 WHERE     DAC.ROLCODE = 'EMPRUNT'
                       AND DAC.DACDTFIN IS NULL
                       AND AAD.AADDTREMPLACE IS NULL
                       AND NVL (AAD.AADTYPE, '0') = '1'
                       AND DAC.DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND ADR.ADRID = AAD.ADRID
                       AND AAD.ACTID = DAC.ACTID
                       AND AAD.AADORDRE =
                           (SELECT MAX (AADORDRE)
                              FROM ACTADRESSE
                             WHERE     ACTID = AAD.ACTID
                                   AND NVL (AADTYPE, '0') = '1');
            EXCEPTION
                WHEN OTHERS
                THEN
                    SHOMECODEPOST := NULL;
            END;

            BEGIN
                SELECT NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
                  INTO SOFFICECODEPOST
                  FROM ADRESSE ADR, ACTADRESSE AAD, DOSACTEUR DAC
                 WHERE     DAC.ROLCODE = 'EMPRUNT'
                       AND DAC.DACDTFIN IS NULL
                       AND AAD.AADDTREMPLACE IS NULL
                       AND NVL (AAD.AADTYPE, '0') = '2'
                       AND DAC.DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND ADR.ADRID = AAD.ADRID
                       AND AAD.ACTID = DAC.ACTID
                       AND AAD.AADORDRE =
                           (SELECT MAX (AADORDRE)
                              FROM ACTADRESSE
                             WHERE     ACTID = AAD.ACTID
                                   AND NVL (AADTYPE, '0') = '2');
            EXCEPTION
                WHEN OTHERS
                THEN
                    SOFFICECODEPOST := NULL;
            END;

            BEGIN
                SELECT NVL (ADR.ADRCODEPOSTNEW, ADR.ADRCODEPOST)
                  INTO SOTHERCODEPOST
                  FROM ADRESSE ADR, ACTADRESSE AAD, DOSACTEUR DAC
                 WHERE     DAC.ROLCODE = 'EMPRUNT'
                       AND DAC.DACDTFIN IS NULL
                       AND AAD.AADDTREMPLACE IS NULL
                       AND NVL (AAD.AADTYPE, '0') = '99'
                       AND DAC.DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND ADR.ADRID = AAD.ADRID
                       AND AAD.ACTID = DAC.ACTID
                       AND AAD.AADORDRE =
                           (SELECT MAX (AADORDRE)
                              FROM ACTADRESSE
                             WHERE     ACTID = AAD.ACTID
                                   AND NVL (AADTYPE, '0') = '99');
            EXCEPTION
                WHEN OTHERS
                THEN
                    SOTHERCODEPOST := NULL;
            END;

            IF SACTTYPE = 'PART'
            THEN
                IF SHOMECODEPOST IS NOT NULL
                THEN
                    SCODEPOST := SHOMECODEPOST;
                    SAADTYPE := '1';
                ELSIF SOFFICECODEPOST IS NOT NULL
                THEN
                    SCODEPOST := SOFFICECODEPOST;
                    SAADTYPE := '2';
                ELSE
                    SCODEPOST := SOTHERCODEPOST;
                    SAADTYPE := '99';
                END IF;
            ELSE
                IF SOFFICECODEPOST IS NOT NULL
                THEN
                    SCODEPOST := SOFFICECODEPOST;
                    SAADTYPE := '2';
                ELSIF SHOMECODEPOST IS NOT NULL
                THEN
                    SCODEPOST := SHOMECODEPOST;
                    SAADTYPE := '1';
                ELSE
                    SCODEPOST := SOTHERCODEPOST;
                    SAADTYPE := '99';
                END IF;
            END IF;

            BEGIN
                SELECT    DDE.DDEHOSTVALUE
                       || DECODE (LDD.DDELABEL, NULL, '', '-')
                       || LDD.DDELABEL
                  INTO SCCENTER
                  FROM DATATRANSCODING  DTR,
                       DTRDETAIL        DDE,
                       LANDTRDETAIL     LDD,
                       ADRESSE          ADR,
                       ACTADRESSE       AAD,
                       ACTEUR           ACT,
                       DOSACTEUR        DAC
                 WHERE     DTR.DTRCODE = 'HCSCCMATRIX'
                       AND DAC.ROLCODE = 'EMPRUNT'
                       AND LDD.LANCODE = SLANCODE
                       AND DAC.DACDTFIN IS NULL
                       AND DAC.DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND AAD.AADDTREMPLACE IS NULL
                       AND AAD.AADTYPE = SAADTYPE
                       AND DTR.DTRID = DDE.DTRID
                       AND LDD.DTRID = DDE.DTRID
                       AND LDD.DDECASSIOPEEVALUE = DDE.DDECASSIOPEEVALUE
                       AND DDE.DDECASSIOPEEVALUE = SCODEPOST
                       AND ADR.ADRID = AAD.ADRID
                       AND AAD.ACTID = ACT.ACTID
                       AND ACT.ACTID = DAC.ACTID;
            EXCEPTION
                WHEN OTHERS
                THEN
                    SCCENTER := NULL;
            END;

            RETURN SCCENTER;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETREPORTEDDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREPORTEDDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SREPORTEDDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_REPORTEDDATE';

            RETURN SREPORTEDDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETRECEIVEDUSER (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRECEIVEDUSER   VARCHAR2 (100);
        BEGIN
            SELECT CVASTRINGVALUE
              INTO SRECEIVEDUSER
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_RECEIVEDUSER';

            RETURN SRECEIVEDUSER;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETRECVMETHOD (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRECVMETHOD   VARCHAR2 (100);
        BEGIN
            SELECT LTP.TUPLIBELLE
              INTO SRECVMETHOD
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH, LANTUSPARAM LTP
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND LTP.LANCODE = 'EN'
                   AND LTP.TUSNOM = 'RECVMETHOD'
                   AND LTP.TUPCODE = CVA.CVASTRINGVALUE
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_RECVMETHOD';

            RETURN SRECVMETHOD;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETFRAUDTYPE (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SFRAUDTYPE   VARCHAR2 (100);
        BEGIN
            SELECT LTP.TUPLIBELLE
              INTO SFRAUDTYPE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH, LANTUSPARAM LTP
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND LTP.LANCODE = 'EN'
                   AND LTP.TUSNOM = 'FRAUDTYPE'
                   AND LTP.TUPCODE = CVA.CVASTRINGVALUE
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_FRAUDTYPE';

            RETURN SFRAUDTYPE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETREGISTEREDDATE (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREGISTEREDDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SREGISTEREDDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_REGISTEREDDATE';

            RETURN SREGISTEREDDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETFRAUDAMT (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NFRAUDAMT   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NFRAUDAMT
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_FRAUDAMT';

            RETURN NFRAUDAMT;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETREPORTDATE (SENTID      VARCHAR2,
                            SLANCODE    VARCHAR2,
                            SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREPORTDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SREPORTDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_REPORTDATE';

            RETURN SREPORTDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETREASONFORCANCEL (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREASONFORCANCEL   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SREASONFORCANCEL
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_REASONFORCANCEL';

            RETURN SREASONFORCANCEL;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETNOOFDEFAULTS (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NNOOFDEFAULTS   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NNOOFDEFAULTS
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_NOOFDEFAULTS';

            RETURN NNOOFDEFAULTS;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETWRITOFF (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SWRITOFF   VARCHAR2 (100);
        BEGIN
            SELECT DECODE (CVA.CVABOOLEANVALUE, 1, 'YES', 'NO')
              INTO SWRITOFF
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_WRITOFF';

            RETURN SWRITOFF;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCONTRACTDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCONTRACTDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SCONTRACTDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_CONTRACTDATE';

            RETURN SCONTRACTDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETMATURITYDATE (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SMATURITYDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SMATURITYDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_MATURITYDATE';

            RETURN SMATURITYDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETPASTCOMPENSATIONLOSS (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SPASTCOMPENSATIONLOSS   VARCHAR2 (100);
        BEGIN
            SELECT DECODE (CVA.CVABOOLEANVALUE, 1, 'YES', 'NO')
              INTO SPASTCOMPENSATIONLOSS
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_PASTCOMPENSATIONLOSS';

            RETURN SPASTCOMPENSATIONLOSS;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETFINALDECISIONDATE (SENTID      VARCHAR2,
                                   SLANCODE    VARCHAR2,
                                   SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SFINALDECISIONDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SFINALDECISIONDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_FINALDECISIONDATE';

            RETURN SFINALDECISIONDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETFINALDECISION (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SFINALDECISION   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SFINALDECISION
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_FINALDECISION';

            RETURN SFINALDECISION;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETFINALDECISIONCATCODE (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SFINALDECISIONCATCODE   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SFINALDECISIONCATCODE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_FINALDECISIONCATCODE';

            RETURN SFINALDECISIONCATCODE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETDEBTTRANSFERSTATUS (SENTID      VARCHAR2,
                                    SLANCODE    VARCHAR2,
                                    SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SDEBTTRANSFERSTATUS   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SDEBTTRANSFERSTATUS
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_DEBTTRANSFERSTATUS';

            RETURN SDEBTTRANSFERSTATUS;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETPOLICEINFOR (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SPOLICEINFOR   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SPOLICEINFOR
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_POLICEINFOR';

            RETURN SPOLICEINFOR;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETREPORTCUSTOMER (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SREPORTCUSTOMER   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SREPORTCUSTOMER
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_REPORTCUSTOMER';

            RETURN SREPORTCUSTOMER;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETACKNOWLEDGEFRAUD (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SACKNOWLEDGEFRAUD   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SACKNOWLEDGEFRAUD
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_ACKNOWLEDGEFRAUD';

            RETURN SACKNOWLEDGEFRAUD;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCUSTCATEGORYCODE (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCUSTCATEGORYCODE   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SCUSTCATEGORYCODE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_CUSTCATEGORYCODE';

            RETURN SCUSTCATEGORYCODE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETATTRIBUTEAMOUNTCUST (SENTID      VARCHAR2,
                                     SLANCODE    VARCHAR2,
                                     SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NATTRIBUTEAMOUNTCUST   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NATTRIBUTEAMOUNTCUST
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_ATTRIBUTEAMOUNTCUST';

            RETURN NATTRIBUTEAMOUNTCUST;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETATTRIBUTERATE (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NATTRIBUTERATE   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NATTRIBUTERATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_ATTRIBUTERATE';

            RETURN NATTRIBUTERATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETRELATIONGUARANTOR (SENTID      VARCHAR2,
                                   SLANCODE    VARCHAR2,
                                   SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRELATIONGUARANTOR   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SRELATIONGUARANTOR
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_RELATIONGUARANTOR';

            RETURN SRELATIONGUARANTOR;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETGUARANTORSTARTDATE (SENTID      VARCHAR2,
                                    SLANCODE    VARCHAR2,
                                    SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SGUARANTORSTARTDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SGUARANTORSTARTDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_GUARANTORSTARTDATE';

            RETURN SGUARANTORSTARTDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETGUARANTORENDDATE (SENTID      VARCHAR2,
                                  SLANCODE    VARCHAR2,
                                  SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SGUARANTORENDDATE   VARCHAR2 (100);
        BEGIN
            SELECT TO_CHAR (CVA.CVADTVALUE, 'YYYY-MM-DD')
              INTO SGUARANTORENDDATE
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_GUARANTORENDDATE';

            RETURN SGUARANTORENDDATE;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETDELETEGUARANTOR (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SDELETEGUARANTOR   VARCHAR2 (100);
        BEGIN
            SELECT DECODE (CVA.CVABOOLEANVALUE, 1, 'YES', 'NO')
              INTO SDELETEGUARANTOR
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_DELETEGUARANTOR';

            RETURN SDELETEGUARANTOR;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETATTRIBUTEAMOUNTAGENCY (SENTID      VARCHAR2,
                                       SLANCODE    VARCHAR2,
                                       SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SATTRIBUTEAMOUNTAGENCY   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SATTRIBUTEAMOUNTAGENCY
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_ATTRIBUTEAMOUNTAGENCY';

            RETURN SATTRIBUTEAMOUNTAGENCY;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCOUNSELLINGNO (SENTID      VARCHAR2,
                               SLANCODE    VARCHAR2,
                               SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NCOUNSELLINGNO   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NCOUNSELLINGNO
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_COUNSELLINGNO';

            RETURN NCOUNSELLINGNO;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETUWCHECK (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SUWCHECK   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SUWCHECK
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_UWCHECK';

            RETURN SUWCHECK;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCONFIRMATION (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCONFIRMATION   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SCONFIRMATION
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_CONFIRMATION';

            RETURN SCONFIRMATION;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCONFIRMEDBY (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCONFIRMEDBY   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SCONFIRMEDBY
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_CONFIRMEDBY';

            RETURN SCONFIRMEDBY;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETUNDERWRITER (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SUNDERWRITER   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SUNDERWRITER
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_UNDERWRITER';

            RETURN SUNDERWRITER;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETCONFIRMEDYN (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SCONFIRMEDYN   VARCHAR2 (100);
        BEGIN
            SELECT DECODE (CVA.CVABOOLEANVALUE, 1, 'YES', 'NO')
              INTO SCONFIRMEDYN
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_CONFIRMEDYN';

            RETURN SCONFIRMEDYN;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETJOURNALNO (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NJOURNALNO   NUMBER;
        BEGIN
            SELECT CVA.CVANUMERICVALUE
              INTO NJOURNALNO
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_JOURNALNO';

            RETURN NJOURNALNO;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    FUNCTION GETBRANCHCHIEF (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SBRANCHCHIEF   VARCHAR2 (100);
        BEGIN
            SELECT CVA.CVASTRINGVALUE
              INTO SBRANCHCHIEF
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.DOSID =
                       SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHVALUECODE = 'CC_BRANCHCHIEF';

            RETURN SBRANCHCHIEF;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
    END;

    --Get CCHVALUE
    FUNCTION F_CVANUMERICVALUE (P_ENT_ID VARCHAR2, P_CCHVALUECODE VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT CVANUMERICVALUE
              INTO NRETURN
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CCH.CCHSID = CVA.CCHSID
                   AND CCH.CCHVALUECODE = P_CCHVALUECODE
                   AND CVAPKEYVALUE = P_ENT_ID;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        RETURN NRETURN;
    END F_CVANUMERICVALUE;

    -- Prebooking run out limit  = SUM (no set up loans amount which used car agency have now X 100%*) Role AP
    FUNCTION SQLFCTBPREBOOKR (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
    BEGIN
        DECLARE
            NCOUNT                 NUMBER := 0;
            NUSEDUPLIMIT           NUMBER (12, 0) := 0;
            NLOANAMOUNT            NUMBER (12, 2) := 0;
            NSECURITYSETTINGDAYS   NUMBER := 0;
            NUSEDUPRATE            NUMBER (10, 2);

            CURSOR C_TAPM
            IS
                SELECT DOS.DOSID
                  FROM DOSACTEUR  DOSACT,
                       DOSPHASE   PHA,
                       DOSSIER    DOS,
                       CCHVALUE   CCH
                 WHERE     DOSACT.ACTID =
                           SUBSTR (
                               P_ENT_ID,
                               INSTR (P_ENT_ID, 'Actid-') + 6,
                                 INSTR (P_ENT_ID, '||')
                               - INSTR (P_ENT_ID, 'Actid-')
                               - 6)
                       AND DOS.DOSID = PHA.DOSID
                       AND DOS.DOSID = DOSACT.DOSID
                       AND DOS.DOSID = CCH.DOSID
                       AND PHA.PHACODE = 'ES'
                       AND DOS.TPGCODE LIKE 'U%'
                       AND CCH.DOSID IS NOT NULL
                       AND CCH.CCHSID =
                           (SELECT CCHSID
                              FROM CUSTOMCHARACTERISTIC
                             WHERE CCHVALUECODE = 'CC_TYPCOLLATERAL')
                       AND CCH.CVASTRINGVALUE = 'PREL'
                       AND DOSACT.ROLCODE = 'AG';
        BEGIN
            FOR REC_CTAPM IN C_TAPM
            LOOP
                BEGIN
                    SELECT NVL (DOSMTSIGN, 0)
                      INTO NLOANAMOUNT
                      FROM DOSSIER
                     WHERE DOSID = REC_CTAPM.DOSID;

                    SELECT NVL (MAX (DAGNBNOTSECDAY), 0)
                      INTO NSECURITYSETTINGDAYS
                      FROM DOSACTGARANTIE
                     WHERE DOSID = REC_CTAPM.DOSID;

                    -- Old version
                    --        IF nSecuritysettingdays>=0 AND nSecuritysettingdays<=5 THEN
                    --          nUseduprate          :=0.50;
                    --        END IF;
                    --        IF nSecuritysettingdays>5 AND nSecuritysettingdays<=10 THEN
                    --          nUseduprate         :=0.70;
                    --        END IF;
                    --        IF nSecuritysettingdays>10 AND nSecuritysettingdays<=15 THEN
                    --          nUseduprate         :=1.0;
                    --        END IF;
                    --        IF nSecuritysettingdays>15 AND nSecuritysettingdays<=20 THEN
                    --          nUseduprate         :=1.5;
                    --        END IF;
                    --        IF nSecuritysettingdays>20 THEN
                    --          nUseduprate         :=2.0;
                    --          -- else if nSecuritysettingdays>25 then
                    --        END IF;
                    NUSEDUPRATE := 1.0;
                    NUSEDUPLIMIT :=
                        NUSEDUPLIMIT + (NLOANAMOUNT * NUSEDUPRATE);
                END;
            END LOOP;

            RETURN NUSEDUPLIMIT;
        END;
    END SQLFCTBPREBOOKR;

    --Total prebooking limit = Basic Pre-Booking Limit(CC_BPREBOOK) + Temporary Pre-Booking Limit(CC_TMPPREBOOK) for deals*
    FUNCTION SQLFCTTPREBOOK (P_ENT_ID     VARCHAR2,
                             P_LANCODE    VARCHAR2,
                             P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
        NTEMP     NUMBER := 0;
    BEGIN
        SELECT SUM (CVANUMERICVALUE)
          INTO NTEMP
          FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
         WHERE     CCH.CCHSID = CVA.CCHSID
               AND CCH.CCHVALUECODE = 'CC_TMPPREBOOK'
               AND (DOSID, DPRVERSION) IN
                       (SELECT DOSID, DPRVERSION
                          FROM DPRACTEUR
                         WHERE     ROLCODE = 'AG'
                               AND ACTID =
                                   SUBSTR (P_ENT_ID,
                                           7,
                                           INSTR (P_ENT_ID, '||') - 7)
                               AND DPRVERSION =
                                   PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                       DOSID));

        NRETURN :=
            F_CVANUMERICVALUE (P_ENT_ID, 'CC_BPREBOOK') + NVL (NTEMP, 0);
        RETURN NRETURN;
    END SQLFCTTPREBOOK;

    -- Remaining pre-booking limit  = Total pre-booking limit - Pre-booking run-out limit
    FUNCTION SQLFCTRPREBOOK (P_ENT_ID     VARCHAR2,
                             P_LANCODE    VARCHAR2,
                             P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        SELECT   SQLFCTTPREBOOK (P_ENT_ID, P_LANCODE, P_UTICODE)
               - SQLFCTBPREBOOKR (P_ENT_ID, P_LANCODE, P_UTICODE)
          INTO NRETURN
          FROM DUAL;

        ---- OLd VALUE f_CVANUMERICVALUE (p_ent_id ,'CC_TPREBOOK') - f_CVANUMERICVALUE (p_ent_id ,'CC_BPREBOOKR')  ;
        -- old value2    select  SQLFCTTPREBOOK ( p_ent_id ,p_lancode ,p_uticode )  - f_CVANUMERICVALUE (p_ent_id ,'CC_BPREBOOKR') into nReturn from dual;
        RETURN NRETURN;
    END SQLFCTRPREBOOK;

    --Temporary prebooking limit for AP for the deal  = Remaining pre-booking limit?? Deal financed amount
    FUNCTION SQLFCTTMPPREBOOK (P_ENT_ID     VARCHAR2,
                               P_LANCODE    VARCHAR2,
                               P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
        NDOSID    NUMBER;
    BEGIN
        BEGIN
            SELECT DOSIDPROSPECT
              INTO NDOSID
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CCH.CCHSID = CVA.CCHSID
                   AND CCH.CCHVALUECODE = 'CC_TMPPREBOOK'
                   AND CVAPKEYVALUE = P_ENT_ID;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;

        BEGIN
            SELECT   (SELECT CVANUMERICVALUE
                        FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
                       WHERE     CCH.CCHVALUECODE = 'CC_RPREBOOK'
                             --AND cch.ENTCODE = ''
                             AND CCH.CCHSID = CVA.CCHSID
                             AND CVA.ACTID = DPR.ACTID)
                   - (SELECT PFI.PFADOUBLE
                        FROM PFIATTRIBUT PFI
                       WHERE     PFACODE = 'FINANCEDVALUE'
                             AND PFI.PFIID =
                                 (PA_AVCOMMUN.F_GETPFIRETENUE (
                                      DPR.DOSID,
                                      PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (
                                          DPR.DOSID))))
              INTO NRETURN
              FROM DPRACTEUR DPR
             WHERE DPR.ROLCODE = 'AG' AND DPR.DOSID = NDOSID;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN 0;
        END;

        RETURN NRETURN;
    END SQLFCTTMPPREBOOK;

    --Pre-booking run-out limit = SUM (no set up loans amount which used car agency have now X pre-booking limit run-out rate*) Rate to define
    FUNCTION SQLFCTBPREBOOKRU (P_ENT_ID     VARCHAR2,
                               P_LANCODE    VARCHAR2,
                               P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
    BEGIN
        DECLARE
            NCOUNT                 NUMBER := 0;
            NUSEDUPLIMIT           NUMBER (12, 0) := 0;
            NLOANAMOUNT            NUMBER (12, 2) := 0;
            NSECURITYSETTINGDAYS   NUMBER := 0;
            NUSEDUPRATE            NUMBER (10, 2);

            CURSOR C_TAPM
            IS
                SELECT DOS.DOSID
                  FROM DOSACTEUR       DOSACT,
                       DOSPHASE        PHA,
                       DOSSIER         DOS,
                       CCHVALUE        CCH,
                       ITRRUBRIQUE     ITR,
                       LKDOSRUBITRRUB  LKD,
                       VARIANT         VAR
                 WHERE     DOSACT.ACTID =
                           SUBSTR (
                               P_ENT_ID,
                               INSTR (P_ENT_ID, 'Actid-') + 6,
                                 INSTR (P_ENT_ID, '||')
                               - INSTR (P_ENT_ID, 'Actid-')
                               - 6)
                       AND DOS.DOSID = PHA.DOSID
                       AND DOS.DOSID = DOSACT.DOSID
                       AND DOS.DOSID = CCH.DOSID
                       AND PHA.PHACODE = 'ES'
                       AND DOS.TPGCODE LIKE 'U%'
                       AND DOS.TPGCODE NOT IN ('U69302',
                                               'U69402',
                                               'U69304',
                                               'U69404')
                       AND CCH.DOSID IS NOT NULL
                       AND PHA.DPHDTFIN IS NULL
                       AND CCH.CCHSID =
                           (SELECT CCHSID
                              FROM CUSTOMCHARACTERISTIC
                             WHERE CCHVALUECODE = 'CC_TYPCOLLATERAL')
                       AND CCH.CVASTRINGVALUE = 'PREL'
                       AND DOSACT.ROLCODE = 'USECARA'
                       AND LKD.DOSID = DOS.DOSID
                       AND ITR.ITRID = LKD.ITRID(+)
                       AND LKD.DRUORDRE = 1
                       AND ITR.VARID = VAR.VARID(+)
                       AND NVL (MMOCODE, ' ') NOT IN
                               (SELECT MMOCODE
                                  FROM MAKMODEL
                                 WHERE MMOFLAGREGISTRED = 1);
        BEGIN
            FOR REC_CTAPM IN C_TAPM
            LOOP
                BEGIN
                    SELECT NVL (DOSMTSIGN, 0)
                      INTO NLOANAMOUNT
                      FROM DOSSIER
                     WHERE DOSID = REC_CTAPM.DOSID;

                    SELECT NVL (MAX (DAGNBNOTSECDAY), -1)
                      INTO NSECURITYSETTINGDAYS
                      FROM DOSACTGARANTIE DAG
                     WHERE     DOSID = REC_CTAPM.DOSID
                           AND EXISTS
                                   (SELECT 1
                                      FROM COLPHASE CPH
                                     WHERE     CPH.COLID = DAG.COLID
                                           AND CPH.PHACODE = 'ACTIVE'
                                           AND CPH.JALCODE = 'PRPRE'
                                           AND CPH.CPHDTEND IS NULL);

                    IF (NSECURITYSETTINGDAYS > 0)
                    THEN
                        IF     NSECURITYSETTINGDAYS >= 0
                           AND NSECURITYSETTINGDAYS <= 5
                        THEN
                            NUSEDUPRATE := 0.25;
                        END IF;

                        IF     NSECURITYSETTINGDAYS > 5
                           AND NSECURITYSETTINGDAYS <= 10
                        THEN
                            NUSEDUPRATE := 0.70;
                        END IF;

                        IF     NSECURITYSETTINGDAYS > 10
                           AND NSECURITYSETTINGDAYS <= 15
                        THEN
                            NUSEDUPRATE := 1.0;
                        END IF;

                        IF     NSECURITYSETTINGDAYS > 15
                           AND NSECURITYSETTINGDAYS <= 20
                        THEN
                            NUSEDUPRATE := 1.5;
                        END IF;

                        IF NSECURITYSETTINGDAYS > 20
                        THEN
                            NUSEDUPRATE := 2.0;
                        -- else if nSecuritysettingdays>25 then
                        END IF;

                        NUSEDUPLIMIT :=
                            NUSEDUPLIMIT + (NLOANAMOUNT * NUSEDUPRATE);
                    END IF;
                END;
            END LOOP;

            RETURN NUSEDUPLIMIT;
        END;
    END SQLFCTBPREBOOKRU;

    --Adjusted pre-booking limit(CC: APREBOOKU) : Logic = basic pre-booking limit X admission rate (Matrix)
    --- Change by Chaker
    FUNCTION SQLAPREBOOKU (P_ENT_ID     VARCHAR2,
                           P_LANCODE    VARCHAR2,
                           P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        ---
        NRETURN    NUMBER := 0;
        NAMOUNT    NUMBER := 0;
        NCOEF      NUMBER := 0;
        NRVVCODE   NUMBER;
    BEGIN
        ---  Get the Coef
        BEGIN
            SELECT NVL (CVANUMERICVALUE * 100, 0)
              INTO NCOEF
              FROM CCHVALUE CCH
             WHERE     ACTID =
                       SUBSTR (
                           P_ENT_ID,
                           INSTR (P_ENT_ID, 'Actid-') + 6,
                             INSTR (P_ENT_ID, '||')
                           - INSTR (P_ENT_ID, 'Actid-')
                           - 6)
                   AND CCH.CCHSID =
                       (SELECT CCHSID
                          FROM CUSTOMCHARACTERISTIC
                         WHERE CCHVALUECODE = 'CC_BPREBOOKURATE');
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                NCOEF := 0;
            WHEN OTHERS
            THEN
                NCOEF := 0;
        END;

        --- Get the Amount
        ---  Get the Coef
        BEGIN
            SELECT NVL (CVANUMERICVALUE, 0)
              INTO NAMOUNT
              FROM CCHVALUE CCH
             WHERE     ACTID =
                       SUBSTR (
                           P_ENT_ID,
                           INSTR (P_ENT_ID, 'Actid-') + 6,
                             INSTR (P_ENT_ID, '||')
                           - INSTR (P_ENT_ID, 'Actid-')
                           - 6)
                   AND CCH.CCHSID = (SELECT CCHSID
                                       FROM CUSTOMCHARACTERISTIC
                                      WHERE CCHVALUECODE = 'CC_BPREBOOKU');
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                NAMOUNT := 0;
            WHEN OTHERS
            THEN
                NAMOUNT := 0;
        END;

        --- Calculation
        BEGIN
            SELECT NVL (AA.RVVCODE, 1)
              INTO NRVVCODE
              FROM AAGRVE AA
             WHERE     ACTID =
                       SUBSTR (
                           P_ENT_ID,
                           INSTR (P_ENT_ID, 'Actid-') + 6,
                             INSTR (P_ENT_ID, '||')
                           - INSTR (P_ENT_ID, 'Actid-')
                           - 6)
                   AND RVACODE = 'ADMRATE'
                   AND AVEDTFIN IS NULL;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                NRVVCODE := 1;
            WHEN OTHERS
            THEN
                NRVVCODE := 1;
        END;

        --- choice the Value
        BEGIN
            SELECT CVALUE * NAMOUNT / 100
              INTO NAMOUNT
              FROM (WITH
                        DEFMATRIX
                        AS
                            (SELECT 0  MINLIMIT,
                                    30 MAXLIMIT,
                                    1  RATENUMBER,
                                    30 CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 30.1 MINLIMIT,
                                    50   MAXLIMIT,
                                    1    RATENUMBER,
                                    50   CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 50.1 MINLIMIT,
                                    70   MAXLIMIT,
                                    1    RATENUMBER,
                                    70   CVALUE
                               FROM DUAL
                             UNION
                             SELECT 70.1 MINLIMIT,
                                    90   MAXLIMIT,
                                    1    RATENUMBER,
                                    90   CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 90.1 MINLIMIT,
                                    100  MAXLIMIT,
                                    1    RATENUMBER,
                                    120  CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 0  MINLIMIT,
                                    30 MAXLIMIT,
                                    2  RATENUMBER,
                                    50 CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 30.1 MINLIMIT,
                                    50   MAXLIMIT,
                                    2    RATENUMBER,
                                    70   CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 50.1 MINLIMIT,
                                    70   MAXLIMIT,
                                    2    RATENUMBER,
                                    90   CVALUE
                               FROM DUAL
                             UNION
                             SELECT 70.1 MINLIMIT,
                                    90   MAXLIMIT,
                                    2    RATENUMBER,
                                    100  CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 90.1 MINLIMIT,
                                    100  MAXLIMIT,
                                    2    RATENUMBER,
                                    120  CVALUE
                               FROM DUAL
                             UNION ALL
                             SELECT 0   MINLIMIT,
                                    100 MAXLIMIT,
                                    3   RATENUMBER,
                                    100 CVALUE
                               FROM DUAL)
                    SELECT *
                      FROM DEFMATRIX)
             WHERE     RATENUMBER = NRVVCODE
                   AND NCOEF BETWEEN MINLIMIT AND MAXLIMIT;
        EXCEPTION
            WHEN OTHERS
            THEN
                NAMOUNT := NULL;
        END;

        NRETURN := NAMOUNT;
        --- OLD VAlue f_CVANUMERICVALUE (p_ent_id ,'CC_TPREBOOK') - f_CVANUMERICVALUE (p_ent_id ,'CC_BPREBOOKR')  ;
        RETURN NRETURN;
    END SQLAPREBOOKU;

    --Total pre-booking limit: (CC: TPREBOOKU) Logic = Adjusted pre-booking limit + temporary pre-booking limit *
    FUNCTION SQLFCTTPREBOOKU (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        --- New Change
        NRETURN :=
              SQLAPREBOOKU (P_ENT_ID, P_LANCODE, P_UTICODE)
            + F_CVANUMERICVALUE (P_ENT_ID, 'CC_TMPPREBOOKU');
        --- Old value nReturn := f_CVANUMERICVALUE (p_ent_id ,'CC_APREBOOKU') + f_CVANUMERICVALUE (p_ent_id ,'CC_TMPPREBOOKU')  ;
        RETURN NRETURN;
    END SQLFCTTPREBOOKU;

    -- Remaining pre-booking limit (CC: RPREBOOKU) Logic = Total pre-booking limit - Pre-booking run-out limit
    FUNCTION SQLFCTRPREBOOKU (P_ENT_ID     VARCHAR2,
                              P_LANCODE    VARCHAR2,
                              P_UTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        ---New Change 2
        NRETURN :=
              SQLFCTTPREBOOKU (P_ENT_ID, P_LANCODE, P_UTICODE)
            - SQLFCTBPREBOOKRU (P_ENT_ID, P_LANCODE, P_UTICODE);
        ---New Change
        --nReturn := SQLFCTTPREBOOKU (p_ent_id,p_lancode,p_uticode) - f_CVANUMERICVALUE (p_ent_id ,'CC_BPREBOOKRU')  ;
        -- Old value nReturn := f_CVANUMERICVALUE (p_ent_id ,'CC_TPREBOOKU') - f_CVANUMERICVALUE (p_ent_id ,'CC_BPREBOOKRU')  ;
        RETURN NRETURN;
    END SQLFCTRPREBOOKU;

    FUNCTION SQLFCTPARK1 (SENTID      VARCHAR2,
                          SLANCODE    VARCHAR2,
                          SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRETURN        CCHVALUE.CVASTRINGVALUE%TYPE;
            NADMIDCHILD    NUMBER;
            NADMIDPARENT   NUMBER;
        BEGIN
            SELECT REPLACE (REPLACE (SENTID, 'Admid-', ''), '||', '')
              INTO NADMIDCHILD
              FROM DUAL;

            SELECT MAX (ADMPARENT.ADMID)
              INTO NADMIDPARENT
              FROM ADMINISTRATIF ADMPARENT, ADMINISTRATIF ADMCHILD, FORMALITE
             WHERE     ADMPARENT.DOSID = ADMCHILD.DOSID
                   AND ADMCHILD.ADMID = NADMIDCHILD
                   AND FORMALITE.FORID = ADMPARENT.FORID
                   AND FORMALITE.FORTYPE = 'SURVEY';

            SELECT F_CVASTRINGVALUE ('Admid-' || NADMIDPARENT || '||',
                                     'CC_VERIPARK')
              INTO SRETURN
              FROM DUAL;

            IF SRETURN IS NULL
            THEN
                RETURN NULL;
            END IF;

            SELECT DECODE (SRETURN, '01', 'Y', 'N') INTO SRETURN FROM DUAL;

            RETURN SRETURN;
        END;
    END SQLFCTPARK1;

    FUNCTION SQLFCTASSOPTION (SENTID      VARCHAR2,
                              SLANCODE    VARCHAR2,
                              SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRETURN   VARCHAR2 (250) := '';
            NDOSID    NUMBER;
        BEGIN
            SELECT CVA.DOSID
              INTO NDOSID
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CCH.CCHSID = CVA.CCHSID
                   AND CCH.CCHVALUECODE = 'CC_INSURANCE'
                   AND CVAPKEYVALUE = SENTID;

            SELECT ASS.ASSNUM || ' - ' || LPTP.TPTLIBELLE
              INTO SRETURN
              FROM ASSDOSSIER ASD, ASSURANCE ASS, LANPASSTYPEPOLICE LPTP
             WHERE     ASD.ASSID = ASS.ASSID
                   AND ASS.PTPTYPE = LPTP.PTPTYPE
                   AND ASD.DOSID = NDOSID
                   AND LPTP.LANCODE = SLANCODE;

            RETURN SRETURN;
        END;
    END SQLFCTASSOPTION;

    FUNCTION SQLFCTCAR1 (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRETURN        CCHVALUE.CVASTRINGVALUE%TYPE;
            NADMIDCHILD    NUMBER;
            NADMIDPARENT   NUMBER;
        BEGIN
            SELECT REPLACE (REPLACE (SENTID, 'Admid-', ''), '||', '')
              INTO NADMIDCHILD
              FROM DUAL;

            SELECT MAX (ADMPARENT.ADMID)
              INTO NADMIDPARENT
              FROM ADMINISTRATIF ADMPARENT, ADMINISTRATIF ADMCHILD, FORMALITE
             WHERE     ADMPARENT.DOSID = ADMCHILD.DOSID
                   AND ADMCHILD.ADMID = NADMIDCHILD
                   AND FORMALITE.FORID = ADMPARENT.FORID
                   AND FORMALITE.FORTYPE = 'SURVEY';

            SELECT F_CVASTRINGVALUE ('Admid-' || NADMIDPARENT || '||',
                                     'CC_VERICAR')
              INTO SRETURN
              FROM DUAL;

            IF SRETURN IS NULL
            THEN
                RETURN NULL;
            END IF;

            SELECT DECODE (SRETURN, '01', 'Y', 'N') INTO SRETURN FROM DUAL;

            RETURN SRETURN;
        END;
    END SQLFCTCAR1;

    FUNCTION SQLFCTVIN1 (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SRETURN        CCHVALUE.CVASTRINGVALUE%TYPE;
            NADMIDCHILD    NUMBER;
            NADMIDPARENT   NUMBER;
        BEGIN
            SELECT REPLACE (REPLACE (SENTID, 'Admid-', ''), '||', '')
              INTO NADMIDCHILD
              FROM DUAL;

            SELECT MAX (ADMPARENT.ADMID)
              INTO NADMIDPARENT
              FROM ADMINISTRATIF ADMPARENT, ADMINISTRATIF ADMCHILD, FORMALITE
             WHERE     ADMPARENT.DOSID = ADMCHILD.DOSID
                   AND ADMCHILD.ADMID = NADMIDCHILD
                   AND FORMALITE.FORID = ADMPARENT.FORID
                   AND FORMALITE.FORTYPE = 'SURVEY';

            SELECT F_CVASTRINGVALUE ('Admid-' || NADMIDPARENT || '||',
                                     'CC_LICVSVIN')
              INTO SRETURN
              FROM DUAL;

            IF SRETURN IS NULL
            THEN
                RETURN NULL;
            END IF;

            IF SRETURN IS NULL
            THEN
                RETURN NULL;
            END IF;

            SELECT DECODE (SRETURN, '01', 'Y', 'N') INTO SRETURN FROM DUAL;

            RETURN SRETURN;
        END;
    END SQLFCTVIN1;

    /*************************** added to test by Park Young min 2014.05.19***********************************/
    FUNCTION F_GETRVACODECNT (P_ACTID NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            W_ROLCNT   NUMBER;
            R_VALUE    NUMBER;
        BEGIN
            W_ROLCNT := 0;
            R_VALUE := 0;

            BEGIN
                  SELECT NVL (SUM (NVL2 (B.RVACODE, 0, 1)), 1)
                    INTO W_ROLCNT
                    FROM (  SELECT ROLCODE, RVACODE
                              FROM ROLVARIABLE
                             WHERE     ROLCODE IN
                                           (SELECT ROLCODE
                                              FROM AROAGE
                                             WHERE     ACTID = P_ACTID
                                                   AND PHACODE = 'ES'
                                                   AND AAGDTFIN IS NULL)
                                   AND RVAFLAGMANDATORY = 1
                          GROUP BY ROLCODE, RVACODE
                          ORDER BY 1) A,
                         (  SELECT AGR.ROLCODE, AGR.RVACODE
                              FROM AAGRVE AGR, ROLVARIABLE RVA
                             WHERE     AGR.ROLCODE IN
                                           (SELECT ROLCODE
                                              FROM AROAGE
                                             WHERE     ACTID = P_ACTID
                                                   AND PHACODE = 'ES'
                                                   AND AAGDTFIN IS NULL)
                                   AND AGR.ROLCODE = RVA.ROLCODE
                                   AND AGR.RVACODE = RVA.RVACODE
                                   AND AGR.AVEDTFIN IS NULL
                                   AND RVA.RVAFLAGMANDATORY = 1
                                   AND ACTID = P_ACTID
                          GROUP BY AGR.ROLCODE, AGR.RVACODE
                          ORDER BY 1) B
                   WHERE A.ROLCODE = B.ROLCODE(+) AND A.RVACODE = B.RVACODE(+)
                ORDER BY 1;
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    W_ROLCNT := 1;
                WHEN OTHERS
                THEN
                    W_ROLCNT := 1;
            END;

            IF W_ROLCNT > 0
            THEN
                R_VALUE := 1;
            ELSE
                R_VALUE := 0;
            END IF;

            RETURN R_VALUE;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.PUT_LINE ('F_GetRvaCodeCnt ERRORS');
                DBMS_OUTPUT.PUT_LINE ('SQLERRM OTHERS = ' || SQLERRM);
                RETURN NULL;
        END;
    END F_GETRVACODECNT;

    /*************************** added to test by Park Young min 2014.05.19***********************************/
    /*************************** added to test by Park Young min 2014.05.20***********************************/
    FUNCTION F_ISRIMORDRECNT (P_REGID REGIMPUTATION.REGID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NORDER    NUMBER;
            NCOUNT    NUMBER;
            R_VALUE   NUMBER;
        BEGIN
            BEGIN
                SELECT MAX (NVL (LKI.RIMORDRE, 0))
                  INTO NORDER
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI
                 WHERE     RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.REGID = P_REGID
                       AND NVL (LKI.RFLMTHTBASE - LKI.RFLMTAMORT, 0) != 0
                       AND EXISTS
                               (SELECT *
                                  FROM FACREFERENCE FRE, DOSSIER DOS
                                 WHERE     DOS.DOSID = FRE.FREDOSID
                                       AND DOS.TPGCODE LIKE 'A%'
                                       AND FRE.FACID = RIM.FACID);
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    NORDER := 0;
                WHEN OTHERS
                THEN
                    NORDER := 0;
            END;

            BEGIN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = P_REGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    NCOUNT := 0;
                WHEN OTHERS
                THEN
                    NCOUNT := 0;
            END;

            IF NCOUNT > 0 AND NORDER > 0
            THEN
                R_VALUE := 1;
            ELSE
                R_VALUE := 0;
            END IF;

            RETURN R_VALUE;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.PUT_LINE ('F_IsRimOrdreCnt ERRORS');
                DBMS_OUTPUT.PUT_LINE ('SQLERRM OTHERS = ' || SQLERRM);
                RETURN NULL;
        END;
    END F_ISRIMORDRECNT;

    /*************************** added to test by Park Young min 2014.05.20***********************************/
    /*************************** added to test by Park Young min 2014.05.21***********************************/
    FUNCTION F_ISCOMPNEWLOAN (P_DOSID DOSSIER.DOSID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NDPRNUMCASSIOPEE   DOSSIER.DOSNUM%TYPE;
            OLDLOANAMOUNT      NUMBER;
            NEWLOANAMOUNT      NUMBER;
            R_VALUE            NUMBER;
        BEGIN
            BEGIN
                SELECT MAX (DPRNUMCASSIOPEE)
                  INTO NDPRNUMCASSIOPEE
                  FROM DOSSIERPROSPECT
                 WHERE DOSID = P_DOSID;                              ---nDOSID
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    NDPRNUMCASSIOPEE := 0;
                WHEN OTHERS
                THEN
                    NDPRNUMCASSIOPEE := 0;
            END;

            BEGIN
                SELECT NVL (DOSMTPRODUCT, 0)
                  INTO OLDLOANAMOUNT
                  FROM DOSSIER
                 WHERE     DOSNUM = NDPRNUMCASSIOPEE
                       AND DOSAVENANT IN (SELECT MAX (DOSAVENANT)
                                            FROM DOSSIER
                                           WHERE DOSNUM = NDPRNUMCASSIOPEE);
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    OLDLOANAMOUNT := 0;
                WHEN OTHERS
                THEN
                    OLDLOANAMOUNT := 0;
            END;

            BEGIN
                SELECT NVL (PFADOUBLE, 0)
                  INTO NEWLOANAMOUNT
                  FROM PFIATTRIBUT
                 WHERE     PFACODE = 'FINANCEDVALUE'
                       AND PFIID IN
                               (SELECT PFIID
                                  FROM DPRPROPFINANCE   DPF,
                                       DOSSIERPROSPECT  DPR
                                 WHERE     DPF.DOSID = P_DOSID
                                       AND DPF.DPRVERSION =
                                           (SELECT DPRVERSION
                                              FROM V_DEAL VDE
                                             WHERE VDE.DOSID = P_DOSID)
                                       AND DPF.DPFFLAGRETENUE = 1
                                       AND ROWNUM = 1);
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    NEWLOANAMOUNT := 0;
                WHEN OTHERS
                THEN
                    NEWLOANAMOUNT := 0;
            END;

            IF NEWLOANAMOUNT > OLDLOANAMOUNT
            THEN
                R_VALUE := 0;
            ELSE
                R_VALUE := 1;
            END IF;

            RETURN R_VALUE;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.PUT_LINE ('F_IsCompNewLoan ERRORS');
                DBMS_OUTPUT.PUT_LINE ('SQLERRM OTHERS = ' || SQLERRM);
                RETURN NULL;
        END;
    END F_ISCOMPNEWLOAN;

    /*************************** added to test by Park Young min 2014.05.21***********************************/

    /*
                    JLE 04/03/2015 - Edit function GETTAEG,GETTAEGBIS and GETTAEA
                                   - We check in the the table 'CCHVALUE' if the table 'DOSMARGEVENTIL' is empty for a dossier

    */
    FUNCTION GETTAEG (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        SELECT DMVTX
          INTO NRETURN
          FROM DOSMARGEVENTIL
         WHERE     DOSID = SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
               AND TMVCODE = 'TAEG'
               AND DMVDTEND IS NULL;

        RETURN NRETURN;
    EXCEPTION
        WHEN OTHERS
        THEN
            BEGIN
                SELECT CVANUMERICVALUE
                  INTO NRETURN
                  FROM CCHVALUE
                 WHERE     DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND CCHSID = (SELECT CCHSID
                                       FROM CUSTOMCHARACTERISTIC
                                      WHERE CCHVALUECODE = 'TAEG');

                RETURN NRETURN;
            EXCEPTION
                WHEN OTHERS
                THEN
                    RETURN NRETURN;
            END;
    END GETTAEG;

    FUNCTION GETTAEGBIS (SENTID      VARCHAR2,
                         SLANCODE    VARCHAR2,
                         SUTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        SELECT DMVTX
          INTO NRETURN
          FROM DOSMARGEVENTIL
         WHERE     DOSID = SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
               AND TMVCODE = 'TAEGBIS'
               AND DMVDTEND IS NULL;

        RETURN NRETURN;
    EXCEPTION
        WHEN OTHERS
        THEN
            BEGIN
                SELECT CVANUMERICVALUE
                  INTO NRETURN
                  FROM CCHVALUE
                 WHERE     DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND CCHSID = (SELECT CCHSID
                                       FROM CUSTOMCHARACTERISTIC
                                      WHERE CCHVALUECODE = 'TAEGBIS');

                RETURN NRETURN;
            EXCEPTION
                WHEN OTHERS
                THEN
                    RETURN NRETURN;
            END;
    END GETTAEGBIS;

    FUNCTION GETTAEA (SENTID VARCHAR2, SLANCODE VARCHAR2, SUTICODE VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        SELECT DMVTX
          INTO NRETURN
          FROM DOSMARGEVENTIL
         WHERE     DOSID = SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
               AND TMVCODE = 'TAEA'
               AND DMVDTEND IS NULL;

        RETURN NRETURN;
    EXCEPTION
        WHEN OTHERS
        THEN
            BEGIN
                SELECT CVANUMERICVALUE
                  INTO NRETURN
                  FROM CCHVALUE
                 WHERE     DOSID =
                           SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                       AND CCHSID = (SELECT CCHSID
                                       FROM CUSTOMCHARACTERISTIC
                                      WHERE CCHVALUECODE = 'TAEA');

                RETURN NRETURN;
            EXCEPTION
                WHEN OTHERS
                THEN
                    RETURN NRETURN;
            END;
    END GETTAEA;

    -- JLE - 20160210 - Fonction qui rp le nombre de renouvelement effectu??
    FUNCTION F_GETNBRENEWAL (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN NUMBER
    AS
        NRETURN   NUMBER := 0;
    BEGIN
        SELECT COUNT (*)
          INTO NRETURN
          FROM crevt c, credata cd
         WHERE     c.creid = cd.creid
               AND c.dosid = SENTID
               AND c.tmffonction = 'EVD_NEGO'
               AND cd.cdacolonne = 'TYPENEGO'
               AND cdadatastring = 'EXTENSION'
               AND c.credtvalid IS NOT NULL
               AND c.credtsup IS NULL;

        RETURN NRETURN;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NRETURN;
    END F_GETNBRENEWAL;

    FUNCTION GETRISKDECIVALUE (sEntId      VARCHAR2,
                               sLanCode    VARCHAR2,
                               sUtiCode    VARCHAR2)
        RETURN VARCHAR2
    IS
        sADELIBELLE   LANAVTDECISION.ADELIBELLE%TYPE;
    BEGIN
        BEGIN
            SELECT ADELIBELLE
              INTO sADELIBELLE
              FROM DPRDECISION D, LANAVTDECISION L
             WHERE     D.DOSID = SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND D.ADECODE = L.ADECODE
                   AND L.LANCODE = sLanCode
                   AND D.ADECODE IN ('SCOREOK', 'RISKOK')
                   AND D.DPRVERSION IN ('PROD')
                   AND D.DDEORDRE =
                       (SELECT MAX (D1.DDEORDRE)
                          FROM DPRDECISION D1
                         WHERE     D1.DOSID = D.DOSID
                               AND D1.ADECODE = D.ADECODE
                               AND D1.DPRVERSION = D.DPRVERSION);
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                BEGIN
                    SELECT ADELIBELLE
                      INTO sADELIBELLE
                      FROM DPRDECISION D, LANAVTDECISION L
                     WHERE     D.DOSID =
                               SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                           AND D.ADECODE = L.ADECODE
                           AND L.LANCODE = sLanCode
                           AND D.ADECODE IN ('SCOREOK', 'RISKOK')
                           AND D.DPRVERSION IN ('FIN')
                           AND D.DDEORDRE =
                               (SELECT MAX (D1.DDEORDRE)
                                  FROM DPRDECISION D1
                                 WHERE     D1.DOSID = D.DOSID
                                       AND D1.ADECODE = D.ADECODE
                                       AND D1.DPRVERSION = D.DPRVERSION);
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sADELIBELLE := NULL;
                END;
            WHEN OTHERS
            THEN
                sADELIBELLE := NULL;
        END;

        RETURN sADELIBELLE;
    END GETRISKDECIVALUE;

    FUNCTION F_GET_ITR (SENTID      VARCHAR2,
                        SLANCODE    VARCHAR2,
                        SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            nDOSID              DOSSIER.DOSID%TYPE;
            sPHACODE            DOSPHASE.PHACODE%TYPE;
            nNBDAYS_DOSDTFIN    NUMBER := 0;
            nCREID              CREVT.CREID%TYPE;
            dtCREDTEFFET        CREVT.CREDTEFFET%TYPE;
            dtDOSDTFIN          DOSSIER.DOSDTFIN%TYPE;
            nFTVID              FORMULETXVAR.FTVID%TYPE;
            dtMAXTVADTVIGUEUR   TAUVALEUR.TVADTVIGUEUR%TYPE;
            nMINDURATION        NUMBER := 0;
            nITR                NUMBER := 0;
        BEGIN
            BEGIN
                ----nDOSID:Get the DOSID from the P_ENTID
                SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDOSID
                  FROM DUAL;

                ----sPHACODE: Contract current phase
                SELECT PHACODE
                  INTO sPHACODE
                  FROM DOSPHASE
                 WHERE DOSID = nDOSID AND DPHDTFIN IS NULL;

                ----dtDOSDTFIN: Contract end date
                SELECT DOSDTFIN
                  INTO dtDOSDTFIN
                  FROM DOSSIER
                 WHERE DOSID = nDOSID;


                ----If contract phase in ('ES', 'TER')then we process, else nITR will be 0 by default

                IF sPHACODE IN ('ES', 'TER')
                THEN
                    ----nCREID: CREID of event EVD_MEL
                    SELECT MAX (CREID)
                      INTO nCREID
                      FROM CREVT
                     WHERE     DOSID = nDOSID
                           AND TMFFONCTION = 'EVD_MEL'
                           AND CREDTVALID IS NOT NULL
                           AND CREDTSUP IS NULL;

                    IF nCREID IS NULL
                    THEN
                        nITR := 0;
                    ELSE
                        ----dtCREDTEFFET: Effective date of EVD_MEL event
                        SELECT CREDTEFFET
                          INTO dtCREDTEFFET
                          FROM CREVT
                         WHERE CREID = nCREID;


                        ----nFTVID: FTVID of the rate formula'ZCR'
                        SELECT MAX (FTVID)
                          INTO nFTVID
                          FROM FORMULETXVAR
                         WHERE FTVCODE = 'ZCR';


                        IF nFTVID IS NULL
                        THEN
                            nITR := 0;
                        ELSE
                            ----nNBDAYS_DOSDTFIN: Number of days from event effective date untill contract maturity date
                            SELECT dtDOSDTFIN - dtCREDTEFFET
                              INTO nNBDAYS_DOSDTFIN
                              FROM DUAL;


                            ----dtMAXTVADTVIGUEUR
                            SELECT MAX (TVADTVIGUEUR)
                              INTO dtMAXTVADTVIGUEUR
                              FROM TAUVALEUR
                             WHERE     TVADTVIGUEUR <= dtCREDTEFFET
                                   AND TAUCODE IN (SELECT TAUCODE
                                                     FROM TAUX
                                                    WHERE FTVID = nFTVID);

                            IF dtMAXTVADTVIGUEUR IS NULL
                            THEN
                                nITR := 0;
                            ELSE
                                ----nMINDURATION
                                SELECT MIN (TVADTFIN - TVADTVIGUEUR)
                                  INTO nMINDURATION
                                  FROM TAUVALEUR
                                 WHERE     TVADTVIGUEUR = dtMAXTVADTVIGUEUR
                                       AND (TVADTFIN - TVADTVIGUEUR) >=
                                           nNBDAYS_DOSDTFIN
                                       AND TAUCODE IN (SELECT TAUCODE
                                                         FROM TAUX
                                                        WHERE FTVID = nFTVID);


                                ----a. Get the ITR value from table TAUVALEUR
                                ----b. The rates (TAUCODE) to consider are those that belong to the formula with FTVCODE = 'ZCP'
                                ----c. The maturity date for each rate is stored in TAUVALEUR.TVADTFIN
                                ----d. The number of days of maturity for each rate is TVADTFIN - TVADTVIGUEUR
                                ----e. We look for the rate value where dtCREDTEFFET = TVADTVIGUEUR. If no records found in TAUVALEUR with that date then we consider the previous TVADTVIGUEUR.
                                -------That's the reason to use TVADTVIGUEUR <= dtCREDTEFFET
                                ----f. The rate maturity to consider is the one with number of days (TVADTFIN - TVADTVIGUEUR) equal to number of days from event date to contract maturity (nNBDAYS_DOSDTFIN)
                                -------If no record found then we consider the upper maturity. That's the reason to use (TVADTFIN - TVADTVIGUEUR) >= nNBDAYS_DOSDTFIN
                                ----g. The rate to get is TVAVAL

                                SELECT NVL (TVAVAL, 0)
                                  INTO nITR
                                  FROM TAUVALEUR
                                 WHERE     TAUCODE IN (SELECT TAUCODE
                                                         FROM TAUX
                                                        WHERE FTVID = nFTVID)
                                       AND TVADTVIGUEUR = dtMAXTVADTVIGUEUR
                                       AND (TVADTFIN - TVADTVIGUEUR) =
                                           nMINDURATION;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            END;

            RETURN NVL (nITR, 0);
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_GET_ITR;



    FUNCTION F_PRODUCTCATALOGUE_CODE (SENTID      VARCHAR2,
                                      SLANCODE    VARCHAR2,
                                      SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            nDOSID          DOSSIER.DOSID%TYPE;
            sBUSINESSLINE   DOSSIER.TACCODE%TYPE;
            sAREA           DOSSIER.TPGCODE%TYPE;
            sCATEGORY       ITRRUBRIQUE.IRUETATIMMO%TYPE;
            sVEHICLETYPE    ITRRUBRIQUE.NAPCODE%TYPE;
            sTEST           VARCHAR2 (50);
            sPCCODE         VARCHAR2 (50);
        BEGIN
            BEGIN
                ----nDOSID: Get the DOSID from the P_ENTID
                SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDOSID
                  FROM DUAL;


                ----sBUSINESSLINE: Level 1 - Business line
                SELECT TUPLIBELLE
                  INTO sBUSINESSLINE
                  FROM LANTUSPARAM
                 WHERE     TUSNOM = 'PCBUSINESSLINE'
                       AND LANCODE = sLANCODE
                       AND TUPCODE = (SELECT TACCODE
                                        FROM DOSSIER
                                       WHERE DOSID = nDOSID);


                ----sAREA: Level 2 - Area
                SELECT TUPLIBELLE
                  INTO sAREA
                  FROM LANTUSPARAM
                 WHERE     TUSNOM = 'PCAREA'
                       AND LANCODE = sLANCODE
                       AND TUPCODE = (SELECT TPGCODE
                                        FROM DOSSIER
                                       WHERE DOSID = nDOSID);


                ----sTEST = Test on the level 2
                sTEST := sBUSINESSLINE || sAREA;

                IF sTEST IN ('14', '23', '133')
                THEN
                    sPCCODE := sTEST;
                ELSE
                    ----sCATEGORY: Level 4 - Category
                    SELECT TUPLIBELLE
                      INTO sCATEGORY
                      FROM LANTUSPARAM
                     WHERE     TUSNOM = 'PCCATEGORY'
                           AND LANCODE = sLANCODE
                           AND TUPCODE =
                               (SELECT IRUETATIMMO
                                  FROM ITRRUBRIQUE
                                 WHERE ITRID = (SELECT MAX (ITRID)
                                                  FROM LKDOSRUBITRRUB
                                                 WHERE DOSID = nDOSID));


                    ----sVEHICLETYPE: Level 5 - Vehicle type
                    SELECT TUPLIBELLE
                      INTO sVEHICLETYPE
                      FROM LANTUSPARAM
                     WHERE     TUSNOM = 'PCVEHICLETYPE'
                           AND LANCODE = sLANCODE
                           AND TUPCODE =
                               (SELECT NAPCODE
                                  FROM ITRRUBRIQUE
                                 WHERE ITRID = (SELECT MAX (ITRID)
                                                  FROM LKDOSRUBITRRUB
                                                 WHERE DOSID = nDOSID));



                    ----sPCCODE = concatenation of the 4 levels

                    sPCCODE :=
                        sBUSINESSLINE || sAREA || sCATEGORY || sVEHICLETYPE;
                END IF;
            END;


            RETURN NVL (sPCCODE, '');
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_PRODUCTCATALOGUE_CODE;



    FUNCTION F_PRODUCTCATALOGUE_LABEL (SENTID      VARCHAR2,
                                       SLANCODE    VARCHAR2,
                                       SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            sPCCODE    VARCHAR2 (50);
            sPCLABEL   VARCHAR2 (200);
        BEGIN
            BEGIN
                SELECT PAV4_CCFUNCTIONS.F_PRODUCTCATALOGUE_CODE (SENTID,
                                                                 SLANCODE,
                                                                 SUTICODE)
                  INTO sPCCODE
                  FROM DUAL;

                SELECT MAX (TUPLIBELLE)
                  INTO sPCLABEL
                  FROM LANTUSPARAM
                 WHERE     TUSNOM = 'PCDESCRIPTION'
                       AND LANCODE = sLANCODE
                       AND TUPCODE = sPCCODE;
            END;

            RETURN sPCLABEL;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_PRODUCTCATALOGUE_LABEL;



    FUNCTION F_CLIENTTYPE_CODE (SENTID      VARCHAR2,
                                SLANCODE    VARCHAR2,
                                SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            nDOSID               DOSSIER.DOSID%TYPE;
            nACTID               DOSACTEUR.ACTID%TYPE;
            sNAFCODE             ACTEUR.NAFCODE%TYPE;
            sACTTYPE             ACTEUR.ACTTYPE%TYPE;
            sOTHER_PART          LANTUSPARAM.TUPLIBELLE%TYPE;
            sCTCODE_PART         LANTUSPARAM.TUPLIBELLE%TYPE;
            sOTHER_EI            LANTUSPARAM.TUPLIBELLE%TYPE;
            sCTCODE_EI           LANTUSPARAM.TUPLIBELLE%TYPE;
            sOTHER_PME           LANTUSPARAM.TUPLIBELLE%TYPE;
            sCTCODE_PME          LANTUSPARAM.TUPLIBELLE%TYPE;
            sOTHER_ENTREPRISE    LANTUSPARAM.TUPLIBELLE%TYPE;
            sCTCODE_ENTREPRISE   LANTUSPARAM.TUPLIBELLE%TYPE;
            nANAID               ANALYSIS.ANAID%TYPE;
            nCA                  NUMBER;
            nEFFECTIF            NUMBER;

            sCTCODE              VARCHAR2 (50);
        BEGIN
            BEGIN
                ----nDOSID: Get the DOSID from the P_ENTID
                SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDOSID
                  FROM DUAL;

                ----nACTID: Client ACTID
                SELECT MAX (ACTID)
                  INTO nACTID
                  FROM DOSACTEUR
                 WHERE     DOSID = nDOSID
                       AND ROLCODE IN ('CLIENT', 'EMPRUNT')
                       AND DACDTFIN IS NULL
                       AND DACORDRE =
                           (SELECT MAX (DACORDRE)
                              FROM DOSACTEUR
                             WHERE     DOSID = nDOSID
                                   AND ROLCODE IN ('CLIENT', 'EMPRUNT')
                                   AND DACDTFIN IS NULL);

                IF nACTID IS NULL
                THEN
                    sCTCODE := '';
                ELSE
                    ----sACTTYPE: Actor type
                    SELECT MAX (ACTTYPE)
                      INTO sACTTYPE
                      FROM ACTEUR
                     WHERE ACTID = nACTID;


                    IF sACTTYPE IS NULL
                    THEN
                        sCTCODE := '';
                    ELSE
                        ----sNAFCODE: NAFCODE
                        SELECT MAX (NAFCODE)
                          INTO sNAFCODE
                          FROM ACTEUR
                         WHERE ACTID = nACTID;

                        SELECT MAX (ANAID)
                          INTO nANAID
                          FROM ANALYSIS
                         WHERE     ANMID = 96
                               AND ANATARGET = 'ACTEUR'
                               AND ANASTATUS <> 'ARCH'
                               AND ACTID = nACTID;



                        --Code by default when ACTTYPE = 'PART'
                        SELECT MAX (TUPLIBELLE)
                          INTO sOTHER_PART
                          FROM LANTUSPARAM
                         WHERE     TUSNOM = 'CTPART'
                               AND LANCODE = sLANCODE
                               AND TUPCODE = 'OTHER';

                        --Code by default when ACTTYPE = 'EI'
                        SELECT MAX (TUPLIBELLE)
                          INTO sOTHER_EI
                          FROM LANTUSPARAM
                         WHERE     TUSNOM = 'CTEI'
                               AND LANCODE = sLANCODE
                               AND TUPCODE = 'OTHER';

                        --Code by default when ACTTYPE = 'PM' and the client is a PME regarding the criteria
                        SELECT MAX (TUPLIBELLE)
                          INTO sOTHER_PME
                          FROM LANTUSPARAM
                         WHERE     TUSNOM = 'CTPME'
                               AND LANCODE = sLANCODE
                               AND TUPCODE = 'OTHER';

                        --Code by default when ACTTYPE = 'PM' and the client is ENTREPRISE regarding the criteria
                        SELECT MAX (TUPLIBELLE)
                          INTO sOTHER_ENTREPRISE
                          FROM LANTUSPARAM
                         WHERE     TUSNOM = 'CTENTREPRISE'
                               AND LANCODE = sLANCODE
                               AND TUPCODE = 'OTHER';


                        IF sACTTYPE = 'PART'
                        --We check in external table 'CTPART' if there is a TUPCODE = sNAFCODE;
                        --If yes, then sCTCODE = the related TUPLIBELLE, else sCTCODE = TUPLIBELLE related to TUPCODE = 'OTHER'
                        THEN
                            SELECT MAX (TUPLIBELLE)
                              INTO sCTCODE_PART
                              FROM LANTUSPARAM
                             WHERE     TUSNOM = 'CTPART'
                                   AND TUPCODE = sNAFCODE
                                   AND LANCODE = sLANCODE;

                            IF sCTCODE_PART IS NOT NULL
                            THEN
                                sCTCODE := sCTCODE_PART;
                            ELSE
                                sCTCODE := sOTHER_PART;
                            END IF;
                        END IF;


                        IF sACTTYPE = 'EI'
                        --We check in external table 'CTEI' if there is a TUPCODE = sNAFCODE;
                        --If yes, then sCTCODE = the related TUPLIBELLE, else sCTCODE = TUPLIBELLE related to TUPCODE = 'OTHER'
                        THEN
                            SELECT MAX (TUPLIBELLE)
                              INTO sCTCODE_EI
                              FROM LANTUSPARAM
                             WHERE     TUSNOM = 'CTEI'
                                   AND TUPCODE = sNAFCODE
                                   AND LANCODE = sLANCODE;

                            IF sCTCODE_EI IS NOT NULL
                            THEN
                                sCTCODE := sCTCODE_EI;
                            ELSE
                                sCTCODE := sOTHER_EI;
                            END IF;
                        END IF;


                        IF sACTTYPE NOT IN ('PART', 'EI')            --PM, BPM
                        THEN
                            SELECT NVL (MAX (RATVALUE), 0)
                              INTO nCA
                              FROM LKANARAT
                             WHERE ANAID = nANAID AND RATID = 40149;

                            SELECT NVL (MAX (RATVALUE), 0)
                              INTO nEFFECTIF
                              FROM LKANARAT
                             WHERE ANAID = nANAID AND RATID = 40165;

                            SELECT MAX (TUPLIBELLE)
                              INTO sCTCODE_PME
                              FROM LANTUSPARAM
                             WHERE     TUSNOM = 'CTPME'
                                   AND TUPCODE = sNAFCODE
                                   AND LANCODE = sLANCODE;

                            SELECT MAX (TUPLIBELLE)
                              INTO sCTCODE_ENTREPRISE
                              FROM LANTUSPARAM
                             WHERE     TUSNOM = 'CTENTREPRISE'
                                   AND TUPCODE = sNAFCODE
                                   AND LANCODE = sLANCODE;

                            IF nCA <= 50000000 AND nEFFECTIF < 250
                            --We check in external table 'CTPME' if there is a TUPCODE = sNAFCODE;
                            --If yes, then sCTCODE = the related TUPLIBELLE, else sCTCODE = TUPLIBELLE related to TUPCODE = 'OTHER'
                            THEN
                                IF sCTCODE_PME IS NOT NULL
                                THEN
                                    sCTCODE := sCTCODE_PME;
                                ELSE
                                    sCTCODE := sOTHER_PME;
                                END IF;
                            ELSE
                                --We check in external table 'CTENTREPRISE' if there is a TUPCODE = sNAFCODE;
                                --If yes, then sCTCODE = the related TUPLIBELLE, else sCTCODE = TUPLIBELLE related to TUPCODE = 'OTHER'
                                IF sCTCODE_ENTREPRISE IS NOT NULL
                                THEN
                                    sCTCODE := sCTCODE_ENTREPRISE;
                                ELSE
                                    sCTCODE := sOTHER_ENTREPRISE;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            END;

            RETURN sCTCODE;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_CLIENTTYPE_CODE;



    FUNCTION F_CLIENTTYPE_LABEL (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            sCTCODE    VARCHAR2 (50);
            sCTLABEL   VARCHAR2 (200);
        BEGIN
            BEGIN
                SELECT PAV4_CCFUNCTIONS.F_CLIENTTYPE_CODE (SENTID,
                                                           SLANCODE,
                                                           SUTICODE)
                  INTO sCTCODE
                  FROM DUAL;

                SELECT MAX (TUPLIBELLE)
                  INTO sCTLABEL
                  FROM LANTUSPARAM
                 WHERE     TUSNOM = 'CTDESCRIPTION'
                       AND LANCODE = sLANCODE
                       AND TUPCODE = sCTCODE;
            END;

            RETURN sCTLABEL;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_CLIENTTYPE_LABEL;


    FUNCTION F_GET_DURATION (SENTID      VARCHAR2,
                             SLANCODE    VARCHAR2,
                             SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            nDOSID       DOSSIER.DOSID%TYPE;
            nIRRGYC      DOSMARGEVENTIL.DMVTX%TYPE;
            nIMAID       NUMBER;
            nMONTH_IMA   NUMBER := 0;
            nMONTH_DOS   NUMBER := 0;
            nDURATION    NUMBER := 0;
        BEGIN
            BEGIN
                ----nDOSID:Get the DOSID from the P_ENTID
                SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDOSID
                  FROM DUAL;

                ----nIMAID: Oldest image of the contract in order to get the initial number of months before any renegotiation
                SELECT MIN (IMAID)
                  INTO nIMAID
                  FROM IMADOSSIER
                 WHERE     DOSID = nDOSID
                       AND   NVL (DOSDUREEAN, 0)
                           + NVL (DOSDUREEMOIS, 0)
                           + NVL (DOSDUREEJOUR, 0) <>
                           0;


                ----nMONTH_IMA: Initial number of months, from the oldest image
                SELECT MIN (
                           (  (NVL (DOSDUREEAN, 0) * 12)
                            + NVL (DOSDUREEMOIS, 0)
                            + ROUND (NVL (DOSDUREEJOUR, 0) / 365)))
                  INTO nMONTH_IMA
                  FROM IMADOSSIER
                 WHERE DOSID = nDOSID AND IMAID = nIMAID;

                ----nMONTH_DOS: Initial number of months from the contract, no image
                SELECT MIN (
                           (  (NVL (DOSDUREEAN, 0) * 12)
                            + NVL (DOSDUREEMOIS, 0)
                            + ROUND (NVL (DOSDUREEJOUR, 0) / 365)))
                  INTO nMONTH_DOS
                  FROM DOSSIER
                 WHERE DOSID = nDOSID;

                ----nIRRGYC: Gross Yield Car
                SELECT NVL (MAX (DMVTX), 0)
                  INTO nIRRGYC
                  FROM DOSMARGEVENTIL
                 WHERE     DOSID = nDOSID
                       AND TMVCODE = 'IRRGYC'
                       AND DMVDTEND IS NULL;

                IF nMONTH_IMA IS NOT NULL
                THEN
                    IF nIRRGYC <> 0
                    THEN
                        nDURATION :=
                            ROUND (
                                  (  (1 + (nIRRGYC / 100 / 12))
                                   / (nIRRGYC / 100 / 12))
                                -   nMONTH_IMA
                                  / (  POWER ((1 + (nIRRGYC / 100 / 12)),
                                              nMONTH_IMA)
                                     - 1),
                                0);
                    ELSE
                        nDURATION := ROUND (nMONTH_IMA / 2);
                    END IF;
                ELSE
                    IF nIRRGYC <> 0
                    THEN
                        nDURATION :=
                            ROUND (
                                  (  (1 + (nIRRGYC / 100 / 12))
                                   / (nIRRGYC / 100 / 12))
                                -   nMONTH_DOS
                                  / (  POWER ((1 + (nIRRGYC / 100 / 12)),
                                              nMONTH_DOS)
                                     - 1),
                                0);
                    ELSE
                        nDURATION := ROUND (nMONTH_DOS / 2);
                    END IF;
                END IF;
            END;

            RETURN NVL (nDURATION, 0);
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN 0;
    END F_GET_DURATION;



    FUNCTION F_GET_ITR_DURATION (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            nDOSID              DOSSIER.DOSID%TYPE;
            sPHACODE            DOSPHASE.PHACODE%TYPE;
            nDURATION           NUMBER := 0;
            nDURATION_NBDAYS    NUMBER := 0;
            nCREID              CREVT.CREID%TYPE;
            dtCREDTEFFET        CREVT.CREDTEFFET%TYPE;
            nFTVID              FORMULETXVAR.FTVID%TYPE;
            dtTVADTVIGUEUR      TAUVALEUR.TVADTVIGUEUR%TYPE;
            sTAUCODE_MINRANGE   TAUVALEUR.TAUCODE%TYPE;
            sTAUCODE_MAXRANGE   TAUVALEUR.TAUCODE%TYPE;
            nRATEMIN            NUMBER := 0;
            nRATEMAX            NUMBER := 0;
            nNBDAYSMIN          NUMBER := 0;
            nNBDAYSMAX          NUMBER := 0;
            nDENOMINATOR        NUMBER;
            nITR                NUMBER := 0;
        BEGIN
            BEGIN
                ----nDOSID:Get the DOSID from the P_ENTID
                SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDOSID
                  FROM DUAL;

                ----sPHACODE: Contract current phase
                SELECT PHACODE
                  INTO sPHACODE
                  FROM DOSPHASE
                 WHERE DOSID = nDOSID AND DPHDTFIN IS NULL;

                ----nDURATION: Contract duration
                SELECT PAV4_CCFUNCTIONS.F_GET_DURATION (SENTID,
                                                        SLANCODE,
                                                        SUTICODE)
                  INTO nDURATION
                  FROM DUAL;

                ----nDURATION_NBDAYS: Number of days corresponding the duration
                SELECT nDURATION * 30.5 --We consider 30.5 as average number of days in the year in order to simplify
                  INTO nDURATION_NBDAYS
                  FROM DUAL;

                ----If contract phase in ('ES', 'TER')then we process, else nITR will be 0 by default

                IF sPHACODE IN ('ES', 'TER')
                THEN
                    ----nCREID: CREID of event EVD_MEL
                    SELECT MAX (CREID)
                      INTO nCREID
                      FROM CREVT
                     WHERE     DOSID = nDOSID
                           AND TMFFONCTION = 'EVD_MEL'
                           AND CREDTVALID IS NOT NULL
                           AND CREDTSUP IS NULL;

                    IF nCREID IS NULL
                    THEN
                        nITR := 0;
                    ELSE
                        ----dtCREDTEFFET: Effective date of EVD_MEL event
                        SELECT TRUNC (CREDTEFFET)
                          INTO dtCREDTEFFET
                          FROM CREVT
                         WHERE CREID = nCREID;


                        ----nFTVID: FTVID of the rate formula'ZCR'
                        SELECT MAX (FTVID)
                          INTO nFTVID
                          FROM FORMULETXVAR
                         WHERE FTVCODE = 'ZCR';


                        IF nFTVID IS NULL
                        THEN
                            nITR := 0;
                        ELSE
                            ----dtTVADTVIGUEUR: the closest TVADTVIGUEUR to dtCREDTEFFET
                            SELECT MAX (TRUNC (TVADTVIGUEUR))
                              INTO dtTVADTVIGUEUR
                              FROM TAUVALEUR
                             WHERE     TAUCODE IN (SELECT TAUCODE
                                                     FROM TAUX
                                                    WHERE FTVID = nFTVID)
                                   AND TRUNC (TVADTVIGUEUR) <= dtCREDTEFFET;


                            ----sTAUCODE_MAXRANGE: TAUCODE where (TVADTFIN - TVADTVIGUEUR) >= nDURATION_NBDAYS
                            SELECT TAUCODE
                              INTO sTAUCODE_MAXRANGE
                              FROM TAUVALEUR
                             WHERE     TRUNC (TVADTVIGUEUR) = dtTVADTVIGUEUR
                                   AND TAUCODE IN (SELECT TAUCODE
                                                     FROM TAUX
                                                    WHERE FTVID = nFTVID)
                                   AND (TRUNC (TVADTFIN) - dtTVADTVIGUEUR) >=
                                       nDURATION_NBDAYS
                                   AND (TRUNC (TVADTFIN) - dtTVADTVIGUEUR) =
                                       (SELECT MIN (
                                                   (  TRUNC (TVADTFIN)
                                                    - dtTVADTVIGUEUR))
                                          FROM TAUVALEUR
                                         WHERE     TRUNC (TVADTVIGUEUR) =
                                                   dtTVADTVIGUEUR
                                               AND TAUCODE IN
                                                       (SELECT TAUCODE
                                                          FROM TAUX
                                                         WHERE FTVID = nFTVID)
                                               AND (  TRUNC (TVADTFIN)
                                                    - dtTVADTVIGUEUR) >=
                                                   nDURATION_NBDAYS);

                            ----sTAUCODE_MINRANGE: TAUCODE where (TVADTFIN - TVADTVIGUEUR) <= nDURATION_NBDAYS
                            SELECT TAUCODE
                              INTO sTAUCODE_MINRANGE
                              FROM TAUVALEUR
                             WHERE     TRUNC (TVADTVIGUEUR) = dtTVADTVIGUEUR
                                   AND TAUCODE IN (SELECT TAUCODE
                                                     FROM TAUX
                                                    WHERE FTVID = nFTVID)
                                   AND (TRUNC (TVADTFIN) - dtTVADTVIGUEUR) <=
                                       nDURATION_NBDAYS
                                   AND (TRUNC (TVADTFIN) - dtTVADTVIGUEUR) =
                                       (SELECT MAX (
                                                   (  TRUNC (TVADTFIN)
                                                    - dtTVADTVIGUEUR))
                                          FROM TAUVALEUR
                                         WHERE     TRUNC (TVADTVIGUEUR) =
                                                   dtTVADTVIGUEUR
                                               AND TAUCODE IN
                                                       (SELECT TAUCODE
                                                          FROM TAUX
                                                         WHERE FTVID = nFTVID)
                                               AND (  TRUNC (TVADTFIN)
                                                    - dtTVADTVIGUEUR) <=
                                                   nDURATION_NBDAYS);



                            ----a. Get the ITR value from table TAUVALEUR
                            ----b. The rates (TAUCODE) to consider are those that belong to the formula with FTVCODE = 'ZCP'
                            ----c. The maturity date for each rate is stored in TAUVALEUR.TVADTFIN
                            ----d. The number of days of maturity for each rate is TVADTFIN - TVADTVIGUEUR
                            ----e. We are doing an interpolation to get the ITR rate using the records in TAUVALEUR where the number of days nDURATION_NBDAYS is located between nNBDAYSMIN and nNBDAYSMAX
                            ----f. The target rate (ITR) is calculated through an interpolation


                            --Rate value corresponding to sTAUCODE_MINRANGE
                            SELECT NVL (TVAVAL, 0),
                                   NVL ((TVADTFIN - TVADTVIGUEUR), 0)
                              INTO nRATEMIN, nNBDAYSMIN
                              FROM TAUVALEUR
                             WHERE     TAUCODE = sTAUCODE_MINRANGE
                                   AND TVADTVIGUEUR = dtTVADTVIGUEUR;


                            --Rate value corresponding to sTAUCODE_MAXRANGE
                            SELECT NVL (TVAVAL, 0),
                                   NVL ((TVADTFIN - TVADTVIGUEUR), 0)
                              INTO nRATEMAX, nNBDAYSMAX
                              FROM TAUVALEUR
                             WHERE     TAUCODE = sTAUCODE_MAXRANGE
                                   AND TVADTVIGUEUR = dtTVADTVIGUEUR;


                            SELECT (nNBDAYSMAX - nNBDAYSMIN)
                              INTO nDENOMINATOR
                              FROM DUAL;

                            IF nDENOMINATOR <> 0
                            THEN
                                SELECT TRUNC (
                                           NVL (
                                                 (  (  (  nDURATION_NBDAYS
                                                        - nNBDAYSMIN)
                                                     / nDENOMINATOR)
                                                  * (nRATEMAX - nRATEMIN))
                                               + nRATEMIN,
                                               0),
                                           13)
                                  INTO nITR
                                  FROM DUAL;
                            ELSE
                                SELECT TRUNC (
                                           NVL (
                                                 (  (  (  nDURATION_NBDAYS
                                                        - nNBDAYSMIN)
                                                     / 1)
                                                  * (nRATEMAX - nRATEMIN))
                                               + nRATEMIN,
                                               0),
                                           13)
                                  INTO nITR
                                  FROM DUAL;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            END;

            RETURN NVL (nITR, 0);
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_GET_ITR_DURATION;


    FUNCTION F_GET_ITRDEP (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            nITRID   IMMOTRANCHE.ITRID%TYPE;
            sEXPEN   DEPENSE.DEPNUM%TYPE;
        BEGIN
            ----nITRID:Get the ITRID from the P_ENTID
            SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
              INTO nITRID
              FROM DUAL;

            --Get the asset exepense
            SELECT MAX (depnum)
              INTO sEXPEN
              FROM depense
             WHERE depid IN (SELECT MAX (depid)
                               FROM itrrubrique
                              WHERE itrid = nITRID);

            RETURN sEXPEN;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_GET_ITRDEP;

    ----ABH on 20210305

    FUNCTION SGMCINACT (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SGMCINACT   VARCHAR2 (100);
            NActid      VARCHAR2 (100);
            nDosid      VARCHAR2 (100);
        BEGIN

        SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDosid
                  FROM DUAL ;

      select max (actid) into NActid from dpracteur where rolcode='CLIENT' and dosid = nDosid ;


            SELECT CVA.CVASTRINGVALUE
              INTO SGMCINACT
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.ACTID =    NActid                                              -- SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHSID ='TFDCCHSID64';

            RETURN SGMCINACT;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
 END ;


 --------ABH 31/03/2021  Chiffre d'affaire client (module acteur) depuis CCH CA module proposition SGM
 FUNCTION SGMACTCA (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            NCA         VARCHAR2 (100);
            NActid      VARCHAR2 (100);
            nDosid      VARCHAR2 (100);
        BEGIN

        SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO NActid
                  FROM DUAL ;

      select max (dosid) into nDosid from dpracteur where rolcode='CLIENT' and actid=NActid ;


     select CVANUMERICVALUE into NCA from cchvalue where DOSIDPROSPECT=nDosid and  cchsid='TFDCCHSID91';



            RETURN NCA;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
 END ;

 FUNCTION SGMIDFISC (SENTID      VARCHAR2,
                                 SLANCODE    VARCHAR2,
                                 SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            SGMCINACT   VARCHAR2 (100);
            NActid      VARCHAR2 (100);
            nDosid      VARCHAR2 (100);
        BEGIN

        SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                  INTO nDosid
                  FROM DUAL ;

      select max (actid) into NActid from dpracteur where rolcode='CLIENT' and dosid = nDosid ;


            SELECT CVA.CVASTRINGVALUE
              INTO SGMCINACT
              FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
             WHERE     CVA.CCHSID = CCH.CCHSID
                   AND CVA.ACTID =    NActid                                              -- SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
                   AND CCH.CCHSID ='TFDCCHSID6402';

            RETURN SGMCINACT;
        EXCEPTION
            WHEN OTHERS
            THEN
                RETURN NULL;
        END;
 END ;


/*FUNCTION LADSGM (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
RETURN VARCHAR2

 IS
    BEGIN
        DECLARE
            nLADSGM   VARCHAR2 (100);
            nDosid      VARCHAR2 (100);
            sRESULT     VARCHAR2 (100);
            NTDATE      DATE;


              BEGIN

              SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
              INTO nDOSID
              FROM DUAL;


              select DPRDTCREATION
                into NTDATE
                from dossierprospect
                where dosid=nDOSID
                 --   and dprversion in (select phacode from dprphase where dosid=nDOSID and DPHDTEND is null) ;
                and dprversion = (SELECT DPRVERSION
                                              FROM V_DEAL VDE
                                             WHERE VDE.DOSID = nDOSID);

              select F_GETMATRIXVALUE(1200,nDOSID,NTDATE) into nLADSGM from dual;

              if nLADSGM ='0' then
              sRESULT:='DEPUIS DCCIT';
              return sRESULT;
              else 
              return  nLADSGM;
              end if;
            end;

    end;
*/
FUNCTION LADSGM(SENTID IN VARCHAR2, SLANCODE IN VARCHAR2, SUTICODE IN VARCHAR2)
        RETURN VARCHAR2
    IS
        NSCORE   VARCHAR2(50);
        NHLAD    VARCHAR2(50);
		NNAP     VARCHAR2(50);
		NMTINV   NUMBER;
		NTACCODE VARCHAR2(50);
		NNBP     NUMBER;
		SRESULT  VARCHAR2(50);
		nDOSID   VARCHAR2(50);

    BEGIN
        BEGIN
         SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
            INTO nDOSID
          FROM DUAL;
         SELECT  pav4_criteria.F_GET_HLAD(nDosId,'ORFI')INTO NHLAD from dual;
         SELECT  pav4_criteria.F_GET_SCORE(nDosId,'ORFI') INTO NSCORE from dual;
         SELECT  pav4_criteria.F_GET_NAPCODE(nDosId,'ORFI')INTO NNAP from dual;
         SELECT  pav4_criteria.F_GET_MTINV(nDosId,'ORFI') INTO NMTINV from dual;
         SELECT  pav4_criteria.F_GET_TACCODE(nDosId,'ORFI') INTO NTACCODE from dual;
         SELECT  pav4_criteria.F_GET_NBPERIOD(nDosId,'ORFI')INTO NNBP from dual;

	IF (NHLAD IS NULL OR NHLAD='LADSGM2') THEN
         IF (NSCORE='SCV' OR NSCORE='SCORE VERT') THEN
             IF (NTACCODE='CBM' AND NNAP in ('1201','3101','3102','3103','3104','3105') AND NMTINV <=500000 AND (NNBP BETWEEN 36 AND 60)
			 OR NTACCODE='CBM' AND NNAP NOT in ('1201','3101','3102','3103','3104','3105') AND NMTINV <=250000 AND (NNBP BETWEEN 36 AND 60)) THEN
			     -- SRESULT:='0';--UNIPERS RMCP
			     SRESULT:='UNIPERS RMCP';--UNIPERS RMCP

			 ELSIF  (NTACCODE='CBM' AND NNAP in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 500000 AND 1000000)  AND (NNBP BETWEEN 36 AND 60)
			 OR NTACCODE='CBM' AND NNAP NOT in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 250000 AND 500000) AND (NNBP BETWEEN 36 AND 60)) THEN
			      --SRESULT:='1';-- UNIPERS DG DGA
			      SRESULT:='UNIPERS DG DGA';-- UNIPERS DG DGA

			 ELSIF (NTACCODE='CBM' AND (NMTINV BETWEEN 1000000 AND 2500000) AND NNBP<=60) THEN
			   --  SRESULT:='2';--DS RMCP RISQUE
			   SRESULT:='DS RMCP RISQUE';--DS RMCP RISQUE
			 ELSIF (NTACCODE='CBM' AND (NMTINV BETWEEN 2500001 AND 5000000) AND NNBP<=60) THEN
			     --SRESULT:='3';--DS DG RISQUE SGM
			     SRESULT:='DS DG RISQUE SGM';--DS DG RISQUE SGM
			 ELSIF 	(NTACCODE='CBI' AND NMTINV<=5000000 AND NNBP<=144) THEN
			     --SRESULT:='2';--DS RMCP RISQUE
			     SRESULT:='DS RMCP RISQUE';--DS RMCP RISQUE

			 ELSIF 	 (NTACCODE='CBI' AND (NMTINV BETWEEN 5000001 AND 10000000)  AND NNBP<=144) THEN
			    -- SRESULT:='3';--DS DG RISQUE SGM
			    SRESULT:='DS DG RISQUE SGM';--DS DG RISQUE SGM
			 ELSE
			     --SRESULT:='4';--DEPUIS DCIIT
			      SRESULT:='DEPUIS DCIIT';--DEPUIS DCIIT
			 END IF;

        ELSIF (NSCORE='SCO' OR NSCORE='SCORE ORANGE') THEN

		   IF ((NTACCODE='CBM' AND NMTINV<=2500000 AND NNBP<=60) OR (NTACCODE='CBI' AND NMTINV<=5000000 AND NNBP<=144)) THEN
		     -- SRESULT:='2';--DS RMCP RISQUE
		     SRESULT:='DS RMCP RISQUE';--DS RMCP RISQUE

		   ELSIF ((NTACCODE='CBM' AND (NMTINV BETWEEN 2500001 AND 5000000) AND NNBP<=60) OR (NTACCODE='CBI' AND (NMTINV BETWEEN 5000001 AND 10000000) AND NNBP<=144)) THEN

             -- SRESULT:='3';--DS DG RISQUE SGM
             SRESULT:='DS DG RISQUE SGM';--DS DG RISQUE SGM
		   ELSE
             -- SRESULT:='4';--DEPUIS DCIIT
               SRESULT:='DEPUIS DCIIT';--DEPUIS DCIIT
		   END IF;
		  ELSIF (NSCORE='SCR' OR NSCORE='SCORE ROUGE') THEN
		    -- SRESULT:='5';--CLASSEMENT SANS SUITE
		      SRESULT:='CLASSEMENT SANS SUITE';--CLASSEMENT SANS SUITE
        END IF;
ELSE
   -- SRESULT:='4';--DEPUIS DCIIT
    SRESULT:='DEPUIS DCIIT';--DEPUIS DCIIT
END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                SRESULT := NULL;
        END;

        RETURN SRESULT;
    END;
/*
IS
    NSCORE   VARCHAR2(50);
    NHLAD    VARCHAR2(50);
    NNAP     VARCHAR2(50);
    NMTINV   NUMBER;
    NTACCODE VARCHAR2(50);
    NNBP     NUMBER;
    SRESULT  VARCHAR2(50);
    nDOSID   VARCHAR2(50);

BEGIN

    SELECT SUBSTR (SENTID, 7, INSTR (SENTID, '||') - 7)
      INTO nDOSID
      FROM DUAL;
    SELECT  pav4_criteria.F_GET_HLAD(nDOSID,'ORFI')INTO NHLAD from dual;     ----Hors LAD SGLM ?
    SELECT  pav4_criteria.F_GET_SCORE(nDOSID,'ORFI') INTO NSCORE from dual;      ---Score calcule ou decision finale
    SELECT  pav4_criteria.F_GET_NAPCODE(nDOSID,'ORFI')INTO NNAP from dual;     ---Nature du produit
   SELECT  pav4_criteria.F_GET_MTINV(nDOSID,'ORFI') INTO NMTINV from dual; ---Montant de financeme
   --select DPMMTINVEST into NMTINV from DPRMATERIEL where dosid = ndosid ;
    SELECT  pav4_criteria.F_GET_TACCODE(nDOSID,'ORFI') INTO NTACCODE from dual;    --Type activite
    SELECT  pav4_criteria.F_GET_NBPERIOD(nDOSID,'ORFI')INTO NNBP from dual;      --Nbre de periode

    --Dbms_Output.put_line('NHLAD: '||NHLAD);
   -- Dbms_Output.put_line('NSCORE: '||NSCORE);
   -- Dbms_Output.put_line('NNAP: '||NNAP);
   -- Dbms_Output.put_line('NMTINV: '||NMTINV);
   -- Dbms_Output.put_line('NTACCODE: '||NTACCODE);
   -- Dbms_Output.put_line('NNBP: '||NNBP);


  IF (NHLAD IS NULL OR NHLAD = 'LADSGM1') THEN

         SRESULT:='DEPUIS DCCIT';
         --SRESULT:='test3';
           RETURN SRESULT;

     END IF;
IF (NHLAD IS NULL OR NHLAD = 'LADSGM2') THEN
    IF (NSCORE='SCV' OR NSCORE='SCORE VERT') THEN
        IF (NTACCODE='CBM'
            AND NNAP in ('1201','3101','3102','3103','3104','3105')
            AND NMTINV <=500000
            AND (NNBP BETWEEN 36 AND 60))
            OR
            (NTACCODE='CBM'
            AND NNAP NOT in ('1201','3101','3102','3103','3104','3105')
            AND NMTINV <=250000 AND (NNBP BETWEEN 36 AND 60)) THEN

            SRESULT:='UNIPERS RMCP';
            RETURN SRESULT;

        END IF;
    END IF;
END IF;

IF (NHLAD IS NULL OR NHLAD = 'LADSGM2') THEN
    IF (NSCORE='SCV' OR NSCORE='SCORE VERT') THEN --Orange LGA
    IF (NTACCODE='CBM'
    AND NNAP in ('1201','3101','3102','3103','3104','3105')
    AND (NMTINV BETWEEN 500000 AND 1000000)
    AND (NNBP BETWEEN 36 AND 60))
    OR
    (NTACCODE='CBM'
    AND NNAP NOT IN ('1201','3101','3102','3103','3104','3105')
    AND (NMTINV BETWEEN 250000 AND 500000)
    AND (NNBP BETWEEN 36 AND 60)) THEN

    SRESULT:='UNIPERS DG DGA';
    RETURN SRESULT;       ------Score vert + HLAD

       END IF;
    END IF;
END IF;

IF (NHLAD IS NULL OR NHLAD = 'LADSGM2') THEN
    IF (NSCORE='SCO' OR NSCORE='SCORE ORANGE') THEN
      IF   
      (NTACCODE='CBM' AND (NMTINV BETWEEN 1000000 AND 2500000) AND NNBP<=60)
        OR
        (NTACCODE='CBI'  AND NMTINV<=5000000 AND NNBP<=144)
        OR
        ((NTACCODE='CBM' AND NMTINV<=2500000 AND NNBP<=60) OR (NTACCODE='CBI' AND NMTINV<=5000000 AND NNBP<=144)) THEN

    SRESULT:='DS RMCP RISQUE';   -----Score orange + HLAD
    RETURN SRESULT;

         END IF;
    END IF;
END IF;

IF (NHLAD IS NULL OR NHLAD = 'LADSGM2') THEN
    IF (NSCORE='SCO' OR NSCORE='SCORE ORANGE') THEN
        IF  
        (NTACCODE='CBM'
            AND (NMTINV BETWEEN 2500001 AND 5000000)
            AND  NNAP='1406'
            AND NNBP<=60)
            OR
            (NTACCODE='CBI'
            AND (NMTINV BETWEEN 5000001 AND 10000000)
            AND NNBP<=144)
            OR 
            ((NTACCODE='CBM'
                AND (NMTINV BETWEEN 2500001 AND 5000000)
                AND NNBP<=60)
                OR (NTACCODE='CBI'
                AND (NMTINV BETWEEN 5000001 AND 10000000)
                AND NNBP<=144)) THEN

        SRESULT:='DS DG RISQUE SGM';  -----Score orange + HLAD    DS DG RISQUE SGL
         RETURN SRESULT;



        ELSE 
        SRESULT:='DEPUIS DCCIT';
        --SRESULT:=SENTID;
        RETURN SRESULT;
        END IF;

    ELSIF (NSCORE='SCR' OR NSCORE='SCORE ROUGE') THEN
        SRESULT:='CLASSEMENT SANS SUITE';
        RETURN SRESULT;
    ELSE
        SRESULT:='DEPUIS DCCIT';
        --SRESULT:='test2';
        RETURN SRESULT;
    END IF;
END IF;

END;*/
---------

FUNCTION F_GET_USER_CASGMA (SENTID      VARCHAR2,
                           SLANCODE    VARCHAR2,
                           SUTICODE    VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        DECLARE
            nUTINOM VARCHAR2(50);

        BEGIN
           select UTINOM INTO nUTINOM from utilisateur where grocode in ('CC_COM','CC_PRO');

            RETURN nUTINOM;
        END;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END F_GET_USER_CASGMA;

END PAV4_CCFUNCTIONS;