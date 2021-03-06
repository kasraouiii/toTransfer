create or replace PACKAGE BODY PAV4_STEPS
AS
    PROCEDURE P_STIP_01 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                         DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                         DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                         P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                         P_ACTID       ACTEUR.ACTID%TYPE,
                         P_PROID       PROCESS.PROID%TYPE,
                         P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                         P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = '1' AND lancode = 'EN';


        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_01;

    PROCEDURE P_STIP_065 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT    NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP065';

        SELECT COUNT (1)
          INTO NCOUNT
          FROM pfiattribut
         WHERE     pfacode = 'FICOSCORE'
               AND PFIID = P_PFIID
               AND PFAENTIER >= 640
               AND PFAENTIER <= 679;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_065;


    PROCEDURE P_STIP_064 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT    NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP064';

        SELECT COUNT (1)
          INTO NCOUNT
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER < 640;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_064;


    PROCEDURE P_STIP_052 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE    VARCHAR2 (100) := '1';
        TPCCODE2   VARCHAR2 (100) := '1';
        TPCCODE3   VARCHAR2 (100) := '1';
        LIBCODE    VARCHAR2 (1000);
        NCOUNTT2   NUMBER := 1;
        LIBCODE2   VARCHAR2 (1000);
        NCOUNTT3   NUMBER := 1;
        LIBCODE3   VARCHAR2 (1000);
        NCOUNTT    NUMBER := 1;
        NCOUNT     NUMBER := 0;
        NCOUNT2    NUMBER := 0;
        NCOUNT3    NUMBER := 0;
        NDOSID     NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP052';
        TPCCODE2 := 'STIP039';
        TPCCODE3 := 'STIP040';


        SELECT COUNT (1)
          INTO NCOUNT
          FROM PFIATTRIBUT
         WHERE     PFIID = (SELECT PFIID
                              FROM DPRPROPFINANCE
                             WHERE DOSID = nDOSID AND DPRVERSION = 'NEGO')
               AND PFACODE IN ('PRGMPLAN10', 'PRGMPLAN9')
               AND PFACHAINE = '006';

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT tcplibelle
          INTO LIBCODE2
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE2 AND lancode = 'EN';

        SELECT tcplibelle
          INTO LIBCODE3
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE3 AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT2
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE2;

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT3
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE3;

        IF NCOUNTT <> 1 AND NCOUNT > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;

        IF NCOUNTT2 <> 1 AND NCOUNT2 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE2 || '',
                                                       '' || LIBCODE2 || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (
                    DOSID,
                    DPRVERSION,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0,
                    0,
                    1,
                    'AUTRES',
                    '' || LIBCODE2 || '',
                    NULL,
                    NULL);
                COMMIT;
            END;
        END IF;

        IF NCOUNTT3 <> 1 AND NCOUNT3 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE3 || '',
                                                       '' || LIBCODE3 || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (
                    DOSID,
                    DPRVERSION,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0,
                    0,
                    1,
                    'AUTRES',
                    '' || LIBCODE3 || '',
                    NULL,
                    NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_052;

    PROCEDURE P_STIP_017 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP017';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER < 640;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_017;

    PROCEDURE P_STIP_018 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP018';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER < 640;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_018;

    PROCEDURE P_STIP_019 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP019';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER < 640;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_019;

    PROCEDURE P_STIP_020 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP020';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER < 640;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_020;

    PROCEDURE P_STIP_021 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP021';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE     pfacode = 'FICOSCORE'
               AND PFIID = P_PFIID
               AND PFAENTIER >= 640
               AND PFAENTIER <= 679;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_021;

    PROCEDURE P_STIP_022 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP022';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE pfacode = 'FICOSCORE' AND PFIID = P_PFIID AND PFAENTIER <= 679;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_022;

    PROCEDURE P_STIP_024 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP024';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM pfiattribut
         WHERE     pfacode = 'FICOSCORE'
               AND PFIID = P_PFIID
               AND PFAENTIER <= 719
               AND PFAENTIER >= 680;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_024;

    PROCEDURE P_STIP_050 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        NCOUNT1   NUMBER := 0;
        NDOSID    NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP050';

        SELECT COUNT (1)
          INTO NCOUNT1
          FROM dossierprospect
         WHERE tpgcode = 'OPELEAS' AND DOSID = NDOSID;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = TPCCODE AND lancode = 'EN';

        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        IF NCOUNTT <> 1 AND NCOUNT1 > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_050;

    /*  PROCEDURE P_STIP_008 (
                    P_LANCODE                       LANPROCESS.LANCODE%TYPE ,
                    DOSID                                   DOSSIERPROSPECT.DOSID%TYPE,
                    DPRVERSION                     DOSSIERPROSPECT.DPRVERSION%TYPE,
                    P_PFIID                                                DPRPROPFINANCE.PFIID%TYPE,
                    P_ACTID                                              ACTEUR.ACTID%TYPE,
                    P_PROID                                              PROCESS.PROID%TYPE,
                    P_PSTORDRE                     PROSTEP.PSTORDRE%TYPE,
                    P_UTICODE                        UTILISATEUR.UTICODE%TYPE
                    )
                    AS
    TPCCODE             VARCHAR2(100):='1';
    LIBCODE               VARCHAR2(1000);
    NCOUNTT   NUMBER:=1;
    NDOSID number:=0;
     l_Result varchar2(100);

    BEGIN

    ndosid := DOSID;



       cursor c_decision (p_string varchar2) is select distinct adecode from dprdecision where dosid=nDosid;
       l_decision dprdecision.adecode%type;
       begin
        open c_decision;
        loop
          fetch c_decision into l_decision;
          exit when c_decision%notfound;
          l_result:=l_result || ';' || l_decision;

        end loop;
        close c_decision;
        RETURN l_result;

        select tcplibelle
    INTO LIBCODE
    from lanavtcondpreal
    where tcpcode = 'STIP008'
    and lancode = 'EN';


    select TO_NUMBER(COUNT(DPPLIBELLE))
    into NCOUNTT
    from dprprealable
    where dprversion = DPRVERSION
    and dosid = ndosid
    and dpplibelle = LIBCODE;

     IF (L_RESULT LIKE '%APPAUTOOK%' OR IF L_RESULT LIKE '%APPAUTOKO%') and ncount <> null
       BEGIN
        pa_updatedossierprospect.I_DPRAUTRECP(DOSID,DPRVERSION,null,''||TPCCODE||'',''||LIBCODE||'',0,0,1,null);
        pa_updatedossierprospect.I_DPRPREALABLE(DOSID,DPRVERSION,NULL,NULL,NULL,NULL,NULL,NULL,NULL,1,NULL,NULL,0,0,1,'AUTRES',''||LIBCODE||'',NULL,NULL);
        commit;
       end;

     end if;

       end;
    END P_STIP_008;

    */
    PROCEDURE P_STIP_009 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        ncount    NUMBER := 0;
        NDOSID    NUMBER := 0;
        SRESULT   NUMBER := 0;
    BEGIN
        BEGIN
            ndosid := DOSID;
            TPCCODE := 'STIP009';

            SELECT TO_NUMBER (PFACHAINE)
              INTO SRESULT
              FROM PFIATTRIBUT
             WHERE     PFIID =
                       (SELECT PFIID
                          FROM DPRPROPFINANCE
                         WHERE     DOSID = nDOSID
                               AND DPRVERSION =
                                   pa_avcommun.f_derniereversiondossier (
                                       NDosId))
                   AND PFACODE IN ('MKTPLAN9', 'MKTPLAN10');
        EXCEPTION
            WHEN OTHERS
            THEN
                SRESULT := NULL;
        END;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = 'STIP009' AND lancode = 'EN';


        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;


        IF SRESULT IN (801, 802, 852) AND ncount <> 1
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_009;

    PROCEDURE P_STIP_013 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE    VARCHAR2 (100) := '1';
        LIBCODE    VARCHAR2 (1000);
        NCOUNTT    NUMBER := 1;
        ncount     NUMBER := 0;
        NDOSID     NUMBER := 0;
        prolcode   NUMBER := 0;
    BEGIN
        TPCCODE := 'STIP013';
        ndosid := DOSID;

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = 'STIP013' AND lancode = 'EN';


        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        BEGIN
            BEGIN
                SELECT COUNT (1)
                  INTO PROLCODE
                  FROM DPRACTEUR
                 WHERE     DOSID = nDOSID
                       AND DPRVERSION =
                           PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (nDOSID)
                       AND rolcode = 'COSSIG';
            EXCEPTION
                WHEN OTHERS
                THEN
                    PROLCODE := NULL;
            END;

            IF prolcode >= 1 AND ncount <> 1
            THEN
                BEGIN
                    pa_updatedossierprospect.I_DPRAUTRECP (
                        DOSID,
                        DPRVERSION,
                        NULL,
                        '' || TPCCODE || '',
                        '' || LIBCODE || '',
                        0,
                        0,
                        1,
                        NULL);
                    pa_updatedossierprospect.I_DPRPREALABLE (
                        DOSID,
                        DPRVERSION,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0,
                        0,
                        1,
                        'AUTRES',
                        '' || LIBCODE || '',
                        NULL,
                        NULL);
                    COMMIT;
                END;
            END IF;
        END;
    END P_STIP_013;

    PROCEDURE P_STIP_038 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE   VARCHAR2 (100) := '1';
        LIBCODE   VARCHAR2 (1000);
        NCOUNTT   NUMBER := 1;
        ncount    NUMBER := 0;
        NDOSID    NUMBER := 0;
        SRESULT   NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP038';

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = 'STIP038' AND lancode = 'EN';


        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        BEGIN
            BEGIN
                SELECT TO_NUMBER (PFACHAINE)
                  INTO SRESULT
                  FROM PFIATTRIBUT
                 WHERE     PFIID =
                           (SELECT PFIID
                              FROM DPRPROPFINANCE
                             WHERE     DOSID = nDOSID
                                   AND DPRVERSION =
                                       pa_avcommun.f_derniereversiondossier (
                                           NDosId))
                       AND PFACODE IN ('PRGMPLAN9', 'PRGMPLAN10');
            EXCEPTION
                WHEN OTHERS
                THEN
                    SRESULT := NULL;
            END;

            IF SRESULT = 005 AND ncount <> 1
            THEN
                BEGIN
                    pa_updatedossierprospect.I_DPRAUTRECP (
                        DOSID,
                        DPRVERSION,
                        NULL,
                        '' || TPCCODE || '',
                        '' || LIBCODE || '',
                        0,
                        0,
                        1,
                        NULL);
                    pa_updatedossierprospect.I_DPRPREALABLE (
                        DOSID,
                        DPRVERSION,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0,
                        0,
                        1,
                        'AUTRES',
                        '' || LIBCODE || '',
                        NULL,
                        NULL);
                    COMMIT;
                END;
            END IF;
        END;
    END P_STIP_038;

    PROCEDURE P_STIP_043 (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                          DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                          DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                          P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                          P_ACTID       ACTEUR.ACTID%TYPE,
                          P_PROID       PROCESS.PROID%TYPE,
                          P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                          P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        TPCCODE       VARCHAR2 (100) := '1';
        LIBCODE       VARCHAR2 (1000);
        NCOUNTT       NUMBER := 1;
        ncount        NUMBER := 0;
        NDOSID        NUMBER := 0;
        n_refurbish   NUMBER := 0;
    BEGIN
        ndosid := DOSID;
        TPCCODE := 'STIP043';

        SELECT tcplibelle
          INTO LIBCODE
          FROM lanavtcondpreal
         WHERE tcpcode = 'STIP043' AND lancode = 'EN';


        SELECT TO_NUMBER (COUNT (DPPLIBELLE))
          INTO NCOUNTT
          FROM dprprealable
         WHERE     dprversion = DPRVERSION
               AND dosid = ndosid
               AND dpplibelle = LIBCODE;

        SELECT COUNT (1)
          INTO n_refurbish
          FROM dprmateriel
         WHERE     DPMREFURBISHCODE = 'PROGVEHICLE'
               AND dprversion = dprversion
               AND dosid = ndosid;

        IF ncount <> 1 AND n_refurbish > 0
        THEN
            BEGIN
                pa_updatedossierprospect.I_DPRAUTRECP (DOSID,
                                                       DPRVERSION,
                                                       NULL,
                                                       '' || TPCCODE || '',
                                                       '' || LIBCODE || '',
                                                       0,
                                                       0,
                                                       1,
                                                       NULL);
                pa_updatedossierprospect.I_DPRPREALABLE (DOSID,
                                                         DPRVERSION,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         NULL,
                                                         0,
                                                         0,
                                                         1,
                                                         'AUTRES',
                                                         '' || LIBCODE || '',
                                                         NULL,
                                                         NULL);
                COMMIT;
            END;
        END IF;
    END P_STIP_043;

    PROCEDURE P_ANALYSTCONCL (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                              DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        ncount   NUMBER := 0;
    BEGIN
        SELECT MAX (dinordre) + 1
          INTO ncount
          FROM dprintervenant
         WHERE dosid = DOSID;

        INSERT INTO dprintervenant
             VALUES (DOSID,
                     DPRVERSION,
                     ncount,
                     P_UTICODE,
                     'DECAN',
                     SYSDATE,
                     0,
                     SYSDATE,
                     NULL,
                     NULL,
                     NULL);

        COMMIT;
    END P_ANALYSTCONCL;

    PROCEDURE P_STUDLOAN_DEF (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                              DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        ncount   NUMBER := 0;
    BEGIN
        SELECT 1 INTO ncount FROM DUAL;
    END P_STUDLOAN_DEF;


    PROCEDURE P_UPDATEQUOTE (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                             DOSID         DOSSIERPROSPECT.DOSID%TYPE,
                             DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                             P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                             P_ACTID       ACTEUR.ACTID%TYPE,
                             P_PROID       PROCESS.PROID%TYPE,
                             P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                             P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        nvalue1        NUMBER := 0;
        nvalue2        NUMBER := 0;
        nvalue3        NUMBER := 0;
        ncount         NUMBER := 0;
        ndosid         NUMBER := 0;
        nmontant       NUMBER := 0;
        P_DPRVERSION   VARCHAR (10) := '';
    BEGIN
        ndosid := DOSID;
        P_DPRVERSION := DPRVERSION;

        SELECT MAX (ctcvalue)
          INTO nvalue1
          FROM cintcr
         WHERE     cinid =
                   (SELECT MAX (cinid)
                      FROM creditinfo
                     WHERE     cintype = 'BLAZE_FAMILY'
                           AND actid =
                               (SELECT MAX (actid)
                                  FROM dpracteur
                                 WHERE     dosid = ndosid
                                       AND rolcode IN ('EMPRUNT', 'CLIENT')))
               AND tcrid IN
                       (SELECT tcrid
                          FROM tcreditinfo
                         WHERE     tcrfamily = 'BLAZE_FAMILY'
                               AND tcrstatus = 'OK'
                               AND (   (    tcrcode = 'MoneyFactor'
                                        AND tcridparent =
                                            (SELECT MAX (tcrid)
                                               FROM tcreditinfo
                                              WHERE     tcrfamily =
                                                        'BLAZE_FAMILY'
                                                    AND tcrcode =
                                                        'PricingFactors'))
                                    OR tcrcode = 'DecisionRate'));

        SELECT MAX (ctcvalue)
          INTO nvalue2
          FROM cintcr
         WHERE     cinid =
                   (SELECT MAX (cinid)
                      FROM creditinfo
                     WHERE     cintype = 'BLAZE_FAMILY'
                           AND actid =
                               (SELECT MAX (actid)
                                  FROM dpracteur
                                 WHERE     dosid = ndosid
                                       AND rolcode IN ('EMPRUNT', 'CLIENT')))
               AND tcrid =
                   (SELECT MAX (tcrid)
                      FROM tcreditinfo
                     WHERE     tcrfamily = 'BLAZE_FAMILY'
                           AND tcrcode = 'ResidualPercent'
                           AND tcrstatus = 'OK'
                           AND tcridparent =
                               (SELECT MAX (tcrid)
                                  FROM tcreditinfo
                                 WHERE     tcrfamily = 'BLAZE_FAMILY'
                                       AND tcrcode = 'ResidualPricing'));

        SELECT MAX (ctcvalue)
          INTO nvalue3
          FROM cintcr
         WHERE     cinid =
                   (SELECT MAX (cinid)
                      FROM creditinfo
                     WHERE     cintype = 'BLAZE_FAMILY'
                           AND actid =
                               (SELECT MAX (actid)
                                  FROM dpracteur
                                 WHERE     dosid = ndosid
                                       AND rolcode IN ('EMPRUNT', 'CLIENT')))
               AND tcrid =
                   (SELECT MAX (tcrid)
                      FROM tcreditinfo
                     WHERE     tcrfamily = 'BLAZE_FAMILY'
                           AND tcrcode = 'Terms'
                           AND tcrstatus = 'OK'
                           AND tcridparent =
                               (SELECT MAX (tcrid)
                                  FROM tcreditinfo
                                 WHERE     tcrfamily = 'BLAZE_FAMILY'
                                       AND tcrcode = 'PricingHighValue'));


        SELECT pfiinvestissement
          INTO NMONTANT
          FROM PROPOSITIONFINANCIERE
         WHERE PFIID = P_PFIID;


        IF nvalue1 IS NOT NULL
        THEN
            UPDATE pfirubrique
               SET pfrtotalnominal = nvalue1
             WHERE pfiid = P_PFIID;

            COMMIT;
        END IF;

        IF nvalue2 IS NOT NULL
        THEN
            UPDATE pfirubrique
               SET pfrvr = (nmontant * nvalue2) / 100
             WHERE pfiid = P_PFIID;

            COMMIT;
        END IF;

        IF nvalue3 IS NOT NULL
        THEN
            UPDATE propositionfinanciere
               SET pfinbperiodes = nvalue3
             WHERE pfiid = P_PFIID;

            COMMIT;
        END IF;

        SELECT COUNT (1)
          INTO ncount
          FROM pfiattribut
         WHERE pfacode = 'RATE_16' AND pfiid = P_PFIID;


        IF ncount = 0
        THEN
            INSERT INTO pfiattribut (pfiid, pfacode, pfachaine)
                 VALUES (P_PFIID, 'RATE_16', TO_CHAR (SYSDATE));

            INSERT INTO LKANARAT (ANAID,
                                  RATID,
                                  RATVALUE,
                                  POPID)
                     VALUES (
                                (SELECT MAX (ANAID)
                                   FROM ANALYSIS
                                  WHERE     PFIID = P_PFIID
                                        AND DPRVERSION = P_DPRVERSION),
                                (SELECT MAX (RATID)
                                   FROM RATIO
                                  WHERE RATCODE = 'DTUPDATE_16'),
                                TO_CHAR (SYSDATE),
                                0);
        ELSE
            UPDATE pfiattribut
               SET pfachaine = TO_CHAR (SYSDATE)
             WHERE pfacode = 'RATE_16' AND pfiid = P_PFIID;

            UPDATE LKANARAT
               SET RATVALUE = TO_CHAR (SYSDATE)
             WHERE     ANAID =
                       (SELECT MAX (ANAID)
                          FROM ANALYSIS
                         WHERE PFIID = P_PFIID AND DPRVERSION = P_DPRVERSION)
                   AND RATID = (SELECT MAX (RATID)
                                  FROM RATIO
                                 WHERE RATCODE = 'DTUPDATE_16');
        END IF;
    END P_UPDATEQUOTE;

    PROCEDURE P_UPDATE_RISKRATING (
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        DOSID         DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        nTCOID          NUMBER;
        nMAX_ANAID      NUMBER;
        n_Count_ANAID   NUMBER;
        nATVORDRE_OLD   NUMBER;
        nATVORDRE_NEW   NUMBER;
        nANMID          NUMBER;
        sTVALIBELLE     VARCHAR2 (500);
        sTVACODE        VARCHAR2 (100);
        nCREID          NUMBER;
    BEGIN
        SELECT MAX (ANMID)
          INTO nANMID
          FROM ANALYSISMATRIX
         WHERE ANMCODE = 'RISKRATING';

        SELECT COUNT (1)
          INTO n_Count_ANAID
          FROM ANALYSIS
         WHERE ANMID = nANMID AND ACTID = P_ACTID;

        IF n_Count_ANAID > 0
        THEN
            SELECT MAX (TCOID)
              INTO nTCOID
              FROM TCOTATION
             WHERE TCOCODE = 'RISKACT';

            SELECT MAX (anaid)
              INTO nMAX_ANAID
              FROM analysis
             WHERE actid = P_ACTID AND ANMID = nANMID;

            INSERT INTO CREVT (CREID,
                               TACCODE,
                               UTICODECREAT,
                               CREDTEFFET,
                               CREDTCREAT,
                               TMOMODULE,
                               TMFFONCTION,
                               TEVDEST,
                               UTICODEVALID,
                               UTICODESUP,
                               DOSID,
                               ITRID,
                               ACTID,
                               DEPID,
                               CREDTVALID,
                               CREDTSUP,
                               CREMT,
                               CRECODEORIGINE,
                               CRECODERESULTANT,
                               DEVCODE,
                               CHAID,
                               COMID,
                               REGID,
                               FACID,
                               BUDID,
                               SINID,
                               CRECODEINTERNE,
                               ASSID,
                               BIMID,
                               CEXID,
                               MANID,
                               CONID,
                               TBAID,
                               IMAID,
                               CREMTVBF,
                               CREMTVBC,
                               CREACTNOM,
                               CREACTNOM2,
                               AGGID,
                               COLID,
                               CRECODEREASON,
                               CREDESCROLLBACK,
                               UTICODERESTOREREQUEST,
                               CREDTRESTOREREQUEST,
                               DOSIDPROSPECT,
                               PCOID,
                               IMAIDAFTER,
                               CCAID,
                               TPGCODE,
                               TCTID,
                               CARID,
                               DPTCODECREAT,
                               DPTCODEVALID,
                               DPTCODESUP)
                 VALUES (SEQ_CREID.NEXTVAL,
                         'GLOBAL',
                         'ORFI',
                         SYSDATE,
                         SYSDATE,
                         'ACTEUR',
                         'EVACT_RATREMP',
                         'ACTEUR',
                         'ORFI',
                         NULL,
                         NULL,
                         NULL,
                         P_ACTID,
                         NULL,
                         SYSDATE,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL);

            INSERT INTO CREDATA (CREID,
                                 CDAORDRE,
                                 TSGCODE,
                                 CDATABLE,
                                 CDACOLONNE,
                                 CDADATADATE,
                                 CDADATASTRING,
                                 CDADATANUMBER,
                                 CDADATABOOLEAN,
                                 COVID,
                                 CDACOVENANTRESULTINGSTATUS,
                                 CDADRUORDRE,
                                 CDAMEMO,
                                 CDAIRUORDRE,
                                 CDAOLDVALUE,
                                 CDANEWVALUE,
                                 CDADCOORDRE,
                                 RULID,
                                 CRIID)
                 VALUES (SEQ_CREID.CURRVAL,
                         '1',
                         NULL,
                         'RATING',
                         'REASON',
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL,
                         NULL);

            SELECT SEQ_CREID.CURRVAL INTO nCREID FROM DUAL;


            SELECT NVL (MAX (ATVORDRE), 0)
              INTO nATVORDRE_OLD
              FROM ACTTCOVALEUR
             WHERE ACTID = P_ACTID AND TCOID = nTCOID AND ATVDTFIN IS NULL;

            SELECT nATVORDRE_OLD + 1
              INTO nATVORDRE_NEW
              FROM DUAL;

            SELECT ratvalue
              INTO sTVALIBELLE
              FROM lkanarat
             WHERE     anaid = nMAX_ANAID
                   AND ratid = (SELECT ratid
                                  FROM ratio
                                 WHERE ratcode = 'CUR_RAT_ACT');

            SELECT TVACODE
              INTO sTVACODE
              FROM LANTCOVALEUR
             WHERE     TCOID = nTCOID
                   AND TVALIBELLE = sTVALIBELLE
                   AND LANCODE = 'EN';

            IF nATVORDRE_OLD > 0
            THEN
                UPDATE ACTTCOVALEUR
                   SET ATVDTFIN = SYSDATE, ATVDTMAJ = SYSDATE, CREID = nCREID
                 WHERE     ACTID = P_ACTID
                       AND TCOID = nTCOID
                       AND ATVORDRE = nATVORDRE_OLD;

                INSERT INTO ACTTCOVALEUR (ACTID,
                                          ATVORDRE,
                                          TCOID,
                                          TVACODE,
                                          ATVDTDEB,
                                          CREID,
                                          UTICODE,
                                          ATVDTMAJ,
                                          ATVDTFIN,
                                          ATVCODEEXTERNE,
                                          ATVETABLISSEMENTPILOTE,
                                          ATVETABLISSEMENTCOTEUR,
                                          ATVFLAGFORCE,
                                          ATVCOMMENT,
                                          ATVSCORE,
                                          ATVDTVALIDITY,
                                          ATVPERIOD)
                     VALUES (P_ACTID,
                             nATVORDRE_NEW,
                             nTCOID,
                             sTVACODE,
                             SYSDATE,
                             nCREID,
                             'ORFI',
                             SYSDATE,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             '0',
                             NULL,
                             NULL,
                             NULL,
                             NULL);
            ELSE
                INSERT INTO ACTTCOVALEUR (ACTID,
                                          ATVORDRE,
                                          TCOID,
                                          TVACODE,
                                          ATVDTDEB,
                                          CREID,
                                          UTICODE,
                                          ATVDTMAJ,
                                          ATVDTFIN,
                                          ATVCODEEXTERNE,
                                          ATVETABLISSEMENTPILOTE,
                                          ATVETABLISSEMENTCOTEUR,
                                          ATVFLAGFORCE,
                                          ATVCOMMENT,
                                          ATVSCORE,
                                          ATVDTVALIDITY,
                                          ATVPERIOD)
                     VALUES (P_ACTID,
                             nATVORDRE_NEW,
                             nTCOID,
                             sTVACODE,
                             SYSDATE,
                             nCREID,
                             'ORFI',
                             SYSDATE,
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             '0',
                             NULL,
                             NULL,
                             NULL,
                             NULL);
            END IF;
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            ROLLBACK;

            COMMIT;
    END P_UPDATE_RISKRATING;



    PROCEDURE P_UPDATE_RISKRATING2 (
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        DOSID         DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        n_Count_ANAID              NUMBER;
        n_MAX_ANAID                NUMBER;
        n_ANMID                    NUMBER;
        n_DOSID                    NUMBER := 0;
        n_AMOUNT                   NUMBER;
        s_CREDITLINE_LABEL         VARCHAR (500);
        s_CREDITLINE               VARCHAR (100);
        s_ACTION_LABEL             VARCHAR (500);
        s_ACTION                   VARCHAR (100);
        n_RATID_CUR_RAT            NUMBER;
        n_RATID_CUR_ACT_ACT        NUMBER;
        n_RATID_AMO                NUMBER;
        s_CCHSID_CREDITLINE        CCHVALUE.CCHSID%TYPE;
        s_CCHSID_RISKRATACTION     CCHVALUE.CCHSID%TYPE;
        s_CCHSID_FORMULAICAMOUNT   CCHVALUE.CCHSID%TYPE;
    BEGIN
        n_DOSID := DOSID;

        SELECT MAX (ANMID)
          INTO n_ANMID
          FROM ANALYSISMATRIX
         WHERE ANMCODE = 'RISKRATING2';

        SELECT COUNT (1)
          INTO n_Count_ANAID
          FROM ANALYSIS
         WHERE     ANMID = n_ANMID
               AND DOSID = n_DOSID
               AND DPRVERSION =
                   pa_avcommun.f_derniereversiondossier (n_DOSID);

        IF n_Count_ANAID > 0
        THEN
            SELECT MAX (ANAID)
              INTO n_MAX_ANAID
              FROM ANALYSIS
             WHERE     ANMID = n_ANMID
                   AND DOSID = n_DOSID
                   AND DPRVERSION =
                       pa_avcommun.f_derniereversiondossier (n_DOSID);


            SELECT RATID
              INTO n_RATID_CUR_RAT
              FROM RATIO
             WHERE ratcode = 'CUR_RAT';

            SELECT RATVALUE
              INTO s_CREDITLINE_LABEL
              FROM lkanarat
             WHERE anaid = n_MAX_ANAID AND ratid = n_RATID_CUR_RAT;

            SELECT TUPCODE
              INTO s_CREDITLINE
              FROM LANTUSPARAM
             WHERE     TUSNOM = 'CREDITLINE'
                   AND LANCODE = 'EN'
                   AND TUPLIBELLE = s_CREDITLINE_LABEL;


            SELECT RATID
              INTO n_RATID_CUR_ACT_ACT
              FROM RATIO
             WHERE ratcode = 'CUR_ACT_ACT';

            SELECT RATVALUE
              INTO s_ACTION_LABEL
              FROM lkanarat
             WHERE anaid = n_MAX_ANAID AND ratid = n_RATID_CUR_ACT_ACT;

            SELECT TUPCODE
              INTO s_ACTION
              FROM LANTUSPARAM
             WHERE     TUSNOM = 'RISKRATACTION'
                   AND LANCODE = 'EN'
                   AND TUPLIBELLE = s_ACTION_LABEL;


            SELECT RATID
              INTO n_RATID_AMO
              FROM RATIO
             WHERE ratcode = '_AMO';

            SELECT RATVALUE
              INTO n_AMOUNT
              FROM lkanarat
             WHERE anaid = n_MAX_ANAID AND ratid = n_RATID_AMO;


            SELECT MAX (CCHSID)
              INTO s_CCHSID_CREDITLINE
              FROM CUSTOMCHARACTERISTIC
             WHERE CCHVALUECODE = 'CREDITLINE';

            SELECT MAX (CCHSID)
              INTO s_CCHSID_RISKRATACTION
              FROM CUSTOMCHARACTERISTIC
             WHERE CCHVALUECODE = 'RISKRATACTION';

            SELECT MAX (CCHSID)
              INTO s_CCHSID_FORMULAICAMOUNT
              FROM CUSTOMCHARACTERISTIC
             WHERE CCHVALUECODE = 'FORMULAICAMOUNT';


            DELETE FROM CCHVALUE
                  WHERE     CCHSID = s_CCHSID_CREDITLINE
                        AND DOSIDPROSPECT = n_DOSID
                        AND DPRVERSION =
                            pa_avcommun.f_derniereversiondossier (n_DOSID);

            INSERT INTO CCHVALUE (CVAID,
                                  CCHSID,
                                  ENTCODE,
                                  CVASTRINGVALUE,
                                  CVADTVALUE,
                                  CVANUMERICVALUE,
                                  CVABOOLEANVALUE,
                                  CVADT,
                                  FACID,
                                  DEPID,
                                  REGID,
                                  ANAID,
                                  AGGID,
                                  DOSIDPROSPECT,
                                  DPRVERSION,
                                  ACTID,
                                  ITRID,
                                  BIMID,
                                  DOSID,
                                  CREID,
                                  CVAPKEYVALUE,
                                  MANID,
                                  TPGCODE,
                                  VARID,
                                  ASSID,
                                  COLID,
                                  SINID,
                                  BUDID,
                                  CHAID,
                                  CEXID,
                                  COMID,
                                  ADMID)
                     VALUES (
                                SEQ_CVAID.NEXTVAL,
                                s_CCHSID_CREDITLINE,
                                'AVDOSS',
                                s_CREDITLINE,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                n_DOSID,
                                pa_avcommun.f_derniereversiondossier (
                                    n_DOSID),
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                   'Dosid-'
                                || n_DOSID
                                || '||Dprversion-'
                                || pa_avcommun.f_derniereversiondossier (
                                       n_DOSID)
                                || '||',
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL);


            DELETE FROM CCHVALUE
                  WHERE     CCHSID = s_CCHSID_RISKRATACTION
                        AND DOSIDPROSPECT = n_DOSID
                        AND DPRVERSION =
                            pa_avcommun.f_derniereversiondossier (n_DOSID);

            INSERT INTO CCHVALUE (CVAID,
                                  CCHSID,
                                  ENTCODE,
                                  CVASTRINGVALUE,
                                  CVADTVALUE,
                                  CVANUMERICVALUE,
                                  CVABOOLEANVALUE,
                                  CVADT,
                                  FACID,
                                  DEPID,
                                  REGID,
                                  ANAID,
                                  AGGID,
                                  DOSIDPROSPECT,
                                  DPRVERSION,
                                  ACTID,
                                  ITRID,
                                  BIMID,
                                  DOSID,
                                  CREID,
                                  CVAPKEYVALUE,
                                  MANID,
                                  TPGCODE,
                                  VARID,
                                  ASSID,
                                  COLID,
                                  SINID,
                                  BUDID,
                                  CHAID,
                                  CEXID,
                                  COMID,
                                  ADMID)
                     VALUES (
                                SEQ_CVAID.NEXTVAL,
                                s_CCHSID_RISKRATACTION,
                                'AVDOSS',
                                s_ACTION,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                n_DOSID,
                                pa_avcommun.f_derniereversiondossier (
                                    n_DOSID),
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                   'Dosid-'
                                || n_DOSID
                                || '||Dprversion-'
                                || pa_avcommun.f_derniereversiondossier (
                                       n_DOSID)
                                || '||',
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL);


            DELETE FROM CCHVALUE
                  WHERE     CCHSID = s_CCHSID_FORMULAICAMOUNT
                        AND DOSIDPROSPECT = n_DOSID
                        AND DPRVERSION =
                            pa_avcommun.f_derniereversiondossier (n_DOSID);

            INSERT INTO CCHVALUE (CVAID,
                                  CCHSID,
                                  ENTCODE,
                                  CVASTRINGVALUE,
                                  CVADTVALUE,
                                  CVANUMERICVALUE,
                                  CVABOOLEANVALUE,
                                  CVADT,
                                  FACID,
                                  DEPID,
                                  REGID,
                                  ANAID,
                                  AGGID,
                                  DOSIDPROSPECT,
                                  DPRVERSION,
                                  ACTID,
                                  ITRID,
                                  BIMID,
                                  DOSID,
                                  CREID,
                                  CVAPKEYVALUE,
                                  MANID,
                                  TPGCODE,
                                  VARID,
                                  ASSID,
                                  COLID,
                                  SINID,
                                  BUDID,
                                  CHAID,
                                  CEXID,
                                  COMID,
                                  ADMID)
                     VALUES (
                                SEQ_CVAID.NEXTVAL,
                                s_CCHSID_FORMULAICAMOUNT,
                                'AVDOSS',
                                NULL,
                                NULL,
                                n_AMOUNT,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                n_DOSID,
                                pa_avcommun.f_derniereversiondossier (
                                    n_DOSID),
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                   'Dosid-'
                                || n_DOSID
                                || '||Dprversion-'
                                || pa_avcommun.f_derniereversiondossier (
                                       n_DOSID)
                                || '||',
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL,
                                NULL);
        END IF;
    EXCEPTION
        WHEN OTHERS
        THEN
            ROLLBACK;

            COMMIT;
    END P_UPDATE_RISKRATING2;

    --HMN

    PROCEDURE P_IMRIMER_OFFRE (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                               P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                               DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                               P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                               P_ACTID       ACTEUR.ACTID%TYPE,
                               P_PROID       PROCESS.PROID%TYPE,
                               P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                               P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        L_CLIENT_ID   ACTEUR.ACTID%TYPE;
        L_IS_ACTIVE   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT MAX (actid)
              INTO L_CLIENT_ID
              FROM dpracteur
             WHERE dosid = P_DOSID AND rolcode = 'CLIENT';

            SELECT COUNT (*)
              INTO L_IS_ACTIVE
              FROM actphase
             WHERE actid = L_CLIENT_ID AND phacode = 'ACTIVE';

            IF L_IS_ACTIVE = 0
            THEN
                UPDATE actphase
                   SET aphdtfin = SYSDATE
                 WHERE     phacode = 'INI'
                       AND actid = L_CLIENT_ID
                       AND jalcode = 'OUVERT';

                INSERT INTO ACTPHASE (ACTID,
                                      APHORDRE,
                                      PHACODE,
                                      PHADEST,
                                      JALCODE,
                                      APHDTDEB,
                                      UTICODE,
                                      CREID,
                                      APHDTFIN,
                                      APHDTMAJ)
                     VALUES (L_CLIENT_ID,
                             '2',
                             'INI',
                             'ACTEUR',
                             'PROSPC',
                             SYSDATE,
                             'ORFI',
                             NULL,
                             NULL,
                             SYSDATE);
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;
    END P_IMRIMER_OFFRE;

    --ARCHIVER DOSSIER
    PROCEDURE P_ARCHIVER_SF (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                             P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                             DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                             P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                             P_ACTID       ACTEUR.ACTID%TYPE,
                             P_PROID       PROCESS.PROID%TYPE,
                             P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                             P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        DECLARE
            sTaccode      VARCHAR2 (15) := NULL;
            nAnaid        NUMBER := NULL;
            Ncreid        NUMBER := 0;
            SDPRVERSION   VARCHAR2 (15) := NULL;
            NRETURNCODE   NUMBER := 0;
            nActid        NUMBER := 0;
        BEGIN
            SELECT MAX (taccode)
              INTO sTaccode
              FROM dossierprospect
             WHERE dosid = P_DOSID;

            SELECT MAX (anaid), MAX (actid)
              INTO nAnaid, nactid
              FROM analysis
             WHERE dosid = P_DOSID AND ANASTATUS = 'EC';

            SELECT DPRVERSION
              INTO SDPRVERSION
              FROM V_deal
             WHERE dosid = P_DOSID;

            PAV4_EVT.P_EVF_STUDY_ARCH (P_DOSID,
                                       SDPRVERSION,
                                       sTaccode,
                                       P_UTICODE,
                                       SYSDATE,
                                       nAnaid,
                                       nactid,
                                       'ARCHIVE',
                                       'BILANSIMP',
                                       2,
                                       Ncreid,
                                       NRETURNCODE);

            UPDATE analysis
               SET dprversion =
                       (SELECT dprversion
                          FROM v_deal
                         WHERE dosid = P_DOSID)
             WHERE dosid = P_DOSID;
        END;
    END P_ARCHIVER_SF;

    PROCEDURE P_INSERT_PORTFL (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                               P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                               DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                               P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                               P_ACTID       ACTEUR.ACTID%TYPE,
                               P_PROID       PROCESS.PROID%TYPE,
                               P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                               P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        DECLARE
            sUticode      VARCHAR2 (15) := NULL;
            nCount        NUMBER := NULL;

            NRETURNCODE   NUMBER := 0;
        BEGIN
            SELECT MAX (lka.uticode)
              INTO sUticode
              FROM dpracteur dpa, lkactutitsm lka
             WHERE     dpa.dosid = P_DOSID
                   AND dpa.rolcode = 'CLIENT'
                   AND dpa.dprversion IN (SELECT dprversion
                                            FROM v_deal
                                           WHERE dosid = P_DOSID)
                   AND lka.actid = dpa.actid
                   AND lka.tsmmetier = 'PORTFL';



            SELECT COUNT (1)
              INTO nCount
              FROM dprintervenant
             WHERE dosid = P_DOSID AND dinmetier = 'PORTFL';

            IF (nCount = 0)
            THEN
                INSERT INTO DPRINTERVENANT (DOSID,
                                            DPRVERSION,
                                            DINORDRE,
                                            UTICODE,
                                            DINMETIER,
                                            DINDTAFFECTATION,
                                            DINFLAGPRINCIPAL,
                                            DINDTSTARTASSIGN,
                                            DINDTENDASSIGN,
                                            DINSALESNETWORK,
                                            DINDPTCODE)
                     VALUES (P_DOSID,
                             'NEGO',
                             (SELECT MAX (dinordre) + 1
                                FROM dprintervenant
                               WHERE dosid = P_DOSID),
                             sUticode,
                             'PORTFL',
                             TO_DATE ('10/01/12', 'DD/MM/RR'),
                             NULL,
                             NULL,
                             NULL,
                             NULL,
                             NULL);
            END IF;
        END;
    END P_INSERT_PORTFL;

    --ENABLE DOSSIER D'?de MMN-20170816
    PROCEDURE P_ENABLE_SF (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                           P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                           DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                           P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                           P_ACTID       ACTEUR.ACTID%TYPE,
                           P_PROID       PROCESS.PROID%TYPE,
                           P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                           P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        DECLARE
            sTaccode      VARCHAR2 (15) := NULL;
            nAnaid        NUMBER := NULL;
            Ncreid        NUMBER := 0;
            NRETURNCODE   NUMBER := 0;
            nActid        NUMBER := 0;
        BEGIN
            DELETE FROM ANAACTEUR
                  WHERE ANAID IN (SELECT ANAID
                                    FROM analysis
                                   WHERE dosid = P_DOSID);

            UPDATE analysis
               SET dprversion =
                       (SELECT dprversion
                          FROM v_deal
                         WHERE dosid = P_DOSID),
                   ANASTATUS = 'EC'
             WHERE dosid = P_DOSID;
        END;
    END P_ENABLE_SF;

    ---MTR 16/04/2018
    PROCEDURE P_CLOSE_CTRL_DERO (
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        BEGIN
            UPDATE DPRCTRLDERO
               SET DCDSTATUS = 'D_DACC'
             WHERE dosid = P_DOSID;
        END;
    END P_CLOSE_CTRL_DERO;

    --MBN 20181023 Annuler classement sans suite
    PROCEDURE P_ANNU_SSUITE (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                             P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                             DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                             P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                             P_ACTID       ACTEUR.ACTID%TYPE,
                             P_PROID       PROCESS.PROID%TYPE,
                             P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                             P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        DECLARE
            l_maxDateExecTer   DATE;
        BEGIN
            UPDATE dossierprospect
               SET DPRDTCLASSEMENT = NULL
             WHERE     dosid = P_DOSID
                   AND DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = P_DOSID);


            DELETE FROM DPRPHASE
                  WHERE     DOSID = P_DOSID
                        AND DPRVERSION = (SELECT DPRVERSION
                                            FROM V_DEAL VDE
                                           WHERE VDE.DOSID = P_DOSID)
                        AND JALCODE = 'D_SSUIT';

            UPDATE DPRPHASE
               SET DPHDTEND = NULL
             WHERE     DOSID = P_DOSID
                   AND DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = P_DOSID)
                   AND DPHORDRE =
                       (SELECT MAX (DPHORDRE)
                          FROM DPRPHASE
                         WHERE     DOSID = P_DOSID
                               AND DPRVERSION = (SELECT DPRVERSION
                                                   FROM V_DEAL VDE
                                                  WHERE VDE.DOSID = P_DOSID));

            SELECT MAX (wstdtexec)
              INTO l_maxDateExecTer
              FROM dprworstep
             WHERE     dosid = P_DOSID
                   AND worcode <> 'WFCLASSSUITE'
                   AND wststatus = 'ANNU'
                   AND DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = P_DOSID);

            UPDATE dprworstep
               SET wststatus = 'INI'
             WHERE     dosid = P_DOSID
                   AND wstdtexec = l_maxDateExecTer
                   AND wststatus = 'ANNU'
                   AND DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = P_DOSID); -- AND WORCODE <> 'WFCLASSSUITE';
        END;
    END P_ANNU_SSUITE;

    --Process activation client
    PROCEDURE P_ACTIVE_CLT (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                            P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                            DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                            P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                            P_ACTID       ACTEUR.ACTID%TYPE,
                            P_PROID       PROCESS.PROID%TYPE,
                            P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                            P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
        L_ACT_INI    NUMBER := 0;
        L_APHORDRE   NUMBER := 0;

        CURSOR C_DPRACTEUR
        IS
            SELECT ACTID
              FROM DPRACTEUR
             WHERE     DOSID = P_DOSID
                   AND DPRVERSION = (SELECT DPRVERSION
                                       FROM V_DEAL VDE
                                      WHERE VDE.DOSID = P_DOSID);
    BEGIN
        BEGIN
            FOR VC_DPRACTEUR IN C_DPRACTEUR
            LOOP
                SELECT COUNT (*)
                  INTO L_ACT_INI
                  FROM actphase
                 WHERE     actid = VC_DPRACTEUR.actid
                       AND phacode = 'INI'
                       AND APHDTFIN IS NULL;

                IF (L_ACT_INI > 0)
                THEN
                    UPDATE actphase
                       SET APHDTFIN = SYSDATE
                     WHERE actid = VC_DPRACTEUR.actid AND phacode = 'INI';

                    SELECT MAX (APHORDRE)
                      INTO L_APHORDRE
                      FROM actphase
                     WHERE actid = VC_DPRACTEUR.actid;

                    INSERT INTO ACTPHASE (ACTID,
                                          APHORDRE,
                                          PHACODE,
                                          PHADEST,
                                          JALCODE,
                                          APHDTDEB,
                                          UTICODE,
                                          CREID,
                                          APHDTFIN,
                                          APHDTMAJ)
                         VALUES (VC_DPRACTEUR.actid,
                                 L_APHORDRE + 1,
                                 'ACTIVE',
                                 'ACTEUR',
                                 'ACTIVE',
                                 SYSDATE,
                                 'ORFI',
                                 NULL,
                                 NULL,
                                 SYSDATE);
                END IF;

                L_ACT_INI := 0;
                L_APHORDRE := 0;
            END LOOP;
        --commit;
        EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
        END;
    END P_ACTIVE_CLT;
    ---HME PROCESS DELETE CHECKLIST --SOGELEASE
      PROCEDURE P_DELETE_CHECK_CLICOM(
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        BEGIN
            DELETE FROM ADMINISTRATIF where DOSIDPROSPECT=P_DOSID 
			and forid=30000;
        END;
    END P_DELETE_CHECK_CLICOM;

PROCEDURE P_DELETE_CHECK_CLIPRO(
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        BEGIN
            DELETE FROM ADMINISTRATIF where DOSIDPROSPECT=P_DOSID 
			and forid=30100;
        END;
    END P_DELETE_CHECK_CLIPRO;
    ----HME DELETE CHECKLIST SELON SEGMENTATION CLIENT
    PROCEDURE P_DELETE_CHECK(
        P_LANCODE     LANPROCESS.LANCODE%TYPE,
        P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
        DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
        P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
        P_ACTID       ACTEUR.ACTID%TYPE,
        P_PROID       PROCESS.PROID%TYPE,
        P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
        P_UTICODE     UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
	 DECLARE
         nACTID      VARCHAR2 (15) := NULL;
		 nseg        VARCHAR2 (15) := NULL;
        BEGIN
		
		Select actid  into nACTID
           from dpracteur 
              where dosid =P_DOSID 
              and dprversion=DPRVERSION
              and rolcode in ('EMPRUNT','CLIENT') ;
			  
		SELECT CVASTRINGVALUE into nseg FROM CCHVALUE WHERE ACTID=nACTID and cchsid='CMBCCHSID82' ;
       
        
		IF (nseg IS NOT NULL AND nseg IN ('10201','10202')) THEN 
		
            DELETE FROM ADMINISTRATIF where DOSIDPROSPECT=P_DOSID 
			and forid=30000;
		ELSE 
       
		  DELETE FROM ADMINISTRATIF where DOSIDPROSPECT=P_DOSID 
			and forid=30100;
        END IF;    
        END;
    END P_DELETE_CHECK;
----HME PROCESS GENERATION DOSSIER ETUDE
PROCEDURE P_FICHEPROP (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                              P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
   AS
       BEGIN
      DECLARE
         nACTID      VARCHAR2 (15) := NULL;
         nANNEE      VARCHAR2 (15) := NULL;

      BEGIN
      Select actid  into nACTID
      from dpracteur 
              where dosid =P_DOSID 
              and dprversion=DPRVERSION
              and rolcode in ('EMPRUNT','CLIENT') ;
      
      select to_char (sysdate,'RRRR') -1 into nANNEE
      from dual ;
      
           Insert into ANALYSIS  values 
			(SEQ_ANAID.NEXTVAL,
			P_PFIID,
			nACTID,
			P_DOSID,
			'NEGO',null,'AVDOSS','BILANSIMP',SYSDATE,'ORFI','CLIENT',null,
      '5',null,null,null,null,null,null,null,'EC',nANNEE,null,null);
       
  END ;
   END P_FICHEPROP;
   
   
   
PROCEDURE P_SGMDTLIM (P_LANCODE     LANPROCESS.LANCODE%TYPE,        --ABH 07/04/2021  La date d???expiration d???une enveloppe est ??gale ?? la date d???effet de l???enveloppe incr??ment??e de la dur??e de validit?? moins un jour ;
                              P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
   AS
       BEGIN
      DECLARE
         Nbmois    number ;
         Dtlim      date  ;
         

      BEGIN
   /*   select decode (PFIPERIODICITE,'030',PFINBPERIODES/12,'360',PFINBPERIODES/1,'090',PFINBPERIODES/4,'180',PFINBPERIODES/2) 
            into Nbannee from PROPOSITIONFINANCIERE where pfiid in 
                (select pfiid from dprpropfinance where DPFFLAGRETENUE=1 
                              and tpgcode in ('AUT02','AUT') and dosid=P_DOSID )  ; */
                              
     /*  select decode (PFIPERIODICITE,'030',PFINBPERIODES,'360',PFINBPERIODES*12,'090',PFINBPERIODES*3,
            '180',PFINBPERIODES*6) 
            into  Nbmois 
            from PROPOSITIONFINANCIERE where pfiid in 
                (select pfiid from dprpropfinance where DPFFLAGRETENUE=1 
                              and tpgcode in ('AUT02','AUT') and dosid=P_DOSID )  ;   */
                              
                              
                              
    /*  select to_date( to_char(extract (day from max( WSTDTEXEC)))||'/' ||
        to_char(extract (month from max( WSTDTEXEC)))||'/' || 
           to_char(extract( year from max( WSTDTEXEC)) +Nbannee) ,'dd/mm/yyyy') into Dtlim from dprworstep where dosid =P_DOSID 
           and worcode||wstorder 
             in (select worcode||wstorder from lanworstep where worcode in 
              ('WFCLIPRO','WFCLICOM')and lancode='FR' and wstlabel like '%??cision%') ; */
            --select  MAX(ADD_MONTHS(WSTDTEXEC, Nbmois )) 
           select  MAX(ADD_MONTHS(WSTDTEXEC, 12 )) 
           into Dtlim 
                  from dprworstep where dosid =P_DOSID 
                   and worcode||wstorder 
                         in (select worcode||wstorder from lanworstep where worcode in 
                          ('WFCLIPRO','WFCLICOM')and lancode='FR' and wstlabel like '%??cision%') ;         
      
     update DPRPROPFINANCE set DPFDTLIMITE= Dtlim -1  where dosid =P_DOSID ;
      
      
       
  END ;
   END P_SGMDTLIM;
   
   PROCEDURE P_FICHEEVAL (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                              P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
   AS
       BEGIN
      DECLARE
         nACTID      VARCHAR2 (15) := NULL;
         nANNEE      VARCHAR2 (15) := NULL;

      BEGIN
      Select actid  into nACTID
      from dpracteur 
              where dosid =P_DOSID 
              and dprversion=DPRVERSION
              and rolcode in ('EMPRUNT','CLIENT') ;
      
      select to_char (sysdate,'RRRR') -1 into nANNEE
      from dual ;
      
           Insert into ANALYSIS  values 
			(SEQ_ANAID.NEXTVAL,
			P_PFIID,
			nACTID,
			P_DOSID,
			'NEGO',null,'AVDOSS','BILANSIMP',SYSDATE,'ORFI','CLIENT',null,
      '7000',null,null,null,null,null,null,null,'EC',nANNEE,null,null);
       
  END ;
   END P_FICHEEVAL;
   
   
   PROCEDURE P_PS_PURGE (P_LANCODE     LANPROCESS.LANCODE%TYPE,
                              P_DOSID       DOSSIERPROSPECT.DOSID%TYPE,
                              DPRVERSION    DOSSIERPROSPECT.DPRVERSION%TYPE,
                              P_PFIID       DPRPROPFINANCE.PFIID%TYPE,
                              P_ACTID       ACTEUR.ACTID%TYPE,
                              P_PROID       PROCESS.PROID%TYPE,
                              P_PSTORDRE    PROSTEP.PSTORDRE%TYPE,
                              P_UTICODE     UTILISATEUR.UTICODE%TYPE)
   AS
       BEGIN
      DECLARE
         ncount1      number (15) := NULL;
         nCOUNT      number (15) := NULL;
         nDOSID     number (15) := NULL;

      BEGIN
    
      
             SELECT  count (*) 
            INTO  ncount1
            FROM dprdecision  
            WHERE  DOSID = nDOSID           
            AND adecode ='ACCORD';    
               
               
            SELECT  count(*)  
			INTO nCOUNT
            FROM PFISIMULATION  
            WHERE  PFIID in ( SELECT  PFIID   
             FROM DPRPROPFINANCE  
             WHERE  DOSID = nDOSID )   ;  
     
     
     IF   ( ncount1 > 0  AND nCOUNT  > 0   )     THEN
     
     Delete from PFISIMULATION where pfiid in ( SELECT  PFIID FROM DPRPROPFINANCE  WHERE  DOSID = nDOSID )   ;
      
    
        END IF ;
  END ;
   END P_PS_PURGE;
   

   
END PAV4_STEPS;