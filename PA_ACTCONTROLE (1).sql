create or replace PACKAGE BODY pa_actcontrole
AS
    -- Declaration des exceptions a trapper
    ERR_CHILD_FOUND                       EXCEPTION;
    ERR_PARENT_NOT_FOUND                  EXCEPTION;
    -- Association des exceptions avec les erreurs ORACLE
    PRAGMA EXCEPTION_INIT (ERR_PARENT_NOT_FOUND, -2291);
    PRAGMA EXCEPTION_INIT (ERR_CHILD_FOUND, -2292);
    -- Declaration des constantes de codes retours a renvoyer
    ERR_CHILD_FOUND_CONST        CONSTANT INTEGER := 1045;
    ERR_PARENT_NOT_FOUND_CONST   CONSTANT INTEGER := 1044;
    ERR_DUP_VAL_ON_INDEX_CONST   CONSTANT INTEGER := 1046;
    ERR_NO_ROWS_CONST            CONSTANT INTEGER := 1;

    FUNCTION OCCODEAPE (SCODEAPE     IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCCATEGORIEJURIDIQUE (
        SCATJ        IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSIREN (SCTL         IN     VARCHAR2,
                      NACTID              ACTEUR.ACTID%TYPE,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSIRET (SACTEUR      IN     VARCHAR2,
                      SSIRET       IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCPHYSIQUE (SACTEUR      IN     VARCHAR2,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSUBADRESSE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        CREVT_REC    IN     CREVT%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSUBRIB (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                       CREVT_REC    IN     CREVT%ROWTYPE,
                       SCTL         IN     VARCHAR2,
                       NB_ELEMENT   IN OUT BINARY_INTEGER,
                       AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                       AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                       ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                       ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSUBPHASE (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                         CREVT_REC    IN     CREVT%ROWTYPE,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION ocRib (SACTEUR      IN     VARCHAR2,
                    SUGECODE     IN     VARCHAR2,
                    SCTL         IN     VARCHAR2,
                    NB_ELEMENT   IN OUT BINARY_INTEGER,
                    AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                    AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                    ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                    ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION F_ROLEBAFI (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER;

    FUNCTION OCROLE (SACTEUR      IN     VARCHAR2,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCPHASE (SACTEUR      IN     VARCHAR2,
                      SCTL         IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCRELA (SACTEUR      IN     VARCHAR2,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSITUJUDI (SACTEUR      IN     VARCHAR2,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSOCRELA (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCACTRELA (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSOCGESTION (
        SACTEUR      IN     VARCHAR2,
        SDEVCODE     IN     VARCHAR2,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCTELECOM (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCCORRESP (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCSTAT (NACTEUR      IN     NUMBER,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCATTRIBUTBAFI (
        SACTEUR      IN     VARCHAR2,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCACTIR (NACTEUR      IN     NUMBER,
                      SCTL         IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCATTRIBUTSAISIE (SACTEUR IN VARCHAR2)
        RETURN NUMBER;

    FUNCTION OCATTRIBUTOK (
        SCTL         IN     VARCHAR2,
        SACTEUR      IN     VARCHAR2,
        SATTCODE     IN     VARCHAR2,
        SATTVALEUR   IN     ACTTAB.ATBVALEUR%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCHAVINGROLE (NACTID             IN ACTEUR.ACTID%TYPE,
                           SROLECODEEXTERNE   IN ROLE.ROLCODEEXTERNE%TYPE)
        RETURN NUMBER;

    FUNCTION OCHAVINGENTRANT (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER;

    -- CV-29032001: ACTRAYONACTION - Secteur geographique
    FUNCTION OCACTRAYONACTION (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    -- TD-12/06/01: Audiences des procedures individuelles
    FUNCTION OCACTPROPHAAUDIENCE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCACTCANDIDATURE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    -- TD le 06/12/01 : Table ACTCANBESOIN
    FUNCTION OCACTCANBESOIN (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

  /*  FUNCTION OCPRVDOUT (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                        CREVT_REC    IN     CREVT%ROWTYPE,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;  */

    FUNCTION OCCHKBILLINGACC (
        NCREID       IN     CREVT.CREID%TYPE,
        NACTID       IN     ACTEUR.ACTID%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    FUNCTION OCPARTICIPATIONLOAN (
        NCREID       IN     CREVT.CREID%TYPE,
        NACTID       IN     ACTEUR.ACTID%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;

    /*
    // Detail du controle par phase d'un acteur
    */
    PROCEDURE PACTEUR (NIDENTIFIANT       IN     NUMBER,
                       NCTLID             IN     NUMBER,
                       SPARA              IN     VARCHAR2,
                       SCONTROLE          IN     VARCHAR2,
                       NUSEROPTION        IN     NUMBER,
                       STYPECONTROLE      IN     VARCHAR2,
                       ACTEUR_REC         IN     ACTEUR%ROWTYPE,
                       CREVT_REC          IN     CREVT%ROWTYPE,
                       NERREURBLOQUANTE   IN OUT NUMBER,
                       NWARNING           IN OUT NUMBER,
                       SUTICODE           IN     UTILISATEUR.UTICODE%TYPE,
                       SUGECODE           IN     UTILISATEUR.UGECODE%TYPE,
                       NDEROGATION        IN OUT NUMBER)
    IS
    BEGIN
        DECLARE
            LOK                NUMBER;
            NMSG               NUMBER;
            NANY               NUMBER;
            NB_ELEMENT         BINARY_INTEGER;
            ATYPE              PA_FUNGENCONTROLE.TBL_VARCHAR2;
            AORDRE             PA_FUNGENCONTROLE.TBL_NUMBER;
            ADEC               PA_FUNGENCONTROLE.TBL_NUMBER;
            AMSG               PA_FUNGENCONTROLE.TBL_VARCHAR2;
            NPART              ACTEURINDIVISION.AINNBPARTTOTAL%TYPE;
            NJEVEUXDUBAFI      NUMBER;
            NLOGIQUE           TOPPARAM.TPALOGIQUE%TYPE;
            NISACTEURSOCIETE   NUMBER;
            NCOUNT             NUMBER;
            DTLASTEXECLOP      DATE;
            NTROUVE            NUMBER;
            NET                NUMBER;
            NETMULTI           NUMBER;
            SPHACODE           ACTPHASE.PHACODE%TYPE;
            NPAYFLAGSIRET      PAYS.PAYFLAGSIRET%TYPE;
            NORDRE             NUMBER;
            BIEN_REC           BIENIMMOBILIER%ROWTYPE;
            -- CV-08032013 CASNT-693
            SPAYCODE           ACTEUR.PAYCODE%TYPE;
            NOPTCHECKSUM       TOPPARAM.TPALOGIQUE%TYPE;
            SREPRESENTATIVE    ACTEUR.ACTSIRET%TYPE;
            SCORPORATE         ACTEUR.ACTSIRET%TYPE;
            NCHECKSUM          NUMBER;
            -- CV-08032013 CASNT-605
            NDTRID             DATATRANSCODING.DTRID%TYPE;
            SDDEHOSTVALUE      DTRDETAIL.DDEHOSTVALUE%TYPE;

            CURSOR CET
            IS
                SELECT *
                  FROM ACTEURGROUPE
                 WHERE ACTID = ACTEUR_REC.ACTID;

            CURSOR CACT
            IS
                SELECT DISTINCT ACTID
                  FROM ACTRELATION
                 WHERE ACTIDRELATION = ACTEUR_REC.ACTID AND TRECODE = 'ET';
        BEGIN
            LOK := 1;
            NB_ELEMENT := 0;

            -- Initialisation du N de message
            SELECT MAX (MSGID)
              INTO NMSG
              FROM TPCTACCONTROLE
             WHERE     TPCCODE = SPARA
                   AND TTCCODE = SCONTROLE
                   AND TPCDEST = 'ACTEUR';

            SELECT COUNT (*)
              INTO NET
              FROM ACTEURGROUPE
             WHERE ACTID = ACTEUR_REC.ACTID;

            -- CV-08032013 CASNT-693
            --PA_COMMON.S_TPALOGIQUE( 'ACTEUR', 'CHECKSUM', nOptCheckSum );
            BEGIN
                SELECT NVL (TPALOGIQUE, 0)
                  INTO NOPTCHECKSUM
                  FROM TOPPARAM
                 WHERE     TOPTABLE = 'ACTEUR'
                       AND TPAPARAM = 'CHECKSUM'
                       AND UGECODE = ACTEUR_REC.UGECODE;
            EXCEPTION
                WHEN OTHERS
                THEN
                    NOPTCHECKSUM := 0;
            END;

            IF SCONTROLE = 'MNBACC'
            THEN --for EVACT_SUBUNITE, check the fields not null for the target Bank Account
                LOK :=
                    OCCHKBILLINGACC (CREVT_REC.CREID,
                                     ACTEUR_REC.ACTID,
                                     NB_ELEMENT,
                                     AORDRE,
                                     AMSG,
                                     ATYPE,
                                     ADEC);
            ELSIF SCONTROLE = 'PARTLN'
            THEN --EVACT_SUBUNITE not allowed for a Bank Account linked to a Participation Loan
                LOK :=
                    OCPARTICIPATIONLOAN (CREVT_REC.CREID,
                                         ACTEUR_REC.ACTID,
                                         NB_ELEMENT,
                                         AORDRE,
                                         AMSG,
                                         ATYPE,
                                         ADEC);
            ELSIF SCONTROLE = 'DFUNITE'
            THEN --EVACT_SUBUNITE not allowed to select multiple default invoice center
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM ACTUNITE
                 WHERE ACTID = ACTEUR_REC.ACTID AND AUNFLAGDEFAULT = 1;

                IF NCOUNT > 1
                THEN
                    LOK := 0;
                END IF;
            ELSIF SUBSTR (SCONTROLE, 1, 2) = 'U_'
            THEN
                PA_USERCONTROLE.P_CONTROLEUSER ('ACTEUR',
                                                NIDENTIFIANT,
                                                SUBSTR (SCONTROLE, 3),
                                                CREVT_REC.CREID,
                                                SUTICODE,
                                                SUGECODE,
                                                LOK);
            -- Paragraphe A01 = Identite de l'acteur
            -- Alinea 01 reference externe non vide
            ELSIF SCONTROLE = 'A0101'
            THEN
                IF ACTEUR_REC.ACTCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 02 libelle court non vide
            ELSIF SCONTROLE = 'A0102'
            THEN
                IF ACTEUR_REC.ACTLIBCOURT IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 03 pays d'immatriculation non vide
            ELSIF SCONTROLE = 'A0103'
            THEN
                IF ACTEUR_REC.PAYCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 04 numero de siret non vide (1068)
            -- CV-06052016 SCFSWL-82 on ne controle pas si HEADGRP
            ELSIF     SCONTROLE = 'A0104'
                  AND (   ACTEUR_REC.ACTTYPE IS NULL
                       OR ACTEUR_REC.ACTTYPE != 'HEADGRP')
            THEN
                --13092006ALA ne pas controler SIRET si l'acteur est entite titulaire
                IF NET = 0
                THEN
                    SELECT NVL (PAYFLAGSIRET, 0)
                      INTO NPAYFLAGSIRET
                      FROM PAYS
                     WHERE PAYCODE = ACTEUR_REC.PAYCODE;

                    IF NPAYFLAGSIRET = 1
                    THEN
                        -- CFS 21952
                        IF ACTEUR_REC.PAYCODE IN ('NC', 'PF')
                        THEN
                            IF     ACTEUR_REC.CJUCODE NOT IN
                                       ('3210', '4140', '1000')
                               AND ACTEUR_REC.ACTSIRET IS NULL
                            THEN
                                LOK := 0;
                            END IF;
                        ELSE
                            IF NOT (    ACTEUR_REC.CJUCODE = '9999'
                                    AND ACTEUR_REC.NAFCODE = '000Z'
                                    AND ACTEUR_REC.PAYCODECATJURIDIQUE = 'FR'
                                    AND ACTEUR_REC.ACTSIRET IS NULL)
                            THEN
                                --IF F_PLGETCJUTYPE(ACTEUR_REC.CJUCODE, ACTEUR_REC.PAYCODECATJURIDIQUE) != 'P' THEN
                                -- CV-29012002 FSA-7683
                                IF     ACTEUR_REC.CJUCODE NOT IN ('3220',
                                                                  '1900',
                                                                  '3210',
                                                                  '4140')
                                   AND ACTEUR_REC.PAYCODECATJURIDIQUE = 'FR'
                                   AND ACTEUR_REC.ACTSIRET IS NULL
                                THEN
                                    LOK := 0;
                                ELSIF     ACTEUR_REC.PAYCODECATJURIDIQUE !=
                                          'FR'
                                      AND ACTEUR_REC.ACTSIRET IS NULL
                                THEN
                                    LOK := 0;
                                END IF;
                            --END IF;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            -- Alinea 05 raison sociale non vide
            ELSIF SCONTROLE = 'A0105'
            THEN
                IF ACTEUR_REC.ACTNOM IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCONTROLE IN ('A30')                 -----------20191211 AKA
            THEN
                DECLARE
                    CREATDT   CCHVALUE.CVADTVALUE%TYPE:=null;
                BEGIN
                   /* SELECT MAX (CVADTVALUE)
                      INTO CREATDT
                      FROM cchvalue cch, acteur act, actrole aro
                     WHERE     cch.cchsid = 'TFDCCHSID191'
                           AND act.actid = ACTEUR_REC.ACTID
                           AND act.actid = aro.actid
                           AND act.actid = cch.actid
                           AND aro.rolcode = 'CLIENT'
                           AND act.acttype = 'E';
                     */

                     SELECT MAX (actdtimmatriculation)
                      INTO CREATDT
                      FROM acteur act, actrole aro
                     WHERE
                           act.actid = ACTEUR_REC.ACTID
                           AND act.actid = aro.actid
                           AND aro.rolcode = 'CLIENT'
                           AND act.acttype = 'E';


                    IF TO_DATE (CREATDT, 'DD/MM/YY') > SYSDATE

                    THEN
                        LOK := 0;
                    ELSE
                        LOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        LOK := 1;
                END;
            -- Alinea 06 code NAF non vide si la categorie juridique est celle d'une personne morale
            ELSIF SCONTROLE = 'A0106'
            THEN
                SELECT COUNT (1)
                  INTO NETMULTI
                  FROM ACTEURGROUPE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND AGRCOMPOUNDINGMCODE != '1';

                IF     NETMULTI != 1
                   AND F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) IN
                           ('E', 'J')
                THEN
                    IF ACTEUR_REC.NAFCODE IS NULL
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            -- Alinea 07 code catj non vide
            ELSIF SCONTROLE = 'A0107'
            THEN
                IF ACTEUR_REC.CJUCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- CV-16092002 FSA-9614
            -- Alinea 08 code registre non vide
            ELSIF     SCONTROLE = 'A0108'
                  AND (ACTEUR_REC.PAYCODECATJURIDIQUE IN ('FR',
                                                          'NC',
                                                          'PF',
                                                          'TN'))
                  AND ACTEUR_REC.CJUCODE != '4140'
            THEN
                IF     ACTEUR_REC.ACTCODERCM IS NULL
                   AND F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) !=
                       'P'
                THEN
                    LOK := 0;
                END IF;
            -- CV-16092002 FSA-9614
            -- Alinea 09 numero de registre non vide
            ELSIF     SCONTROLE = 'A0109'
                  AND (ACTEUR_REC.PAYCODECATJURIDIQUE IN ('FR',
                                                          'NC',
                                                          'PF',
                                                          'TN'))
                  AND ACTEUR_REC.CJUCODE != '4140'
            THEN
                IF ACTEUR_REC.CJUCODE IS NOT NULL
                THEN
                    IF F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) !=
                       'P'
                    THEN
                        IF ACTEUR_REC.ACTNUMRCM IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END IF;
            -- Alinea 11 capital non vide si catj >= 3000
            ELSIF SCONTROLE = 'A0111'
            THEN
                IF ACTEUR_REC.CJUCODE IS NOT NULL
                THEN
                    IF F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) =
                       'E'
                    THEN
                        IF ACTEUR_REC.ACTCAPITAL IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END IF;
            -- Alinea 12 si indivision nb part non vide
            ELSIF SCONTROLE = 'A0112'
            THEN
                IF ACTEUR_REC.CJUCODE IS NOT NULL
                THEN
                    IF F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) =
                       'I'
                    THEN
                        BEGIN
                            SELECT AINNBPARTTOTAL
                              INTO NPART
                              FROM ACTEURINDIVISION
                             WHERE ACTID = ACTEUR_REC.ACTID;

                            IF NPART IS NULL
                            THEN
                                LOK := 0;
                            END IF;
                        EXCEPTION
                            WHEN NO_DATA_FOUND
                            THEN
                                LOK := 0;
                        END;
                    END IF;
                END IF;
            -- Alinea 13 dernier mois de l'exercice fiscal non vide
            ELSIF SCONTROLE = 'A0113'
            THEN
                IF ACTEUR_REC.ACTFINMOISFISC IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 14 langue d'edition non vide
            ELSIF SCONTROLE = 'A0114'
            THEN
                IF ACTEUR_REC.LANCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 15 numero de tva intra comm non vide si role client
            ELSIF SCONTROLE = 'A0115'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTROLE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND F_PLROLEEXTERNE (ROLCODE) = 'CLIENT';

                IF NANY != 0 AND ACTEUR_REC.ACTTVACE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Niveau action obligatoire si role fournisseur
            ELSIF SCONTROLE = 'A0116'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTROLE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND (   F_PLROLEEXTERNE (ROLCODE) = 'FOURN'
                            OR ROLCODE = 'FOURN');

                IF NANY != 0 AND ACTEUR_REC.ACTRATTACHEMENT IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- CV-04042001 (4498) Si la case contrat cadre est cochee, la date d'effet est obligatoire
            ELSIF SCONTROLE = 'A0117'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTROLE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND (   (F_PLROLEEXTERNE (ROLCODE) = 'FOURN')
                            OR (ROLCODE = 'FOURN'));

                IF (NANY != 0)
                THEN
                    SELECT COUNT (1)
                      INTO NANY
                      FROM ACTSERVICEAGREEMENT
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND ASAFLAGPOLICYAGREEMENT = 1
                           AND ASADTSTARTPOLICYAGREEMENT IS NULL;

                    IF (NANY != 0)
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            -- CV-03122001 (5593) Date de la creation de la societe obligatoire
            ELSIF     SCONTROLE = 'A0118'
                  AND F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 0
            THEN
                IF     F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                       ACTEUR_REC.PAYCODECATJURIDIQUE) =
                       'E'
                   AND ACTEUR_REC.ACTDTIMMATRICULATION IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF     SCONTROLE = 'A0119'
                  AND F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 0
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTSECTGESTION
                 WHERE ACTID = ACTEUR_REC.ACTID;

                IF NANY = 0
                THEN
                    LOK := 0;
                END IF;
            -- CASNT-263
            ELSIF SCONTROLE = 'A0120' AND ACTEUR_REC.ACTCHANNEL IS NULL
            THEN
                LOK := 0;
            ELSIF     SCONTROLE = 'A0121'
                  AND (    ACTEUR_REC.ACTCHANNEL IS NOT NULL
                       AND ACTEUR_REC.ACTCHANNELCODE IS NULL)
            THEN
                LOK := 0;
            ELSIF     SCONTROLE = 'A0122'
                  AND (   ACTEUR_REC.ACTCHANNEL IS NULL
                       OR ACTEUR_REC.ACTCHANNELCODE IS NULL)
            THEN
                LOK := 0;
            ELSIF SCONTROLE = 'A0123' AND ACTEUR_REC.ACTSIRET IS NULL
            THEN
                IF    ACTEUR_REC.ACTCHANNEL IS NULL
                   OR ACTEUR_REC.ACTCHANNELCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            -- Alinea 31 Controle du numero siren et du numero de siret
            ELSIF SCONTROLE IN ('A0131', 'A0132')
            THEN
                IF NET = 0
                THEN
                    SELECT NVL (PAYFLAGSIRET, 0)
                      INTO NPAYFLAGSIRET
                      FROM PAYS
                     WHERE PAYCODE = ACTEUR_REC.PAYCODECATJURIDIQUE;

                    IF     (ACTEUR_REC.ACTSIRET IS NOT NULL)
                       AND NPAYFLAGSIRET = 1
                    THEN
                        LOK :=
                            OCSIREN (SCONTROLE,
                                     ACTEUR_REC.ACTID,
                                     NB_ELEMENT,
                                     AORDRE,
                                     AMSG,
                                     ATYPE,
                                     ADEC);
                    END IF;
                END IF;
            -- Alinea 50 Controle de l'unicite du code acteur
            ELSIF SCONTROLE = 'A0150'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTEUR
                 WHERE     ACTCODE = ACTEUR_REC.ACTCODE
                       AND UGECODE = ACTEUR_REC.UGECODE
                       AND ACTID != ACTEUR_REC.ACTID;

                IF NANY != 0
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (ACTEUR_REC.ACTCODE);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            -- Alinea 52 Controle de l'unicite du libelle court de l'acteur si SIRET non controlable
            ELSIF SCONTROLE = 'A0152' AND ACTEUR_REC.ACTLIBCOURT IS NOT NULL
            THEN
                --IF SUBSTR( acteur_rec.actsiret, 1, 2 ) = '20' OR SUBSTR( acteur_rec.actsiret, 1, 9 ) = '999999999' THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTEUR
                 WHERE     ACTLIBCOURT = ACTEUR_REC.ACTLIBCOURT
                       AND UGECODE = ACTEUR_REC.UGECODE
                       AND ACTID != ACTEUR_REC.ACTID;

                IF NANY != 0
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) :=
                        F_STDTRIMALL (ACTEUR_REC.ACTLIBCOURT);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            --END IF;
            -- Alinea 51 Controle d'unicite du numero siret
            ELSIF SCONTROLE = 'A0151'
            THEN
                -- CV-22122016 SCFPROD-263
                IF     ACTEUR_REC.ACTSIRET IS NOT NULL
                   AND ACTEUR_REC.ACTTYPE IS NOT NULL
                   AND NVL (ACTEUR_REC.ACTFLAGPROSPECT, 0) = 0
                THEN
                    -- CV-19032013 CASNT-844
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTROLE
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND ROLCODE IN (SELECT TUPCODE
                                             FROM TUSPARAM
                                            WHERE TUSNOM = 'SALESACTSIRET');

                    IF NANY = 0
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM ACTEUR
                         WHERE     ACTSIRET = ACTEUR_REC.ACTSIRET
                               AND ACTEUR.UGECODE = ACTEUR_REC.UGECODE
                               -- CV-07022017 SCFPROD-355
                               AND NOT EXISTS
                                       (SELECT 1
                                          FROM ACTPHASE
                                         WHERE     ACTID = ACTEUR.ACTID
                                               AND (   JALCODE = 'STUDY'
                                                    OR PHACODE = 'TER')
                                               AND APHDTFIN IS NULL);

                        --AND    ACTTYPE  = acteur_rec.ACTTYPE;
                        IF NANY > 1
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) := ACTEUR_REC.ACTSIRET;
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END IF;
                END IF;
            -- Alinea 53 Donnee non null pour ET NON SIMPLE
            --ALA 021060
            ELSIF SCONTROLE = 'A0153'
            THEN
                IF NET = 1
                THEN
                    FOR CET_REC IN CET
                    LOOP
                        IF    CET_REC.AGRCOMPOUNDUSE IS NULL
                           OR CET_REC.AGREXTERNALREF IS NULL
                           OR CET_REC.AGRBADDEBTSTATUS IS NULL
                           OR CET_REC.AGRLIBCOURT IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    END LOOP;
                END IF;
            -- Alinea 70 code NAF dans la table APE
            ELSIF SCONTROLE = 'A0170'
            THEN
                IF ACTEUR_REC.NAFCODE IS NOT NULL
                THEN
                    LOK :=
                        OCCODEAPE (ACTEUR_REC.NAFCODE,
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC);
                END IF;
            -- Alinea 71 Controle de la categorie juridique
            ELSIF SCONTROLE = 'A0171'
            THEN
                IF ACTEUR_REC.CJUCODE IS NOT NULL
                THEN
                    LOK :=
                        OCCATEGORIEJURIDIQUE (ACTEUR_REC.CJUCODE,
                                              NB_ELEMENT,
                                              AORDRE,
                                              AMSG,
                                              ATYPE,
                                              ADEC);
                END IF;
            -- Alinea 72 Controle du code pays
            ELSIF SCONTROLE = 'A0172'
            THEN
                IF ACTEUR_REC.PAYCODE IS NOT NULL
                THEN
                    LOK := F_PLPAYS (ACTEUR_REC.PAYCODE);

                    IF LOK = 0
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                            F_STDTRIMALL (ACTEUR_REC.PAYCODE);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            -- Alinea 73 Controle du code langue <=> table des langues
            ELSIF SCONTROLE = 'A0173'
            THEN
                IF ACTEUR_REC.LANCODE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM LANGUE
                     WHERE LANCODE = ACTEUR_REC.LANCODE;

                    IF NANY = 0
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                            F_STDTRIMALL (ACTEUR_REC.LANCODE);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            -- Alinea 74 Controle du code type de registre
            ELSIF SCONTROLE = 'A0174'
            THEN
                LOK :=
                    PA_FUNGENCONTROLE.OCCODETABLE ('I',
                                                   ACTEUR_REC.ACTCODERCM,
                                                   'RCRM',
                                                   NB_ELEMENT,
                                                   AORDRE,
                                                   AMSG,
                                                   ATYPE,
                                                   ADEC);
            --LG 31/07/2002 FSA 7081 TVA intracommunautaire <> sur acteur
            ELSIF SCONTROLE = 'A0175'
            THEN
                IF (ACTEUR_REC.ACTTVACE IS NOT NULL)
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEUR
                     WHERE     ACTTVACE = ACTEUR_REC.ACTTVACE
                           AND UGECODE = ACTEUR_REC.UGECODE
                           AND ACTID != ACTEUR_REC.ACTID;

                    IF NANY != 0
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                            F_STDTRIMALL (ACTEUR_REC.ACTTVACE);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            -- CV-08032013 CASNT-693 MSG20780
            ELSIF SCONTROLE = 'A0110'
            THEN
                BEGIN
                    SELECT PAYCODE
                      INTO SPAYCODE
                      FROM UNITEGESTION
                     WHERE UGECODE = ACTEUR_REC.UGECODE;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        SPAYCODE := NULL;
                END;

                IF SPAYCODE IS NOT NULL
                THEN
                    IF     ACTEUR_REC.ACTRESIDENTCODE = '1'
                       AND ACTEUR_REC.PAYCODE != SPAYCODE
                    THEN
                        LOK := 0;
                    ELSIF     ACTEUR_REC.ACTRESIDENTCODE = '2'
                          AND ACTEUR_REC.PAYCODE = SPAYCODE
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            -- MSG20782
            --  ELSIF sControle = 'A0130' THEN
            --   IF nOptCheckSum = 1 THEN
            --    nCheckSum       := f_PlGetCustomCharBoolean( 'ACTEUR', 'CHKDISCHECKSUMVAL', acteur_rec.ACTID, NULL );
            --    IF nCheckSum = 0 OR nCheckSum IS NULL THEN
            --     sRepresentative := acteur_rec.ACTREPRESENTATIVE;
            --     sCorporate      := acteur_rec.ACTCORPORATE;
            --     IF acteur_rec.ACTTYPE IS NULL OR acteur_rec.ACTSIRET IS NULL OR acteur_rec.ACTRESIDENTCODE IS NULL THEN
            --                  lOk:=0;
            --     ELSIF (acteur_rec.ACTTYPE = 'BPART' AND sRepresentative IS NULL) OR
            --                     (acteur_rec.ACTTYPE = 'BPM' AND (sRepresentative IS NULL OR sCorporate IS NULL)) OR
            --                     (acteur_rec.ACTTYPE = 'PM' AND sRepresentative IS NULL) THEN
            --                  lOk := 0;
            --     ELSIF f_validateChecksums ( acteur_rec.ACTTYPE, acteur_rec.ACTSIRET, sRepresentative, sCorporate, acteur_rec.ACTRESIDENTCODE ) != 0 THEN
            --                  lOk := 0;
            --     END IF;
            --    END IF;
            --   END IF;
            -- END CV-08032013 CASNT-693 MSG20780
            -- CV-08032013 CASNT-605 MSG20781
            ELSIF SCONTROLE = 'A0133'
            THEN
                BEGIN
                    SELECT DTRID
                      INTO NDTRID
                      FROM DATATRANSCODING
                     WHERE     DTRCODE = 'ACTTYPE'
                           AND DTRTYPE = 'INTERNE'
                           AND UGECODE = ACTEUR_REC.UGECODE;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NDTRID := NULL;
                END;

                IF NDTRID IS NOT NULL
                THEN
                    BEGIN
                        SELECT DDEHOSTVALUE
                          INTO SDDEHOSTVALUE
                          FROM DTRDETAIL
                         WHERE     DTRID = NDTRID
                               AND DDECASSIOPEEVALUE = ACTEUR_REC.ACTTYPE;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            SDDEHOSTVALUE := NULL;
                    END;

                    IF SDDEHOSTVALUE IS NOT NULL
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM CATJURIDIQUE
                         WHERE     CJUCODE = ACTEUR_REC.CJUCODE
                               AND PAYCODE = ACTEUR_REC.PAYCODE
                               AND CJUTYPE = SDDEHOSTVALUE;

                        IF NANY = 0
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END IF;
            --ALA Relation d'une entite titulaire simple
            ELSIF SCONTROLE = 'A0176'
            THEN
                IF ACTEUR_REC.LANCODE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEURGROUPE
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND AGRCOMPOUNDINGMCODE = '1';

                    IF NANY > 0
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM ACTRELATION
                         WHERE     ACTID = ACTEUR_REC.ACTID
                               AND TRECODE = 'ET'
                               AND AREDTFIN IS NOT NULL;

                        IF NANY > 1
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) := NULL;
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END IF;
                END IF;
            --ALA Relation d'une entite titulaire non simple
            ELSIF SCONTROLE = 'A0177'
            THEN
                IF ACTEUR_REC.LANCODE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEURGROUPE
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND AGRCOMPOUNDINGMCODE != '1';

                    IF NANY > 0
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM ACTRELATION
                         WHERE     ACTID = ACTEUR_REC.ACTID
                               AND TRECODE = 'ET'
                               AND AREDTFIN IS NOT NULL;

                        IF NANY > 2
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) := NULL;
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END IF;
                END IF;
            -- Paragraphe A02 = societes de gestion

            --  CONTROLE FORMAT MAIL xxxx@xx.xx
            ELSIF SCONTROLE = 'A03MA'
            THEN
                DECLARE
                    nMAILINV   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nMAILINV
                      FROM ACTTELECOM
                     WHERE     ATETYPE IN ('ONET', 'NET', 'INET')
                           AND ACTID = ACTEUR_REC.ACTID
                           AND NOT REGEXP_LIKE (
                                       ATENUM,
                                       '[a-zA-Z0-9._%-]+@[a-zA-Z0-9._%-]+\.[a-zA-Z]{2,4}');

                    IF nMAILINV > 0
                    THEN
                        LOK := 0;                                        -- KO
                    ELSE
                        LOK := 1;                                        -- OK
                    END IF;
                END;
            -- CONTROLE FORMAT MOBILE
            -- Un mobile doit avoir un format : 06xxxxxxxx ou 07xxxxxxxx
            ELSIF SCONTROLE = 'A03MO'
            THEN
                DECLARE
                    nMOBINV   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nMOBINV
                      FROM ACTTELECOM
                     WHERE     ATETYPE = 'MOB'
                           AND ACTID = ACTEUR_REC.ACTID
                           AND NOT REGEXP_LIKE (ATENUM, '0[6,7][0-9]{8}');

                    IF nMOBINV > 0
                    THEN
                        LOK := 0;                                        -- KO
                    ELSE
                        LOK := 1;                                        -- OK
                    END IF;
                END;
            -- CONTROLE FORMAT TELEPHONE

            ELSIF SCONTROLE = 'A03TE'
            THEN
                DECLARE
                    nTELINV   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nTELINV
                      FROM ACTTELECOM
                     WHERE     ATETYPE = 'TEL'
                           AND ACTID = ACTEUR_REC.ACTID
                           AND NOT REGEXP_LIKE (ATENUM,
                                                '0[1,2,3,4,5,9][0-9]{8}');

                    IF nTELINV > 0
                    THEN
                        LOK := 0;                                        -- KO
                    ELSE
                        LOK := 1;                                        -- OK
                    END IF;
                END;
            -- LNA 13/05/2016 CONTROLE FORMAT   --END

            --- Controle format mail and phone END-----
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A02'
            THEN
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 1
                THEN
                    LOK :=
                        OCSOCGESTION (ACTEUR_REC.ACTID,
                                      ACTEUR_REC.DEVCODE,
                                      SCONTROLE,
                                      NB_ELEMENT,
                                      AORDRE,
                                      AMSG,
                                      ATYPE,
                                      ADEC);
                END IF;
            -- Paragraphe A03 = Personne physique
            -- Alineas 30 pas de record dans ACTEURPARTICULIER SI catj ne commence pas par 1
            ELSIF SCONTROLE = 'A0330'
            THEN
                IF F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                   ACTEUR_REC.PAYCODECATJURIDIQUE) NOT IN
                       ('P', 'EI')
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEURPARTICULIER
                     WHERE ACTID = ACTEUR_REC.ACTID;

                    IF NANY != 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            -- Tous les AUTRES Alineas Controle SUR ACTEURPARTICULIER
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A03'
            THEN
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) != 1
                THEN
                    IF (F_PLGETCJUTYPE (ACTEUR_REC.CJUCODE,
                                        ACTEUR_REC.PAYCODECATJURIDIQUE) IN
                            ('P', 'EI'))
                    THEN
                        -- CV-26112007 CFS25829 report CFS24476
                        -- IF ocHavingRole( acteur_rec.ACTID, 'CLIENT' ) = 1 OR ocHavingRole( acteur_rec.ACTID, 'GARANT' ) = 1 THEN
                        LOK :=
                            OCPHYSIQUE (ACTEUR_REC.ACTID,
                                        SCONTROLE,
                                        NB_ELEMENT,
                                        AORDRE,
                                        AMSG,
                                        ATYPE,
                                        ADEC);
                    -- END IF;
                    END IF;
                END IF;
            -- Controle des adresses d'un tiers Paragraphe 04
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A04'
            THEN
                LOK :=
                    PA_ACTCONTROLE2.OCADRESSE (ACTEUR_REC.ACTID,
                                               SCONTROLE,
                                               NB_ELEMENT,
                                               AORDRE,
                                               AMSG,
                                               ATYPE,
                                               ADEC);
            -- Controle des RIB Paragraphe 05
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A05'
            THEN
                LOK :=
                    ocRib (ACTEUR_REC.ACTID,
                           ACTEUR_REC.UGECODE,
                           SCONTROLE,
                           NB_ELEMENT,
                           AORDRE,
                           AMSG,
                           ATYPE,
                           ADEC);
            -- Controle des donnees TELECOM Paragraphe 06
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A06'
            THEN
                LOK :=
                    OCTELECOM (ACTEUR_REC.ACTID,
                               SCONTROLE,
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC);
            -- Controle des donnees correspondants Paragraphe 07
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A07'
            THEN
                IF OCHAVINGENTRANT (ACTEUR_REC.ACTID) = 1
                THEN
                    LOK :=
                        OCCORRESP (ACTEUR_REC.ACTID,
                                   SCONTROLE,
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC);
                END IF;
            -- Controle des codes statistiques Paragraphe 08
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A08'
            THEN
                LOK :=
                    OCSTAT (ACTEUR_REC.ACTID,
                            SCONTROLE,
                            NB_ELEMENT,
                            AORDRE,
                            AMSG,
                            ATYPE,
                            ADEC);
            -- Controle sur role Paragraphe 09
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A09'
            THEN
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) != 1
                THEN
                    LOK :=
                        OCROLE (ACTEUR_REC.ACTID,
                                SCONTROLE,
                                NB_ELEMENT,
                                AORDRE,
                                AMSG,
                                ATYPE,
                                ADEC);
                END IF;
            -- Controle sur phases Paragraphe 10
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A10'
            THEN
                LOK :=
                    OCPHASE (ACTEUR_REC.ACTID,
                             SCONTROLE,
                             NB_ELEMENT,
                             AORDRE,
                             AMSG,
                             ATYPE,
                             ADEC);
            -- Controle sur situation judiciaire Paragraphe 13
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A13'
            THEN
                LOK :=
                    OCSITUJUDI (ACTEUR_REC.ACTID,
                                SCONTROLE,
                                NB_ELEMENT,
                                AORDRE,
                                AMSG,
                                ATYPE,
                                ADEC);
            -- Controle sur relation hierarchique Paragraphe 14
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A14'
            THEN
                LOK :=
                    OCRELA (ACTEUR_REC.ACTID,
                            SCONTROLE,
                            NB_ELEMENT,
                            AORDRE,
                            AMSG,
                            ATYPE,
                            ADEC);
            -- Controle sur relation hierarchique Paragraphe 15
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A15'
            THEN
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 1
                THEN
                    LOK :=
                        OCSOCRELA (ACTEUR_REC.ACTID,
                                   SCONTROLE,
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC);
                ELSE
                    LOK :=
                        OCACTRELA (ACTEUR_REC.ACTID,
                                   SCONTROLE,
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC);
                END IF;
            -- Controle de l'existence des attributs BAFI Paragraphe 16
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A16'
            THEN
                -- si l'acteur est une societe, le controle des attributs BAFI est systematique
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 1
                THEN
                    NJEVEUXDUBAFI := 1;
                ELSE
                    -- on commence par verifier s'il existe une societe sur le site ayant des attributs
                    -- BAFI E1 ou FA; si tel est le cas nJeVeuxDuBafi vaudra 1
                    -- LG FSA 18254 01/03/05
                    PA_SELECTACTEUR.S_ACTGESTBAFI (NJEVEUXDUBAFI,
                                                   ACTEUR_REC.UGECODE);

                    IF NJEVEUXDUBAFI = 1
                    THEN
                        -- maintenant on va regarder s'il existe un role pour l'acteur necessitant
                        -- la definition d'attributs BAFI
                        NJEVEUXDUBAFI := F_ROLEBAFI (ACTEUR_REC.ACTID);
                    END IF;
                END IF;

                IF NJEVEUXDUBAFI = 1
                THEN
                    LOK :=
                        OCATTRIBUTBAFI (ACTEUR_REC.ACTID,
                                        SCONTROLE,
                                        NB_ELEMENT,
                                        AORDRE,
                                        AMSG,
                                        ATYPE,
                                        ADEC);
                END IF;
            -- Controle de la date d effet de l'evenement
            ELSIF SCONTROLE = 'EFFNULL'
            THEN
                IF CREVT_REC.CREDTEFFET IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCONTROLE = 'EFFSYS'
            THEN
                IF TRUNC (CREVT_REC.CREDTEFFET) < TRUNC (SYSDATE)
                THEN
                    LOK := 0;
                END IF;
            -- Controle sur les interets de retard Paragraphe 18
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A17'
            THEN
                -- On ne fait ce controle que si l'on se trouve dans un acteur de gestion --
                IF F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID) = 1
                THEN
                    LOK :=
                        OCACTIR (ACTEUR_REC.ACTID,
                                 SCONTROLE,
                                 NB_ELEMENT,
                                 AORDRE,
                                 AMSG,
                                 ATYPE,
                                 ADEC);
                END IF;
            -- CV-29032001: Specialites et metiers
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A18'
            THEN
                LOK :=
                    OCACTRAYONACTION (ACTEUR_REC,
                                      SCONTROLE,
                                      NB_ELEMENT,
                                      AORDRE,
                                      AMSG,
                                      ATYPE,
                                      ADEC);
            -- TD-12/06/01: Audiences des procedures individuelles
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A19'
            THEN
                LOK :=
                    OCACTPROPHAAUDIENCE (ACTEUR_REC,
                                         SCONTROLE,
                                         NB_ELEMENT,
                                         AORDRE,
                                         AMSG,
                                         ATYPE,
                                         ADEC);
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A20'
            THEN
                LOK :=
                    OCACTCANDIDATURE (ACTEUR_REC,
                                      SCONTROLE,
                                      NB_ELEMENT,
                                      AORDRE,
                                      AMSG,
                                      ATYPE,
                                      ADEC);
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A21'
            THEN
                IF SCONTROLE = 'A2142'
                THEN
                    PA_COMMON.S_TPALOGIQUE ('ACTEUR', 'URENTRANT', NLOGIQUE);

                    IF (NLOGIQUE = 1)
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM ACTROLE ARO, ROLE ROL
                         WHERE     ARO.ACTID = ACTEUR_REC.ACTID
                               AND ROL.ROLCODE = ARO.ROLCODE
                               AND NVL (ROL.ROLFLAGENTRANT, 0) = 1;

                        IF (NANY > 0)
                        THEN
                            SELECT COUNT (*)
                              INTO NANY
                              FROM ACTUNITE
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND AUNDTFIN IS NULL;

                            IF NANY = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    END IF;
                ELSE
                    LOK :=
                        PA_ACTCONTROLE2.OCACTUNITE (ACTEUR_REC,
                                                    SCONTROLE,
                                                    NB_ELEMENT,
                                                    AORDRE,
                                                    AMSG,
                                                    ATYPE,
                                                    ADEC);
                END IF;
            ELSIF SUBSTR (SCONTROLE, 1, 3) = 'A22'
            THEN
                LOK :=
                    OCACTCANBESOIN (ACTEUR_REC,
                                    SCONTROLE,
                                    NB_ELEMENT,
                                    AORDRE,
                                    AMSG,
                                    ATYPE,
                                    ADEC);
            -- Controle lies a la substitution d'adresse
            -- CV-31122008 CFS28926 MSG11581 : ajout SUBAD4
            ELSIF SCONTROLE IN ('SUBAD1',
                                'SUBAD2',
                                'SUBAD3',
                                'SUBAD4')
            THEN
                LOK :=
                    OCSUBADRESSE (ACTEUR_REC,
                                  CREVT_REC,
                                  SCONTROLE,
                                  NB_ELEMENT,
                                  AORDRE,
                                  AMSG,
                                  ATYPE,
                                  ADEC);
            -- Controle lies a la substitution de rib
            -- CV-31122008 CFS28926 ajout SUBRI1
            ELSIF SCONTROLE IN ('SUBRI1', 'SUBRI3')
            THEN
                LOK :=
                    OCSUBRIB (ACTEUR_REC,
                              CREVT_REC,
                              SCONTROLE,
                              NB_ELEMENT,
                              AORDRE,
                              AMSG,
                              ATYPE,
                              ADEC);
            -- Controle lies a la substitution d'adresse
            ELSIF SCONTROLE IN ('SUBPH1', 'SUBPH2')
            THEN
                LOK :=
                    OCSUBPHASE (ACTEUR_REC,
                                CREVT_REC,
                                SCONTROLE,
                                NB_ELEMENT,
                                AORDRE,
                                AMSG,
                                ATYPE,
                                ADEC);
            ELSIF SCONTROLE = 'EXFERM'
            THEN
                SELECT F_PLACTEURISSOCIETE (ACTEUR_REC.ACTID)
                  INTO NISACTEURSOCIETE
                  FROM DUAL;

                NCOUNT := 0;

                IF NISACTEURSOCIETE = 1
                THEN
                    BEGIN
                        SELECT COUNT (*)
                          INTO NTROUVE
                          FROM CREDATA CDA
                         WHERE     CDA.CDACOLONNE = 'AEXTYPECLOTURE'
                               AND CDA.CREID = CREVT_REC.CREID
                               AND CDADATASTRING = 'P';
                    END;

                    IF NTROUVE > 0
                    THEN
                        BEGIN
                            SELECT CDADATADATE
                              INTO DTLASTEXECLOP
                              FROM CREDATA
                             WHERE     CREID = CREVT_REC.CREID
                                   AND CDACOLONNE = 'AEXDTCLOTURE'
                                   AND CDATABLE = 'ACTEXERCICE';
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                DTLASTEXECLOP := TO_DATE (NULL);
                        END;

                        IF DTLASTEXECLOP IS NOT NULL
                        THEN
                            BEGIN
                                  SELECT COUNT (*)
                                    INTO NCOUNT
                                    FROM DOSSIER, CREVT
                                   WHERE     DOSSIER.ACTID = ACTEUR_REC.ACTID
                                         AND DOSETAT = 'VER'
                                         AND CREID =
                                             (SELECT MAX (CREID)
                                                FROM CREVT
                                               WHERE CREVT.DOSID =
                                                     DOSSIER.DOSID)
                                         AND ROWNUM < 2
                                GROUP BY DOSSIER.DOSID, CREDTEFFET
                                  HAVING CREDTEFFET <= DTLASTEXECLOP
                                ORDER BY CREDTEFFET;
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    NCOUNT := 0;
                            END;
                        END IF;
                    END IF;
                END IF;

                IF NCOUNT > 0
                THEN
                    LOK := 0;
                END IF;
            --ALA DEB 270307 des controls  pour la gestion des incidents
            ELSIF SCONTROLE = 'DTINEW'
            THEN
                IF CREVT_REC.TMFFONCTION = 'EVACT_INCNEW'
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM CREVT CREA
                     WHERE     CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_INCCLOSE'
                                       AND ACTID = ACTEUR_REC.ACTID
                                       AND CRECODEORIGINE =
                                           CREVT_REC.CRECODEORIGINE)
                           AND CREDTEFFET > CREVT_REC.CREDTEFFET;

                    IF NCOUNT > 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'DTIUPD'
            THEN
                IF CREVT_REC.TMFFONCTION = 'EVACT_INCUPD'
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM CREVT CREA
                     WHERE     CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_INCNEW'
                                       AND ACTID = ACTEUR_REC.ACTID
                                       AND CRECODEORIGINE =
                                           CREVT_REC.CRECODEORIGINE)
                           AND CREDTEFFET > CREVT_REC.CREDTEFFET;

                    IF NCOUNT > 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'DTICLO'
            THEN
                IF CREVT_REC.TMFFONCTION = 'EVACT_INCCLOSE'
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM CREVT CREA
                     WHERE     CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION IN
                                               ('EVACT_INCNEW',
                                                'EVACT_INCUPD')
                                       AND ACTID = ACTEUR_REC.ACTID
                                       AND CRECODEORIGINE =
                                           CREVT_REC.CRECODEORIGINE)
                           AND CREDTEFFET > CREVT_REC.CREDTEFFET;

                    IF NCOUNT > 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            --ALA FIN 270307
            --ALA 210507
            ELSIF     SCONTROLE = 'TERET'
                  AND CREVT_REC.TMFFONCTION = 'EVACT_PHAREMP'
            THEN
                SELECT PHACODE
                  INTO SPHACODE
                  FROM ACTPHASE
                 WHERE ACTID = CREVT_REC.ACTID AND APHDTFIN IS NULL;

                IF SPHACODE = 'TER'
                THEN
                    FOR CACT_REC IN CACT
                    LOOP
                        SELECT COUNT (*)
                          INTO NANY
                          FROM ACTPHASE
                         WHERE     ACTID = CACT_REC.ACTID
                               AND PHACODE != 'TER'
                               AND APHDTFIN IS NULL;

                        IF NANY > 0
                        THEN
                            LOK := 0;
                            EXIT;
                        END IF;
                    END LOOP;
                END IF;
           /* ELSIF (SCONTROLE = 'PRVAMNT') OR (SCONTROLE = 'PRVEXCL')
            THEN
                LOK :=
                    OCPRVDOUT (ACTEUR_REC,
                               CREVT_REC,
                               SCONTROLE,
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC);  */
            ELSIF SCONTROLE IN ('TAXESDT', 'TAXEND')
            THEN
                LOK :=
                    PA_BIENIMMOBILIERCONTROLE.OCBIENTAXE (BIEN_REC,
                                                          CREVT_REC,
                                                          SCONTROLE,
                                                          NB_ELEMENT,
                                                          AORDRE,
                                                          AMSG,
                                                          ATYPE,
                                                          ADEC);
            -- CV-19032013 CASNT-860 MSG141
            ELSIF SCONTROLE = 'TFDREPR'
            THEN
                SREPRESENTATIVE :=
                    F_PLGETCUSTOMCHARACTERISTIC ('ACTEUR',
                                                 'TFDREPRESENTVAL',
                                                 ACTEUR_REC.ACTID,
                                                 NULL);

                IF SREPRESENTATIVE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEUR
                     WHERE     ACTTYPE = ACTEUR_REC.ACTTYPE
                           AND ACTSIRET = SREPRESENTATIVE;

                    IF NANY > 1
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := SREPRESENTATIVE;
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            -- CV-19032013 CASNT-860 MSG141
            ELSIF SCONTROLE = 'TFDCORP'
            THEN
                SCORPORATE :=
                    F_PLGETCUSTOMCHARACTERISTIC ('ACTEUR',
                                                 'TFDCORPORATEVAL',
                                                 ACTEUR_REC.ACTID,
                                                 NULL);

                IF SCORPORATE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEUR
                     WHERE     ACTTYPE = ACTEUR_REC.ACTTYPE
                           AND ACTSIRET = SCORPORATE;

                    IF NANY > 1
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := SCORPORATE;
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            END IF;

            -- Existence d'une erreur
            IF LOK = 0
            THEN
                PA_FUNGENCONTROLE.OCECRITCOMPTERENDU (NCTLID,
                                                      STYPECONTROLE,
                                                      SPARA,
                                                      SCONTROLE,
                                                      NMSG,
                                                      NUSEROPTION,
                                                      NERREURBLOQUANTE,
                                                      NWARNING,
                                                      NDEROGATION,
                                                      NB_ELEMENT,
                                                      AORDRE,
                                                      AMSG,
                                                      ATYPE,
                                                      ADEC,
                                                      'ACTEUR',
                                                      'GLOBAL');
            END IF;
        END;
    END PACTEUR;

    -- Controle du code APE
    FUNCTION OCCODEAPE (SCODEAPE     IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK    NUMBER;
            NANY   NUMBER;
        BEGIN
            LOK := 1;

            IF SCODEAPE IS NOT NULL
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM NAF
                 WHERE NAFCODE = SCODEAPE;

                IF NANY = 0
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (SCODEAPE);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCCODEAPE;

    -- adequation role d'un acteur definition attribution BAFI
    FUNCTION F_ROLEBAFI (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK   NUMBER := 0;

            CURSOR CACTROLE
            IS
                SELECT ROLCODE
                  FROM ACTROLE
                 WHERE ACTID = NACTID;
        BEGIN
            FOR CACTROLE_REC IN CACTROLE
            LOOP
                LOK := 0;
                PA_SELECTACTEUR.S_ROLEBAFI (CACTROLE_REC.ROLCODE, LOK);

                IF LOK = 1
                THEN
                    EXIT;
                END IF;
            END LOOP;

            RETURN LOK;
        END;
    END F_ROLEBAFI;

    -- Controle de la categorie juridique si elle est renseignee
    FUNCTION OCCATEGORIEJURIDIQUE (
        SCATJ        IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK    NUMBER;
            NANY   NUMBER;
        BEGIN
            LOK := 1;

            IF SCATJ IS NOT NULL
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM CATJURIDIQUE
                 WHERE CJUCODE = SCATJ;

                IF NANY = 0
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (SCATJ);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCCATEGORIEJURIDIQUE;

    -- Controle societe de gestion
    FUNCTION OCSOCGESTION (
        SACTEUR      IN     VARCHAR2,
        SDEVCODE     IN     VARCHAR2,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK                NUMBER := 1;
            ACTEURSOC_REC      ACTEURGESTION%ROWTYPE;
            NAEXEXERCICE       ACTEXERCICE.AEXEXERCICE%TYPE;
            SLASTDEVCODE       ACTEXERCICE.DEVCODE%TYPE;
            NORDRE             NUMBER;
            DTEXEDEB           DATE;
            DTEXEFIN           DATE;
            NCOUNT             NUMBER;
            SDATE              VARCHAR2 (8);
            DTDATEDEB          DATE;
            DTDATEFIN          DATE;
            -- CV-28052013 CASNT-2573
            NOPTFISCFREECLOS   NUMBER;

            CURSOR C1
            IS
                SELECT *
                  FROM ACTEXERCICE
                 WHERE ACTID = SACTEUR;

            CURSOR C2
            IS
                  SELECT AEXANNEEFIN,
                         AEXMOISFIN,
                         AEXEXERCICE,
                         AEXDTCLOTURE
                    FROM ACTEXERCICE
                   WHERE     ACTID = SACTEUR
                         AND AEXEXERCICE < NAEXEXERCICE
                         AND AEXDTCLOTURE IS NULL
                ORDER BY AEXEXERCICE, AEXANNEEDEB, AEXMOISDEB;

            CURSOR C3
            IS
                SELECT AEXANNEEFIN,
                       AEXMOISFIN,
                       AEXEXERCICE,
                       AEXDTCLOTURE
                  FROM ACTEXERCICE
                 WHERE ACTID = SACTEUR AND AEXEXERCICE = NAEXEXERCICE;
        --          AND    AEXDTCLOTURE IS NOT NULL
        BEGIN
            -- CV-28052013 CASNT-2573
            PA_COMMON.S_TPALOGIQUE ('FISCYEAR',
                                    'FREECLOSUREDATE',
                                    NOPTFISCFREECLOS);

            SELECT *
              INTO ACTEURSOC_REC
              FROM ACTEURGESTION
             WHERE ACTID = SACTEUR;

            IF SCTL = 'A0201'
            THEN
                IF ACTEURSOC_REC.AGEEMETTEUR IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A0205'
            THEN
                IF ACTEURSOC_REC.AGEEMETVIR IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A0204'
            THEN
                IF ACTEURSOC_REC.AGECIB IS NULL
                THEN
                    IF OCATTRIBUTBAFI (SACTEUR,
                                       SCTL,
                                       NB_ELEMENT,
                                       AORDRE,
                                       AMSG,
                                       ATYPE,
                                       ADEC) = 1
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            /*
            ELSIF sCtl = 'A0203' THEN
            IF acteursoc_rec.agepctrecuptva IS NULL THEN
            lOk := 0;
            END IF;
            */
            ELSIF SCTL = 'A0230'
            THEN
                IF ACTEURSOC_REC.AGECIB IS NOT NULL
                THEN
                    LOK :=
                        PA_FUNGENCONTROLE.OCCIBVERIFY (ACTEURSOC_REC.AGECIB);

                    IF LOK = 0
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                            F_STDTRIMALL (ACTEURSOC_REC.AGECIB);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            ELSIF SCTL IN ('A0231',
                           'A0206',
                           'A0207',
                           'A0208',
                           'A0209',
                           'A0232',
                           'A0233',
                           'A0234',
                           'A0235')
            THEN
                IF SCTL = 'A0231' AND SDEVCODE IS NOT NULL
                THEN
                    LOK := 1;
                /*   FOR C1R IN C1 LOOP
                dtExeDeb := TO_DATE( TO_CHAR (1) || '/' || TO_CHAR( C1R.AEXMOISDEB ) || '/' || TO_CHAR( C1R.AEXANNEEDEB ), 'dd/mm/yyyy' );
                dtExeFin := LAST_DAY( TO_DATE( TO_CHAR (1) || '/' || TO_CHAR( C1R.AEXMOISFIN ) || '/' || TO_CHAR( C1R.AEXANNEEFIN ), 'dd/mm/yyyy' ) );
                IF ( dtExeDeb <= SYSDATE ) AND ( SYSDATE <= dtExeFin ) THEN
                sLastDevCode := C1R.DEVCODE;
                IF sLastDevCode != sDevCode THEN
                lOk := 0;
                END IF;
                EXIT;
                END IF;
                END LOOP;
                IF lOk = 0 THEN
                nb_element := nb_element + 1;
                nOrdre := nb_element;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element) := 'LANDEVISE|DEVCODE|' || f_stdtrimall( sLastDevCode );
                aType(nb_element) := 'S';
                aDec(nb_element) := NULL;
                nb_element := nb_element + 1;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element)   := 'LANDEVISE|DEVCODE|' || f_stdtrimall( sDevCode );
                aType(nb_element)  := 'S';
                aDec(nb_element)   := NULL;
                END IF;   */
                ELSE
                    FOR C1R IN C1
                    LOOP
                        IF (SCTL = 'A0206') AND C1R.AEXMOISDEB IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := C1R.AEXEXERCICE;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NULL;
                        ELSIF (SCTL = 'A0207') AND C1R.AEXANNEEDEB IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := C1R.AEXEXERCICE;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NULL;
                        ELSIF (SCTL = 'A0208') AND C1R.AEXMOISFIN IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := C1R.AEXEXERCICE;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NULL;
                        ELSIF (SCTL = 'A0209') AND C1R.AEXANNEEFIN IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := C1R.AEXEXERCICE;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NULL;
                        /* ELSIF (sCtl = 'A0232') AND C1R.AEXDTCLOTURE IS NOT NULL THEN
                        SELECT   COUNT(*)
                        INTO     nCount
                        FROM     ACTEXERCICE
                        WHERE    ACTID = C1R.ACTID
                        AND    AEXEXERCICE < C1R.AEXEXERCICE
                        AND    AEXDTCLOTURE IS NULL;
                        IF nCount>0 THEN
                        lOk := 0;
                        nb_element         := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := C1R.AEXEXERCICE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        ELSE
                        nAexExercice := C1R.AEXEXERCICE;
                        dbms_output.put_line ('Verification de la date de cloture avant' || TO_CHAR(nAexExercice));
                        FOR C2R IN C2 LOOP
                        dbms_output.put_line ('BOUCLE FOR Verification de la date de cloture avant' || TO_CHAR(nAexExercice));
                        sDate := '01' || LPAD(C2R.AEXMOISFIN,2,'0') || LPAD(C2R.AEXANNEEFIN,4,'0');
                        dbms_output.put_line ('Chaine pour la date de fin' || sDate );
                        dtDateFin := TO_DATE ( sDate , 'DDMMYYYY');
                        dtDateFin := LAST_DAY(dtDateFin);
                        dbms_output.put_line ('Date de cloture construite' || TO_CHAR(dtDateFin));
                        dbms_output.put_line ('Date de cloture en base' || TO_CHAR(C2R.AEXDTCLOTURE));
                        IF dtDateFin != C2R.AEXDTCLOTURE THEN
                        lOk := 0;
                        nb_element         := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := C1R.AEXEXERCICE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        END IF;
                        END LOOP;
                        END IF;*/
                        -- CV-27022001 FSA4883
                        -- 4116 Exercice precedent n'est pas cloture
                        ELSIF     (SCTL = 'A0232')
                              AND C1R.AEXDTCLOTURE IS NOT NULL
                        THEN
                            SDATE :=
                                   '01'
                                || LPAD (C1R.AEXMOISFIN, 2, '0')
                                || LPAD (C1R.AEXANNEEFIN, 4, '0');
                            DTDATEFIN := TO_DATE (SDATE, 'DDMMYYYY');
                            DTDATEFIN := LAST_DAY (DTDATEFIN);
                            SDATE :=
                                   '01'
                                || LPAD (C1R.AEXMOISDEB, 2, '0')
                                || LPAD (C1R.AEXANNEEDEB, 4, '0');
                            DTDATEDEB := TO_DATE (SDATE, 'DDMMYYYY');

                            IF    C1R.AEXDTCLOTURE < DTDATEDEB
                               OR C1R.AEXDTCLOTURE > DTDATEFIN
                            THEN
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (C1R.AEXDTCLOTURE, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       LPAD (C1R.AEXMOISDEB, 2, '0')
                                    || '/'
                                    || LPAD (C1R.AEXANNEEDEB, 4, '0');
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       LPAD (C1R.AEXMOISFIN, 2, '0')
                                    || '/'
                                    || LPAD (C1R.AEXANNEEFIN, 4, '0');
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                                LOK := 0;
                            END IF;
                        ELSIF     (SCTL = 'A0233')
                              AND C1R.AEXDTCLOTURE IS NOT NULL
                        THEN
                            NAEXEXERCICE := C1R.AEXEXERCICE - 1;    -- ( n-1 )

                            FOR C3R IN C3
                            LOOP
                                SDATE :=
                                       '01'
                                    || LPAD (C3R.AEXMOISFIN, 2, '0')
                                    || LPAD (C3R.AEXANNEEFIN, 4, '0');
                                DTDATEFIN := TO_DATE (SDATE, 'DDMMYYYY');
                                DTDATEFIN := LAST_DAY (DTDATEFIN);

                                IF    C3R.AEXDTCLOTURE IS NULL
                                   OR C3R.AEXDTCLOTURE != DTDATEFIN
                                THEN
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    NORDRE := NB_ELEMENT;
                                    AORDRE (NB_ELEMENT) := NORDRE;
                                    AMSG (NB_ELEMENT) :=
                                        TO_CHAR (C1R.AEXDTCLOTURE,
                                                 'YYYYMMDD');
                                    ATYPE (NB_ELEMENT) := 'D';
                                    ADEC (NB_ELEMENT) := NULL;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NORDRE;
                                    AMSG (NB_ELEMENT) := C3R.AEXEXERCICE;
                                    ATYPE (NB_ELEMENT) := 'N';
                                    ADEC (NB_ELEMENT) := NULL;
                                    LOK := 0;
                                    EXIT;
                                END IF;
                            END LOOP;
                        -- CV-23052001 FSA-4880
                        -- 4879 La nombre de mois ouvert doit etre
                        ELSIF     (SCTL = 'A0234')
                              AND C1R.AEXDTCLOTURE IS NOT NULL
                        THEN
                            SDATE :=
                                   '01'
                                || LPAD (C1R.AEXMOISDEB, 2, '0')
                                || LPAD (C1R.AEXANNEEDEB, 4, '0');
                            DTDATEDEB := TO_DATE (SDATE, 'DDMMYYYY');
                            SDATE :=
                                   '01'
                                || LPAD (C1R.AEXMOISFIN, 2, '0')
                                || LPAD (C1R.AEXANNEEFIN, 4, '0');
                            DTDATEFIN := TO_DATE (SDATE, 'DDMMYYYY');
                            DTDATEFIN := LAST_DAY (DTDATEFIN);

                            IF NOPTFISCFREECLOS = 1
                            THEN
                                DTEXEFIN :=
                                    ADD_MONTHS (LAST_DAY (C1R.AEXDTCLOTURE),
                                                C1R.AEXNBMOISOUVERT - 1);
                            ELSE
                                DTEXEFIN :=
                                    ADD_MONTHS (C1R.AEXDTCLOTURE,
                                                C1R.AEXNBMOISOUVERT);
                            END IF;

                            IF DTEXEFIN > DTDATEFIN
                            THEN
                                BEGIN
                                    NCOUNT := 1;

                                    WHILE TRUE
                                    LOOP
                                        IF ADD_MONTHS (C1R.AEXDTCLOTURE,
                                                       NCOUNT) <
                                           DTDATEFIN
                                        THEN
                                            NCOUNT := NCOUNT + 1;
                                        ELSE
                                            EXIT;
                                        END IF;
                                    END LOOP;
                                END;

                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) := NCOUNT;
                                ATYPE (NB_ELEMENT) := 'N';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) := C1R.AEXANNEEDEB;
                                ATYPE (NB_ELEMENT) := 'N';
                                ADEC (NB_ELEMENT) := NULL;
                                LOK := 0;
                            END IF;
                        ELSIF     (SCTL = 'A0235')
                              AND C1R.AEXDTCLOTURE IS NOT NULL
                        THEN
                            IF C1R.AEXDTCLOTURE !=
                               LAST_DAY (C1R.AEXDTCLOTURE)
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    END LOOP;
                END IF;
            /*ELSIF sCtl = 'A0270' THEN
            IF acteursoc_rec.AGEREGIMETVA IS NOT NULL THEN
            lOk := pa_fungencontrole.ocCodeTable(    'I',
            acteursoc_rec.AGEREGIMETVA,
            'REGIMETVA',
            nb_element,
            aOrdre,
            aMsg,
            aType,
            aDec );
            END IF;*/
            ELSIF SCTL = 'A0206'
            THEN
                IF ACTEURSOC_REC.TCIID IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A0207'
            THEN
                IF ACTEURSOC_REC.RUBID IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A0208'
            THEN
                IF ACTEURSOC_REC.TAXCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCSOCGESTION;

    -- Controle des relations non hierarchiques des acteurs autres que societe de gestion
    FUNCTION OCACTRELA (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CRELA
            IS
                SELECT APECODE, APEDTDEB, APEDTFIN
                  FROM ACTPERIMETRE
                 WHERE ACTID = SACTEUR;

            LOK      NUMBER := 1;
            NORDRE   NUMBER := 0;
            NANY     NUMBER;
        BEGIN
            FOR CRELA_REC IN CRELA
            LOOP
                IF SCTL IN ('A1501', 'A1502')
                THEN
                    IF SCTL = 'A1501' AND CRELA_REC.APEDTDEB IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1502' AND CRELA_REC.APECODE IS NULL
                    THEN
                        LOK := 0;
                    END IF;
                ELSIF     SCTL = 'A1530'
                      AND CRELA_REC.APEDTDEB IS NOT NULL
                      AND CRELA_REC.APEDTFIN IS NOT NULL
                THEN
                    IF CRELA_REC.APEDTDEB > CRELA_REC.APEDTFIN
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.APEDTDEB, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.APEDTFIN, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            'LANTUSPARAM|RELATION|' || CRELA_REC.APECODE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL = 'A1570' AND CRELA_REC.APECODE IS NOT NULL
                THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE ('U',
                                                      CRELA_REC.APECODE,
                                                      'RELATION',
                                                      NB_ELEMENT,
                                                      AORDRE,
                                                      AMSG,
                                                      ATYPE,
                                                      ADEC) = 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            END LOOP;

            RETURN LOK;
        END;
    END OCACTRELA;

    -- Controle des relations non hierarchiques des societes de gestion
    FUNCTION OCSOCRELA (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CRELA
            IS
                SELECT AGMCODE,
                       AGMNATURE,
                       AGMDTDEB,
                       AGMDTFIN,
                       DEVCODE,
                       AGMMT
                  FROM AGEGROMESURE
                 WHERE ACTID = SACTEUR;

            LOK      NUMBER := 1;
            NORDRE   NUMBER := 0;
            NANY     NUMBER;
        BEGIN
            FOR CRELA_REC IN CRELA
            LOOP
                IF SCTL IN ('A1501',
                            'A1502',
                            'A1503',
                            'A1504',
                            'A1505')
                THEN
                    IF SCTL = 'A1501' AND CRELA_REC.AGMDTDEB IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1502' AND CRELA_REC.AGMCODE IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1503' AND CRELA_REC.AGMNATURE IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1504' AND CRELA_REC.AGMMT IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1505' AND CRELA_REC.DEVCODE IS NULL
                    THEN
                        LOK := 0;
                    END IF;
                ELSIF     SCTL = 'A1530'
                      AND CRELA_REC.AGMDTDEB IS NOT NULL
                      AND CRELA_REC.AGMDTFIN IS NOT NULL
                THEN
                    IF CRELA_REC.AGMDTDEB > CRELA_REC.AGMDTFIN
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.AGMDTDEB, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.AGMDTFIN, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            'LANTUSPARAM|RELATION|' || CRELA_REC.AGMCODE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL = 'A1570' AND CRELA_REC.AGMCODE IS NOT NULL
                THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE ('U',
                                                      CRELA_REC.AGMCODE,
                                                      'RELATION',
                                                      NB_ELEMENT,
                                                      AORDRE,
                                                      AMSG,
                                                      ATYPE,
                                                      ADEC) = 0
                    THEN
                        LOK := 0;
                    END IF;
                ELSIF SCTL = 'A1571' AND CRELA_REC.AGMNATURE IS NOT NULL
                THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE ('U',
                                                      CRELA_REC.AGMNATURE,
                                                      'NATRELAT',
                                                      NB_ELEMENT,
                                                      AORDRE,
                                                      AMSG,
                                                      ATYPE,
                                                      ADEC) = 0
                    THEN
                        LOK := 0;
                    END IF;
                ELSIF SCTL = 'A1572' AND CRELA_REC.DEVCODE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM DEVISE
                     WHERE DEVCODE = CRELA_REC.DEVCODE;

                    IF NANY = 0
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := F_STDTRIMALL (CRELA_REC.DEVCODE);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            END LOOP;

            RETURN LOK;
        END;
    END OCSOCRELA;

    -- Controle des relations hierarchiques des acteurs
    FUNCTION OCSITUJUDI (SACTEUR      IN     VARCHAR2,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CPROC
            IS
                SELECT APRTYPE, APRDTDEB
                  FROM ACTPROCEDURE
                 WHERE ACTID = SACTEUR;

            /* MP MODIFICATION BASE DU 25/10/00
            CURSOR cJugement IS SELECT APRTYPE, APRDTDEB, APJTYPE, APJDTDEB, APJDUREEAN, APJDUREEMOIS
            FROM ACTPROJUGEMENT WHERE actid = sActeur;
            CURSOR cMandat IS SELECT APRTYPE, APRDTDEB, APMTYPE, ACTIDMANDAT
            FROM ACTPROMANDAT WHERE actid = sActeur; */
            LOK            NUMBER := 1;
            NORDRE         NUMBER := 0;
            NANY           NUMBER;
            SACTLIBCOURT   ACTEUR.ACTLIBCOURT%TYPE;
        BEGIN
            IF SCTL IN ('A1301', 'A1302', 'A1370')
            THEN
                FOR CPROC_REC IN CPROC
                LOOP
                    IF SCTL = 'A1301' AND CPROC_REC.APRTYPE IS NULL
                    THEN
                        LOK := 0;
                    ELSIF SCTL = 'A1370' AND CPROC_REC.APRTYPE IS NOT NULL
                    THEN
                        IF     PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CPROC_REC.APRTYPE,
                                   'PROJUD',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                           AND PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CPROC_REC.APRTYPE,
                                   'HARDCOLL',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                        THEN
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CPROC_REC.APRTYPE,
                                   'TYPROCIND',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A1302' AND CPROC_REC.APRDTDEB IS NULL
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            'LANTUSPARAM|PROJUD|' || CPROC_REC.APRTYPE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END LOOP;
            END IF;

            IF SCTL = 'A1375'
            THEN
                SELECT COUNT (DISTINCT APRTYPE)
                  INTO NANY
                  FROM ACTPROCEDURE
                 WHERE ACTID = SACTEUR AND APRDTFIN IS NULL;

                IF NANY > 1
                THEN
                    LOK := 0;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCSITUJUDI;

    -- Controle des relations hierarchiques des acteurs
    FUNCTION OCRELA (SACTEUR      IN     VARCHAR2,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CRELA
            IS
                SELECT ACTIDRELATION,
                       TRECODE,
                       AREDTDEB,
                       AREDTFIN,
                       AREQP
                  FROM ACTRELATION
                 WHERE ACTID = SACTEUR;

            LOK               NUMBER := 1;
            NORDRE            NUMBER := 0;
            NANY              NUMBER;
            NQP               NUMBER;
            SRELA             ACTRELATION.TRECODE%TYPE;
            NCOUNTCLIENT      NUMBER;
            NCOUNTROLEJUST    NUMBER;
            NACTIDRELGREFFE   ACTEUR.ACTID%TYPE;
            SCODEPOSTGREFF    ADRESSE.ADRCODEPOST%TYPE;
            SCODEPOSTACT      ADRESSE.ADRCODEPOST%TYPE;
            NCOUNT            NUMBER;
            STATATTRIBUTE     TREATTRIBUTE.TATATTRIBUTE%TYPE;
            SACTCODE          ACTEUR.ACTCODE%TYPE;
            SACTLIBCOURT      ACTEUR.ACTLIBCOURT%TYPE;
        BEGIN
            FOR CRELA_REC IN CRELA
            LOOP
                IF SCTL = 'A1431' AND CRELA_REC.AREDTFIN IS NULL
                THEN
                    SRELA := CRELA_REC.TRECODE;

                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTRELATION
                     WHERE     ACTID = SACTEUR
                           AND AREDTFIN IS NULL
                           AND TRECODE = SRELA;

                    IF NANY > 1
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := 'LANTRELATION|TRECODE|' || SRELA;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := NANY;
                        ATYPE (NB_ELEMENT) := 'N';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL IN ('A1401', 'A1402', 'A1403')
                THEN
                    IF CRELA_REC.ACTIDRELATION IS NULL AND SCTL = 'A1403'
                    THEN
                        LOK := 0;
                    ELSIF CRELA_REC.TRECODE IS NULL AND SCTL = 'A1401'
                    THEN
                        LOK := 0;
                    ELSIF CRELA_REC.AREDTDEB IS NULL AND SCTL = 'A1402'
                    THEN
                        LOK := 0;
                    END IF;
                ELSIF     SCTL = 'A1430'
                      AND CRELA_REC.AREDTDEB IS NOT NULL
                      AND CRELA_REC.AREDTFIN IS NOT NULL
                THEN
                    IF CRELA_REC.AREDTDEB > CRELA_REC.AREDTFIN
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.AREDTDEB, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            TO_CHAR (CRELA_REC.AREDTFIN, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;

                        SELECT COUNT (*)
                          INTO NANY
                          FROM TRELATION
                         WHERE TRECODE = CRELA_REC.TRECODE;

                        IF NANY = 1
                        THEN
                            AMSG (NB_ELEMENT) :=
                                'LANTRELATION|TRECODE|' || CRELA_REC.TRECODE;
                            ATYPE (NB_ELEMENT) := 'S';
                        ELSE
                            AMSG (NB_ELEMENT) := '';
                            ATYPE (NB_ELEMENT) := 'C';
                        END IF;

                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL = 'A1470' AND CRELA_REC.TRECODE IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM TRELATION
                     WHERE TRECODE = CRELA_REC.TRECODE;

                    IF NANY != 1
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := F_STDTRIMALL (CRELA_REC.TRECODE);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL = 'A1471' AND CRELA_REC.ACTIDRELATION IS NOT NULL
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTEUR
                     WHERE ACTID = CRELA_REC.ACTIDRELATION;

                    IF NANY != 1
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;

                        SELECT COUNT (*)
                          INTO NANY
                          FROM TRELATION
                         WHERE TRECODE = CRELA_REC.TRECODE;

                        IF NANY = 1
                        THEN
                            AMSG (NB_ELEMENT) :=
                                'LANTRELATION|TRECODE|' || CRELA_REC.TRECODE;
                            ATYPE (NB_ELEMENT) := 'S';
                        ELSE
                            AMSG (NB_ELEMENT) := '';
                            ATYPE (NB_ELEMENT) := 'C';
                        END IF;

                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                --ALA 150807
                ELSIF SCTL = 'A1472' AND CRELA_REC.TRECODE = 'TGROUPE'
                THEN
                    PA_COMMON.S_TPATEXTE ('BALE2BTG',
                                          'VALLIENLIMITE',
                                          STATATTRIBUTE);

                    SELECT COUNT (DISTINCT (TAT.TATATTRIBUTE))
                      INTO NANY
                      FROM TREATTRIBUTE TAT
                     WHERE     TAT.TRECODE = CRELA_REC.TRECODE
                           AND (   STATATTRIBUTE IS NULL
                                OR TAT.TATATTRIBUTE != STATATTRIBUTE);

                    SELECT COUNT (DISTINCT (TATATTRIBUTE))
                      INTO NCOUNT
                      FROM ACTRELPRODETAIL
                     WHERE     ACTID = SACTEUR
                           AND TRECODE = CRELA_REC.TRECODE
                           AND ACTIDRELATION = CRELA_REC.ACTIDRELATION
                           AND AREDTDEB = CRELA_REC.AREDTDEB
                           AND (   STATATTRIBUTE IS NULL
                                OR TATATTRIBUTE != STATATTRIBUTE);

                    IF NANY != NCOUNT
                    THEN
                        LOK := 0;
                    END IF;
                -- CV17092007 CFS24490 report CFS21414 PROCRE MSG10246
                ELSIF SCTL = 'A1473'
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTPHASE
                     WHERE     ACTID = CRELA_REC.ACTIDRELATION
                           AND PHACODE = 'ACTIVE'
                           AND APHDTDEB <= CRELA_REC.AREDTDEB
                           AND (   APHDTFIN IS NULL
                                OR (    CRELA_REC.AREDTFIN IS NOT NULL
                                    AND APHDTFIN >= CRELA_REC.AREDTFIN));

                    IF NANY = 0
                    THEN
                        BEGIN
                            SELECT ACTCODE, ACTLIBCOURT
                              INTO SACTCODE, SACTLIBCOURT
                              FROM ACTEUR
                             WHERE ACTID = CRELA_REC.ACTIDRELATION;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                SACTCODE := NULL;
                        END;

                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;

                        IF SACTCODE IS NOT NULL
                        THEN
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (
                                    SACTCODE || ' - ' || SACTLIBCOURT);
                        ELSE
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (
                                    TO_CHAR (CRELA_REC.ACTIDRELATION));
                        END IF;

                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSIF SCTL = 'A1405' AND CRELA_REC.TRECODE IS NOT NULL
                THEN
                    BEGIN
                        SELECT TREFLAGQP
                          INTO NQP
                          FROM TRELATION
                         WHERE TRECODE = CRELA_REC.TRECODE;

                        IF NQP = 1 AND CRELA_REC.AREQP IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                'LANTRELATION|TRECODE|' || CRELA_REC.TRECODE;
                            ATYPE (NB_ELEMENT) := 'S';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            LOK := 1;
                    END;
                END IF;
            END LOOP;

            IF SCTL = 'A1406'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNTCLIENT
                  FROM ACTROLE
                 WHERE     ACTID = SACTEUR
                       AND F_PLROLEEXTERNE (ROLCODE) = 'CLIENT';

                IF NCOUNTCLIENT > 0
                THEN
                    NACTIDRELGREFFE := F_PLACTRELATIONASC (SACTEUR, 'GREFFE');

                    IF NACTIDRELGREFFE IS NOT NULL
                    THEN
                        SELECT COUNT (*)
                          INTO NCOUNTROLEJUST
                          FROM ACTROLE
                         WHERE     ACTID = NACTIDRELGREFFE
                               AND F_PLROLEEXTERNE (ROLCODE) = 'JUSTICE';

                        IF NCOUNTROLEJUST = 0
                        THEN
                            LOK := 0;
                        END IF;
                    --ELSE
                    --lOk := 0;
                    END IF;
                END IF;
            ELSIF SCTL = 'A1407'
            THEN
                NACTIDRELGREFFE := F_PLACTRELATIONASC (SACTEUR, 'GREFFE');

                IF NACTIDRELGREFFE IS NOT NULL
                THEN
                    BEGIN
                        SELECT SUBSTR (ADRCODEPOST, 1, 2)
                          INTO SCODEPOSTACT
                          FROM ADRESSE ADR, ACTADRESSE AAD
                         WHERE     AAD.ACTID = SACTEUR
                               AND AADORDRE =
                                   (SELECT MAX (A.AADORDRE)
                                      FROM ACTADRESSE A
                                     WHERE     A.ACTID = SACTEUR
                                           AND NVL (A.AADFLAGSIEGE, 0) = 1
                                           AND A.AADDTREMPLACE IS NULL)
                               AND ADR.ADRID = AAD.ADRID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            SCODEPOSTACT := NULL;
                    END;

                    BEGIN
                        SELECT SUBSTR (ADRCODEPOST, 1, 2)
                          INTO SCODEPOSTGREFF
                          FROM ADRESSE ADR, ACTADRESSE AAD
                         WHERE     AAD.ACTID = NACTIDRELGREFFE
                               AND AADORDRE =
                                   (SELECT MAX (A.AADORDRE)
                                      FROM ACTADRESSE A
                                     WHERE     A.ACTID = NACTIDRELGREFFE
                                           AND NVL (A.AADFLAGSIEGE, 0) = 1
                                           AND A.AADDTREMPLACE IS NULL)
                               AND ADR.ADRID = AAD.ADRID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            SCODEPOSTGREFF := NULL;
                    END;

                    IF     SCODEPOSTACT IS NOT NULL
                       AND SCODEPOSTGREFF IS NOT NULL
                    THEN
                        IF SCODEPOSTACT != SCODEPOSTGREFF
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCRELA;

    FUNCTION OCCHKBILLINGACC (
        NCREID       IN     CREVT.CREID%TYPE,
        NACTID       IN     ACTEUR.ACTID%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK      NUMBER;
            NORDRE   NUMBER := 0;
            STR      VARCHAR2 (200);

            CURSOR CCREDATA
            IS
                SELECT AUNDELAIPMT,
                       TMPCODE,
                       AADORDRE,
                       AUNNOTICEMETHODSEND,
                       AUNMULTIPLE,
                       AUNORDRE
                  FROM ACTUNITE ACU, CREDATA CDA
                 WHERE     ACU.ACTID = NACTID
                       AND ACU.AUNORDRE = CDA.CDADATANUMBER
                       AND CDA.CREID = NCREID
                       AND CDA.CDATABLE = 'TARGETBA'
                       AND CDA.CDACOLONNE = 'AUNORDRE_NEW';
        BEGIN
            FOR CCREDATA_REC IN CCREDATA
            LOOP
                IF (CCREDATA_REC.AUNDELAIPMT IS NULL)
                THEN
                    STR := ' Due Date, ';
                END IF;

                IF (CCREDATA_REC.TMPCODE IS NULL)
                THEN
                    STR := STR || ' Payment method, ';
                END IF;

                IF (CCREDATA_REC.AADORDRE IS NULL)
                THEN
                    STR := STR || ' billing address, ';
                END IF;

                IF (CCREDATA_REC.AUNNOTICEMETHODSEND IS NULL)
                THEN
                    STR := STR || ' Sending method, ';
                END IF;

                IF (CCREDATA_REC.AUNMULTIPLE IS NULL)
                THEN
                    STR := STR || ' Sending frequency ';
                END IF;

                IF STR IS NOT NULL
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    NORDRE := NB_ELEMENT;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := STR;
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                    RETURN LOK;
                END IF;
            END LOOP;

            RETURN LOK;
        END;
    END OCCHKBILLINGACC;

    FUNCTION OCPARTICIPATIONLOAN (
        NCREID       IN     CREVT.CREID%TYPE,
        NACTID       IN     ACTEUR.ACTID%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK      NUMBER;
            NORDRE   NUMBER := 0;
            STR      VARCHAR2 (200);

            CURSOR CCREDATA
            IS
                SELECT DOS.TPGCODE
                  FROM DOSACTUNITE DAU, DOSSIER DOS, CREDATA CDA
                 WHERE     DAU.ACTID = NACTID
                       AND DAU.DOSID = DOS.DOSID
                       AND DAU.AUNORDRE = CDA.CDADATANUMBER
                       AND CDA.CREID = NCREID
                       AND CDA.CDATABLE = 'TARGETBA'
                       AND CDA.CDACOLONNE = 'AUNORDRE_NEW';
        BEGIN
            FOR CCREDATA_REC IN CCREDATA
            LOOP
                IF CCREDATA_REC.TPGCODE IN ('A64232',
                                            'A64234',
                                            'A64612',
                                            'A64712')
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    NORDRE := NB_ELEMENT;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := CCREDATA_REC.TPGCODE;
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                    RETURN LOK;
                END IF;
            END LOOP;

            RETURN LOK;
        END;
    END OCPARTICIPATIONLOAN;

    --/ Controle du numero SIREN
    FUNCTION OCSIREN (SCTL         IN     VARCHAR2,
                      NACTID       IN     ACTEUR.ACTID%TYPE,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK        NUMBER;
            NORDRE     NUMBER := 0;
            SSIRET     ACTEUR.ACTSIRET%TYPE;
            SPAYS      ACTEUR.PAYCODE%TYPE;
            SCJUCODE   ACTEUR.CJUCODE%TYPE;
        BEGIN
            SELECT ACTSIRET, PAYCODE, SCJUCODE
              INTO SSIRET, SPAYS, SCJUCODE
              FROM ACTEUR
             WHERE ACTID = NACTID;

            IF SCTL = 'A0131'
            THEN
                LOK := PA_FUNGENCONTROLE.OCSIRENVERIFY (NACTID);

                IF LOK = 0
                THEN
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (SSIRET); --SUBSTR( sSiret, 1, 9 );
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            ELSIF SCTL = 'A0132' AND SPAYS = 'BE' AND SSIRET IS NOT NULL
            THEN
                -- CV-30032005 FSA-19183 KBC
                -- IF LENGTH(sSiret) != 9  THEN
                IF LENGTH (SSIRET) != 10
                THEN
                    --nb_element          := nb_element + 1;
                    --aOrdre(nb_element)  := nb_element;
                    --aMsg(nb_element)    := F_stdtrimall(sSiret);
                    --aType(nb_element)   := 'C';
                    --aDec(nb_element)    := NULL;
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    NORDRE := NB_ELEMENT;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (SSIRET);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := 'LANPAYS|PAYCODE|' || SPAYS;
                    ATYPE (NB_ELEMENT) := 'S';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCSIREN;

    -- Controle d'unicite du numero SIRET
    FUNCTION OCSIRET (SACTEUR      IN     VARCHAR2,
                      SSIRET       IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK        NUMBER;
            NNBSIRET   NUMBER;
            NNBCASG    NUMBER;
            NDOUBLON   NUMBER;
            NANY       NUMBER;
            SUGECODE   ACTEUR.UGECODE%TYPE;
            SPAYCODE   ACTEUR.PAYCODE%TYPE;
            SCJUTYPE   CATJURIDIQUE.CJUTYPE%TYPE;
        BEGIN
            LOK := 1;

            SELECT UGECODE,
                   PAYCODECATJURIDIQUE,
                   F_PLGETCJUTYPE (ACTEUR.CJUCODE,
                                   ACTEUR.PAYCODECATJURIDIQUE)
              INTO SUGECODE, SPAYCODE, SCJUTYPE
              FROM ACTEUR
             WHERE ACTID = SACTEUR;

            SELECT COUNT (*)
              INTO NNBSIRET
              FROM ACTEUR
             WHERE     ACTID != SACTEUR
                   AND UGECODE = SUGECODE
                   AND ACTSIRET = SSIRET
                   AND PAYCODE = SPAYCODE
                   AND F_PLGETCJUTYPE (ACTEUR.CJUCODE,
                                       ACTEUR.PAYCODECATJURIDIQUE) =
                       SCJUTYPE;

            IF (NNBSIRET > 0)
            THEN
                BEGIN
                    SELECT NVL (TPALOGIQUE, 0)
                      INTO NDOUBLON
                      FROM TOPPARAM
                     WHERE     TOPTABLE = 'CTLPARPHASE'
                           AND TPAPARAM = 'SIRETCASG'
                           AND UGECODE = SUGECODE;

                    IF (NDOUBLON = 1)
                    THEN
                        SELECT COUNT (1)
                          INTO NANY
                          FROM ACTPERIMETRE
                         WHERE ACTID = SACTEUR AND APECODE = 'CASG';

                        IF (NANY = 0)
                        THEN
                            NDOUBLON := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NDOUBLON := 0;
                END;

                IF (NDOUBLON = 1)
                THEN
                    SELECT COUNT (1)
                      INTO NNBCASG
                      FROM ACTEUR ACT, ACTPERIMETRE APE
                     WHERE     ACT.ACTID != SACTEUR
                           AND ACT.UGECODE = SUGECODE
                           AND ACT.ACTSIRET = SSIRET
                           AND APE.ACTID = ACT.ACTID
                           AND APE.APECODE = 'CASG';

                    IF (NNBCASG != NNBSIRET)
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := SSIRET;
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                ELSE
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := SSIRET;
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCSIRET;

    -- Controle de la presence des codes statistiques obligatoires dans l'onglet statistique
    FUNCTION OCSTAT (NACTEUR      IN     NUMBER,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK        NUMBER;

            CURSOR CSTAT
            IS
                SELECT SACCODE, SACFLAGOBLIG FROM STATISTIQUEACTEUR;

            CURSOR CACTSTAT
            IS
                SELECT SACCODE, SAVCODEVAL, ASTLIBRE
                  FROM ACTSTATISTIQUE
                 WHERE ACTID = NACTEUR;

            SVAR       ACTSTATISTIQUE.SAVCODEVAL%TYPE;
            SCODE      ACTSTATISTIQUE.SACCODE%TYPE;
            NANY       NUMBER;
            NETMULTI   NUMBER;
        BEGIN
            LOK := 1;

            IF SCTL IN ('A0870', 'A0871', 'A0802')
            THEN
                FOR CACTSTAT_REC IN CACTSTAT
                LOOP
                    IF SCTL = 'A0870'
                    THEN
                        IF CACTSTAT_REC.SACCODE IS NOT NULL
                        THEN
                            SELECT COUNT (*)
                              INTO NANY
                              FROM STATISTIQUEACTEUR
                             WHERE SACCODE = CACTSTAT_REC.SACCODE;

                            IF NANY = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    F_STDTRIMALL (CACTSTAT_REC.SACCODE);
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0802'
                    THEN
                        IF     CACTSTAT_REC.SAVCODEVAL IS NULL
                           AND CACTSTAT_REC.ASTLIBRE IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CACTSTAT_REC.SACCODE);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0871'
                    THEN
                        IF CACTSTAT_REC.SAVCODEVAL IS NOT NULL
                        THEN
                            SELECT COUNT (*)
                              INTO NANY
                              FROM SACVALEUR
                             WHERE     SAVCODEVAL = CACTSTAT_REC.SAVCODEVAL
                                   AND SACCODE = CACTSTAT_REC.SACCODE;

                            IF NANY = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                       F_STDTRIMALL (CACTSTAT_REC.SACCODE)
                                    || ' '
                                    || F_STDTRIMALL (CACTSTAT_REC.SAVCODEVAL);
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCTL = 'A0801'
            THEN
                SELECT COUNT (1)
                  INTO NETMULTI
                  FROM ACTEURGROUPE
                 WHERE ACTID = NACTEUR AND AGRCOMPOUNDINGMCODE != '1';

                IF NETMULTI = 0
                THEN
                    -- Code stat obligatoire
                    FOR CSTAT_REC IN CSTAT
                    LOOP
                        IF NVL (CSTAT_REC.SACFLAGOBLIG, 0) = 1
                        THEN
                            --TRUNK-2644
                            --SELECT COUNT( *) INTO NANY FROM ACTSTATISTIQUE WHERE ACTID = NACTEUR AND SACCODE = CSTAT_REC.SACCODE;
                            BEGIN
                                SELECT 1
                                  INTO NANY
                                  FROM DUAL
                                 WHERE F_PLGETCUSTOMCHARACTERISTIC (
                                           'ACTEUR',
                                           CSTAT_REC.SACCODE,
                                           NACTEUR,
                                           NULL)
                                           IS NOT NULL;
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    NANY := 0;
                            END;

                            IF NANY = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                       'LANSTATISTIQUEACTEUR|SACCODE|'
                                    || CSTAT_REC.SACCODE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    END LOOP;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCSTAT;

    -- Controle des personnes physiques
    FUNCTION OCPHYSIQUE (SACTEUR      IN     VARCHAR2,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            ACTPHYSIQUE   ACTEURPARTICULIER%ROWTYPE;
            LOK           NUMBER := 1;
            NANY          NUMBER;
            NLOGIQUE      NUMBER;
            SPAYCODE      ACTEUR.PAYCODE%TYPE;
            SCJUCODE      ACTEUR.CJUCODE%TYPE;
        BEGIN
            IF SCTL IN ('A0301',
                        'A0302',
                        'A0303',
                        'A0304',
                        'A0305',
                        'A0306',
                        'A0307',
                        'A0308',
                        'A0331',
                        'A0332',
                        'A0333',
                        'A0334',
                        'A0311',
                        'A0310',
                        'A0309',
                        'A0335')
            THEN
                BEGIN
                    SELECT PAYCODE
                      INTO SPAYCODE
                      FROM ACTEURPARTICULIER
                     WHERE ACTID = SACTEUR;

                    SELECT CJUCODE
                      INTO SCJUCODE
                      FROM ACTEUR
                     WHERE ACTID = SACTEUR;

                    SELECT *
                      INTO ACTPHYSIQUE
                      FROM ACTEURPARTICULIER
                     WHERE ACTID = SACTEUR;

                    IF SCTL = 'A0301'
                    THEN
                        IF ACTPHYSIQUE.APATITRE IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0302'
                    THEN
                        IF     ACTPHYSIQUE.APANOMMARITAL IS NULL
                           AND NVL (ACTPHYSIQUE.APASEXE, 0) = 2
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0303'
                    THEN
                        IF ACTPHYSIQUE.APADTNAISS IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0304'
                    THEN
                        IF ACTPHYSIQUE.PAYCODE IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0305'
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM CODEPOSTAL
                         WHERE PAYCODE = ACTPHYSIQUE.PAYCODE;

                        IF NANY != 0
                        THEN
                            IF    ACTPHYSIQUE.APADEPTNAISS IS NULL
                               OR ACTPHYSIQUE.APACOMNAISS IS NULL
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0306'
                    THEN
                        IF ACTPHYSIQUE.APANBENFANT IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0307'
                    THEN
                        IF ACTPHYSIQUE.APASITFAM = 'M'
                        THEN
                            IF ACTPHYSIQUE.APAREGIMEMATRIM IS NULL
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0308'
                    THEN
                        IF ACTPHYSIQUE.APAEMPLOYEUR IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0309'
                    THEN
                        IF ACTPHYSIQUE.APAPRENOM IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0310'
                    THEN
                        IF ACTPHYSIQUE.APANOMPATRONYMIQUE IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF     SCTL = 'A0311'
                          AND SCJUCODE IS NOT NULL
                          AND NVL (ACTPHYSIQUE.APAFLAGDIRIGEANT, 0) = 1
                    THEN
                        IF ACTPHYSIQUE.APADIRFONCTION IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF sCtl = 'A0335'
                    THEN                 ----MTR 20131121 Controle EXCIN MIGRE
                        DECLARE
                            l_cjucode   VARCHAR2 (25);
                            l_cin       VARCHAR2 (25);
                        BEGIN
                            SELECT cjucode
                              INTO l_cjucode
                              FROM acteur
                             WHERE actid = sActeur;

                            IF l_cjucode <> '1000'
                            THEN
                                lok := 1; -- personne morale, donc pas de controle
                            ELSE
                                SELECT MAX (cvastringvalue)
                                  INTO l_cin
                                  FROM cchvalue
                                 WHERE     cchsid = 'TFDCCHSID1676'
                                       AND SUBSTR (
                                               cvapkeyvalue,
                                               INSTR (cvapkeyvalue, '-') + 1,
                                               INSTR (cvapkeyvalue, '|') - 7) =
                                           sActeur;

                                IF (l_cin IS NULL)
                                THEN
                                    lok := 0;
                                ELSE
                                    lok := 1;
                                END IF;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                lok := 0;
                        END;
                    ELSIF SCTL = 'A0336'
                    THEN ----------RBS 01/11/2018:  CONTROLE pour ne pas changer la phase des acteurs terminer avant le 30/10/2018
                        DECLARE
                            l_count   NUMBER := 0;
                        BEGIN
                            SELECT NVL (COUNT (*), 0)
                              INTO l_count
                              FROM actphase
                             WHERE     phadest = 'ACTEUR'
                                   AND phacode = 'TER'
                                   AND aphdtdeb <
                                       TO_DATE ('30/10/2018', 'dd/mm/yyyy')
                                   AND aphdtfin IS NULL
                                   AND actid = sActeur;

                            IF l_count > 0
                            THEN
                                lOk := 0;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                lOk := 1;
                        END;
                    ELSIF sCtl = 'A0333'
                    THEN --------MTR 20131121 CIN Existe deja Migration (test)
                        DECLARE
                            l_count   NUMBER := 0;
                        BEGIN
                            SELECT COUNT (*)
                              INTO l_count
                              FROM cchvalue cch
                             WHERE     cch.cchsid = 'TFDCCHSID1676'
                                   AND cch.cvastringvalue IN
                                           (SELECT cvastringvalue
                                              FROM cchvalue
                                             WHERE     SUBSTR (
                                                           cvapkeyvalue,
                                                             INSTR (
                                                                 cvapkeyvalue,
                                                                 '-')
                                                           + 1,
                                                             INSTR (
                                                                 cvapkeyvalue,
                                                                 '|')
                                                           - 7) =
                                                       sActeur
                                                   AND cchsid =
                                                       'TFDCCHSID1676'
                                                   AND LTRIM (cvastringvalue)
                                                           IS NOT NULL)
                                   AND SUBSTR (cvapkeyvalue,
                                               INSTR (cvapkeyvalue, '-') + 1,
                                               INSTR (cvapkeyvalue, '|') - 7) IN
                                           (SELECT actid FROM acteur);

                            IF l_count > 1
                            THEN                                  ---TLBO-1118
                                lOk := 0;
                            ELSE
                                lOk := 1;
                            END IF;
                        END;
                    /* ELSIF sCtl = 'A0332' THEN ----MTR 20130111 Controle user controle cassiopae "CIN format incorrecte"
                     DECLARE
                        nCIN   varchar2(100);


                     BEGIN
                     select max(cch.cvastringvalue) into nCIN
                     from cchvalue cch
                     where cch.cchsid='TFDCCHSID1676'
                     and substr(cvapkeyvalue,instr(cvapkeyvalue,'-')+1,instr(cvapkeyvalue,'|')-7)=sActeur
                     and ltrim(cvastringvalue) is not null;

                     lOk := case when translate(UPPER(nCIN), '#0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', '#') is  null then 1 else 0 end;

                     END; */
                    ELSIF sCtl = 'A0332'
                    THEN ----RBS 20160818 TLBO-1103: Controle user controle cassiopae "CIN format incorrecte" mise ? jour
                        DECLARE
                            nCIN    VARCHAR2 (100);
                            sType   VARCHAR2 (100);
                        BEGIN
                            SELECT MAX (cch.cvastringvalue)
                              INTO sType
                              FROM cchvalue cch
                             WHERE     cch.cchsid = 'CMBCCHSID1574'
                                   AND SUBSTR (cvapkeyvalue,
                                               INSTR (cvapkeyvalue, '-') + 1,
                                               INSTR (cvapkeyvalue, '|') - 7) =
                                       sActeur;

                            IF sType = 'CIN'
                            THEN
                                SELECT NVL (MAX (cch.cvastringvalue), 0)
                                  INTO nCIN
                                  FROM cchvalue cch
                                 WHERE     cch.cchsid = 'TFDCCHSID1676'
                                       AND SUBSTR (
                                               cvapkeyvalue,
                                               INSTR (cvapkeyvalue, '-') + 1,
                                               INSTR (cvapkeyvalue, '|') - 7) =
                                           sActeur
                                       AND LTRIM (cvastringvalue) IS NOT NULL;

                                IF LENGTH (nCIN) = 8
                                THEN
                                    lOk :=
                                        CASE
                                            WHEN TRANSLATE (UPPER (nCIN),
                                                            '#0123456789',
                                                            '#')
                                                     IS NULL
                                            THEN
                                                1
                                            ELSE
                                                0
                                        END;
                                ELSE
                                    lOk := 0;
                                END IF;
                            ELSE
                                lOk := 1;
                            END IF;
                        END;
                    ELSIF SCTL = 'A0331'
                    THEN
                        PA_COMMON.S_TPALOGIQUE ('ACTEUR',
                                                'VILLEINSEE',
                                                NLOGIQUE);

                        IF    SPAYCODE = 'FR'
                           OR SPAYCODE = 'GP'
                           OR SPAYCODE = 'WF'
                           OR SPAYCODE = 'PF'
                           OR SPAYCODE = 'NC'
                           OR SPAYCODE = 'MQ'
                           OR SPAYCODE = 'PM'
                           OR SPAYCODE = 'RE'
                           OR SPAYCODE = 'YT'
                           OR SPAYCODE = 'GF'
                        THEN
                            IF NVL (NLOGIQUE, 0) = 1
                            THEN
                                SELECT COUNT (1)
                                  INTO NANY
                                  FROM LANTUSPARAM LTU, ACTEURPARTICULIER APA
                                 WHERE     APA.ACTID = SACTEUR
                                       AND APA.APAVILLENAISS = LTU.TUPLIBELLE
                                       AND LTU.TUPCODE LIKE
                                               APA.APADEPTNAISS || '%'
                                       AND LTU.TUSNOM = 'COMMUNEINSEE'
                                       AND LTU.LANCODE = 'FR';

                                IF NANY = 0
                                THEN
                                    LOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        IF SCTL != 'A0305'
                        THEN
                            LOK := 0;
                        ELSE
                            LOK := 1;
                        END IF;
                END;
            ELSE
                BEGIN
                    SELECT *
                      INTO ACTPHYSIQUE
                      FROM ACTEURPARTICULIER
                     WHERE ACTID = SACTEUR;

                    IF SCTL = 'A0370'
                    THEN
                        IF     ACTPHYSIQUE.PAYCODE IS NOT NULL
                           AND ACTPHYSIQUE.PAYCODE = 'FR'
                        THEN
                            IF     ACTPHYSIQUE.APADEPTNAISS IS NOT NULL
                               AND ACTPHYSIQUE.APACOMNAISS IS NOT NULL
                            THEN
                                SELECT COUNT (*)
                                  INTO NANY
                                  FROM CODEPOSTAL
                                 WHERE PAYCODE = ACTPHYSIQUE.PAYCODE;

                                IF NANY != 0
                                THEN
                                    SELECT COUNT (*)
                                      INTO NANY
                                      FROM CODEPOSTAL
                                     WHERE     PAYCODE = ACTPHYSIQUE.PAYCODE
                                           AND CPOCODE =
                                                  ACTPHYSIQUE.APADEPTNAISS
                                               || ACTPHYSIQUE.APACOMNAISS;

                                    IF NANY = 0
                                    THEN
                                        LOK := 0;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                        AMSG (NB_ELEMENT) :=
                                            F_STDTRIMALL (
                                                   ACTPHYSIQUE.APADEPTNAISS
                                                || ACTPHYSIQUE.APACOMNAISS);
                                        ATYPE (NB_ELEMENT) := 'C';
                                        ADEC (NB_ELEMENT) := NULL;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0371'
                    THEN
                        IF ACTPHYSIQUE.PAYCODE IS NOT NULL
                        THEN
                            IF F_PLPAYS (ACTPHYSIQUE.PAYCODE) = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    F_STDTRIMALL (ACTPHYSIQUE.PAYCODE);
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0372'
                    THEN
                        IF ACTPHYSIQUE.APATITRE IS NOT NULL
                        THEN
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   ACTPHYSIQUE.APATITRE,
                                   'TITRE',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0373'
                    THEN
                        IF ACTPHYSIQUE.APAREGIMEMATRIM IS NOT NULL
                        THEN
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   ACTPHYSIQUE.APAREGIMEMATRIM,
                                   'REMATRIM',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        LOK := 1;
                END;
            END IF;

            RETURN LOK;
        END;
    END OCPHYSIQUE;

    -- Controle des adresses sur subsitution
    FUNCTION OCSUBADRESSE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        CREVT_REC    IN     CREVT%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            ACTADRESSE_REC      ACTADRESSE%ROWTYPE;
            LOK                 NUMBER := 1;
            NAADORDREREMPLACE   ACTADRESSE.AADORDREREMPLACE%TYPE;
            DTTEMP              DATE;
            NOKTRAIT            NUMBER := 1;
            -- CV-31122008 CFS28926
            DTFACTURE           FACTURE.FACDTFACTURE%TYPE;
        BEGIN
            BEGIN
                BEGIN
                    SELECT *
                      INTO ACTADRESSE_REC
                      FROM ACTADRESSE
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND CREID = CREVT_REC.CREID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOKTRAIT := 0;
                END;

                IF NOKTRAIT = 1
                THEN
                    -- Controle que la date d'effet est < date deb adresse
                    IF SCTL = 'SUBAD1'
                    THEN
                        IF TRUNC (ACTADRESSE_REC.AADDTDEB) >
                           TRUNC (CREVT_REC.CREDTEFFET)
                        THEN
                            LOK := 0;
                        END IF;
                    -- Controle que l'adresse remplacante a au moins un type d'adresse
                    ELSIF SCTL = 'SUBAD2'
                    THEN
                        NAADORDREREMPLACE := ACTADRESSE_REC.AADORDREREMPLACE;

                        IF NAADORDREREMPLACE IS NULL
                        THEN
                            LOK := 0;
                        ELSE
                            BEGIN
                                SELECT *
                                  INTO ACTADRESSE_REC
                                  FROM ACTADRESSE
                                 WHERE     ACTID = ACTEUR_REC.ACTID
                                       AND AADORDRE = NAADORDREREMPLACE;

                                IF     NVL (ACTADRESSE_REC.AADFLAGSIEGE, 0) =
                                       0
                                   AND NVL (ACTADRESSE_REC.AADFLAGCOURRIER,
                                            0) =
                                       0
                                   AND NVL (
                                           ACTADRESSE_REC.AADFLAGFACTURATION,
                                           0) =
                                       0
                                   AND NVL (ACTADRESSE_REC.AADFLAGLIVRAISON,
                                            0) =
                                       0
                                THEN
                                    LOK := 0;
                                END IF;
                            EXCEPTION
                                WHEN NO_DATA_FOUND
                                THEN
                                    LOK := 0;
                            END;
                        END IF;
                    -- Controle sur la date d'effet de l'affectation de la nouvelle adresse comparativement
                    -- aux acteurs associes a la tranche, ou aux dossiers.
                    ELSIF SCTL = 'SUBAD3'
                    THEN
                        BEGIN
                            BEGIN
                                -- Verification de la date au niveau des acteurs associes a la tranche
                                SELECT MAX (IAADTDEB)
                                  INTO DTTEMP
                                  FROM ITRACTADRESSE
                                 WHERE     ACTID = ACTEUR_REC.ACTID
                                       AND CREID = CREVT_REC.CREID
                                       AND AADORDRE = ACTADRESSE_REC.AADORDRE;

                                DBMS_OUTPUT.PUT_LINE (
                                       'ActId itractadresse : '
                                    || TO_CHAR (ACTEUR_REC.ACTID));
                                DBMS_OUTPUT.PUT_LINE (
                                       'AadOrdre itractadresse : '
                                    || TO_CHAR (ACTADRESSE_REC.AADORDRE));
                                DBMS_OUTPUT.PUT_LINE (
                                       'Date retour itractadresse : '
                                    || TO_CHAR (DTTEMP, 'DD/MM/YYYY'));

                                IF TRUNC (DTTEMP) >
                                   TRUNC (CREVT_REC.CREDTEFFET)
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        TO_CHAR (DTTEMP, 'YYYYMMDD');
                                    ATYPE (NB_ELEMENT) := 'D';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    DBMS_OUTPUT.PUT_LINE (
                                        'Exception itractadresse');
                                    NULL;
                            END;

                            IF LOK = 1
                            THEN
                                BEGIN
                                    -- Verification de la date au niveau des acteurs associes au dossier
                                    SELECT MAX (DAADTDEB)
                                      INTO DTTEMP
                                      FROM DOSACTADRESSE
                                     WHERE     ACTID = ACTEUR_REC.ACTID
                                           AND CREID = CREVT_REC.CREID
                                           AND AADORDRE =
                                               ACTADRESSE_REC.AADORDRE;

                                    DBMS_OUTPUT.PUT_LINE (
                                           'ActId dosactadresse : '
                                        || TO_CHAR (ACTEUR_REC.ACTID));
                                    DBMS_OUTPUT.PUT_LINE (
                                           'AadOrdre dosactadresse : '
                                        || TO_CHAR (ACTADRESSE_REC.AADORDRE));
                                    DBMS_OUTPUT.PUT_LINE (
                                           'Date retour dosactadresse : '
                                        || TO_CHAR (DTTEMP, 'DD/MM/YYYY'));

                                    IF TRUNC (DTTEMP) >
                                       TRUNC (CREVT_REC.CREDTEFFET)
                                    THEN
                                        LOK := 0;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                        AMSG (NB_ELEMENT) :=
                                            TO_CHAR (DTTEMP, 'YYYYMMDD');
                                        ATYPE (NB_ELEMENT) := 'D';
                                        ADEC (NB_ELEMENT) := NULL;
                                    END IF;
                                EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                        DBMS_OUTPUT.PUT_LINE (
                                            'Exception dosactadresse');
                                        NULL;
                                END;
                            END IF;
                        END;
                    -- CV-31122008 CFS28926
                    ELSIF SCTL = 'SUBAD4'
                    THEN
                        BEGIN
                            -- Recherche la date la plus ancienne
                            SELECT MIN (FACDTFACTURE)
                              INTO DTFACTURE
                              FROM FACTURE FAC, FACECHEANCE FEC
                             WHERE     F_PLRESTANTFACTURE (FAC.FACID,
                                                           FEC.FECORDRE) >
                                       0
                                   AND FEC.ACTID = ACTEUR_REC.ACTID
                                   AND FAC.AADORDRE = ACTADRESSE_REC.AADORDRE
                                   AND FAC.FACID = FEC.FACID
                                   AND FAC.ACTIDCLIENT = FEC.ACTID;

                            -- Recherche la date d'exigibite existe ==> il y a impaye
                            SELECT MAX (
                                       F_PLGETEXIGIBILITE (FEC.FECDTEXIGIBLE,
                                                           FAC.FACDTEXIGIBLE))
                              INTO DTTEMP
                              FROM FACTURE FAC, FACECHEANCE FEC
                             WHERE     F_PLRESTANTFACTURE (FAC.FACID,
                                                           FEC.FECORDRE) >
                                       0
                                   AND FEC.ACTID = ACTEUR_REC.ACTID
                                   AND FAC.AADORDRE = ACTADRESSE_REC.AADORDRE
                                   AND FAC.FACID = FEC.FACID
                                   AND FAC.ACTIDCLIENT = FEC.ACTID;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                DTFACTURE := NULL;
                                DTTEMP := NULL;
                        END;

                        IF     DTFACTURE IS NOT NULL
                           AND DTTEMP IS NOT NULL
                           AND DTTEMP >= CREVT_REC.CREDTEFFET
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (DTFACTURE, 'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END IF;
                END IF;
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    LOK := 0;
            END;

            RETURN LOK;
        END;
    END OCSUBADRESSE;

    /*
    // Controle des rib sur subsitution
    */
    FUNCTION OCSUBRIB (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                       CREVT_REC    IN     CREVT%ROWTYPE,
                       SCTL         IN     VARCHAR2,
                       NB_ELEMENT   IN OUT BINARY_INTEGER,
                       AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                       AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                       ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                       ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            ACTRIB_REC       ACTRIB%ROWTYPE;
            LOK              NUMBER := 1;
            NRIBIDREMPLACE   ACTRIB.RIBIDREMPLACE%TYPE;
            DTTEMP           DATE;
            -- CV-31122008 CFS28926
            DTFACTURE        FACTURE.FACDTFACTURE%TYPE;
        BEGIN
            BEGIN
                SELECT *
                  INTO ACTRIB_REC
                  FROM ACTRIB
                 WHERE ACTID = ACTEUR_REC.ACTID AND CREID = CREVT_REC.CREID;

                -- Controle sur la date d'effet de l'affectation de la nouvelle adresse comparativement
                -- aux acteurs associes a la tranche, ou aux dossiers.
                IF SCTL = 'SUBRI3'
                THEN
                    BEGIN
                        BEGIN
                            -- Verification de la date au niveau des acteurs associes a la tranche
                            SELECT MAX (IAPDTDEB)
                              INTO DTTEMP
                              FROM ITRACTPAIEMENT
                             WHERE CREID = CREVT_REC.CREID;

                            IF TRUNC (DTTEMP) >= TRUNC (CREVT_REC.CREDTEFFET)
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DTTEMP, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                NULL;
                        END;

                        IF LOK = 1
                        THEN
                            BEGIN
                                -- Verification de la date au niveau des acteurs associes au dossier
                                SELECT MAX (DAPDTDEB)
                                  INTO DTTEMP
                                  FROM DOSACTPAIEMENT
                                 WHERE CREID = CREVT_REC.CREID;

                                IF TRUNC (DTTEMP) >=
                                   TRUNC (CREVT_REC.CREDTEFFET)
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        TO_CHAR (DTTEMP, 'YYYYMMDD');
                                    ATYPE (NB_ELEMENT) := 'D';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    NULL;
                            END;
                        END IF;
                    END;
                -- CV-31122008 CFS28926 MSG11580
                -- si sur l'acteur il y a une ou plusieurs factures impayees presentes sur le RIB a substituer
                ELSIF SCTL = 'SUBRI1'
                THEN
                    BEGIN
                        SELECT MIN (FACDTFACTURE)
                          INTO DTFACTURE
                          FROM FACTURE FAC, FACECHEANCE FEC
                         WHERE     F_PLRESTANTFACTURE (FAC.FACID,
                                                       FEC.FECORDRE) >
                                   0
                               AND FEC.ACTID = ACTEUR_REC.ACTID
                               AND FEC.RIBID = ACTRIB_REC.RIBID
                               AND FAC.FACID = FEC.FACID
                               AND FAC.ACTIDCLIENT = FEC.ACTID;

                        -- Recherche la date d'exigibite existe ==> Il y a impaye
                        SELECT MAX (
                                   F_PLGETEXIGIBILITE (FEC.FECDTEXIGIBLE,
                                                       FAC.FACDTEXIGIBLE))
                          INTO DTTEMP
                          FROM FACTURE FAC, FACECHEANCE FEC
                         WHERE     F_PLRESTANTFACTURE (FAC.FACID,
                                                       FEC.FECORDRE) >
                                   0
                               AND FEC.ACTID = ACTEUR_REC.ACTID
                               AND FEC.RIBID = ACTRIB_REC.RIBID
                               AND FAC.FACID = FEC.FACID
                               AND FAC.ACTIDCLIENT = FEC.ACTID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            DTFACTURE := NULL;
                            DTTEMP := NULL;
                    END;

                    IF     (DTFACTURE IS NOT NULL)
                       AND (DTTEMP IS NOT NULL)
                       AND DTTEMP >= CREVT_REC.CREDTEFFET
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := TO_CHAR (DTFACTURE, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            EXCEPTION
                WHEN NO_DATA_FOUND
                THEN
                    LOK := 0;
            END;

            RETURN LOK;
        END;
    END OCSUBRIB;

    /*
    // Controle des phases sur substitution
    */
    FUNCTION OCSUBPHASE (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                         CREVT_REC    IN     CREVT%ROWTYPE,
                         SCTL         IN     VARCHAR2,
                         NB_ELEMENT   IN OUT BINARY_INTEGER,
                         AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                         AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                         ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            ACTPHASE_REC   ACTPHASE%ROWTYPE;
            LOK            NUMBER := 1;
            NPHORDRE       NUMBER := 1;
        BEGIN
            BEGIN
                SELECT *
                  INTO ACTPHASE_REC
                  FROM ACTPHASE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND APHDTDEB = CREVT_REC.CREDTEFFET
                       AND CREID = CREVT_REC.CREID;

                NPHORDRE := ACTPHASE_REC.APHORDRE;

                SELECT *
                  INTO ACTPHASE_REC
                  FROM ACTPHASE
                 WHERE ACTID = ACTEUR_REC.ACTID AND APHORDRE = NPHORDRE - 1;

                -- Controle que la date d'effet est < date deb adresse
                IF SCTL = 'SUBPH1'
                THEN
                    IF TRUNC (ACTPHASE_REC.APHDTDEB) >
                       TRUNC (CREVT_REC.CREDTEFFET)
                    THEN
                        LOK := 0;
                    END IF;
                -- Controle que la date de fin > date debut
                ELSIF SCTL = 'SUBPH2'
                THEN
                    IF TRUNC (ACTPHASE_REC.APHDTDEB) >
                       TRUNC (ACTPHASE_REC.APHDTFIN)
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            EXCEPTION
                WHEN OTHERS
                THEN
                    LOK := 1;
            END;

            RETURN LOK;
        END;
    END OCSUBPHASE;

    FUNCTION ISGESTIBAN (SBGUBANQUE    IN BANQUEGUICHET.BGUBANQUE%TYPE,
                         SBGUGUICHET   IN BANQUEGUICHET.BGUGUICHET%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK             NUMBER := 0;
            NPAYNBCARIBAN   PAYS.PAYNBCARIBAN%TYPE;
        BEGIN
            BEGIN
                SELECT NVL (PAYNBCARIBAN, 0)
                  INTO NPAYNBCARIBAN
                  FROM BANQUEGUICHET BGU, ADRESSE ADR, PAYS PAY
                 WHERE     BGU.BGUBANQUE = SBGUBANQUE
                       AND BGU.BGUGUICHET = SBGUGUICHET
                       AND ADR.ADRID = BGU.ADRID
                       AND PAY.PAYCODE = ADR.PAYCODE;

                IF (NPAYNBCARIBAN >= 1) AND (NPAYNBCARIBAN <= 36)
                THEN
                    NOK := 1;
                END IF;
            EXCEPTION
                WHEN OTHERS
                THEN
                    NOK := 0;
            END;

            RETURN NOK;
        END;
    END ISGESTIBAN;

    /*
    // Controle des donnees liees aux RIB
    */
    FUNCTION ocRib(
      sActeur    IN     VARCHAR2,
      sUgeCode   IN     VARCHAR2,
      sCtl       IN     VARCHAR2,
      nb_element IN OUT BINARY_INTEGER,
      aOrdre     IN OUT pa_fungencontrole.TBL_NUMBER,
      aMsg       IN OUT pa_fungencontrole.TBL_VARCHAR2,
      aType      IN OUT pa_fungencontrole.TBL_VARCHAR2,
      aDec       IN OUT pa_fungencontrole.TBL_NUMBER ) RETURN NUMBER IS
	BEGIN
		DECLARE
			sBanque        VARCHAR2(50);
			sGuichet       VARCHAR2(50);
			sCpte          VARCHAR2(50);
         sRIBIBANCOmpte RIB.RIBIBANCOMPTE%TYPE; -- CV-27112002 FSA-10616
			sCle           VARCHAR2(50);
         nRibIBANCle    RIB.RIBIBANCLE%TYPE;    -- CV-27112002 FSA-10616
			sIntitule      RIB.RIBINTITULE%TYPE;
			nAny           NUMBER;
			lOk            NUMBER := 1;
			nAdId          NUMBER;
			sAgence        VARCHAR2(2000);
			sVille         ADRESSE.ADRVILLE%TYPE;
			sPays          ADRESSE.PAYCODE%TYPE;
         sVoie          ADRESSE.ADRVOIE%TYPE;
         sLieu          ADRESSE.ADRLIEUDIT%TYPE;
         sPostal        ADRESSE.ADRCODEPOST%TYPE;
         nDelaiPrel     NUMBER;
         nDelaiVir      NUMBER;
         nChRemHP       NUMBER;
         nChRemP        NUMBER;
         nRetraitPrl    NUMBER;
         nRetraitVir    NUMBER;
         nRetraitCh     NUMBER;
			-- sCompta info_compta.infcatcompta%TYPE; -- Supprime JPB160698
			nOrdre         NUMBER := 0;
			lCt            NUMBER;
         dtDate         DATE;             -- CV-20032002 FSA-8111

         nPosChar       PLS_INTEGER;
         nLenChaine     NUMBER;
         nPayFlagCleRib PAYS.PAYFLAGCLERIB%TYPE;
	 nCount NUMBER;

			CURSOR cRib IS SELECT ARI.RIBID,
                               ARIDTDEB,        -- Benoit 13/03/2002 FSA-8109
                               ARIDTREMPLACE,   -- CV-20032002 FSA-8111
                               RIBIBANCLE,      -- CV-27112002 FSA-10616
                               RIBIBANCOMPTE    -- CV-27112002 FSA-10616
                        FROM   ACTRIB ARI,
                               RIB
                        WHERE  ACTID     = sActeur
                        AND    RIB.RIBID = ARI.RIBID;
		BEGIN
			-- Existence d'un Rib
			IF sCtl = 'A0570' THEN
				IF F_placteurissociete( sActeur ) = 1 THEN
        			SELECT COUNT(*) INTO nAny FROM actrib WHERE actid = sActeur;
	        		IF nAny = 0 THEN
		      		lOk := 0;
			     	END IF;
            END IF;
			ELSE
				FOR cRib_rec IN cRib LOOP
					BEGIN
						SELECT  BGUBANQUE, BGUGUICHET, RIBCOMPTE, RIBCLE, RIBINTITULE, RIBIBANCLE, RIBIBANCOMPTE
                  INTO    sBanque, sGuichet, sCpte, sCle, sIntitule, nRibIBANCle, sRibIBANCompte
						FROM    RIB
                  WHERE   RIBID = cRib_rec.RIBID;
                  -- ok sur code banque
                  IF sCtl = 'A0571' THEN
                     IF sBanque IS NOT NULL THEN
        					   SELECT COUNT (*) INTO nAny FROM banqueguichet WHERE bgubanque = sBanque;
                        IF nAny = 0 THEN
        						   lOk          	    := 0;
	        						nb_element   	    := nb_element + 1;
		        					nOrdre		       := nb_element;
			        				aOrdre(nb_element) := nOrdre;
				        			aMsg(nb_element)   := F_stdtrimall(sBanque);
					        		aType(nb_element)  := 'C';
						        	aDec(nb_element)   := NULL;
							      nb_element   	    := nb_element + 1;
								   aOrdre(nb_element) := nOrdre;
        							aMsg(nb_element)   := F_stdtrimall(sGuichet);
	        						aType(nb_element)  := 'C';
		        					aDec(nb_element)   := NULL;
                        END IF;
                     END IF;
                  ELSIF sCtl = 'A0572' THEN
                     IF sBanque IS NOT NULL AND sGuichet IS NOT NULL THEN
        				   	SELECT COUNT (*) INTO nAny FROM banqueguichet WHERE bgubanque = sBanque AND bguguichet = sGuichet;
                        IF nAny = 0 THEN
        						   lOk          	    := 0;
	        						nb_element   	    := nb_element + 1;
		        					nOrdre		       := nb_element;
			        				aOrdre(nb_element) := nOrdre;
				        			aMsg(nb_element)   := F_stdtrimall(sBanque);
					        		aType(nb_element)  := 'C';
						        	aDec(nb_element)   := NULL;
							      nb_element   	    := nb_element + 1;
								   aOrdre(nb_element) := nOrdre;
        							aMsg(nb_element)   := F_stdtrimall(sGuichet);
	        						aType(nb_element)  := 'C';
		        					aDec(nb_element)   := NULL;
                        END IF;
                     END IF;
						-- Controle de l'existence d'une adresse et du pays de l'agence dans les codes pays
						ELSIF sCtl IN ( 'A0505', 'A0506', 'A0507', 'A0508', 'A0509', 'A0574', 'A0573' ) THEN
							lCt := 1;
							BEGIN
								-- Controle que l'agence bancaire a une adresse
								SELECT adrid, bguagence INTO nAdid, sAgence FROM banqueguichet WHERE bgubanque = sBanque AND
									bguguichet = sGuichet;
								IF nAdid IS NULL THEN
									lCt := 0;
								ELSE
									BEGIN
										SELECT ADRVILLE, PAYCODE, ADRVOIE, ADRLIEUDIT, ADRCODEPOST
                              INTO   sVille, sPays, sVoie, sLieu, sPostal
                              FROM   ADRESSE
                              WHERE  ADRID = nAdId;
							   		-- Controle que la ville est renseignee
                              IF sCtl = 'A0508' THEN
        							      IF sVille IS NULL THEN
	        								   lCt := 0;
                                 END IF;
										-- Controle que la voie est renseignee
                              ELSIF sCtl = 'A0505' THEN
        								   IF sVoie IS NULL THEN
	        								   lCt := 0;
                                 END IF;
                              -- Controle du code postal (not null)
                              ELSIF sCtl = 'A0507' THEN
                                 lCt := 1;
                                 SELECT COUNT(*) INTO nAny FROM CODEPOSTAL WHERE paycode = sPays;
                                 IF nAny != 0 THEN
                                    IF sPostal IS NULL THEN
                                       lCt := 0;
                                    END IF;
                                 END IF;
                              -- Controle du code postal (not null)
                              ELSIF sCtl = 'A0574' THEN
                                                                                        lCt := 1;
                                                                      			SELECT COUNT(*) INTO nAny FROM CODEPOSTAL WHERE paycode = sPays;
                                                                                        IF nAny != 0 THEN
                                                                                                IF sPostal IS NOT NULL THEN
                                                     		                                	SELECT COUNT(*) INTO nAny FROM CODEPOSTAL WHERE paycode = sPays AND cpocode = sPostal;
                                                                                                        IF nAny = 0 THEN
                                                                                                                lCt := 0;
                                                                                                        END IF;
                                                                                                END IF;
                                                                                        END IF;
										-- Controle que le lieu dit est renseigne
                              ELSIF sCtl = 'A0506' THEN
        										IF sLieu IS NULL THEN
	        									        lCt := 0;
                                                                                        END IF;
                                                                                -- Controle du pays (not null)
                              ELSIF sCtl = 'A0509' THEN
        										IF sPays IS NULL THEN
	        										lCt := 0;
                                                                                        END IF;
                                                                                -- Controle du pays (ok)
                              ELSIF sCtl = 'A0573' THEN
        										IF sPays IS NOT NULL THEN
        											lCt := F_plpays( sPays );
                                                                                        END IF;
										END IF;
									EXCEPTION
										WHEN NO_DATA_FOUND THEN
											lCt := 0;
									END;
								END IF;
							EXCEPTION
								WHEN NO_DATA_FOUND THEN
									lCt := 1;
							END;
							IF lCt = 0 THEN
								lOk          	   := 0;
								nb_element   	   := nb_element + 1;
								nOrdre		   := nb_element;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sBanque);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	   := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sGuichet);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
							END IF;
						-- Controle de l'existence de l'agence bancaire
						ELSIF sCtl IN ( 'A0501', 'A0502', 'A0503' ) THEN
                                                        nAny := 1;
                                                        IF sCtl = 'A0501' THEN
                                                                IF sBanque IS NULL THEN
                                                                        nAny := 0;
                                                                END IF;
                                                        ELSIF sCtl = 'A0502' THEN
                                                                IF sGuichet IS NULL THEN
                                                                        nAny := 0;
                                                                END IF;
                                                        ELSE
                                                                BEGIN
                							SELECT BGUAGENCE INTO sAgence FROM BANQUEGUICHET WHERE bgubanque = sBanque AND bguguichet = sGuichet;
                                                                        IF sAgence IS NULL THEN
                                                                                nAny := 0;
                                                                        END IF;
                                                                EXCEPTION
                                                                        WHEN NO_DATA_FOUND THEN
                                                                                nAny := 0;
                                                                END;
                                                        END IF;
							IF nAny = 0 THEN
								lOk          	   := 0;
								nb_element   	   := nb_element + 1;
								nOrdre		   := nb_element;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sBanque);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	   := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sGuichet);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
							END IF;
						-- Controle de l'existence des informations complementaires (delais, Chrono) du RIB
						ELSIF sCtl IN ('A0520', 'A0521', 'A0522' ) THEN
						   -- Uniquement si l'acteur est aussi une societe
							IF F_placteurissociete( sActeur ) = 1 THEN
								SELECT COUNT(*) INTO nAny FROM ribinfo WHERE ribid = cRib_rec.ribid;
                        IF nAny = 1 THEN
        						   SELECT  RINDELAIP,
                                   RINDELAIV,
                                   RINVALREMHP,
                                   RINVALREMSP,
                                   RINVALPRL,
                                   RINVALVIR,
                                   RINVALCHQ
                           INTO    nDelaiPrel,
                                   nDelaiVir,
                                   nChRemHP,
                                   nChRemP,
                                   nRetraitPrl,
                                   nRetraitVir,
                                   nRetraitCh
                           FROM    ribinfo
                           WHERE   ribid = cRib_rec.ribid;
                           IF sCtl = 'A0520' THEN
                              IF nDelaiPrel IS NULL OR nDelaiPrel <= 0 THEN
                                 nAny := 0;
                              END IF;
                           ELSIF sCtl = 'A0521' THEN
                              IF nRetraitPrl IS NULL OR nRetraitPrl < 0 OR nRetraitVir IS NULL OR nRetraitVir < 0 OR nRetraitCh IS NULL OR nRetraitCh < 0 THEN
                                 nAny := 0;
                              END IF;
                           ELSIF sCtl = 'A0522' THEN
                              IF nChRemHP IS NULL OR nChRemHP < 0 OR nChRemP IS NULL OR nChRemP < 0 THEN
                                 nAny := 0;
                              END IF;
                           END IF;
                        END IF;
                        IF nAny = 0 THEN
									lOk          	    := 0;
									nb_element   	    := nb_element + 1;
									nOrdre		       := nb_element;
									aOrdre(nb_element) := nOrdre;
									aMsg(nb_element)   := F_stdtrimall(sBanque);
									aType(nb_element)  := 'C';
									aDec(nb_element)   := NULL;
									nb_element   	    := nb_element + 1;
									aOrdre(nb_element) := nOrdre;
									aMsg(nb_element)   := F_stdtrimall(sGuichet);
									aType(nb_element)  := 'C';
									aDec(nb_element)   := NULL;
									nb_element   	    := nb_element + 1;
									aOrdre(nb_element) := nOrdre;
									aMsg(nb_element)   := F_stdtrimall(sCpte);
									aType(nb_element)  := 'C';
									aDec(nb_element)   := NULL;
									nb_element   	    := nb_element + 1;
									aOrdre(nb_element) := nOrdre;
									aMsg(nb_element)   := F_stdtrimall(sCle);
									aType(nb_element)  := 'C';
									aDec(nb_element)   := NULL;
								END IF;
							END IF;
						-- Existence d'un intitule ou d'un compte ou d'une cle
						ELSIF sCtl IN ( 'A0512', 'A0511', 'A0510' ) THEN
							IF ( sIntitule IS NULL AND sCtl = 'A0512' ) OR ( sCle IS NULL AND sCtl = 'A0511' ) OR ( sCpte IS NULL AND sCtl = 'A0510' ) THEN
								lOk          	    := 0;
								nb_element   	    := nb_element + 1;
								nOrdre		       := nb_element;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sBanque);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	   := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sGuichet);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	   := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sCpte);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	   := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sCle);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
							END IF;
                  -- CV-27112002 FSA-10616  ( MSG6740 - MSG6741 )
                  --ELSIF sCtl IN ( 'A0513', 'A0514', 'A0533' ) THEN
                     -- ce controle valable unique dans les entite ou il y a la presence de societe
                     --SELECT COUNT(*)
                     --INTO   nAny
                     --FROM   AGEOPTION
                     --WHERE  ACTID    = sActeur
                     --AND    TOSCODE  = 'FORMATRIB'
                     --AND    AOPTEXTE = 'IBAN';
                     -- Obligatoire pour la societe si option est active
                  ELSIF sCtl = 'A0513' AND IsGestIBAN( sBanque, sGuichet ) = 1
                                       AND cRib_rec.RIBIBANCLE IS NULL THEN
   							lOk          	    := 0;
	   						nb_element   	    := nb_element + 1;
		   					nOrdre		       := nb_element;
			   				aOrdre(nb_element) := nOrdre;
				   			aMsg(nb_element)   := F_stdtrimall(sBanque);
					   		aType(nb_element)  := 'C';
						   	aDec(nb_element)   := NULL;
							   nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
   							aMsg(nb_element)   := F_stdtrimall(sGuichet);
	   						aType(nb_element)  := 'C';
		   					aDec(nb_element)   := NULL;
			   				nb_element   	    := nb_element + 1;
				   			aOrdre(nb_element) := nOrdre;
					   		aMsg(nb_element)   := F_stdtrimall(sCpte);
						   	aType(nb_element)  := 'C';
							   aDec(nb_element)   := NULL;
                  ELSIF sCtl = 'A0514' AND IsGestIBAN( sBanque, sGuichet ) = 1
                                       AND cRib_rec.RIBIBANCOMPTE IS NULL THEN
   							lOk          	    := 0;
	   						nb_element   	    := nb_element + 1;
		   					nOrdre		       := nb_element;
			   				aOrdre(nb_element) := nOrdre;
				   			aMsg(nb_element)   := F_stdtrimall(sBanque);
					   		aType(nb_element)  := 'C';
						   	aDec(nb_element)   := NULL;
							   nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
   							aMsg(nb_element)   := F_stdtrimall(sGuichet);
	   						aType(nb_element)  := 'C';
		   					aDec(nb_element)   := NULL;
			   				nb_element   	    := nb_element + 1;
				   			aOrdre(nb_element) := nOrdre;
					   		aMsg(nb_element)   := F_stdtrimall(sCpte);
						   	aType(nb_element)  := 'C';
							   aDec(nb_element)   := NULL;
                     -- on teste si le compte IBAN saisi est correct pour cela on a besoin le code
                     -- pays de la banque via A0507
                  ELSIF sCtl = 'A0533' AND cRib_rec.RIBIBANCOMPTE IS NOT NULL AND cRib_rec.RIBIBANCLE IS NOT NULL THEN
                     IF sGuichet IS NOT NULL AND sBanque IS NOT NULL THEN
                        DBMS_OUTPUT.PUT_LINE( 'RIB IBAN : ' || cRib_rec.RIBIBANCOMPTE ) ;
                        sPays := NULL;
	   					   BEGIN
                           -- CV-04072007 CFS22519 Report CFS22507 il faut prendre pays de la banque et non pays de l adr de la banque
	   						   SELECT ADRID, PAYCODE
                           INTO   nAdid, sPays
                           FROM   BANQUEGUICHET
                           WHERE  BGUBANQUE  = sBanque
                           AND    BGUGUICHET = sGuichet;

                           SELECT COUNT(1)
                           INTO   nCount
                           FROM   topparam
                           WHERE  TOPTABLE = 'FORMATRIB'
                           AND    TPAPARAM = 'IBAN_' || sPays
                           AND    NVL(TPALOGIQUE,0) = 1;

			   IF nCount = 1 AND nAdid IS NOT NULL THEN
			   	BEGIN
				-- SELECT paycode INTO sPays FROM adresse WHERE adrid = nAdid;
   			   	IF sPays IS NOT NULL THEN
   				   lCt := F_plpays( sPays );
	   			   IF lCt = 1 THEN
                                       IF pa_fungencontrole.ocRibIBANVerify( sRibIBANCompte, nRibIBANCle, sPays ) != 1 THEN
														lOk          	    := 0;
							      					nb_element   	    := nb_element + 1;
									   				nOrdre		       := nb_element;
										   			aOrdre(nb_element) := nOrdre;
											   		aMsg(nb_element)   := F_stdtrimall(sBanque);
												   	aType(nb_element)  := 'C';
													   aDec(nb_element)   := NULL;
   													nb_element   	    := nb_element + 1;
	   												aOrdre(nb_element) := nOrdre;
		   											aMsg(nb_element)   := F_stdtrimall(sGuichet);
			   										aType(nb_element)  := 'C';
				   									aDec(nb_element)   := NULL;
					   								nb_element   	    := nb_element + 1;
						   							aOrdre(nb_element) := nOrdre;
														aMsg(nb_element)   := sPays || TO_CHAR(nRibIBANCle);
							      					aType(nb_element)  := 'C';
									   				aDec(nb_element)   := NULL;
										   			nb_element   	    := nb_element + 1;
											   		aOrdre(nb_element) := nOrdre;
												   	aMsg(nb_element)   := F_stdtrimall(sRibIBANCompte);
													   aType(nb_element)  := 'C';
   													aDec(nb_element)   := NULL;
	   											END IF;
		   									END IF;
			   							END IF;
				   					EXCEPTION
					   					WHEN NO_DATA_FOUND THEN
						   					nAny := 1;
                              END;
                           END IF;
   				   	   END;
                     ELSE
                        DBMS_OUTPUT.PUT_LINE( 'Format incorrect IBAN : ' || cRib_rec.RIBIBANCOMPTE ) ;
								lOk          	    := 0;
							   nb_element   	    := nb_element + 1;
								nOrdre		       := nb_element;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sBanque);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
   							nb_element   	    := nb_element + 1;
	   						aOrdre(nb_element) := nOrdre;
		   					aMsg(nb_element)   := F_stdtrimall(sGuichet);
			   				aType(nb_element)  := 'C';
				   			aDec(nb_element)   := NULL;
					   		nb_element   	    := nb_element + 1;
						   	aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := nRibIBANCle;
							   aType(nb_element)  := 'N';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sRibIBANCompte);
								aType(nb_element)  := 'C';
   							aDec(nb_element)   := NULL;
                     END IF;
						-- Controle du RIB
						ELSIF sCtl = 'A0530' THEN
                     IF cRib_rec.ARIDTREMPLACE  IS NULL THEN
							   sPays := NULL;
							   BEGIN
                           -- CV-04072007 CFS22519 Report CFS22507 il faut prendre pays de la banque et non pays de l adr de la banque
							      SELECT ADRID, PAYCODE
                           INTO   nAdid, sPays
                           FROM   BANQUEGUICHET
                           WHERE  BGUBANQUE  = sBanque
                           AND    BGUGUICHET = sGuichet;
								   IF nAdid IS NOT NULL THEN
									   BEGIN
										   -- SELECT paycode INTO sPays FROM adresse WHERE adrid = nAdid;
										   -- On ne controle le RIB que si la bgubanque a une adresse dans un pays	repertorie
										   IF sPays IS NOT NULL THEN
											   lCt := F_plpays( sPays );
											   IF lCt = 1 THEN
											      SELECT PAYFLAGCLERIB INTO nPayFlagCleRib FROM PAYS WHERE PAYCODE = sPays;
                                       -- CV-04072007 CFS22519 Report CFS22507 PARFIP Suisse va saisir l'INTEGRALITE de son IBAN - soit 21 caracteres alpha - dans le num.compte standard (RIB.RIBCOMPTE)
												   -- IF pa_fungencontrole.ocRibVerify( sBanque, sGuichet, sCpte, sCle, sPays ) != 0 THEN
                                       IF ( sPays  = 'CH' AND pa_fungencontrole.ocRibIBANVerify( SUBSTR(sCpte, 5, 17), TO_NUMBER(SUBSTR(sCpte, 3, 2)), SUBSTR(sCpte, 1, 2 )) != 1 )
										         OR ( sPays != 'CH' AND NVL(nPayFlagCleRib,0) = 1 AND pa_fungencontrole.ocRibVerify( sBanque, sGuichet, sCpte, sCle, sPays ) != 0 ) THEN
													   lOk          	   := 0;
													   nb_element   	   := nb_element + 1;
													   nOrdre		   := nb_element;
													   aOrdre(nb_element) := nOrdre;
													   aMsg(nb_element)   := F_stdtrimall(sBanque);
													   aType(nb_element)  := 'C';
													   aDec(nb_element)   := NULL;
													   nb_element   	   := nb_element + 1;
													   aOrdre(nb_element) := nOrdre;
													   aMsg(nb_element)   := F_stdtrimall(sGuichet);
													   aType(nb_element)  := 'C';
													   aDec(nb_element)   := NULL;
													   nb_element   	   := nb_element + 1;
													   aOrdre(nb_element) := nOrdre;
													   aMsg(nb_element)   := F_stdtrimall(sCpte);
													   aType(nb_element)  := 'C';
													   aDec(nb_element)   := NULL;
													   nb_element   	   := nb_element + 1;
													   aOrdre(nb_element) := nOrdre;
													   aMsg(nb_element)   := F_stdtrimall(sCle);
													   aType(nb_element)  := 'C';
													   aDec(nb_element)   := NULL;
												   END IF;
											   END IF;
										   END IF;
									   EXCEPTION
										   WHEN NO_DATA_FOUND THEN
											   nAny := 1;
									   END;
								   END IF;
							   EXCEPTION
								   WHEN NO_DATA_FOUND THEN
									   nAny := 1;
								   END;
                     END if;
						-- Unicite de l'intitule du RIB
						ELSIF sCtl = 'A0531' AND ( sIntitule IS NOT NULL ) THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   ACTRIB ARI, RIB RIB
                     WHERE  ARI.ACTID       = sActeur
                     AND    RIB.RIBID       = ARI.RIBID
                     AND    RIB.RIBINTITULE = sIntitule;
							IF nAny > 1 THEN
							   lOk          	    := 0;
								nb_element   	    := nb_element + 1;
								nOrdre		       := nb_element;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_StdTrimAll( sIntitule );
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_StdTrimAll( sBanque );
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_StdTrimAll( sGuichet );
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_StdTrimAll( sCpte );
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sCle);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := nAny;
								aType(nb_element)  := 'N';
								aDec(nb_element)   := NULL;
						   END IF;
                  -- Benoit + CV le 13032002 FSA-8109 (5898)
                  ELSIF sCtl = 'A0523' AND  cRib_rec.ARIDTDEB IS NULL THEN
                     lOk                := 0;
                     nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := F_StdTrimAll( sCpte );
                     aType(nb_element)  := 'C';
                     aDec(nb_element)   := NULL;
                  -- CV-20032002 FSA-8111 ( 5900 )
                  ELSIF sCtl = 'A0524' AND  cRib_rec.ARIDTREMPLACE IS NULL THEN
                     BEGIN
                        SELECT APHDTDEB
                        INTO   dtDate
                        FROM   ACTPHASE
                        WHERE  ACTID   = sActeur
                        AND    PHACODE = 'ACTIVE'
                        AND    APHDTFIN IS NULL;
                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           dtDate := NULL;
                     END;
                     IF dtDate IS NOT NULL AND cRib_rec.ARIDTDEB < dtDate THEN
								lOk          	    := 0;
								nb_element   	    := nb_element + 1;
								nOrdre             := nb_element;
								aOrdre(nb_element) := nOrdre;
   				   		aMsg(nb_element)   := TO_CHAR(cRib_rec.ARIDTDEB,'YYYYMMDD');
								aType(nb_element)  := 'D';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
								aMsg(nb_element)   := F_stdtrimall(sCpte);
								aType(nb_element)  := 'C';
								aDec(nb_element)   := NULL;
								nb_element   	    := nb_element + 1;
								aOrdre(nb_element) := nOrdre;
   				   		aMsg(nb_element)   := TO_CHAR(dtDate,'YYYYMMDD');
								aType(nb_element)  := 'D';
								aDec(nb_element)   := NULL;
                     END IF;
                  ELSIF sCtl = 'A0515' THEN
                  	IF sGuichet IS NOT NULL AND sBanque IS NOT NULL THEN

                  		SELECT COUNT (*) INTO nAny
                  		FROM banqueguichet
                  		WHERE bgubanque = sBanque
                  		AND bguguichet = sGuichet
                  		AND BGUCODEINTERBANCAIRE IS NULL;

                  		IF nAny > 0 THEN
                  			lOk          	    := 0;
	        						nb_element   	    := nb_element + 1;
		        					nOrdre		       := nb_element;
			        				aOrdre(nb_element) := nOrdre;
				        			aMsg(nb_element)   := F_stdtrimall(sBanque);
					        		aType(nb_element)  := 'C';
						        	aDec(nb_element)   := NULL;
							      nb_element   	    := nb_element + 1;
								   aOrdre(nb_element) := nOrdre;
        							aMsg(nb_element)   := F_stdtrimall(sGuichet);
	        						aType(nb_element)  := 'C';
		        					aDec(nb_element)   := NULL;

                  		END IF;
                  	END IF;
                  ELSIF sCtl = 'A0532' THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   ACTRIB ARI,
                            ACTEUR ACT
                     WHERE  ARI.ACTID  != sActeur
                       AND  ARI.RIBID   = cRib_rec.RIBID
                       AND  ACT.ACTID   = ARI.ACTID
                       AND  ACT.UGECODE = sUgeCode;
                     IF nAny > 0 THEN
                        lOk                := 0;
                        nb_element         := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_StdTrimAll( sBanque );
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                        nb_element         := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_StdTrimAll( sGuichet );
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                        nb_element         := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_StdTrimAll( sCpte );
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                        nb_element         := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_stdtrimall(sCle);
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                        nb_element         := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_StdTrimAll( sIntitule );
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                      END IF;
                  -- CV-09102007 CFS20250 ajout les parentheses MSG9970
                  ELSIF ( sCtl = 'A0534' ) AND ( sIntitule IS NOT NULL ) THEN
         	         dbms_output.put_line ( 'sIntitule  = ' || sIntitule );
                     SELECT PAYCODE
                     INTO   sPays
                     FROM   BANQUEGUICHET
                     WHERE  BGUBANQUE  = sBanque
                       AND  BGUGUICHET = sGuichet;
         	         dbms_output.put_line ( 'sPays      = ' || sPays );
                     IF ( sPays IN ( 'FR', 'NC', 'PF' ) ) THEN
                        nLenChaine := LENGTH( sIntitule );
         	            dbms_output.put_line ( 'nLenChaine = ' || TO_CHAR(nLenChaine) );
                        FOR nPosChar IN 1..nLenChaine LOOP
                           IF ( INSTR( '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ *-./()', SUBSTR( sIntitule, nPosChar, 1 ) ) = 0 ) THEN
                              lOk                := 0;
                              nb_element         := nb_element + 1;
                              nOrdre             := nb_element;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := F_StdTrimAll( sBanque );
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := F_StdTrimAll( sGuichet );
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := F_StdTrimAll( sCpte );
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := F_StdTrimAll( sCle );
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := F_StdTrimAll( sIntitule );
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                              EXIT;
                           END IF;
                        END LOOP;
                     END IF;
						END IF;
					EXCEPTION
						WHEN NO_DATA_FOUND THEN
							nAny := 1;
					END;
				END LOOP;
			END IF;
			RETURN lOk;
		END;
	END ocRib;

    /*
    // Controle des phases de l'acteur
    */
    FUNCTION OCPHASE (SACTEUR      IN     VARCHAR2,
                      SCTL         IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CPHASE
            IS
                SELECT PHACODE,
                       APHDTDEB,
                       UTICODE,
                       APHDTFIN,
                       APHDTMAJ
                  FROM ACTPHASE
                 WHERE ACTID = SACTEUR;

            LOK       NUMBER := 1;
            LFLAGOK   NUMBER := 1;
            NANY      NUMBER;
            NORDRE    NUMBER := 0;
        BEGIN
            IF SCTL = 'A1071'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTPHASE
                 WHERE ACTID = SACTEUR AND APHDTFIN IS NULL;

                IF NANY != 1
                THEN
                    LOK := 0;
                END IF;
            ELSE
                FOR CPHASE_REC IN CPHASE
                LOOP
                    BEGIN
                        IF SCTL IN ('A1001',
                                    'A1002',
                                    'A1003',
                                    'A1004')
                        THEN
                            LFLAGOK := 1;

                            IF SCTL = 'A1001'
                            THEN
                                IF CPHASE_REC.APHDTDEB IS NULL
                                THEN
                                    LFLAGOK := 0;
                                END IF;
                            ELSIF SCTL = 'A1002'
                            THEN
                                IF CPHASE_REC.PHACODE IS NULL
                                THEN
                                    LFLAGOK := 0;
                                END IF;
                            ELSIF SCTL = 'A1003'
                            THEN
                                IF CPHASE_REC.APHDTMAJ IS NULL
                                THEN
                                    LFLAGOK := 0;
                                END IF;
                            ELSIF SCTL = 'A1004'
                            THEN
                                IF CPHASE_REC.UTICODE IS NULL
                                THEN
                                    LFLAGOK := 0;
                                END IF;
                            END IF;

                            IF LFLAGOK = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    F_STDTRIMALL (CPHASE_REC.PHACODE);
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        ELSIF SCTL = 'A1030'
                        THEN
                            IF     CPHASE_REC.APHDTDEB IS NOT NULL
                               AND CPHASE_REC.APHDTFIN IS NOT NULL
                            THEN
                                IF CPHASE_REC.APHDTDEB > CPHASE_REC.APHDTFIN
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    NORDRE := NB_ELEMENT;
                                    AORDRE (NB_ELEMENT) := NORDRE;
                                    AMSG (NB_ELEMENT) :=
                                        TO_CHAR (CPHASE_REC.APHDTDEB,
                                                 'YYYYMMDD');
                                    ATYPE (NB_ELEMENT) := 'D';
                                    ADEC (NB_ELEMENT) := NULL;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NORDRE;
                                    AMSG (NB_ELEMENT) :=
                                        TO_CHAR (CPHASE_REC.APHDTFIN,
                                                 'YYYYMMDD');
                                    ATYPE (NB_ELEMENT) := 'D';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            END IF;
                        ELSIF SCTL = 'A1070'
                        THEN
                            IF CPHASE_REC.PHACODE IS NOT NULL
                            THEN
                                SELECT COUNT (*)
                                  INTO NANY
                                  FROM PHASE
                                 WHERE PHACODE = CPHASE_REC.PHACODE;

                                IF NANY = 0
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        F_STDTRIMALL (CPHASE_REC.PHACODE);
                                    ATYPE (NB_ELEMENT) := 'C';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            END IF;
                        END IF;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            LOK := 1;
                    END;
                END LOOP;
            END IF;

            RETURN LOK;
        END;
    END OCPHASE;

    /*
    // Controle des roles de l'acteur
    */
    FUNCTION OCROLE (SACTEUR      IN     VARCHAR2,
                     SCTL         IN     VARCHAR2,
                     NB_ELEMENT   IN OUT BINARY_INTEGER,
                     AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                     AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                     ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK                NUMBER := 1;
            LFLAGOK            NUMBER := 1;
            SROLCODE           ACTROLE.ROLCODE%TYPE;            -- CV-20062001
            DTDATE             ACTPHASE.APHDTDEB%TYPE; -- CV-21032002 FSA-8111
            NANY               NUMBER;
            NORDRE             NUMBER := 0;
            BCOTATION          BOOLEAN := FALSE;
            NSORTANT           NUMBER;
            NTMPFLAGRIB        TMOYENPMT.TMPFLAGRIB%TYPE;
            NTCOID             TCOTATION.TCOID%TYPE;  -- CV-09042004 FSA-14740
            STVACODE           TCOVALEUR.TVACODE%TYPE; -- CV-09042004 FSA-14740
            NFIRST             NUMBER;                -- CV-09042004 FSA-14740
            NTAUX1             NUMBER;                -- CV-09042004 FSA-14740
            NTAUX2             NUMBER;                -- CV-09042004 FSA-14740
            ERR_CONTROLE       EXCEPTION;
            ERR_AAG_PARM1      EXCEPTION;
            ERR_AAG_PARM2      EXCEPTION;
            NCOUNT             NUMBER;

            CURSOR CROLE
            IS
                  SELECT ARO.ROLCODE,
                         AROGRPFAC,
                         ARONBEXFAC,
                         AROREFEXTERNE,
                         AROPERIODE,                            -- CV-20062001
                         AROMULTIPLE,                           -- CV-20062001
                         -- Modif Base 21/09/01
                         -- AROQUANTIEME,           -- CV-20062001
                         -- ARODELAIA,
                         -- ARODELAIE,
                         ROLFLAGSORTANT,                        -- CV-20062001
                         ROLFLAGENTRANT                         -- CV-20062001
                    FROM ACTROLE ARO, ROLE ROL
                   WHERE ACTID = SACTEUR AND ROL.ROLCODE = ARO.ROLCODE -- CV-20062001
                ORDER BY ARO.ROLCODE;

            --CURSOR cAroAgeAtt IS SELECT ROLCODE, ACTIDGESTION, AAADTDEB, AAAATTRIBUT, AAAFLAGIMPAYE
            --                     FROM AROAGEATTRIBUT
            --                     WHERE ACTID = sActeur;
            CURSOR C_AROAGE
            IS
                  SELECT ROLCODE,
                         ACTCODE,                      -- CV-21032002 FSA-8111
                         ACTLIBCOURT,                  -- CV-21032002 FSA-8111
                         TCIID,
                         RUBID,
                         TAXCODE,
                         TMPCODE,
                         TMPCODEENC,                            -- CV-20062001
                         AAGDTVALID,                   -- CV-21032002 FSA-8111
                         AAGDTFIN,                     -- CV-21032002 FSA-8111
                         AAGDELAIPMT,
                         AAGJOUR,
                         ACTIDGESTION COMPANY_ID
                    FROM AROAGE ARO, ACTEUR ACT
                   WHERE     ARO.ACTID = SACTEUR
                         AND ROLCODE = SROLCODE                 -- CV-20062001
                         AND ACT.ACTID = ACTIDGESTION
                ORDER BY ROLCODE;

            NSOCIETE           ACTEURGESTION.ACTID%TYPE;

            CURSOR COBJ
            IS
                SELECT AVENBOBJECTIF,
                       AVENBREALISE,
                       AVEMTOBJECTIF,
                       AVEMTREALISE,
                       DEVCODE,
                       B.RVALIBELLE LIMITLABEL
                  FROM AAGRVE A, LANROLVARIABLE B
                 WHERE     A.ACTID = SACTEUR
                       AND A.ROLCODE = SROLCODE
                       AND A.ACTIDGESTION = NSOCIETE
                       AND (   A.AVENBOBJECTIF IS NOT NULL
                            OR A.AVEMTOBJECTIF IS NOT NULL)
                       AND B.ROLCODE = A.ROLCODE
                       AND B.RVACODE = A.RVACODE
                       AND B.LANCODE = 'EN';

            --CV-09042004 FSA-14740
            CURSOR CTCO
            IS
                SELECT DISTINCT TCOID
                  FROM ACTTCOVALEUR
                 WHERE ACTID = SACTEUR;

            CURSOR CATV
            IS
                  SELECT *
                    FROM ACTTCOVALEUR
                   WHERE ACTID = SACTEUR AND TCOID = NTCOID
                ORDER BY ATVORDRE DESC;

            SROLECODEEXTERNE   TOPPARAM.TPATEXTE%TYPE;
            STEXTE             TOPPARAM.TPATEXTE%TYPE;
            SUGECODE           ACTEUR.UGECODE%TYPE;
        BEGIN
            PA_COMMON.S_TPATEXTE ('ACTEUR', 'A0901', SROLECODEEXTERNE);

            BEGIN
                SELECT UGECODE
                  INTO SUGECODE
                  FROM ACTEUR
                 WHERE ACTID = SACTEUR;
            EXCEPTION
                WHEN OTHERS
                THEN
                    SUGECODE := NULL;
            END;

            IF SCTL = 'A0970'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTROLE
                 WHERE ACTID = SACTEUR;

                IF NANY = 0
                THEN
                    LOK := 0;
                END IF;
            ELSE
                FOR CROLE_REC IN CROLE
                LOOP
                    BEGIN
                        IF SCTL = 'A0901'
                        THEN
                            -- 1134 : Reference externe obligatoire
                            IF SROLECODEEXTERNE IS NULL
                            THEN
                                IF CROLE_REC.AROREFEXTERNE IS NULL
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                                    ATYPE (NB_ELEMENT) := 'S';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            ELSE
                                STEXTE := F_PLROLEEXTERNE (CROLE_REC.ROLCODE);

                                IF     F_ISOPTIONSITETEXTE ('ACTEUR',
                                                            'A0901',
                                                            SUGECODE,
                                                            STEXTE) = 1
                                   AND CROLE_REC.AROREFEXTERNE IS NULL
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                                    ATYPE (NB_ELEMENT) := 'S';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            END IF;
                        ELSIF SCTL = 'A0902'
                        THEN
                            IF     F_PLROLEEXTERNE (CROLE_REC.ROLCODE) =
                                   'CLIENT'
                               AND CROLE_REC.ARONBEXFAC IS NULL
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        -- CV-20062001 5039 Periodicite non renseigne
                        ELSIF SCTL = 'A0903' AND CROLE_REC.AROPERIODE IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                            ATYPE (NB_ELEMENT) := 'S';
                        -- CV-20062001 5040 Multiple de periode non renseigne
                        ELSIF     SCTL = 'A0904'
                              AND CROLE_REC.AROPERIODE IS NOT NULL
                              AND CROLE_REC.AROMULTIPLE IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                            ATYPE (NB_ELEMENT) := 'S';
                            ADEC (NB_ELEMENT) := NULL;
                        -- FSA 17131
                        ELSIF SCTL = 'A0907'
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ROLE A, ROLVARIABLE B
                             WHERE     A.ROLCODE = CROLE_REC.ROLCODE
                                   AND B.ROLCODE = A.ROLCODE
                                   AND (   RVATYPE = 'OBJ'
                                        OR RVATYPE = 'ATTRIB'
                                        OR A.ROLFLAGENTRANT = 1
                                        OR A.ROLFLAGSORTANT = 1
                                        OR ROLCODEEXTERNE = 'CLIENT'
                                        OR ROLCODEEXTERNE = 'GARANT'
                                        OR ROLCODEEXTERNE = 'FOURN');

                            IF NCOUNT != 0
                            THEN
                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM AROAGE
                                 WHERE     ACTID = SACTEUR
                                       AND ROLCODE = CROLE_REC.ROLCODE;

                                IF NCOUNT = 0
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                                    ATYPE (NB_ELEMENT) := 'S';
                                END IF;
                            END IF;
                        ELSIF SCTL IN ('A0913',
                                       'A0914',
                                       'A0932',
                                       'A0933',
                                       'A0934',
                                       'A0935',
                                       'A0976',
                                       'A0977')
                        THEN
                            SROLCODE := CROLE_REC.ROLCODE;

                            -- CV-20062001 5044 mode de reglement en encaissement non renseigne
                            FOR C_AROAGE_REC IN C_AROAGE
                            LOOP
                                BEGIN
                                    IF     SCTL = 'A0913'
                                       AND CROLE_REC.ROLFLAGENTRANT = 1
                                       AND C_AROAGE_REC.TMPCODEENC IS NULL
                                    THEN
                                        RAISE ERR_AAG_PARM1;
                                    ELSIF SCTL = 'A0977'
                                    THEN
                                        FOR COBJ_REC IN COBJ
                                        LOOP
                                            IF     COBJ_REC.AVENBOBJECTIF
                                                       IS NOT NULL
                                               AND COBJ_REC.AVENBREALISE
                                                       IS NOT NULL
                                            THEN
                                                IF COBJ_REC.AVENBREALISE >
                                                   COBJ_REC.AVENBOBJECTIF
                                                THEN
                                                    LOK := 0;
                                                    NB_ELEMENT :=
                                                        NB_ELEMENT + 1;
                                                    NORDRE := NB_ELEMENT;
                                                    AORDRE (NB_ELEMENT) :=
                                                        NB_ELEMENT;
                                                    AMSG (NB_ELEMENT) :=
                                                           F_STDTRIMALL (
                                                               C_AROAGE_REC.ACTCODE)
                                                        || ' - '
                                                        || F_STDTRIMALL (
                                                               C_AROAGE_REC.ACTLIBCOURT);
                                                    ATYPE (NB_ELEMENT) := 'C';
                                                    ADEC (NB_ELEMENT) := NULL;
                                                    NB_ELEMENT :=
                                                        NB_ELEMENT + 1;
                                                    AORDRE (NB_ELEMENT) :=
                                                        NORDRE;
                                                    AMSG (NB_ELEMENT) :=
                                                           'LANROLE|ROLES|'
                                                        || CROLE_REC.ROLCODE;
                                                    ATYPE (NB_ELEMENT) := 'S';
                                                    ADEC (NB_ELEMENT) := NULL;
                                                    NB_ELEMENT :=
                                                        NB_ELEMENT + 1;
                                                    AORDRE (NB_ELEMENT) :=
                                                        NORDRE;
                                                    AMSG (NB_ELEMENT) :=
                                                        F_STDTRIMALL (
                                                            COBJ_REC.LIMITLABEL);
                                                    ATYPE (NB_ELEMENT) := 'C';
                                                    ADEC (NB_ELEMENT) := NULL;
                                                    NB_ELEMENT :=
                                                        NB_ELEMENT + 1;
                                                    AORDRE (NB_ELEMENT) :=
                                                        NORDRE;
                                                    AMSG (NB_ELEMENT) :=
                                                        TO_CHAR (
                                                            COBJ_REC.AVENBREALISE);
                                                    ATYPE (NB_ELEMENT) := 'C';
                                                    ADEC (NB_ELEMENT) := NULL;
                                                    NB_ELEMENT :=
                                                        NB_ELEMENT + 1;
                                                    AORDRE (NB_ELEMENT) :=
                                                        NORDRE;
                                                    AMSG (NB_ELEMENT) :=
                                                        TO_CHAR (
                                                            COBJ_REC.AVENBOBJECTIF);
                                                    ATYPE (NB_ELEMENT) := 'C';
                                                    ADEC (NB_ELEMENT) := NULL;
                                                END IF;
                                            ELSE
                                                IF     COBJ_REC.AVEMTREALISE
                                                           IS NOT NULL
                                                   AND COBJ_REC.AVEMTOBJECTIF
                                                           IS NOT NULL
                                                THEN
                                                    IF COBJ_REC.AVEMTREALISE >
                                                       COBJ_REC.AVEMTOBJECTIF
                                                    THEN
                                                        LOK := 0;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        NORDRE := NB_ELEMENT;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NB_ELEMENT;
                                                        AMSG (NB_ELEMENT) :=
                                                               F_STDTRIMALL (
                                                                   C_AROAGE_REC.ACTCODE)
                                                            || ' - '
                                                            || F_STDTRIMALL (
                                                                   C_AROAGE_REC.ACTLIBCOURT);
                                                        ATYPE (NB_ELEMENT) :=
                                                            'C';
                                                        ADEC (NB_ELEMENT) :=
                                                            NULL;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NORDRE;
                                                        AMSG (NB_ELEMENT) :=
                                                               'LANROLE|ROLES|'
                                                            || CROLE_REC.ROLCODE;
                                                        ATYPE (NB_ELEMENT) :=
                                                            'S';
                                                        ADEC (NB_ELEMENT) :=
                                                            NULL;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NORDRE;
                                                        AMSG (NB_ELEMENT) :=
                                                            F_STDTRIMALL (
                                                                COBJ_REC.LIMITLABEL);
                                                        ATYPE (NB_ELEMENT) :=
                                                            'C';
                                                        ADEC (NB_ELEMENT) :=
                                                            NULL;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NORDRE;
                                                        AMSG (NB_ELEMENT) :=
                                                            COBJ_REC.AVEMTREALISE;
                                                        ATYPE (NB_ELEMENT) :=
                                                            'N';
                                                        ADEC (NB_ELEMENT) :=
                                                            F_NBDECIMALDANSDEVISE (
                                                                COBJ_REC.DEVCODE);
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NORDRE;
                                                        AMSG (NB_ELEMENT) :=
                                                            COBJ_REC.AVEMTOBJECTIF;
                                                        ATYPE (NB_ELEMENT) :=
                                                            'N';
                                                        ADEC (NB_ELEMENT) :=
                                                            F_NBDECIMALDANSDEVISE (
                                                                COBJ_REC.DEVCODE);
                                                    END IF;
                                                END IF;
                                            END IF;
                                        END LOOP;
                                    -- CV-21032002 FSA-8111 ( 5917 ) Date de debut doit etre >= date de validite
                                    ELSIF     SCTL = 'A0914'
                                          AND C_AROAGE_REC.AAGDTVALID
                                                  IS NOT NULL
                                          AND C_AROAGE_REC.AAGDTFIN IS NULL
                                    THEN
                                        BEGIN
                                            SELECT APHDTDEB
                                              INTO DTDATE
                                              FROM ACTPHASE
                                             WHERE     ACTID = SACTEUR
                                                   AND PHACODE = 'ACTIVE'
                                                   AND APHDTFIN IS NULL;
                                        EXCEPTION
                                            WHEN NO_DATA_FOUND
                                            THEN
                                                DTDATE := NULL;
                                        END;

                                        DBMS_OUTPUT.PUT_LINE (
                                               'Traitement du role '
                                            || SROLCODE
                                            || ' au '
                                            || TO_CHAR (DTDATE, 'DD/MM/YYYY'));

                                        IF     DTDATE IS NOT NULL
                                           AND C_AROAGE_REC.AAGDTVALID <
                                               DTDATE
                                        THEN
                                            DBMS_OUTPUT.PUT_LINE (
                                                   'Traitement du role '
                                                || SROLCODE
                                                || ' valide au '
                                                || TO_CHAR (
                                                       C_AROAGE_REC.AAGDTVALID,
                                                       'DD/MM/YYYY')
                                                || ' Date validite '
                                                || TO_CHAR (DTDATE,
                                                            'DD/MM/YYYY'));
                                            LOK := 0;
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                            NORDRE := NB_ELEMENT;
                                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                            AMSG (NB_ELEMENT) :=
                                                TO_CHAR (
                                                    C_AROAGE_REC.AAGDTVALID,
                                                    'YYYYMMDD');
                                            ATYPE (NB_ELEMENT) := 'D';
                                            ADEC (NB_ELEMENT) := NULL;
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                            AORDRE (NB_ELEMENT) := NORDRE;
                                            AMSG (NB_ELEMENT) :=
                                                   'LANROLE|ROLES|'
                                                || CROLE_REC.ROLCODE;
                                            ATYPE (NB_ELEMENT) := 'S';
                                            ADEC (NB_ELEMENT) := NULL;
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                            AORDRE (NB_ELEMENT) := NORDRE;
                                            AMSG (NB_ELEMENT) :=
                                                   F_STDTRIMALL (
                                                       C_AROAGE_REC.ACTCODE)
                                                || ' - '
                                                || F_STDTRIMALL (
                                                       C_AROAGE_REC.ACTLIBCOURT);
                                            ATYPE (NB_ELEMENT) := 'C';
                                            ADEC (NB_ELEMENT) := NULL;
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                            AORDRE (NB_ELEMENT) := NORDRE;
                                            AMSG (NB_ELEMENT) :=
                                                TO_CHAR (DTDATE, 'YYYYMMDD');
                                            ATYPE (NB_ELEMENT) := 'D';
                                            ADEC (NB_ELEMENT) := NULL;
                                        END IF;
                                    ELSIF SCTL = 'A0932'
                                    THEN
                                        NTMPFLAGRIB := 0;

                                        IF (    (C_AROAGE_REC.TMPCODE
                                                     IS NOT NULL)
                                            AND (   SUBSTR (
                                                        C_AROAGE_REC.TMPCODE,
                                                        1,
                                                        3) =
                                                    'VIR'
                                                 OR F_ISTTRPARAM (
                                                        'VIRSEPA',
                                                        C_AROAGE_REC.TMPCODE) =
                                                    1
                                                 OR C_AROAGE_REC.TMPCODE =
                                                    'MA'))
                                        THEN
                                            SELECT NVL (TMPFLAGRIB, 0)
                                              INTO NTMPFLAGRIB
                                              FROM TMOYENPMT
                                             WHERE TMPCODE =
                                                   C_AROAGE_REC.TMPCODE;
                                        END IF;

                                        IF     (NTMPFLAGRIB = 0)
                                           AND (    (C_AROAGE_REC.TMPCODEENC
                                                         IS NOT NULL)
                                                AND (SUBSTR (
                                                         C_AROAGE_REC.TMPCODEENC,
                                                         1,
                                                         3) =
                                                     'PRL'))
                                        THEN
                                            SELECT NVL (TMPFLAGRIB, 0)
                                              INTO NTMPFLAGRIB
                                              FROM TMOYENPMT
                                             WHERE TMPCODE =
                                                   C_AROAGE_REC.TMPCODEENC;
                                        END IF;

                                        IF (NTMPFLAGRIB = 1)
                                        THEN
                                            SELECT COUNT (1)
                                              INTO NANY
                                              FROM ACTRIB
                                             WHERE     ACTID = SACTEUR
                                                   AND ARIDTREMPLACE IS NULL;

                                            IF (NANY = 0)
                                            THEN
                                                RAISE ERR_AAG_PARM1;
                                            END IF;
                                        END IF;
                                    ELSIF     SCTL = 'A0933'
                                          AND C_AROAGE_REC.AAGDELAIPMT
                                                  IS NOT NULL
                                          AND C_AROAGE_REC.AAGJOUR IS NULL
                                    THEN
                                        IF (C_AROAGE_REC.AAGDELAIPMT IN
                                                ('FIXE', 'FMFIXE'))
                                        THEN
                                            RAISE ERR_AAG_PARM1;
                                        ELSIF     (SUBSTR (
                                                       C_AROAGE_REC.AAGDELAIPMT,
                                                       1,
                                                       2) =
                                                   'FM')
                                              AND (INSTR (
                                                       C_AROAGE_REC.AAGDELAIPMT,
                                                       'FX') !=
                                                   0)
                                        THEN
                                            RAISE ERR_AAG_PARM1;
                                        END IF;
                                    ELSIF     SCTL = 'A0934'
                                          AND C_AROAGE_REC.AAGDELAIPMT
                                                  IS NOT NULL
                                          AND C_AROAGE_REC.AAGDELAIPMT =
                                              'DECAL'
                                          AND (   C_AROAGE_REC.AAGJOUR
                                                      IS NULL
                                               OR C_AROAGE_REC.AAGJOUR < 1)
                                    THEN
                                        RAISE ERR_AAG_PARM1;
                                    ELSIF     SCTL = 'A0935'
                                          AND C_AROAGE_REC.AAGDELAIPMT
                                                  IS NOT NULL
                                          AND C_AROAGE_REC.AAGJOUR
                                                  IS NOT NULL
                                          AND (   C_AROAGE_REC.AAGJOUR < 1
                                               OR C_AROAGE_REC.AAGJOUR > 31)
                                    THEN
                                        IF (C_AROAGE_REC.AAGDELAIPMT IN
                                                ('FIXE', 'FMFIXE'))
                                        THEN
                                            RAISE ERR_AAG_PARM1;
                                        ELSIF     (SUBSTR (
                                                       C_AROAGE_REC.AAGDELAIPMT,
                                                       1,
                                                       2) =
                                                   'FM')
                                              AND (INSTR (
                                                       C_AROAGE_REC.AAGDELAIPMT,
                                                       'FX') !=
                                                   0)
                                        THEN
                                            RAISE ERR_AAG_PARM1;
                                        END IF;
                                    ELSIF     SCTL = 'A0976'
                                          AND C_AROAGE_REC.AAGDELAIPMT
                                                  IS NOT NULL
                                    THEN
                                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                                               'I',
                                               C_AROAGE_REC.AAGDELAIPMT,
                                               'CONDREGLT',
                                               NB_ELEMENT,
                                               AORDRE,
                                               AMSG,
                                               ATYPE,
                                               ADEC) = 0
                                        THEN
                                            RAISE ERR_AAG_PARM1;
                                        END IF;
                                    END IF;
                                EXCEPTION
                                    WHEN ERR_AAG_PARM1
                                    THEN
                                        LOK := 0;

                                        IF (SCTL = 'A0976')
                                        THEN
                                            NORDRE := NB_ELEMENT;
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                        ELSE
                                            NB_ELEMENT := NB_ELEMENT + 1;
                                            NORDRE := NB_ELEMENT;
                                        END IF;

                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) :=
                                               'LANROLE|ROLCODE|'
                                            || CROLE_REC.ROLCODE;
                                        ATYPE (NB_ELEMENT) := 'S';
                                        ADEC (NB_ELEMENT) := NULL;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) :=
                                               F_STDTRIMALL (
                                                   C_AROAGE_REC.ACTCODE)
                                            || ' '
                                            || F_STDTRIMALL (
                                                   C_AROAGE_REC.ACTLIBCOURT);
                                        ATYPE (NB_ELEMENT) := 'C';
                                        ADEC (NB_ELEMENT) := NULL;
                                END;
                            END LOOP;
                        ELSIF SCTL = 'A0931'
                        THEN
                            -- 2315 : Definition de la cotation role client garant
                            IF     F_PLROLEEXTERNE (CROLE_REC.ROLCODE) IN
                                       ('CLIENT', 'GARANT')
                               AND BCOTATION = FALSE
                            THEN
                                SELECT COUNT (*)
                                  INTO NANY
                                  FROM ACTTCOVALEUR
                                 WHERE ACTID = SACTEUR AND ATVDTFIN IS NULL;

                                IF NANY = 0
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                           'LANROLE|ROLCODE|'
                                        || CROLE_REC.ROLCODE;
                                    ATYPE (NB_ELEMENT) := 'S';
                                    ADEC (NB_ELEMENT) := NULL;
                                    BCOTATION := TRUE;
                                END IF;
                            END IF;
                        -- CV-09042004 FSA-14740 Simens MSG(7950)
                        ELSIF SCTL = 'A0936'
                        THEN
                            --TRUNK-2644
                            --SELECT COUNT( *) INTO NANY FROM ACTSTATISTIQUE WHERE ACTID = SACTEUR AND SACCODE = 'STATREC';
                            BEGIN
                                SELECT 1
                                  INTO NANY
                                  FROM DUAL
                                 WHERE F_PLGETCUSTOMCHARACTERISTIC (
                                           'ACTEUR',
                                           'STATREC',
                                           SACTEUR,
                                           NULL)
                                           IS NOT NULL;
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    NANY := 0;
                            END;

                            IF NANY > 0
                            THEN
                                FOR CTCO_REC IN CTCO
                                LOOP
                                    NTCOID := CTCO_REC.TCOID;

                                    SELECT COUNT (*)
                                      INTO NANY
                                      FROM ACTTCOVALEUR
                                     WHERE ACTID = SACTEUR AND TCOID = NTCOID;

                                    NTAUX1 := NULL;
                                    NTAUX2 := NULL;
                                    NFIRST := 1;

                                    IF NANY > 1
                                    THEN
                                        FOR CATV_REC IN CATV
                                        LOOP
                                            IF    (NFIRST = 1)
                                               OR (NTAUX1 IS NULL)
                                            THEN
                                                -- On est sur le 1er enregistrement ( le plus recent )
                                                BEGIN
                                                    -- On recherche le taux de la cotation
                                                    STVACODE :=
                                                        CATV_REC.TVACODE;

                                                    SELECT NVL (TVATAUX, 0)
                                                      INTO NTAUX1
                                                      FROM TCOVALEUR
                                                     WHERE     TCOID = NTCOID
                                                           AND TVACODE =
                                                               STVACODE;
                                                EXCEPTION
                                                    WHEN OTHERS
                                                    THEN
                                                        NTAUX1 := NULL;
                                                END;

                                                NFIRST := NFIRST + 1;
                                                DBMS_OUTPUT.PUT_LINE (
                                                       CATV_REC.ATVORDRE
                                                    || ' - '
                                                    || CATV_REC.TVACODE
                                                    || ' taux '
                                                    || NTAUX1);
                                            ELSE
                                                BEGIN
                                                    -- On recherche le taux de la cotation sur l'enregistrement suivant
                                                    SELECT NVL (TVATAUX, 0)
                                                      INTO NTAUX2
                                                      FROM TCOVALEUR
                                                     WHERE     TCOID = NTCOID
                                                           AND TVACODE =
                                                               CATV_REC.TVACODE;
                                                EXCEPTION
                                                    WHEN OTHERS
                                                    THEN
                                                        NTAUX2 := NULL;
                                                END;

                                                IF NTAUX2 IS NOT NULL
                                                THEN
                                                    IF NTAUX1 < NTAUX2
                                                    THEN
                                                        DBMS_OUTPUT.PUT_LINE (
                                                               STVACODE
                                                            || ' : '
                                                            || NTAUX1
                                                            || ' < '
                                                            || CATV_REC.TVACODE
                                                            || ' : '
                                                            || NTAUX2);
                                                        LOK := 0;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        NORDRE := NB_ELEMENT;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NB_ELEMENT;
                                                        AMSG (NB_ELEMENT) :=
                                                               'LANTCOVALEUR|'
                                                            || TO_CHAR (
                                                                   NTCOID)
                                                            || '|'
                                                            || STVACODE;
                                                        ATYPE (NB_ELEMENT) :=
                                                            'S';
                                                        ADEC (NB_ELEMENT) :=
                                                            NULL;
                                                        NB_ELEMENT :=
                                                            NB_ELEMENT + 1;
                                                        AORDRE (NB_ELEMENT) :=
                                                            NORDRE;
                                                        AMSG (NB_ELEMENT) :=
                                                               'LANTCOVALEUR|'
                                                            || TO_CHAR (
                                                                   NTCOID)
                                                            || '|'
                                                            || CATV_REC.TVACODE;
                                                        ATYPE (NB_ELEMENT) :=
                                                            'S';
                                                        ADEC (NB_ELEMENT) :=
                                                            NULL;
                                                    END IF;

                                                    NTAUX1 := NTAUX2;
                                                    STVACODE :=
                                                        CATV_REC.TVACODE;
                                                    DBMS_OUTPUT.PUT_LINE (
                                                           CATV_REC.ATVORDRE
                                                        || ' - '
                                                        || CATV_REC.TVACODE
                                                        || ' taux '
                                                        || NTAUX2);
                                                END IF;
                                            END IF;
                                        END LOOP;
                                    END IF;
                                END LOOP;
                            END IF;
                        ELSIF SCTL = 'A0970'
                        THEN
                            BEGIN
                                SELECT COUNT (*)
                                  INTO NANY
                                  FROM ACTROLE
                                 WHERE     ACTID = SACTEUR
                                       AND ROLCODE = CROLE_REC.ROLCODE;

                                IF NANY > 1
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                        'LANROLE|ROLES|' || CROLE_REC.ROLCODE;
                                    ATYPE (NB_ELEMENT) := 'S';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            END;
                        -- CV-20062001 4856 : Code QUANTIEME inconnu de la table externe
                        /* Modif Base 21/09/01
                        ELSIF sCtl = 'A0971' THEN
                        IF cRole_rec.AROQUANTIEME IS NOT NULL THEN
                        IF pa_fungencontrole.ocCodeTable( 'U', cRole_rec.AROQUANTIEME, 'QUANTIEME', nb_element, aOrdre, aMsg, aType, aDec ) = 0 THEN
                        lOk := 0;
                        END IF;
                        END IF;
                        */
                        ELSIF SCTL = 'A0972'
                        THEN
                            IF CROLE_REC.AROGRPFAC IS NOT NULL
                            THEN
                                IF PA_FUNGENCONTROLE.OCCODETABLE (
                                       'I',
                                       CROLE_REC.AROGRPFAC,
                                       'GRPFAC',
                                       NB_ELEMENT,
                                       AORDRE,
                                       AMSG,
                                       ATYPE,
                                       ADEC) = 0
                                THEN
                                    LOK := 0;
                                END IF;
                            END IF;
                        -- CV-20062001 5038 Code PERIODICITE non trouve de la table interne
                        ELSIF SCTL = 'A0975'
                        THEN
                            IF CROLE_REC.AROPERIODE IS NOT NULL
                            THEN
                                IF PA_FUNGENCONTROLE.OCCODETABLE (
                                       'I',
                                       CROLE_REC.AROPERIODE,
                                       'PERIODE',
                                       NB_ELEMENT,
                                       AORDRE,
                                       AMSG,
                                       ATYPE,
                                       ADEC) = 0
                                THEN
                                    LOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            LOK := 1;
                    END;
                END LOOP;
            END IF;

            IF SCTL = 'A0915'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM ACTEUR
                 WHERE ACTID = SACTEUR AND ACTFLAGGROUPE = 1;

                IF NCOUNT > 0
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM ACTROLE
                     WHERE ACTID = SACTEUR AND ROLCODE = 'ET';

                    IF NCOUNT = 0
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := 'LANROLE|ROLCODE|' || 'ET';
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCROLE;

    /*
    // Controle des donnees correspondants
    */
    FUNCTION OCCORRESP (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK               NUMBER := 1;
            LFLAGOK           NUMBER := 1;
            NORDRE            NUMBER := 0;
            NANY              NUMBER;
            NLASTVAR          NUMBER;
            NCOUNTFLAG        NUMBER := 0;
            -- CV-25042013 CASNT-2166
            SACTQUALIFCLASS   ACTEUR.ACTQUALIFCLASS%TYPE;

            CURSOR CCORRES
            IS
                SELECT ACOORDRE,
                       ACONOM,
                       ACOQUALITE,
                       ACOTITRE,
                       ACOQUALIFCLASS,
                       ACTIDEMPLOYER
                  FROM ACTCORRESPONDANT
                 WHERE ACTID = SACTEUR;

            CURSOR CLKTEL
            IS
                  SELECT ACOORDRE, ATEORDRE, ATCFLAGPREFERE
                    FROM LKACTTELCOR
                   WHERE ACTID = SACTEUR
                ORDER BY ACOORDRE, ATEORDRE;
        BEGIN
            IF SCTL = 'A0770'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTCORRESPONDANT
                 WHERE ACTID = SACTEUR;

                IF NANY = 0
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A0730'
            THEN
                SELECT COUNT (1)
                  INTO NANY
                  FROM ACTCORRESPONDANT
                 WHERE ACTID = SACTEUR AND NVL (ACOFLAGPREFERE, 0) = 1;

                IF NANY > 1
                THEN
                    LOK := 0;
                END IF;
            ELSE
                FOR CCORRES_REC IN CCORRES
                LOOP
                    IF SCTL = 'A0701'
                    THEN
                        IF CCORRES_REC.ACONOM IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0771'
                    THEN
                        IF CCORRES_REC.ACOQUALITE IS NOT NULL
                        THEN
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CCORRES_REC.ACOQUALITE,
                                   'QUALITE',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0702'
                    THEN
                        IF CCORRES_REC.ACOQUALITE IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CCORRES_REC.ACONOM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0703'
                    THEN
                        SELECT COUNT (*)
                          INTO NANY
                          FROM LKACTTELCOR
                         WHERE     ACTID = SACTEUR
                               AND ACOORDRE = CCORRES_REC.ACOORDRE;

                        IF NANY = 0
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CCORRES_REC.ACONOM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF (SCTL = 'A0704') AND (CCORRES_REC.ACOTITRE IS NULL)
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                            F_STDTRIMALL (CCORRES_REC.ACONOM);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    -- CV-25042013 CASNT-2166 MSG20824
                    ELSIF     SCTL = 'A0705'
                          AND CCORRES_REC.ACTIDEMPLOYER IS NOT NULL
                          AND CCORRES_REC.ACOQUALIFCLASS IS NOT NULL
                          AND CCORRES_REC.ACOQUALIFCLASS != '999'
                    THEN
                        BEGIN
                            SELECT ACTQUALIFCLASS
                              INTO SACTQUALIFCLASS
                              FROM ACTEUR
                             WHERE ACTID = CCORRES_REC.ACTIDEMPLOYER;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                SACTQUALIFCLASS := NULL;
                        END;

                        IF     SACTQUALIFCLASS IS NOT NULL
                           AND CCORRES_REC.ACOQUALIFCLASS != SACTQUALIFCLASS
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CCORRES_REC.ACOQUALIFCLASS);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (SACTQUALIFCLASS);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0750'
                    THEN
                        NLASTVAR := -1;
                        LFLAGOK := 1;

                        FOR CLKTEL_REC IN CLKTEL
                        LOOP
                            IF CLKTEL_REC.ACOORDRE = CCORRES_REC.ACOORDRE
                            THEN
                                IF CLKTEL_REC.ATEORDRE = NLASTVAR
                                THEN
                                    LFLAGOK := 0;
                                    EXIT;
                                ELSE
                                    NLASTVAR := CLKTEL_REC.ATEORDRE;
                                END IF;
                            END IF;
                        END LOOP;

                        IF LFLAGOK = 0
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CCORRES_REC.ACONOM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0772'
                    THEN
                        LOK := 1;

                        FOR CLKTEL_REC IN CLKTEL
                        LOOP
                            IF CLKTEL_REC.ACOORDRE = CCORRES_REC.ACOORDRE
                            THEN
                                SELECT COUNT (*)
                                  INTO NANY
                                  FROM ACTTELECOM
                                 WHERE     ACTID = SACTEUR
                                       AND ATEORDRE = CLKTEL_REC.ATEORDRE;

                                IF NANY = 0
                                THEN
                                    LOK := 0;
                                    NB_ELEMENT := NB_ELEMENT + 1;
                                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                    AMSG (NB_ELEMENT) :=
                                           F_STDTRIMALL (CCORRES_REC.ACONOM)
                                        || TO_CHAR (CLKTEL_REC.ATEORDRE);
                                    ATYPE (NB_ELEMENT) := 'C';
                                    ADEC (NB_ELEMENT) := NULL;
                                END IF;
                            END IF;
                        END LOOP;
                    ELSIF SCTL = 'A0773'
                    THEN
                        NCOUNTFLAG := 0;

                        FOR CLKTEL_REC IN CLKTEL
                        LOOP
                            IF CLKTEL_REC.ACOORDRE = CCORRES_REC.ACOORDRE
                            THEN
                                IF NVL (CLKTEL_REC.ATCFLAGPREFERE, 0) = 1
                                THEN
                                    NCOUNTFLAG := NCOUNTFLAG + 1;
                                END IF;
                            END IF;
                        END LOOP;

                        IF NCOUNTFLAG > 1
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CCORRES_REC.ACONOM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN LOK;
        END;
    END OCCORRESP;

    /*
    // Controle des donnees TELECOM
    */
    FUNCTION OCTELECOM (SACTEUR      IN     VARCHAR2,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CTEL
            IS
                SELECT ATETYPE,
                       ATEDTEFFET,
                       ATENUM,
                       ATEDTFIN
                  FROM ACTTELECOM
                 WHERE ACTID = SACTEUR;

            LOK      NUMBER := 1;
            NANY     NUMBER;
            NCOUNT   NUMBER;
        BEGIN
            IF SCTL = 'A0670'
            THEN
                SELECT COUNT (*)
                  INTO NANY
                  FROM ACTTELECOM
                 WHERE ACTID = SACTEUR AND ATEDTFIN IS NULL;

                IF NANY = 0
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM ACTTELECOM
                     WHERE     ACTID = SACTEUR
                           AND ATEDTFIN IS NOT NULL
                           AND ATEDTFIN > TRUNC (SYSDATE);

                    IF NANY = 0
                    THEN
                        LOK := 0;
                    END IF;
                END IF;
            ELSIF SCTL = 'A0631'
            THEN
                SELECT COUNT (1)
                  INTO NANY
                  FROM ACTTELECOM
                 WHERE ACTID = SACTEUR AND NVL (ATEFLAGPREFERE, 0) = 1;

                IF NANY > 1
                THEN
                    LOK := 0;
                END IF;
            ELSE
                FOR CTEL_REC IN CTEL
                LOOP
                    IF SCTL = 'A0601'
                    THEN
                        IF CTEL_REC.ATEDTEFFET IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CTEL_REC.ATENUM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0602'
                    THEN
                        IF CTEL_REC.ATETYPE IS NULL
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) :=
                                F_STDTRIMALL (CTEL_REC.ATENUM);
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    ELSIF SCTL = 'A0603'
                    THEN
                        IF CTEL_REC.ATENUM IS NULL
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A0630'
                    THEN
                        IF     CTEL_REC.ATEDTFIN IS NOT NULL
                           AND CTEL_REC.ATEDTEFFET IS NOT NULL
                        THEN
                            IF CTEL_REC.ATEDTFIN < CTEL_REC.ATEDTEFFET
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) :=
                                    F_STDTRIMALL (CTEL_REC.ATENUM);
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    ELSIF SCTL = 'A0671'
                    THEN
                        IF CTEL_REC.ATETYPE IS NOT NULL
                        THEN
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CTEL_REC.ATETYPE,
                                   'TELTYPE',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    --MME06062007
                    ELSIF SCTL = 'A0672'
                    THEN
                        SELECT COUNT (1)
                          INTO NCOUNT
                          FROM ACTTELECOM
                         WHERE     ATETYPE = CTEL_REC.ATETYPE
                               AND ATEFLAGPREFERE = 1
                               AND ACTID = SACTEUR;

                        IF NCOUNT > 1
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN LOK;
        END;
    END OCTELECOM;

    /*
    // Controle des attributs BAFI
    */
    FUNCTION OCATTRIBUTBAFI (
        SACTEUR      IN     VARCHAR2,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK           NUMBER := 1;
            DDATEMAXFIN   DATE;
            NORDRE        NUMBER := 0;
            LERREUR       NUMBER := 1;
            DTTEMP        ACTTAB.ATBDTVALID%TYPE
                              := TO_DATE ('01011900', 'DDMMYYYY');
            BERREUR       BOOLEAN;
            NANY          NUMBER;
            NLENGTH       NUMBER;
            STABCODE      TATTRIBUTBAFI.TABCODE%TYPE;
            SATBVALEUR    ACTTAB.ATBVALEUR%TYPE;
            SRESID1       TATTRIBUTBAFI.TABCODE%TYPE;
            SRESID2       TATTRIBUTBAFI.TABCODE%TYPE;

            CURSOR CATTRIBUT
            IS
                SELECT TABCODE,
                       REPLACE (ATBVALEUR, '||', '') ATBVALEUR,
                       ATBDTFIN,
                       ATBDTVALID
                  FROM ACTTAB
                 WHERE ACTID = SACTEUR AND ATBDTFIN IS NULL;

            CURSOR CATTRIBALL
            IS
                  SELECT TABCODE,
                         REPLACE (ATBVALEUR, '||', '') ATBVALEUR,
                         ATBDTFIN,
                         ATBDTVALID
                    FROM ACTTAB
                   WHERE ACTID = SACTEUR
                ORDER BY ATBDTVALID, TABCODE;
        BEGIN
            IF SCTL = 'A1602'
            THEN
                IF OCATTRIBUTSAISIE (SACTEUR) = 0
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A1603'
            THEN
                FOR CATTRIBALL_REC IN CATTRIBALL
                LOOP
                    IF (CATTRIBALL_REC.ATBDTVALID != DTTEMP)
                    THEN
                        BERREUR := FALSE;

                        IF (CATTRIBALL_REC.ATBVALEUR IS NULL)
                        THEN
                            SELECT COUNT (1)
                              INTO NANY
                              FROM TATTRIBUTBAFI
                             WHERE TABCODEMAITRE = CATTRIBALL_REC.TABCODE;

                            BERREUR := (NANY != 0);
                        ELSE
                            NLENGTH := LENGTH (CATTRIBALL_REC.ATBVALEUR);

                            IF MOD (NLENGTH, 2) != 0
                            THEN
                                BERREUR := TRUE;
                            ELSE
                                IF (CATTRIBALL_REC.TABCODE = '0R')
                                THEN
                                    IF (NLENGTH < 6)
                                    THEN
                                        STABCODE :=
                                            SUBSTR (CATTRIBALL_REC.ATBVALEUR,
                                                    NLENGTH - 1,
                                                    2);

                                        SELECT COUNT (1)
                                          INTO NANY
                                          FROM TATTRIBUTBAFI
                                         WHERE TABCODEMAITRE = STABCODE;

                                        BERREUR := (NANY != 0);
                                    END IF;
                                ELSIF (CATTRIBALL_REC.TABCODE = '0E')
                                THEN
                                    IF (NLENGTH < 8)
                                    THEN
                                        -- Recuperation des codes residents
                                        BEGIN
                                            SELECT REPLACE (ATBVALEUR,
                                                            '||',
                                                            '')
                                              INTO SATBVALEUR
                                              FROM ACTTAB
                                             WHERE     ACTID = SACTEUR
                                                   AND ATBDTVALID =
                                                       CATTRIBALL_REC.ATBDTVALID
                                                   AND TABCODE = '0R';

                                            SRESID1 :=
                                                SUBSTR (SATBVALEUR, 1, 2);
                                            SRESID2 :=
                                                SUBSTR (SATBVALEUR, 3, 2);
                                        EXCEPTION
                                            WHEN OTHERS
                                            THEN
                                                SRESID1 := NULL;
                                                SRESID2 := NULL;
                                        END;

                                        STABCODE :=
                                            SUBSTR (CATTRIBALL_REC.ATBVALEUR,
                                                    NLENGTH - 1,
                                                    2);

                                        SELECT COUNT (1)
                                          INTO NANY
                                          FROM TATTRIBUTBAFI
                                         WHERE     TABCODEMAITRE = STABCODE
                                               AND (F_STRSCAN (TABCONDITION,
                                                               SRESID1) =
                                                    -1)
                                               AND (F_STRSCAN (TABCONDITION,
                                                               SRESID2) =
                                                    -1);

                                        BERREUR := (NANY != 0);
                                    END IF;
                                END IF;
                            END IF;
                        END IF;

                        IF BERREUR
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (CATTRIBALL_REC.ATBDTVALID,
                                         'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                            DTTEMP := CATTRIBALL_REC.ATBDTVALID;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCTL = 'A1631'
            THEN
                FOR CATTRIBALL_REC IN CATTRIBALL
                LOOP
                    IF     CATTRIBALL_REC.ATBDTFIN IS NOT NULL
                       AND CATTRIBALL_REC.ATBDTVALID IS NOT NULL
                    THEN
                        IF CATTRIBALL_REC.ATBDTFIN <
                           CATTRIBALL_REC.ATBDTVALID
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (CATTRIBALL_REC.ATBDTVALID,
                                         'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (CATTRIBALL_REC.ATBDTFIN, 'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCTL = 'A1632'
            THEN
                FOR CATTRIBALL_REC IN CATTRIBALL
                LOOP
                    -- on commence par recuperer la plus grande des dates fin
                    IF CATTRIBALL_REC.ATBDTFIN IS NOT NULL
                    THEN
                        IF DDATEMAXFIN IS NULL
                        THEN
                            DDATEMAXFIN := CATTRIBALL_REC.ATBDTFIN;
                        ELSE
                            IF CATTRIBALL_REC.ATBDTFIN > DDATEMAXFIN
                            THEN
                                DDATEMAXFIN := CATTRIBALL_REC.ATBDTFIN;
                            END IF;
                        END IF;
                    END IF;

                    -- si cette date fin a ete valorisee on va regarder que les attributs
                    -- actifs ont une date debut > a cette date fin
                    IF DDATEMAXFIN IS NULL
                    THEN
                        FOR CATTRIBUT_REC IN CATTRIBUT
                        LOOP
                            IF CATTRIBUT_REC.ATBDTVALID <= DDATEMAXFIN
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (CATTRIBUT_REC.ATBDTVALID,
                                             'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DDATEMAXFIN, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                EXIT;
                            END IF;
                        END LOOP;
                    END IF;
                END LOOP;
            ELSIF SCTL = 'A0204'
            THEN
                LOK := 0;

                FOR CATTRIBUT_REC IN CATTRIBUT
                LOOP
                    LERREUR := F_STRSCAN (CATTRIBUT_REC.ATBVALEUR, 'E1');

                    IF LERREUR != -1
                    THEN
                        LOK := 1;
                        EXIT;
                    ELSE
                        LERREUR := F_STRSCAN (CATTRIBUT_REC.ATBVALEUR, 'FA');

                        IF LERREUR != -1
                        THEN
                            LOK := 1;
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCTL IN ('A1630', 'A1670')
            THEN
                FOR CATTRIBUT_REC IN CATTRIBUT
                LOOP
                    LERREUR :=
                        OCATTRIBUTOK (SCTL,
                                      SACTEUR,
                                      CATTRIBUT_REC.TABCODE,
                                      CATTRIBUT_REC.ATBVALEUR,
                                      NB_ELEMENT,
                                      AORDRE,
                                      AMSG,
                                      ATYPE,
                                      ADEC);

                    IF (LERREUR = 0)
                    THEN
                        LOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            END IF;

            RETURN LOK;
        END;
    END OCATTRIBUTBAFI;

    -- Controle de la saisie des attributs BAFI
    FUNCTION OCATTRIBUTSAISIE (SACTEUR IN VARCHAR2)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK    NUMBER := 1;
            NANY   NUMBER;
        BEGIN
            SELECT COUNT (*)
              INTO NANY
              FROM ACTTAB
             WHERE     ACTID = SACTEUR
                   AND TABCODE IN ('0E', '0R')
                   AND ATBDTFIN IS NULL;

            IF NANY < 2
            THEN
                LOK := 0;
            END IF;

            RETURN LOK;
        END;
    END OCATTRIBUTSAISIE;

    -- Controle de l'existence et de la dependance des attributs BAFI
    FUNCTION OCATTRIBUTOK (
        SCTL         IN     VARCHAR2,
        SACTEUR      IN     VARCHAR2,
        SATTCODE     IN     VARCHAR2,
        SATTVALEUR   IN     ACTTAB.ATBVALEUR%TYPE,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK               NUMBER := 1;
            NANY              NUMBER;
            NORDRE            NUMBER := 0;
            K                 NUMBER := 1;
            SATTRIBUT         VARCHAR2 (2);
            SATTRIBUTMAITRE   VARCHAR2 (2);
            STBLATTCODE       VARCHAR2 (2);
        BEGIN
            -- Existence des codes attributs
            IF SCTL = 'A1670'
            THEN
                -- Controle du code attribut principale
                SELECT COUNT (*)
                  INTO NANY
                  FROM TATTRIBUTBAFI
                 WHERE TABCODE = SATTCODE;

                IF NANY = 0
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NB_ELEMENT;
                    AMSG (NB_ELEMENT) := F_STDTRIMALL (SATTCODE);
                    ATYPE (NB_ELEMENT) := 'C';
                    ADEC (NB_ELEMENT) := NULL;
                END IF;
            END IF;

            K := 1;

            -- Controle des codes attributs mis en valeur du code attribut principal
            WHILE K <= LENGTH (F_STDTRIMALL (SATTVALEUR))
            LOOP
                SATTRIBUT := SUBSTR (SATTVALEUR, K, 2);

                -- Si l'attribut n'est pas en premiere position, son attribut maitre est sur les deux positions precedentes,
                -- sinon c'est l'attribut principal
                IF K > 1
                THEN
                    SATTRIBUTMAITRE := SUBSTR (SATTVALEUR, K - 2, 2);
                ELSE
                    SATTRIBUTMAITRE := SATTCODE;
                END IF;

                -- Existence des codes attributs mis en valeur
                IF SCTL = 'A1670'
                THEN
                    SELECT COUNT (*)
                      INTO NANY
                      FROM TATTRIBUTBAFI
                     WHERE TABCODE = SATTRIBUT;

                    IF NANY = 0
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := F_STDTRIMALL (SATTRIBUT);
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;
                -- controle de la dependance des attributs
                /*
                ELSIF sCtl = 'A1630' THEN
                BEGIN
                SELECT tabcodemaitre, tablibelle INTO sTblAttCode, sAttLibelle FROM tattributbafi WHERE tabcode = sAttribut;
                IF sAttributMaitre != sTblAttCode THEN
                -- recuperation du libelle du code attribut
                BEGIN
                SELECT tablibelle INTO sAttMaitreLibelle FROM tattributbafi WHERE tabcode = sAttributMaitre;
                EXCEPTION
                WHEN NO_DATA_FOUND THEN
                sAttMaitreLibelle := 'INCONNU';
                END;
                lOk                := 0;
                nb_element     := nb_element + 1;
                nOrdre           := nb_element;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element)   := F_stdtrimall(sAttLibelle);
                aType(nb_element)  := 'C';
                aDec(nb_element)   := NULL;
                nb_element     := nb_element + 1;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element)   := F_stdtrimall(sAttMaitreLibelle);
                aType(nb_element)  := 'C';
                aDec(nb_element)   := NULL;
                END IF;
                EXCEPTION
                WHEN NO_DATA_FOUND THEN
                nAny := 0;
                END;
                */
                END IF;

                K := K + 2;
            END LOOP;

            RETURN LOK;
        END;
    END OCATTRIBUTOK;

    FUNCTION OCACTIR (NACTEUR      IN     NUMBER,
                      SCTL         IN     VARCHAR2,
                      NB_ELEMENT   IN OUT BINARY_INTEGER,
                      AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                      AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                      ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK             NUMBER := 1;
            ACTEURSOC_REC   ACTEURGESTION%ROWTYPE;
            NORDRE          NUMBER;
        BEGIN
            SELECT *
              INTO ACTEURSOC_REC
              FROM ACTEURGESTION
             WHERE ACTID = NACTEUR;

            IF SCTL = 'A1701'
            THEN
                IF ACTEURSOC_REC.TCIID IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A1702'
            THEN
                IF ACTEURSOC_REC.RUBID IS NULL
                THEN
                    LOK := 0;
                END IF;
            ELSIF SCTL = 'A1703'
            THEN
                IF ACTEURSOC_REC.TAXCODE IS NULL
                THEN
                    LOK := 0;
                END IF;
            END IF;

            RETURN LOK;
        END;
    END OCACTIR;

    FUNCTION OCHAVINGROLE (NACTID             IN ACTEUR.ACTID%TYPE,
                           SROLECODEEXTERNE   IN ROLE.ROLCODEEXTERNE%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR C1
            IS
                SELECT DISTINCT ROLCODE
                  FROM ACTROLE
                 WHERE ACTID = NACTID;

            NOK   NUMBER := 0;
        BEGIN
            FOR C1R IN C1
            LOOP
                IF F_PLROLEEXTERNE (C1R.ROLCODE) = SROLECODEEXTERNE
                THEN
                    NOK := 1;
                    EXIT;
                END IF;
            END LOOP;

            RETURN NOK;
        END;
    END OCHAVINGROLE;

    FUNCTION OCHAVINGENTRANT (NACTID IN ACTEUR.ACTID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR C1
            IS
                SELECT DISTINCT ROLCODE
                  FROM ACTROLE
                 WHERE ACTID = NACTID;

            NOK   NUMBER := 0;
        BEGIN
            FOR C1R IN C1
            LOOP
                SELECT NVL (ROLFLAGENTRANT, 0)
                  INTO NOK
                  FROM ROLE
                 WHERE ROLCODE = C1R.ROLCODE;

                IF NOK = 1
                THEN
                    EXIT;
                END IF;
            END LOOP;

            RETURN NOK;
        END;
    END OCHAVINGENTRANT;

    -- CV-29032001: ACTRAYONACTION
    FUNCTION OCACTRAYONACTION (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR C1
            IS
                SELECT *
                  FROM ACTRAYONACTION
                 WHERE ACTID = ACTEUR_REC.ACTID;

            LOK         NUMBER := 1;
            NORDRE      NUMBER := 0;
            NCOUNT      NUMBER;
            SMESSAGE1   VARCHAR2 (50);
            SMESSAGE2   VARCHAR2 (50);
            SMESSAGE3   VARCHAR2 (50);
            ERR_PARM1   EXCEPTION;
            ERR_PARM2   EXCEPTION;
        BEGIN
            FOR C1R IN C1
            LOOP
                BEGIN
                    -- 4470 : le secteur geographique doit etre unique pour un fournisseur donne
                    IF SCTL = 'A1832'
                    THEN
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM ACTRAYONACTION
                         WHERE     ACTID = ACTEUR_REC.ACTID
                               AND ARASECTGEO = C1R.ARASECTGEO;

                        IF NCOUNT > 1
                        THEN
                            SMESSAGE1 :=
                                   'LANTUSPARAM|FRSSECTEURGEO|'
                                || C1R.ARASECTGEO;
                            SMESSAGE2 :=
                                   'LANTTRPARAM|FRSNIVEAUACTION|'
                                || C1R.ARANIVEAU;
                            SMESSAGE3 := NULL;
                            RAISE ERR_PARM1;
                        END IF;
                    -- 4499 : date de signature doit etre <= date du jour si elle est renseignee
                    ELSIF SCTL = 'A1830'
                    THEN
                        SMESSAGE1 := NULL;
                        SMESSAGE2 := NULL;
                    --RAISE err_parm2;
                    -- 4478 : date d'expiration doit etre strictement superieure a la date d'effet
                    ELSIF SCTL = 'A1831'
                    THEN
                        SMESSAGE1 := NULL;
                        SMESSAGE2 := NULL;
                        SMESSAGE3 := NULL;
                    --RAISE err_parm2;
                    -- 4501 : Secteur geographique obligatoire
                    ELSIF     SCTL = 'A1802'
                          AND C1R.ARANIVEAU IS NOT NULL
                          AND C1R.ARASECTGEO IS NULL
                    THEN
                        SMESSAGE1 :=
                            'LANTTRPARAM|FRSNIVEAUACTION|' || C1R.ARANIVEAU;
                        SMESSAGE2 := NULL;
                        SMESSAGE3 := NULL;
                        RAISE ERR_PARM1;
                    -- 4471 : le niveau rattache au secteur geographique doit etre inferieur au niveau superieur d'action
                    ELSIF SCTL = 'A1801'
                    THEN
                        IF ACTEUR_REC.ACTRATTACHEMENT IN ('INTER', 'NATION')
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ACTRAYONACTION
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND ARANIVEAU = 'INTER';
                        ELSIF ACTEUR_REC.ACTRATTACHEMENT = 'REGION'
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ACTRAYONACTION
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND ARANIVEAU IN ('INTER', 'NATION');
                        ELSIF ACTEUR_REC.ACTRATTACHEMENT = 'DEPART'
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ACTRAYONACTION
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND ARANIVEAU IN
                                           ('INTER', 'NATION', 'REGION');
                        ELSIF ACTEUR_REC.ACTRATTACHEMENT = 'CANTON'
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ACTRAYONACTION
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND ARANIVEAU IN ('INTER',
                                                     'NATION',
                                                     'REGION',
                                                     'DEPART');
                        ELSIF ACTEUR_REC.ACTRATTACHEMENT = 'COMMUNE'
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM ACTRAYONACTION
                             WHERE     ACTID = ACTEUR_REC.ACTID
                                   AND ARANIVEAU IN ('INTER',
                                                     'NATION',
                                                     'REGION',
                                                     'DEPART',
                                                     'CANTON');
                        END IF;

                        IF NCOUNT > 0
                        THEN
                            SMESSAGE1 :=
                                   'LANTTRPARAM|FRSNIVEAUACTION|'
                                || C1R.ARANIVEAU;
                            SMESSAGE2 :=
                                   'LANTUSPARAM|FRSSECTEURGEO|'
                                || C1R.ARASECTGEO;
                            SMESSAGE3 :=
                                   'LANTTRPARAM|FRSNIVEAUACTION|'
                                || ACTEUR_REC.ACTRATTACHEMENT;
                            RAISE ERR_PARM1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN ERR_PARM1
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := SMESSAGE1;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := SMESSAGE2;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := SMESSAGE3;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        LOK := 0;
                    WHEN ERR_PARM2
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) := SMESSAGE1;
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := SMESSAGE2;
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        LOK := 0;
                END;
            END LOOP;

            RETURN LOK;
        END;
    END OCACTRAYONACTION;

    FUNCTION OCACTPROPHAAUDIENCE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NAPRORDRE              ACTPROCEDURE.APRORDRE%TYPE;
            NAPPORDRE              ACTPROPHASE.APPORDRE%TYPE;

            CURSOR CPROCEDURE
            IS
                SELECT APRNUM,
                       APRORDRE,
                       APRDTDEB,
                       APRDTFIN,
                       APRTYPE,
                       APRTRANSACTION,
                       APRTYPEMISSION,
                       APRTYPETRIBUNAL,
                       APRNIVEAURISQUE
                  FROM ACTPROCEDURE
                 WHERE     ACTID = ACTEUR_REC.ACTID
                       AND APRTYPE IN (SELECT TUPCODE
                                         FROM TUSPARAM
                                        WHERE TUSNOM = 'TYPROCIND');

            CURSOR CPHASE
            IS
                SELECT APP.JALCODE,
                       APP.APPORDRE,
                       APP.APPDTFIN,
                       APP.APPDTDEB
                  FROM ACTPROPHASE APP
                 WHERE     APP.ACTID = ACTEUR_REC.ACTID
                       AND APP.APRORDRE = NAPRORDRE
                       AND APP.PHACODE = 'CTX';

            CURSOR CPHASEPRECTX
            IS
                SELECT APP.JALCODE,
                       APP.APPORDRE,
                       APP.APPDTFIN,
                       APP.APPDTDEB
                  FROM ACTPROPHASE APP
                 WHERE     APP.ACTID = ACTEUR_REC.ACTID
                       AND APP.APRORDRE = NAPRORDRE
                       AND APP.PHACODE = 'PRECTX';

            CURSOR CAUDIENCE
            IS
                SELECT APA.APAORDRE,
                       APA.APADECISION,
                       APA.APADTDECISION,
                       APA.APAFLAGPOURVOI,
                       APA.APAFLAGRENVOI,
                       APA.APADTAUDIENCE,
                       APA.APAEXPERTISE,
                       APA.APATYPE,
                       APA.APACONDAMNE
                  FROM ACTPROPHAAUDIENCE APA
                 WHERE     APA.ACTID = ACTEUR_REC.ACTID
                       AND APA.APRORDRE = NAPRORDRE
                       AND APA.APPORDRE = NAPPORDRE;

            LOK                    NUMBER := 1;
            NORDRE                 NUMBER;
            SAPRNUM                ACTPROCEDURE.APRNUM%TYPE;
            NCOUNT                 NUMBER;
            SMESSAGE1              VARCHAR2 (50);
            SMESSAGE2              VARCHAR2 (50);
            ERR_PARM1              EXCEPTION;
            ERR_PARM2              EXCEPTION;
            ERR_PARM3              EXCEPTION;
            ERR_PARM4              EXCEPTION;
            VERIFDATEFINAUDIENCE   DATE;
            VERIFDATEPHASE         DATE;
            DTDATEMSG              DATE;
            DTDATEMSG2             DATE;
        BEGIN
            FOR CPROC_REC IN CPROCEDURE
            LOOP
                BEGIN
                    NAPRORDRE := CPROC_REC.APRORDRE;

                    FOR CPHASE_REC IN CPHASE
                    LOOP
                        BEGIN
                            NAPPORDRE := CPHASE_REC.APPORDRE;
                            SMESSAGE1 :=
                                'LANJALON|JALCODE|' || CPHASE_REC.JALCODE;

                            FOR CAUDIENCE_REC IN CAUDIENCE
                            LOOP
                                BEGIN
                                    SAPRNUM := CPROC_REC.APRNUM;

                                    IF SAPRNUM = 'TEST'
                                    THEN
                                        SAPRNUM := NULL;
                                    END IF;

                                    --sMessage1 := cPhase_rec.JALCODE;
                                    IF     SCTL = 'A1901'
                                       AND (    CAUDIENCE_REC.APADTDECISION
                                                    IS NULL
                                            AND CAUDIENCE_REC.APADECISION
                                                    IS NOT NULL)
                                    THEN
                                        RAISE ERR_PARM1;
                                    ELSIF     SCTL = 'A1902'
                                          AND (    CAUDIENCE_REC.APADTDECISION
                                                       IS NOT NULL
                                               AND CAUDIENCE_REC.APADECISION
                                                       IS NULL)
                                    THEN
                                        RAISE ERR_PARM1;
                                    ELSIF     SCTL = 'A1905'
                                          AND NVL (
                                                  CAUDIENCE_REC.APAFLAGRENVOI,
                                                  0) =
                                              1
                                          AND (   CAUDIENCE_REC.APADECISION
                                                      IS NOT NULL
                                               OR CAUDIENCE_REC.APADTDECISION
                                                      IS NOT NULL)
                                    THEN
                                        RAISE ERR_PARM1;
                                    ELSIF    SCTL = 'A1930'
                                          OR SCTL = 'A1903'
                                          OR SCTL = 'A1904'
                                          OR SCTL = 'A1931'
                                    THEN
                                        BEGIN
                                            SELECT COUNT (APAORDRE)
                                              INTO NCOUNT
                                              FROM ACTPROPHAAUDIENCE  APA,
                                                   ACTPROCEDURE       APR,
                                                   ACTPROPHASE        APP
                                             WHERE     APR.ACTID =
                                                       ACTEUR_REC.ACTID
                                                   AND APR.APRORDRE =
                                                       NAPRORDRE
                                                   AND APP.APPORDRE =
                                                       NAPPORDRE
                                                   AND APR.APRORDRE =
                                                       APA.APRORDRE
                                                   AND APA.ACTID = APR.ACTID
                                                   AND APP.ACTID = APR.ACTID
                                                   AND APP.APRORDRE =
                                                       APR.APRORDRE
                                                   AND APP.PHACODE = 'CTX'
                                                   AND APA.APPORDRE =
                                                       APP.APPORDRE
                                                   AND APR.APRTYPE IN
                                                           (SELECT TUPCODE
                                                              FROM TUSPARAM
                                                             WHERE TUSNOM =
                                                                   'TYPROCIND')
                                                   AND APA.APAORDRE >
                                                       CAUDIENCE_REC.APAORDRE;

                                            IF SCTL = 'A1930'
                                            THEN
                                                IF NCOUNT >= 1
                                                THEN
                                                    IF NVL (
                                                           CAUDIENCE_REC.APAFLAGPOURVOI,
                                                           0) =
                                                       1
                                                    THEN
                                                        RAISE ERR_PARM1;
                                                    END IF;
                                                END IF;
                                            ELSIF SCTL = 'A1903'
                                            THEN
                                                IF     NCOUNT > 1
                                                   AND (   CAUDIENCE_REC.APADECISION
                                                               IS NULL
                                                        OR CAUDIENCE_REC.APADTDECISION
                                                               IS NULL)
                                                THEN
                                                    IF NVL (
                                                           CAUDIENCE_REC.APAFLAGRENVOI,
                                                           0) !=
                                                       1
                                                    THEN
                                                        RAISE ERR_PARM1;
                                                    END IF;
                                                END IF;
                                            ELSIF SCTL = 'A1904'
                                            THEN
                                                IF NCOUNT = 0
                                                THEN
                                                    IF NVL (
                                                           CAUDIENCE_REC.APAFLAGRENVOI,
                                                           0) =
                                                       1
                                                    THEN
                                                        RAISE ERR_PARM1;
                                                    END IF;
                                                END IF;
                                            ELSIF     SCTL = 'A1931'
                                                  AND VERIFDATEFINAUDIENCE
                                                          IS NOT NULL
                                                  AND CAUDIENCE_REC.APADTAUDIENCE
                                                          IS NOT NULL
                                            THEN
                                                IF NCOUNT >= 1
                                                THEN
                                                    IF VERIFDATEFINAUDIENCE >
                                                       CAUDIENCE_REC.APADTAUDIENCE
                                                    THEN
                                                        DTDATEMSG :=
                                                            CAUDIENCE_REC.APADTAUDIENCE;
                                                        DTDATEMSG2 :=
                                                            VERIFDATEFINAUDIENCE;
                                                        RAISE ERR_PARM2;
                                                    END IF;
                                                END IF;
                                            END IF;
                                        END;
                                    -- Controle de date d'audience ET de phase
                                    ELSIF     SCTL = 'A1934'
                                          AND CAUDIENCE_REC.APADTAUDIENCE
                                                  IS NOT NULL
                                          AND CAUDIENCE_REC.APADTDECISION
                                                  IS NOT NULL
                                    THEN
                                        IF CAUDIENCE_REC.APADTAUDIENCE >
                                           CAUDIENCE_REC.APADTDECISION
                                        THEN
                                            DTDATEMSG :=
                                                CAUDIENCE_REC.APADTAUDIENCE;
                                            DTDATEMSG2 :=
                                                CAUDIENCE_REC.APADTDECISION;
                                            RAISE ERR_PARM2;
                                        END IF;
                                    ELSIF SCTL = 'A1970'
                                    THEN
                                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                                               'U',
                                               CAUDIENCE_REC.APATYPE,
                                               'TYPEAUDIENCE',
                                               NB_ELEMENT,
                                               AORDRE,
                                               AMSG,
                                               ATYPE,
                                               ADEC) = 0
                                        THEN
                                            LOK := 0;
                                        END IF;
                                    ELSIF SCTL = 'A1971'
                                    THEN
                                        IF     PA_FUNGENCONTROLE.OCCODETABLE (
                                                   'U',
                                                   CAUDIENCE_REC.APADECISION,
                                                   'DECISION1',
                                                   NB_ELEMENT,
                                                   AORDRE,
                                                   AMSG,
                                                   ATYPE,
                                                   ADEC) = 0
                                           AND PA_FUNGENCONTROLE.OCCODETABLE (
                                                   'U',
                                                   CAUDIENCE_REC.APADECISION,
                                                   'DECISION2',
                                                   NB_ELEMENT,
                                                   AORDRE,
                                                   AMSG,
                                                   ATYPE,
                                                   ADEC) = 0
                                           AND PA_FUNGENCONTROLE.OCCODETABLE (
                                                   'U',
                                                   CAUDIENCE_REC.APADECISION,
                                                   'DECISION3',
                                                   NB_ELEMENT,
                                                   AORDRE,
                                                   AMSG,
                                                   ATYPE,
                                                   ADEC) = 0
                                        THEN
                                            LOK := 0;
                                        END IF;
                                    ELSIF SCTL = 'A1972'
                                    THEN
                                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                                               'U',
                                               CAUDIENCE_REC.APAEXPERTISE,
                                               'EXPERTISE',
                                               NB_ELEMENT,
                                               AORDRE,
                                               AMSG,
                                               ATYPE,
                                               ADEC) = 0
                                        THEN
                                            LOK := 0;
                                        END IF;
                                    ELSIF SCTL = 'A1973'
                                    THEN
                                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                                               'U',
                                               CAUDIENCE_REC.APACONDAMNE,
                                               'PARTIECONDAMNE',
                                               NB_ELEMENT,
                                               AORDRE,
                                               AMSG,
                                               ATYPE,
                                               ADEC) = 0
                                        THEN
                                            LOK := 0;
                                        END IF;
                                    END IF;

                                    VERIFDATEFINAUDIENCE :=
                                        CAUDIENCE_REC.APADTDECISION;
                                EXCEPTION
                                    WHEN ERR_PARM1
                                    THEN
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        NORDRE := NB_ELEMENT;
                                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                        AMSG (NB_ELEMENT) := SAPRNUM;
                                        ATYPE (NB_ELEMENT) := 'C';
                                        ADEC (NB_ELEMENT) := NULL;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) := SMESSAGE1;
                                        ATYPE (NB_ELEMENT) := 'S';
                                        ADEC (NB_ELEMENT) := NULL;
                                        LOK := 0;
                                    WHEN ERR_PARM2
                                    THEN
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        NORDRE := NB_ELEMENT;
                                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                        AMSG (NB_ELEMENT) := SAPRNUM;
                                        ATYPE (NB_ELEMENT) := 'C';
                                        ADEC (NB_ELEMENT) := NULL;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) := SMESSAGE1;
                                        ATYPE (NB_ELEMENT) := 'S';
                                        ADEC (NB_ELEMENT) := NULL;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) :=
                                            TO_CHAR (DTDATEMSG, 'YYYYMMDD');
                                        ATYPE (NB_ELEMENT) := 'D';
                                        ADEC (NB_ELEMENT) := NULL;
                                        NB_ELEMENT := NB_ELEMENT + 1;
                                        AORDRE (NB_ELEMENT) := NORDRE;
                                        AMSG (NB_ELEMENT) :=
                                            TO_CHAR (DTDATEMSG2, 'YYYYMMDD');
                                        ATYPE (NB_ELEMENT) := 'D';
                                        ADEC (NB_ELEMENT) := NULL;
                                        LOK := 0;
                                END;
                            END LOOP;

                            IF SCTL = 'A1933'
                            THEN
                                BEGIN
                                    SELECT COUNT (APAORDRE)
                                      INTO NCOUNT
                                      FROM ACTPROPHAAUDIENCE  APA,
                                           ACTPROCEDURE       APR,
                                           ACTPROPHASE        APP
                                     WHERE     APR.ACTID = ACTEUR_REC.ACTID
                                           AND APR.APRORDRE = NAPRORDRE
                                           AND APP.APPORDRE = NAPPORDRE
                                           AND APR.APRORDRE = APA.APRORDRE
                                           AND APA.ACTID = APR.ACTID
                                           AND APP.ACTID = APR.ACTID
                                           AND APP.APRORDRE = APR.APRORDRE
                                           AND APP.PHACODE = 'CTX'
                                           AND APA.APPORDRE = APP.APPORDRE
                                           AND APR.APRTYPE IN
                                                   (SELECT TUPCODE
                                                      FROM TUSPARAM
                                                     WHERE TUSNOM =
                                                           'TYPROCIND')
                                           AND APA.APAORDRE > NAPRORDRE;

                                    IF     VERIFDATEPHASE IS NOT NULL
                                       AND CPHASE_REC.APPDTDEB IS NOT NULL
                                    THEN
                                        IF     SCTL = 'A1933'
                                           AND VERIFDATEPHASE >
                                               CPHASE_REC.APPDTDEB
                                        THEN
                                            DTDATEMSG := CPHASE_REC.APPDTDEB;
                                            DTDATEMSG2 := VERIFDATEPHASE;
                                            RAISE ERR_PARM3;
                                        END IF;
                                    END IF;
                                END;
                            ELSIF     SCTL = 'A1932'
                                  AND CPHASE_REC.APPDTDEB IS NOT NULL
                                  AND CPHASE_REC.APPDTFIN IS NOT NULL
                            THEN
                                IF CPHASE_REC.APPDTDEB > CPHASE_REC.APPDTFIN
                                THEN
                                    DTDATEMSG := CPHASE_REC.APPDTDEB;
                                    DTDATEMSG2 := CPHASE_REC.APPDTFIN;
                                    RAISE ERR_PARM3;
                                END IF;
                            END IF;

                            VERIFDATEPHASE := CPHASE_REC.APPDTFIN;
                        EXCEPTION
                            WHEN ERR_PARM3
                            THEN
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) := SAPRNUM;
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) := SMESSAGE1;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DTDATEMSG, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DTDATEMSG2, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                LOK := 0;
                        END;
                    END LOOP;

                    VERIFDATEPHASE := NULL;

                    FOR CPHASEPRECTX_REC IN CPHASEPRECTX
                    LOOP
                        BEGIN
                            IF SCTL = 'A1935'
                            THEN
                                BEGIN
                                    SELECT COUNT (APAORDRE)
                                      INTO NCOUNT
                                      FROM ACTPROPHAAUDIENCE  APA,
                                           ACTPROCEDURE       APR,
                                           ACTPROPHASE        APP
                                     WHERE     APR.ACTID = ACTEUR_REC.ACTID
                                           AND APR.APRORDRE = NAPRORDRE
                                           AND APP.APPORDRE = NAPPORDRE
                                           AND APR.APRORDRE = APA.APRORDRE
                                           AND APA.ACTID = APR.ACTID
                                           AND APP.ACTID = APR.ACTID
                                           AND APP.APRORDRE = APR.APRORDRE
                                           AND APP.PHACODE = 'PRECTX'
                                           AND APA.APPORDRE = APP.APPORDRE
                                           AND APR.APRTYPE IN
                                                   (SELECT TUPCODE
                                                      FROM TUSPARAM
                                                     WHERE TUSNOM =
                                                           'TYPROCIND')
                                           AND APA.APAORDRE > NAPRORDRE;

                                    IF     VERIFDATEPHASE IS NOT NULL
                                       AND CPHASEPRECTX_REC.APPDTDEB
                                               IS NOT NULL
                                    THEN
                                        IF     SCTL = 'A1935'
                                           AND VERIFDATEPHASE >
                                               CPHASEPRECTX_REC.APPDTDEB
                                        THEN
                                            DTDATEMSG :=
                                                CPHASEPRECTX_REC.APPDTDEB;
                                            DTDATEMSG2 := VERIFDATEPHASE;
                                            RAISE ERR_PARM4;
                                        END IF;
                                    END IF;
                                END;
                            ELSIF     SCTL = 'A1936'
                                  AND CPHASEPRECTX_REC.APPDTDEB IS NOT NULL
                                  AND CPHASEPRECTX_REC.APPDTFIN IS NOT NULL
                            THEN
                                IF CPHASEPRECTX_REC.APPDTDEB >
                                   CPHASEPRECTX_REC.APPDTFIN
                                THEN
                                    DTDATEMSG := CPHASEPRECTX_REC.APPDTDEB;
                                    DTDATEMSG2 := CPHASEPRECTX_REC.APPDTFIN;
                                    RAISE ERR_PARM4;
                                END IF;
                            END IF;

                            VERIFDATEPHASE := CPHASEPRECTX_REC.APPDTFIN;
                        EXCEPTION
                            WHEN ERR_PARM4
                            THEN
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NB_ELEMENT;
                                AMSG (NB_ELEMENT) := SAPRNUM;
                                ATYPE (NB_ELEMENT) := 'C';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) := SMESSAGE1;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DTDATEMSG, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                    TO_CHAR (DTDATEMSG2, 'YYYYMMDD');
                                ATYPE (NB_ELEMENT) := 'D';
                                ADEC (NB_ELEMENT) := NULL;
                                LOK := 0;
                        END;
                    END LOOP;

                    IF     SCTL = 'A1937'
                       AND CPROC_REC.APRDTDEB IS NOT NULL
                       AND CPROC_REC.APRDTFIN IS NOT NULL
                    THEN
                        IF CPROC_REC.APRDTDEB > CPROC_REC.APRDTFIN
                        THEN
                            DTDATEMSG := CPROC_REC.APRDTDEB;
                            DTDATEMSG2 := CPROC_REC.APRDTFIN;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) := SAPRNUM;
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (DTDATEMSG, 'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                TO_CHAR (DTDATEMSG2, 'YYYYMMDD');
                            ATYPE (NB_ELEMENT) := 'D';
                            ADEC (NB_ELEMENT) := NULL;
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A1974'
                    THEN
                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                               'U',
                               CPROC_REC.APRTYPETRIBUNAL,
                               'TRIBUNAL',
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A1975'
                    THEN
                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                               'U',
                               CPROC_REC.APRNIVEAURISQUE,
                               'NIVEAURISQUE',
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A1976'
                    THEN
                        IF     PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CPROC_REC.APRTYPE,
                                   'TYPROCIND',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                           AND PA_FUNGENCONTROLE.OCCODETABLE (
                                   'U',
                                   CPROC_REC.APRTYPE,
                                   'PROJUD',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A1977'
                    THEN
                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                               'U',
                               CPROC_REC.APRTRANSACTION,
                               'TRANSCTX',
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    ELSIF SCTL = 'A1978'
                    THEN
                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                               'U',
                               CPROC_REC.APRTYPEMISSION,
                               'MISEEXJUD',
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                END;
            END LOOP;

            RETURN LOK;
        END;
    END OCACTPROPHAAUDIENCE;

    FUNCTION OCACTCANDIDATURE (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            CURSOR CACTCANDIDATURE
            IS
                SELECT ACAORDRE,
                       ACADTEFFET,
                       ACADTTOT,
                       ACADTTARD,
                       ACADTLIMITE,
                       ACAORIGINE,
                       ACATYPE,
                       ACANATURE
                  FROM ACTCANDIDATURE
                 WHERE ACTID = ACTEUR_REC.ACTID;

            LOK             NUMBER := 1;
            NORDRE          NUMBER;
            NCOUNT          NUMBER;
            ERR_PARM1       EXCEPTION;
            ERR_PARM2       EXCEPTION;
            DTCANDIDATURE   DATE;
            DTDATEMSG       DATE;
            DTDATEMSG2      DATE;
        BEGIN
            FOR CACTCANDIDATURE_REC IN CACTCANDIDATURE
            LOOP
                BEGIN
                    IF     SCTL = 'A2001'
                       AND CACTCANDIDATURE_REC.ACADTEFFET IS NULL
                    THEN
                        RAISE ERR_PARM1;
                    ELSIF     SCTL = 'A2030'
                          AND CACTCANDIDATURE_REC.ACADTEFFET IS NOT NULL
                          AND CACTCANDIDATURE_REC.ACADTTOT IS NOT NULL
                    THEN
                        IF CACTCANDIDATURE_REC.ACADTTOT <
                           CACTCANDIDATURE_REC.ACADTEFFET
                        THEN
                            DTDATEMSG := CACTCANDIDATURE_REC.ACADTEFFET;
                            DTDATEMSG2 := CACTCANDIDATURE_REC.ACADTTOT;
                            RAISE ERR_PARM2;
                        END IF;
                    ELSIF     SCTL = 'A2031'
                          AND CACTCANDIDATURE_REC.ACADTTARD IS NOT NULL
                          AND CACTCANDIDATURE_REC.ACADTEFFET IS NOT NULL
                    THEN
                        IF CACTCANDIDATURE_REC.ACADTTARD <=
                           CACTCANDIDATURE_REC.ACADTEFFET
                        THEN
                            DTDATEMSG := CACTCANDIDATURE_REC.ACADTEFFET;
                            DTDATEMSG2 := CACTCANDIDATURE_REC.ACADTTARD;
                            RAISE ERR_PARM2;
                        END IF;
                    ELSIF     SCTL = 'A2032'
                          AND CACTCANDIDATURE_REC.ACADTTARD IS NOT NULL
                          AND CACTCANDIDATURE_REC.ACADTTOT IS NOT NULL
                    THEN
                        IF CACTCANDIDATURE_REC.ACADTTARD <=
                           CACTCANDIDATURE_REC.ACADTTOT
                        THEN
                            DTDATEMSG := CACTCANDIDATURE_REC.ACADTTOT;
                            DTDATEMSG2 := CACTCANDIDATURE_REC.ACADTTARD;
                            RAISE ERR_PARM2;
                        END IF;
                    ELSIF     SCTL = 'A2033'
                          AND CACTCANDIDATURE_REC.ACADTLIMITE IS NOT NULL
                    THEN
                        IF CACTCANDIDATURE_REC.ACADTTARD IS NOT NULL
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTTARD;
                        ELSIF CACTCANDIDATURE_REC.ACADTTOT IS NOT NULL
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTTOT;
                        ELSIF CACTCANDIDATURE_REC.ACADTEFFET IS NOT NULL
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTTOT;
                        END IF;

                        IF     CACTCANDIDATURE_REC.ACADTTOT IS NOT NULL
                           AND CACTCANDIDATURE_REC.ACADTTOT > DTCANDIDATURE
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTTOT;
                        END IF;

                        IF     CACTCANDIDATURE_REC.ACADTEFFET IS NOT NULL
                           AND CACTCANDIDATURE_REC.ACADTEFFET > DTCANDIDATURE
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTEFFET;
                        END IF;

                        IF     CACTCANDIDATURE_REC.ACADTTARD IS NOT NULL
                           AND CACTCANDIDATURE_REC.ACADTTARD > DTCANDIDATURE
                        THEN
                            DTCANDIDATURE := CACTCANDIDATURE_REC.ACADTTARD;
                        END IF;

                        IF CACTCANDIDATURE_REC.ACADTLIMITE < DTCANDIDATURE
                        THEN
                            DTDATEMSG := CACTCANDIDATURE_REC.ACADTLIMITE;
                            DTDATEMSG2 := DTCANDIDATURE;
                            RAISE ERR_PARM2;
                        END IF;
                    ELSIF     SCTL = 'A2070'
                          AND CACTCANDIDATURE_REC.ACAORIGINE IS NOT NULL
                    THEN
                        IF PA_FUNGENCONTROLE.OCCODETABLE (
                               'U',
                               CACTCANDIDATURE_REC.ACAORIGINE,
                               'ACTORIGCANDIDAT',
                               NB_ELEMENT,
                               AORDRE,
                               AMSG,
                               ATYPE,
                               ADEC) = 0
                        THEN
                            LOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN ERR_PARM1
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTTYPECANDIDAT|'
                            || CACTCANDIDATURE_REC.ACATYPE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTNATCANDIDAT|'
                            || CACTCANDIDATURE_REC.ACANATURE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        LOK := 0;
                    WHEN ERR_PARM2
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTTYPECANDIDAT|'
                            || CACTCANDIDATURE_REC.ACATYPE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTNATCANDIDAT|'
                            || CACTCANDIDATURE_REC.ACANATURE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := TO_CHAR (DTDATEMSG, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := TO_CHAR (DTDATEMSG2, 'YYYYMMDD');
                        ATYPE (NB_ELEMENT) := 'D';
                        ADEC (NB_ELEMENT) := NULL;
                        LOK := 0;
                END;
            END LOOP;

            RETURN LOK;
        END;
    END OCACTCANDIDATURE;

    FUNCTION OCACTCANBESOIN (
        ACTEUR_REC   IN     ACTEUR%ROWTYPE,
        SCTL         IN     VARCHAR2,
        NB_ELEMENT   IN OUT BINARY_INTEGER,
        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NACAORDRE      ACTCANBESOIN.ACAORDRE%TYPE;

            CURSOR CACTCANDIDATURE
            IS
                SELECT ACATYPE, ACANATURE, ACAORDRE
                  FROM ACTCANDIDATURE
                 WHERE ACTID = ACTEUR_REC.ACTID;

            CURSOR CACTCANBESOIN
            IS
                SELECT ACBORDRE,
                       TBTCODE,
                       TBNCODE,
                       TNGCODE,
                       ACBDESTINATION,
                       ACBUSAGE
                  FROM ACTCANBESOIN
                 WHERE ACTID = ACTEUR_REC.ACTID AND ACAORDRE = NACAORDRE;

            LOK            NUMBER := 1;
            NCOUNT         NUMBER;
            NORDRE         NUMBER;
            ERR_PARM1      EXCEPTION;
            ERR_PARM2      EXCEPTION;
            NFLAGLOCATIF   NUMBER;
        BEGIN
            FOR CACTCANDIDATURE_REC IN CACTCANDIDATURE
            LOOP
                NACAORDRE := CACTCANDIDATURE_REC.ACAORDRE;

                IF SCTL = 'A2232'
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM ACTCANBESOIN
                     WHERE     ACTID = ACTEUR_REC.ACTID
                           AND ACAORDRE = NACAORDRE
                           AND ACGFLAGPRINCIPAL = 1;

                    IF NCOUNT > 1
                    THEN
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NB_ELEMENT;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTTYPECANDIDAT|'
                            || CACTCANDIDATURE_REC.ACATYPE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                               'LANTTRPARAM|ACTNATCANDIDAT|'
                            || CACTCANDIDATURE_REC.ACANATURE;
                        ATYPE (NB_ELEMENT) := 'S';
                        ADEC (NB_ELEMENT) := NULL;
                        LOK := 0;
                    END IF;
                END IF;

                FOR CACTCANBESOIN_REC IN CACTCANBESOIN
                LOOP
                    BEGIN
                        -- MsgId 5612
                        IF     SCTL = 'A2201'
                           AND CACTCANBESOIN_REC.ACBDESTINATION IS NULL
                           AND CACTCANBESOIN_REC.TBTCODE IS NULL
                        THEN
                            RAISE ERR_PARM1;
                        ELSIF     SCTL IN ('A2202',
                                           'A2203',
                                           'A2204',
                                           'A2230')
                              AND CACTCANDIDATURE_REC.ACATYPE = 'DEMANDE'
                              AND CACTCANDIDATURE_REC.ACANATURE = 'LOCAT'
                        THEN
                            IF     SCTL = 'A2202'
                               AND CACTCANBESOIN_REC.TBTCODE IS NULL
                            THEN                                 -- MsgId 5613
                                RAISE ERR_PARM1;
                            ELSIF     SCTL = 'A2203'
                                  AND CACTCANBESOIN_REC.TBTCODE IS NOT NULL
                            THEN                                 -- MsgId 5614
                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM TBTNATURE
                                 WHERE TBTCODE = CACTCANBESOIN_REC.TBTCODE;

                                IF     NCOUNT > 0
                                   AND CACTCANBESOIN_REC.TBNCODE IS NULL
                                THEN
                                    RAISE ERR_PARM1;
                                END IF;
                            ELSIF     SCTL = 'A2204'
                                  AND CACTCANBESOIN_REC.TBNCODE IS NOT NULL
                                  AND CACTCANBESOIN_REC.TBTCODE IS NOT NULL
                            THEN                                 -- MsgId 5615
                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM TBTNATGENRE
                                 WHERE     TBTCODE =
                                           CACTCANBESOIN_REC.TBTCODE
                                       AND TBNCODE =
                                           CACTCANBESOIN_REC.TBNCODE;

                                IF     NCOUNT > 0
                                   AND CACTCANBESOIN_REC.TNGCODE IS NULL
                                THEN
                                    RAISE ERR_PARM1;
                                END IF;
                            ELSIF     SCTL = 'A2230'
                                  AND CACTCANBESOIN_REC.TBNCODE IS NOT NULL
                            THEN                                 -- MsgId 5616
                                BEGIN
                                    SELECT TBTFLAGLOCATPOSS
                                      INTO NFLAGLOCATIF
                                      FROM TBIENTYPE
                                     WHERE TBTCODE =
                                           CACTCANBESOIN_REC.TBTCODE;
                                EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                        NFLAGLOCATIF := 0;
                                END;

                                IF NFLAGLOCATIF = 0
                                THEN
                                    RAISE ERR_PARM1;
                                END IF;
                            END IF;
                        ELSIF     SCTL = 'A2231'
                              AND CACTCANBESOIN_REC.ACBDESTINATION
                                      IS NOT NULL
                              AND CACTCANBESOIN_REC.ACBUSAGE IS NULL
                        THEN                                     -- MsgId 5617
                            RAISE ERR_PARM1;
                            RAISE ERR_PARM1;
                        ELSIF     SCTL = 'A2270'
                              AND CACTCANBESOIN_REC.ACBDESTINATION
                                      IS NOT NULL
                        THEN                                     -- MsgId 5618
                            IF PA_FUNGENCONTROLE.OCCODETABLE (
                                   'I',
                                   CACTCANBESOIN_REC.ACBDESTINATION,
                                   'BIENDESTINATION',
                                   NB_ELEMENT,
                                   AORDRE,
                                   AMSG,
                                   ATYPE,
                                   ADEC) = 0
                            THEN
                                LOK := 0;
                            END IF;
                        ELSIF     SCTL = 'A2271'
                              AND CACTCANBESOIN_REC.ACBUSAGE IS NOT NULL
                        THEN                                     -- MsgId 5619
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM TUSPARAM
                             WHERE TUPCODE = CACTCANBESOIN_REC.ACBUSAGE;

                            IF NCOUNT = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       'LANTUSPARAM|TUPCODE|'
                                    || CACTCANBESOIN_REC.ACBUSAGE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        ELSIF     SCTL = 'A2272'
                              AND CACTCANBESOIN_REC.TBTCODE IS NOT NULL
                        THEN                                     -- MsgId 5620
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM TBIENTYPE
                             WHERE TBTCODE = CACTCANBESOIN_REC.TBTCODE;

                            IF NCOUNT = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       'LANTBIENTYPE|TBTCODE|'
                                    || CACTCANBESOIN_REC.TBTCODE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        ELSIF     SCTL = 'A2273'
                              AND CACTCANBESOIN_REC.TBNCODE IS NOT NULL
                              AND CACTCANBESOIN_REC.TBTCODE IS NOT NULL
                        THEN                                     -- MsgId 5621
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM TBTNATURE
                             WHERE     TBTCODE = CACTCANBESOIN_REC.TBTCODE
                                   AND TBNCODE = CACTCANBESOIN_REC.TBNCODE;

                            IF NCOUNT = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       'LANTBTNATURE|TBNCODE|'
                                    || CACTCANBESOIN_REC.TBNCODE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        ELSIF     SCTL = 'A2274'
                              AND CACTCANBESOIN_REC.TNGCODE IS NOT NULL
                              AND CACTCANBESOIN_REC.TBNCODE IS NOT NULL
                              AND CACTCANBESOIN_REC.TBTCODE IS NOT NULL
                        THEN                                     -- MsgId 5622
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM TBTNATGENRE
                             WHERE     TBTCODE = CACTCANBESOIN_REC.TBTCODE
                                   AND TBNCODE = CACTCANBESOIN_REC.TBNCODE
                                   AND TNGCODE = CACTCANBESOIN_REC.TNGCODE;

                            IF NCOUNT = 0
                            THEN
                                LOK := 0;
                                NB_ELEMENT := NB_ELEMENT + 1;
                                NORDRE := NB_ELEMENT;
                                AORDRE (NB_ELEMENT) := NORDRE;
                                AMSG (NB_ELEMENT) :=
                                       'LANTBTNATGENRE|TNGCODE|'
                                    || CACTCANBESOIN_REC.TNGCODE;
                                ATYPE (NB_ELEMENT) := 'S';
                                ADEC (NB_ELEMENT) := NULL;
                            END IF;
                        END IF;
                    EXCEPTION
                        WHEN ERR_PARM1
                        THEN
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NB_ELEMENT;
                            AMSG (NB_ELEMENT) := CACTCANBESOIN_REC.ACBORDRE;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                   'LANTTRPARAM|ACTTYPECANDIDAT|'
                                || CACTCANDIDATURE_REC.ACATYPE;
                            ATYPE (NB_ELEMENT) := 'S';
                            ADEC (NB_ELEMENT) := NULL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                   'LANTTRPARAM|ACTNATCANDIDAT|'
                                || CACTCANDIDATURE_REC.ACANATURE;
                            ATYPE (NB_ELEMENT) := 'S';
                            ADEC (NB_ELEMENT) := NULL;
                            LOK := 0;
                    END;
                END LOOP;
            END LOOP;

            RETURN LOK;
        END;
    END OCACTCANBESOIN;

    /*
    // Controle evenement sur les provisions
    
    FUNCTION OCPRVDOUT (ACTEUR_REC   IN     ACTEUR%ROWTYPE,
                        CREVT_REC    IN     CREVT%ROWTYPE,
                        SCTL         IN     VARCHAR2,
                        NB_ELEMENT   IN OUT BINARY_INTEGER,
                        AORDRE       IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
                        AMSG         IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ATYPE        IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
                        ADEC         IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            LOK              NUMBER := 1;
            NDEVDECIMAL      NUMBER;
            NMTPROVISION     NUMBER;
            NCUMMTPROVFISC   AROAGEPROVISION.APRMTPROVFISC%TYPE;
            NMTOUTSTANDING   NUMBER;
            NEXCLUDE         NUMBER;
            NORDRE           NUMBER;
            NACTID           AROAGEPROVISION.ACTID%TYPE;
            SROLCODE         AROAGEPROVISION.ROLCODE%TYPE;
            NACTIDGESTION    AROAGEPROVISION.ACTIDGESTION%TYPE;
            NAPRORDRE        AROAGEPROVISION.APRORDRE%TYPE;

            CURSOR CAPR
            IS
                  SELECT APR.ACTID,
                         APR.ROLCODE,
                         APR.ACTIDGESTION,
                         APR.APRORDRE,
                         APR.DOSID,
                         APR.APRMTPROVFISC,
                         APR.APRMTIMPAYETTC,
                         APR.APRMTGARANTI,
                         APR.APRMTSAISISURETE,
                         APR.APRMTDEPOTGARANTI,
                         APR.APRMTPPA,
                         DOS.DOSNUM,
                         DOS.DOSAVENANT,
                         DOS.DOSNOM
                    FROM AROAGEPROVISION APR, DOSSIER DOS
                   WHERE     APR.ACTID = ACTEUR_REC.ACTID
                         AND APR.CREID = CREVT_REC.CREID
                         AND DOS.DOSID(+) = APR.DOSID
                ORDER BY APR.ACTID,
                         APR.ROLCODE,
                         APR.ACTIDGESTION,
                         APR.APRORDRE;

            CURSOR CAPL
            IS
                  SELECT APL.APLMTPROVFISC,
                         APL.APLMTIMPAYETTC,
                         APL.APLMTGARANTI,
                         APL.APLMTSAISISURETE,
                         APL.APLMTDEPOTGARANTI,
                         APL.APLMTPPA
                    FROM AROAGEPROLIGNE APL
                   WHERE     APL.ACTID = NACTID
                         AND APL.ROLCODE = SROLCODE
                         AND APL.ACTIDGESTION = NACTIDGESTION
                         AND APL.APRORDRE = NAPRORDRE
                ORDER BY APL.APLORDRE;
        BEGIN
            NDEVDECIMAL := F_NBDECIMALDANSDEVISE (ACTEUR_REC.DEVCODE);

            IF (SCTL = 'PRVAMNT')
            THEN
                BEGIN
                    SELECT NVL (CDADATANUMBER, 0)
                      INTO NMTPROVISION
                      FROM CREDATA
                     WHERE     CREID = CREVT_REC.CREID
                           AND CDATABLE = 'AROAGEPROVISION'
                           AND CDACOLONNE = 'AMOUNT';
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NMTPROVISION := 0;
                END;

                NCUMMTPROVFISC := 0;

                FOR CAPR_REC IN CAPR
                LOOP
                    NMTOUTSTANDING :=
                        PAV4_SELECTAROAGEPROVISION.F_NETBALANCE (
                            CAPR_REC.APRMTIMPAYETTC,
                            CAPR_REC.APRMTGARANTI,
                            CAPR_REC.APRMTSAISISURETE,
                            CAPR_REC.APRMTDEPOTGARANTI,
                            CAPR_REC.APRMTPPA);
                    PAV4_SELECTAROAGEPROVISION.P_EXCLUDEFROMOUTSTANDING (
                        CAPR_REC.DOSID,
                        CAPR_REC.ACTIDGESTION,
                        NMTOUTSTANDING,
                        NEXCLUDE);

                    IF (NEXCLUDE = 0)
                    THEN
                        NCUMMTPROVFISC :=
                            NCUMMTPROVFISC + NVL (CAPR_REC.APRMTPROVFISC, 0);
                    END IF;
                END LOOP;

                IF    ((NMTPROVISION > 0) AND (NCUMMTPROVFISC > NMTPROVISION))
                   OR ((NMTPROVISION < 0) AND (NCUMMTPROVFISC < NMTPROVISION))
                THEN
                    LOK := 0;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    NORDRE := NB_ELEMENT;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := NCUMMTPROVFISC;
                    ATYPE (NB_ELEMENT) := 'N';
                    ADEC (NB_ELEMENT) := NDEVDECIMAL;
                    NB_ELEMENT := NB_ELEMENT + 1;
                    AORDRE (NB_ELEMENT) := NORDRE;
                    AMSG (NB_ELEMENT) := NMTPROVISION;
                    ATYPE (NB_ELEMENT) := 'N';
                    ADEC (NB_ELEMENT) := NDEVDECIMAL;
                END IF;
            ELSIF (SCTL = 'PRVEXCL')
            THEN
                FOR CAPR_REC IN CAPR
                LOOP
                    NMTOUTSTANDING :=
                        PAV4_SELECTAROAGEPROVISION.F_NETBALANCE (
                            CAPR_REC.APRMTIMPAYETTC,
                            CAPR_REC.APRMTGARANTI,
                            CAPR_REC.APRMTSAISISURETE,
                            CAPR_REC.APRMTDEPOTGARANTI,
                            CAPR_REC.APRMTPPA);

                    IF    (    (NMTOUTSTANDING > 0)
                           AND (CAPR_REC.APRMTPROVFISC > NMTOUTSTANDING))
                       OR (    (NMTOUTSTANDING < 0)
                           AND (CAPR_REC.APRMTPROVFISC < NMTOUTSTANDING))
                    THEN
                        LOK := 0;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        NORDRE := NB_ELEMENT;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := CAPR_REC.APRMTPROVFISC;
                        ATYPE (NB_ELEMENT) := 'N';
                        ADEC (NB_ELEMENT) := NDEVDECIMAL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) := NMTOUTSTANDING;
                        ATYPE (NB_ELEMENT) := 'N';
                        ADEC (NB_ELEMENT) := NDEVDECIMAL;
                        NB_ELEMENT := NB_ELEMENT + 1;
                        AORDRE (NB_ELEMENT) := NORDRE;
                        AMSG (NB_ELEMENT) :=
                            CAPR_REC.DOSNUM || ' ' || CAPR_REC.DOSNOM;
                        ATYPE (NB_ELEMENT) := 'C';
                        ADEC (NB_ELEMENT) := NULL;
                    END IF;

                    NACTID := CAPR_REC.ACTID;
                    SROLCODE := CAPR_REC.ROLCODE;
                    NACTIDGESTION := CAPR_REC.ACTIDGESTION;
                    NAPRORDRE := CAPR_REC.APRORDRE;

                    FOR CAPL_REC IN CAPL
                    LOOP
                        NMTOUTSTANDING :=
                            PAV4_SELECTAROAGEPROVISION.F_NETBALANCE (
                                CAPL_REC.APLMTIMPAYETTC,
                                CAPL_REC.APLMTGARANTI,
                                CAPL_REC.APLMTSAISISURETE,
                                CAPL_REC.APLMTDEPOTGARANTI,
                                CAPL_REC.APLMTPPA);

                        IF    (    (NMTOUTSTANDING > 0)
                               AND (CAPL_REC.APLMTPROVFISC > NMTOUTSTANDING))
                           OR (    (NMTOUTSTANDING < 0)
                               AND (CAPL_REC.APLMTPROVFISC < NMTOUTSTANDING))
                        THEN
                            LOK := 0;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            NORDRE := NB_ELEMENT;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := CAPL_REC.APLMTPROVFISC;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NDEVDECIMAL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) := NMTOUTSTANDING;
                            ATYPE (NB_ELEMENT) := 'N';
                            ADEC (NB_ELEMENT) := NDEVDECIMAL;
                            NB_ELEMENT := NB_ELEMENT + 1;
                            AORDRE (NB_ELEMENT) := NORDRE;
                            AMSG (NB_ELEMENT) :=
                                CAPR_REC.DOSNUM || ' ' || CAPR_REC.DOSNOM;
                            ATYPE (NB_ELEMENT) := 'C';
                            ADEC (NB_ELEMENT) := NULL;
                        END IF;
                    END LOOP;
                END LOOP;
            END IF;

            RETURN LOK;
        END;
    END OCPRVDOUT;
    */
END PA_ACTCONTROLE;