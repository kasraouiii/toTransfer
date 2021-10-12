create or replace PACKAGE BODY PA_GENERER_NUMEROS_CASSIOPEE
AS
    FUNCTION F_GENERER_DPRNUMERO (
        P_TPGCODE                 DOSSIERPROSPECT.TPGCODE%TYPE,
        P_UTICODE                 UTILISATEUR.UTICODE%TYPE,
        P_ACTID                   ACTEUR.ACTID%TYPE,
        P_PFIPERIODICITE          PROPOSITIONFINANCIERE.PFIPERIODICITE%TYPE,
        P_PFINBPERIODES           PROPOSITIONFINANCIERE.PFINBPERIODES%TYPE,
        P_DPRNUMERO        IN OUT DOSSIERPROSPECT.DPRNUMERO%TYPE)
        RETURN VARCHAR2
    AS
    BEGIN
        RETURN P_DPRNUMERO;
    END F_GENERER_DPRNUMERO;

    --
    -- AU2-2012-W-0000001:
    -- deparment code(Departement HCI a l'origine de du deal (creation)3-length string: AU2)
    -- year(4 digits: YYYY)
    -- product code(first character product code: W --or Y)
    -- serial number(7 digits)
    --
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

    --
    -- Genere le numero de chantier
    -- Ici, on va chercher le prochain numero dans la sequence
    --
    PROCEDURE F_GENERER_CHANUM (
        P_DOSID               CAS_DOSSIER.DOSID%TYPE,
        P_ACTID               ACTEUR.ACTID%TYPE,
        P_CHANUM       IN OUT CAS_CHANTIER.CHANUM%TYPE,
        P_CHAAVENANT   IN OUT CAS_CHANTIER.CHAAVENANT%TYPE)
    AS
        DTFACTURE   DATE;
        BANNEE      NUMBER;
        BMOIS       NUMBER;
        NNUMERO     NUMERO.NUMNUMERO%TYPE; -- le numero indiquant si on met a jour la table numerogestion (-1 oui)
    BEGIN
        DTFACTURE := SYSDATE; --WJ le 07/11/03. L'affectation de la date est importante pour la generation de numero !
        NNUMERO := -1;
        CAS_PA_COMMON.S_NUMEROFORMATE ('CHAN', ---TOPPARAM%TPAPARAM (il y a une erreur dans la decalration de S_NUMEROFORMATE )
                                       P_ACTID, -- ACTEUR.ACTID%TYPE (nSOCIETE)
                                       'CHANUM', -- NUMERO.NUMCIBLE%TYPE (nCIBLE)
                                       DTFACTURE,
                                       BANNEE,
                                       BMOIS,
                                       NNUMERO, --NUMERO.NUMNUMERO%TYPE ( en entree, il faut lui affecter -1, sinon la table NUMEROGESTION ne sera pas mise a jour , ainsi le numero obtenu sera toujor le meme)
                                       P_CHANUM);
        P_CHAAVENANT := 0;
    END F_GENERER_CHANUM;

    --
    -- Genere le numero de bien
    -- Ici, on va chercher le prochain numero dans la sequence
    --
    PROCEDURE F_GENERER_BIMNUM (
        P_DOSID           CAS_DOSSIER.DOSID%TYPE,
        P_BIMNUM   IN OUT CAS_BIENIMMOBILIER.BIMNUM%TYPE)
    AS
        DTFACTURE   DATE;
        BANNEE      NUMBER;
        BMOIS       NUMBER;
        NNUMERO     NUMERO.NUMNUMERO%TYPE; -- le numero indiquant si on met a jour la table numerogestion (-1 oui)
    BEGIN
        DTFACTURE := SYSDATE; --WJ le 07/11/03. L'affectation de la date est importante pour la generation de numero !
        NNUMERO := -1;
        CAS_PA_COMMON.S_NUMEROFORMATE ('BIEN', ---TOPPARAM%TPAPARAM (il y a une erreur dans la decalration de S_NUMEROFORMATE )
                                       NULL, -- Un bien n'est pas associe a une societe de gestion
                                       'BIMNUM', -- NUMERO.NUMCIBLE%TYPE (nCIBLE)
                                       DTFACTURE,
                                       BANNEE, -- pas utilise dans NUMEROFORMATE
                                       BMOIS, -- pas utilise dans NUMEROFORMATE
                                       NNUMERO, --NUMERO.NUMNUMERO%TYPE ( en entree, il faut lui affecter -1, sinon la table NUMEROGESTION ne sera pas mise a jour , ainsi le numero obtenu sera toujor le meme)
                                       P_BIMNUM);
    END F_GENERER_BIMNUM;

    PROCEDURE F_GENERER_ITRNUM (
        P_DOSID           CAS_DOSSIER.DOSID%TYPE,
        P_ACTID           ACTEUR.ACTID%TYPE,
        P_ITRNUM   IN OUT CAS_IMMOTRANCHE.ITRNUM%TYPE)
    AS
        DTFACTURE   DATE := SYSDATE;
        BANNEE      NUMBER := NULL;
        BMOIS       NUMBER := NULL;
        NNUMERO     NUMERO.NUMNUMERO%TYPE := -1; -- le numero indiquant si on met a jour la table numerogestion (-1 oui)
    BEGIN
        CAS_PA_COMMON.S_NUMEROFORMATE ('TRANCHE', -- TOPPARAM%TPAPARAM (il y a une erreur dans la decalration de S_NUMEROFORMATE )
                                       P_ACTID, -- ACTEUR.ACTID%TYPE (nSOCIETE)
                                       'ITRNUM', -- NUMERO.NUMCIBLE%TYPE (nCIBLE)
                                       DTFACTURE,
                                       BANNEE,
                                       BMOIS,
                                       NNUMERO, -- NUMERO.NUMNUMERO%TYPE ( en entree, il faut lui affecter -1, sinon la table NUMEROGESTION ne sera pas mise a jour , ainsi le numero obtenu sera toujor le meme)
                                       P_ITRNUM);
    END F_GENERER_ITRNUM;

    FUNCTION GET_NEXT_ALPHANUMERIC_SEQUENCE (OLDSEQUENCE          VARCHAR2,
                                             INCREMENTPOSITION    NUMBER)
        RETURN VARCHAR2
    IS
        NEXTSEQUENCE   VARCHAR2 (15) := '000000';
        ASCIIID        NUMBER;
    BEGIN
        SELECT ASCII (SUBSTR (OLDSEQUENCE, INCREMENTPOSITION, 1))
          INTO ASCIIID
          FROM DUAL;

        IF (   (ASCIIID >= 48 AND ASCIIID < 57)
            OR (ASCIIID > 57 AND ASCIIID < 90))
        THEN
            NEXTSEQUENCE :=
                   SUBSTR (OLDSEQUENCE, 0, INCREMENTPOSITION - 1)
                || CHR (ASCIIID + 1)
                || SUBSTR (OLDSEQUENCE,
                           INCREMENTPOSITION + 1,
                           LENGTH (OLDSEQUENCE));
        END IF;

        IF (ASCIIID = 90 OR ASCIIID = 57)
        THEN
            IF (INCREMENTPOSITION != 1)
            THEN
                NEXTSEQUENCE :=
                       SUBSTR (OLDSEQUENCE, 0, INCREMENTPOSITION - 2)
                    || CHR (
                           ASCII (
                               SUBSTR (OLDSEQUENCE, INCREMENTPOSITION - 1, 1)))
                    || CHR (48)
                    || SUBSTR (OLDSEQUENCE,
                               INCREMENTPOSITION + 1,
                               LENGTH (OLDSEQUENCE));
                NEXTSEQUENCE :=
                    GET_NEXT_ALPHANUMERIC_SEQUENCE (NEXTSEQUENCE,
                                                    INCREMENTPOSITION - 1);
            ELSE
                NEXTSEQUENCE := '-ERROR';
            END IF;
        END IF;

        RETURN NEXTSEQUENCE;
    END GET_NEXT_ALPHANUMERIC_SEQUENCE;
END PA_GENERER_NUMEROS_CASSIOPEE;