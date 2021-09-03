CREATE OR REPLACE PACKAGE PA_ACTCONTROLE2
AS
    FUNCTION OCADRESSE(
            SACTEUR IN VARCHAR2,
            SCTL IN VARCHAR2,
            NB_ELEMENT IN OUT BINARY_INTEGER,
            AORDRE IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
            AMSG IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
            ATYPE IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
            ADEC IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;
    FUNCTION OCACTUNITE(
            ACTEUR_REC IN ACTEUR%ROWTYPE,
            SCTL IN VARCHAR2,
            NB_ELEMENT IN OUT BINARY_INTEGER,
            AORDRE IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
            AMSG IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
            ATYPE IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
            ADEC IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
        RETURN NUMBER;
END PA_ACTCONTROLE2;
/
CREATE OR REPLACE PACKAGE BODY PA_ACTCONTROLE2
AS
    -- Declaration des exceptions a trapper
    ERR_CHILD_FOUND      EXCEPTION;
    ERR_PARENT_NOT_FOUND EXCEPTION;
    -- Association des exceptions avec les erreurs ORACLE
    PRAGMA EXCEPTION_INIT(ERR_PARENT_NOT_FOUND, - 2291) ;
    PRAGMA EXCEPTION_INIT(ERR_CHILD_FOUND, - 2292) ;
    -- Declaration des constantes de codes retours a renvoyer
    ERR_CHILD_FOUND_CONST      CONSTANT INTEGER := 1045;
    ERR_PARENT_NOT_FOUND_CONST CONSTANT INTEGER := 1044;
    ERR_DUP_VAL_ON_INDEX_CONST CONSTANT INTEGER := 1046;
    ERR_NO_ROWS_CONST          CONSTANT INTEGER := 1;
    /*
    // Controle des adresses
    // Modifie le 26.09.2001 par CV:FSA-6700
    */
FUNCTION OCADRESSE(
        SACTEUR IN VARCHAR2,
        SCTL IN VARCHAR2,
        NB_ELEMENT IN OUT BINARY_INTEGER,
        AORDRE IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
    RETURN NUMBER
IS
BEGIN
    DECLARE
        SADRESSE VARCHAR2(200) ;
        SCODEPOSTAL ADRESSE.ADRCODEPOST%TYPE;
        SVOIE ADRESSE.ADRVOIE%TYPE;
        SLIEUDIT ADRESSE.ADRLIEUDIT%TYPE;
        SVILLE ADRESSE.ADRVILLE%TYPE;
        SPAYS ADRESSE.PAYCODE%TYPE;
        SCEDEX ADRESSE.ADRCEDEX%TYPE;
        SPAYFORMATPOST PAYS.PAYFORMATPOST%TYPE; -- CV-26092001 FSA-6700
        DTDATE ACTPHASE.APHDTDEB%TYPE; -- CV-18032002 FSA-8111
        SANY   VARCHAR2(200) ;
        NANY   NUMBER;
        LOK    NUMBER := 1;
        NORDRE NUMBER := 0;
        LCT    NUMBER;
        DTDATEDEB ACTADRESSE.AADDTDEB%TYPE; --LG 09/09/2002 FSA 9585
        SAADCOMPL ACTADRESSE.AADCOMPL%TYPE; --LG 09/09/2002 FSA 9585
        DTDATEREMP ACTADRESSE.AADDTREMPLACE%TYPE; --LG 09/09/2002 FSA 9585
        NDATE NUMBER; --LG 09/09/2002 FSA 9585
        NADRID ACTADRESSE.ADRID%TYPE; --LG 09/09/2002 FSA 9585
        NAADORDRE ACTADRESSE.AADORDRE%TYPE; -- CV-12092002
        BERREUR          BOOLEAN;
        BCONTROLEADRESSE BOOLEAN; -- LG FSA 15771 04/05/05
        NROLE            NUMBER; -- LG FSA 15771 04/05/05
        CURSOR CAD
        IS
            SELECT
                ADRID
              , AADFLAGSIEGE
              , AADFLAGCOURRIER
              , AADFLAGFACTURATION
              , AADFLAGLIVRAISON
              , AADDTDEB
              , AADORDRE
              , AADDTREMPLACE
              , AADCOMPL
            FROM
                ACTADRESSE
            WHERE
                ACTID = SACTEUR
            ORDER BY
                AADORDRE;
        CURSOR CADRESSE
        IS
            SELECT
                AADDTDEB
              , AADDTREMPLACE
              , AADCOMPL
              , AADORDRE
            FROM
                ACTADRESSE
            WHERE
                ACTID     = SACTEUR
                AND ADRID = NADRID
                AND
                (
                    (
                        SAADCOMPL    IS NULL
                        AND AADCOMPL IS NULL
                    )
                    OR
                    (
                        SAADCOMPL   IS NOT NULL
                        AND AADCOMPL = SAADCOMPL
                    )
                )
                AND AADORDRE != NAADORDRE
            ORDER BY
                AADDTDEB
              , AADORDRE;
    BEGIN
        -- LG FSA 15771 04/05/05
        BCONTROLEADRESSE := TRUE;
        SELECT COUNT( *) INTO NROLE FROM ACTROLE WHERE ACTID = SACTEUR;
        IF NROLE              = 0 THEN
            BCONTROLEADRESSE := FALSE;
        ELSE
            SELECT COUNT( *) INTO NROLE FROM ACTROLE WHERE ACTID = SACTEUR AND ROLCODE != 'DIRIG';
            IF NROLE              = 0 THEN
                BCONTROLEADRESSE := FALSE;
            END IF;
        END IF;
        -- Controle de l'existence d'une adresse
        IF SCTL = 'A0470' THEN
            IF BCONTROLEADRESSE THEN
                SELECT COUNT( *) INTO NANY FROM ACTADRESSE WHERE ACTID = SACTEUR AND
                    (AADDTREMPLACE IS NULL OR AADDTREMPLACE > TRUNC(SYSDATE)
                    ) ;
                IF NANY  = 0 THEN
                    LOK := 0;
                END IF;
            END IF;
            -- Controle de l'existence d'une adresse siege sociale
            -- LG 11/04/03 FSA 11903
        ELSIF SCTL = 'A0473' OR SCTL = 'A0474' THEN
            SELECT COUNT( *) INTO NANY FROM ACTADRESSE WHERE ACTID = SACTEUR AND AADDTREMPLACE IS NULL AND AADFLAGSIEGE = 1;
            IF NANY  = 0 AND SCTL = 'A0473' THEN
                LOK := 0;
                --ALA 082007  existence plus qu une adresse de siege social
            ELSIF NANY > 1 AND SCTL = 'A0474' THEN
                LOK   := 0;
            END IF;
        ELSE
            FOR CAD_REC IN CAD
            LOOP
                BEGIN
                    NADRID := CAD_REC.ADRID;
                    NDATE  := 0;
                    SELECT
                        ADRVOIE
                      , ADRLIEUDIT
                      , ADRCODEPOST
                      , ADRVILLE
                      , ADR.PAYCODE
                      , ADRCEDEX
                      , PAYFORMATPOST
                    INTO
                        SVOIE
                      , SLIEUDIT
                      , SCODEPOSTAL
                      , SVILLE
                      , SPAYS
                      , SCEDEX
                      , SPAYFORMATPOST
                    FROM
                        ADRESSE ADR
                      , PAYS
                    WHERE
                        ADRID            = CAD_REC.ADRID
                        AND PAYS.PAYCODE = ADR.PAYCODE; -- CV-26092001 FSA-6700
                    SADRESSE            := '';
                    IF SVOIE            IS NOT NULL THEN
                        SADRESSE        := SADRESSE || F_STDTRIMALL(SUBSTR(SVOIE, 1, 50)) ;
                    END IF;
                    IF SLIEUDIT  IS NOT NULL THEN
                        SADRESSE := F_STDTRIMALL(SADRESSE) || ' ' || F_STDTRIMALL(SUBSTR(SLIEUDIT, 1, 50)) ;
                    END IF;
                    IF SCODEPOSTAL IS NOT NULL THEN
                        SADRESSE   := F_STDTRIMALL(SADRESSE) || ' ' || F_STDTRIMALL(SCODEPOSTAL) ;
                    END IF;
                    IF SVILLE    IS NOT NULL THEN
                        SADRESSE := F_STDTRIMALL(SADRESSE) || ' ' || F_STDTRIMALL(SUBSTR(SVILLE, 1, 50)) ;
                    END IF;
                    IF SCEDEX    IS NOT NULL THEN
                        SADRESSE := F_STDTRIMALL(SADRESSE) || ' ' || F_STDTRIMALL(SCEDEX) ;
                    END IF;
                    IF(SADRESSE  IS NOT NULL) THEN
                        SADRESSE := SUBSTR(SADRESSE, 1, 200) ;
                    END IF;
                    IF SCTL                         = 'A0430' THEN
                        IF CAD_REC.AADDTDEB        IS NOT NULL AND CAD_REC.AADDTREMPLACE IS NOT NULL THEN
                            IF CAD_REC.AADDTDEB     > CAD_REC.AADDTREMPLACE THEN
                                LOK                := 0;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                NORDRE             := NB_ELEMENT;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                                ATYPE(NB_ELEMENT)  := 'S';
                                ADEC(NB_ELEMENT)   := NULL;
                            END IF;
                        END IF;
                    ELSIF SCTL                  = 'A0401' THEN
                        IF CAD_REC.AADDTDEB    IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                    ELSIF SCTL                  = 'A0402' THEN
                        IF SVOIE               IS NULL AND SCEDEX IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                    ELSIF SCTL                  = 'A0403' THEN
                        IF SLIEUDIT            IS NULL AND SCEDEX IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        -- Controle de l'existence du code postal
                        -- CV-26092001 FSA-6700
                    ELSIF SCTL                  = 'A0404' THEN
                        IF SCODEPOSTAL         IS NULL AND SPAYFORMATPOST IS NOT NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        -- Controle du code postal
                        -- LG le 14/04/03 FSA 11850
                    ELSIF SCTL          = 'A0471' THEN
                        IF SCODEPOSTAL IS NOT NULL AND SPAYS = 'FR' THEN
                            SELECT COUNT( *) INTO NANY FROM CODEPOSTAL WHERE CPOCODE = SCODEPOSTAL;
                            IF NANY                 = 0 AND SCEDEX IS NULL THEN
                                LOK                := 0;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                NORDRE             := NB_ELEMENT;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SCODEPOSTAL) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                                ATYPE(NB_ELEMENT)  := 'S';
                                ADEC(NB_ELEMENT)   := NULL;
                            END IF;
                        END IF;
                        -- Controle du code pays s'il n'est pas null
                    ELSIF SCTL                      = 'A0472' THEN
                        IF SPAYS                   IS NOT NULL THEN
                            LCT                    := F_PLPAYS(SPAYS) ;
                            IF LCT                  = 0 THEN
                                LOK                := 0;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                NORDRE             := NB_ELEMENT;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SPAYS) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                            END IF;
                        END IF;
                        -- Controle de la selection d'un pays
                    ELSIF SCTL                  = 'A0406' THEN
                        IF SPAYS               IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NB_ELEMENT;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        -- Controle de l'existence d'un type d'adresse
                    ELSIF SCTL                          = 'A0431' THEN
                        IF NVL(CAD_REC.AADFLAGSIEGE, 0) = 0 AND NVL(CAD_REC.AADFLAGCOURRIER, 0) = 0 AND NVL(CAD_REC.AADFLAGFACTURATION, 0) = 0 AND NVL(CAD_REC.AADFLAGLIVRAISON, 0) = 0 THEN
                            LOK                        := 0;
                            NB_ELEMENT                 := NB_ELEMENT + 1;
                            NORDRE                     := NB_ELEMENT;
                            AORDRE(NB_ELEMENT)         := NORDRE;
                            AMSG(NB_ELEMENT)           := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)          := 'C';
                            ADEC(NB_ELEMENT)           := NULL;
                            NB_ELEMENT                 := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT)         := NORDRE;
                            AMSG(NB_ELEMENT)           := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)          := 'S';
                            ADEC(NB_ELEMENT)           := NULL;
                        END IF;
                        -- Controle de la definition du complement d'adresse
                    ELSIF SCTL                  = 'A0407' THEN
                        IF CAD_REC.AADCOMPL    IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        -- CV-18032002 FSA-8111 (5899)Date debut doit >= date validite ( APHDTDEB )
                    ELSIF SCTL = 'A0408' AND CAD_REC.AADDTDEB IS NOT NULL AND CAD_REC.AADDTREMPLACE IS NULL THEN
                        BEGIN
                            SELECT APHDTDEB INTO DTDATE FROM ACTPHASE WHERE ACTID = SACTEUR AND PHACODE = 'ACTIVE' AND APHDTFIN IS NULL;
                        EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                            DTDATE := NULL;
                        END;
                        IF DTDATE              IS NOT NULL AND CAD_REC.AADDTDEB < DTDATE THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := TO_CHAR(CAD_REC.AADDTDEB, 'YYYYMMDD') ;
                            ATYPE(NB_ELEMENT)  := 'D';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := TO_CHAR(DTDATE, 'YYYYMMDD') ;
                            ATYPE(NB_ELEMENT)  := 'D';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        -- Controle de l'existence de la ville
                    ELSIF SCTL                  = 'A0405' THEN
                        IF SVILLE              IS NULL THEN
                            LOK                := 0;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            NORDRE             := NB_ELEMENT;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)  := 'C';
                            ADEC(NB_ELEMENT)   := NULL;
                            NB_ELEMENT         := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT) := NORDRE;
                            AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)  := 'S';
                            ADEC(NB_ELEMENT)   := NULL;
                        END IF;
                        --LG 09/09/2002 FSA 9585 Controle de l'unicite de l'adresse
                    ELSIF(SCTL = 'A0432') AND
                        (
                            CAD_REC.AADDTDEB IS NOT NULL
                        )
                        THEN
                        SAADCOMPL := CAD_REC.AADCOMPL;
                        NAADORDRE := CAD_REC.AADORDRE;
                        FOR CADRESSE_REC IN CADRESSE
                        LOOP
                            BERREUR                      := FALSE;
                            IF(CADRESSE_REC.AADDTDEB      > CAD_REC.AADDTDEB) THEN
                                IF(CAD_REC.AADDTREMPLACE IS NULL) THEN
                                    BERREUR              := TRUE;
                                ELSE
                                    IF(CADRESSE_REC.AADDTDEB           < CAD_REC.AADDTREMPLACE) THEN
                                        BERREUR                       := TRUE;
                                    ELSIF(CADRESSE_REC.AADDTDEB        = CAD_REC.AADDTREMPLACE) THEN
                                        IF(CADRESSE_REC.AADDTREMPLACE IS NULL) OR
                                            (
                                                CADRESSE_REC.AADDTREMPLACE != CAD_REC.AADDTREMPLACE
                                            )
                                            THEN
                                            BERREUR := TRUE;
                                        END IF;
                                    END IF;
                                END IF;
                            ELSIF(CADRESSE_REC.AADDTDEB            = CAD_REC.AADDTDEB) THEN
                                IF(CAD_REC.AADDTREMPLACE          IS NULL) THEN
                                    IF(CADRESSE_REC.AADDTREMPLACE IS NULL) OR
                                        (
                                            CADRESSE_REC.AADDTREMPLACE > CAD_REC.AADDTDEB
                                        )
                                        THEN
                                        BERREUR := TRUE;
                                    END IF;
                                ELSIF(CAD_REC.AADDTDEB            != CAD_REC.AADDTREMPLACE) THEN
                                    IF(CADRESSE_REC.AADDTREMPLACE IS NULL) OR
                                        (
                                            CADRESSE_REC.AADDTREMPLACE != CADRESSE_REC.AADDTDEB
                                        )
                                        THEN
                                        BERREUR := TRUE;
                                    END IF;
                                END IF;
                            ELSE
                                IF(CADRESSE_REC.AADDTREMPLACE IS NULL) OR
                                    (
                                        CADRESSE_REC.AADDTREMPLACE >= CAD_REC.AADDTDEB
                                    )
                                    THEN
                                    BERREUR := TRUE;
                                END IF;
                            END IF;
                            IF(BERREUR) THEN
                                LOK                := 0;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                NORDRE             := NB_ELEMENT;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(CADRESSE_REC.AADCOMPL) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SADRESSE) ;
                                ATYPE(NB_ELEMENT)  := 'C';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := 'LANPAYS|PAYCODE|' || SPAYS;
                                ATYPE(NB_ELEMENT)  := 'S';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := NAADORDRE;
                                ATYPE(NB_ELEMENT)  := 'N';
                                ADEC(NB_ELEMENT)   := NULL;
                                NB_ELEMENT         := NB_ELEMENT + 1;
                                AORDRE(NB_ELEMENT) := NORDRE;
                                AMSG(NB_ELEMENT)   := TO_CHAR(CADRESSE_REC.AADDTDEB, 'YYYYMMDD') ;
                                ATYPE(NB_ELEMENT)  := 'D';
                                ADEC(NB_ELEMENT)   := NULL;
                            END IF;
                        END LOOP;
                        -- Controle de l'existence d'un type d'adresse
                    ELSIF SCTL                                = 'A0409' THEN
                        IF NVL(CAD_REC.AADFLAGFACTURATION, 0) = 0 THEN
                            LOK                              := 0;
                            NB_ELEMENT                       := NB_ELEMENT + 1;
                            NORDRE                           := NB_ELEMENT;
                            AORDRE(NB_ELEMENT)               := NORDRE;
                            AMSG(NB_ELEMENT)                 := F_STDTRIMALL(SADRESSE) ;
                            ATYPE(NB_ELEMENT)                := 'C';
                            ADEC(NB_ELEMENT)                 := NULL;
                            NB_ELEMENT                       := NB_ELEMENT + 1;
                            AORDRE(NB_ELEMENT)               := NORDRE;
                            AMSG(NB_ELEMENT)                 := 'LANPAYS|PAYCODE|' || SPAYS;
                            ATYPE(NB_ELEMENT)                := 'S';
                            ADEC(NB_ELEMENT)                 := NULL;
                        END IF;
                    END IF;
                END;
            END LOOP;
        END IF;
        RETURN LOK;
    END;
END OCADRESSE;
FUNCTION OCACTUNITE(
        ACTEUR_REC IN ACTEUR%ROWTYPE,
        SCTL IN VARCHAR2,
        NB_ELEMENT IN OUT BINARY_INTEGER,
        AORDRE IN OUT PA_FUNGENCONTROLE.TBL_NUMBER,
        AMSG IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ATYPE IN OUT PA_FUNGENCONTROLE.TBL_VARCHAR2,
        ADEC IN OUT PA_FUNGENCONTROLE.TBL_NUMBER)
    RETURN NUMBER
IS
BEGIN
    DECLARE
        CURSOR CACTUNITE
        IS
            SELECT
                AUN.AADORDRE
              , AUN.AUNORDRE
              , AUN.AUNDTDEB
              , AUN.AUNDTFIN
              , AUN.TMPCODE
              , AUN.RIBID
              , AAD.AADDTDEB
              , AAD.AADDTREMPLACE
              , AUN.ACOORDRE
              , AUN.AUNRECAPFAC
              , AUN.AUNDELAIPMT
              , AUN.AUNQUANTIEMEFAC
              , AUN.AUNUNITE
              , AUN.AUNJOUR
              , AUN.AUNFACAVANCE
              , AUN.ACTIDGESTION
              , AUN.AUNPERIODE
              , AUN.AUNMULTIPLE
              , AUN.AUNGRPFAC
              , AUN.AUNGRPPRL
              , AUN.AUNRAISON1
              , AUN.AUNRAISON2
              , AUN.AUNCOMMENT
            FROM
                ACTUNITE AUN
              , ACTADRESSE AAD
            WHERE
                AUN.ACTID        = ACTEUR_REC.ACTID
                AND AUN.AADORDRE = AAD.AADORDRE
                AND AAD.ACTID    = AUN.ACTID;
        CURSOR CACTUNITE2
        IS
            SELECT AUNORDRE, AUNDTDEB, AUNDTFIN, AADORDRE, AUNUNITE FROM ACTUNITE WHERE ACTID = ACTEUR_REC.ACTID ORDER BY AUNORDRE, AUNDTDEB;
        LOK        NUMBER := 1;
        NORDRE     NUMBER;
        NCOUNT     NUMBER;
        ERR_PARM1  EXCEPTION;
        ERR_PARM2  EXCEPTION;
        ERR_PARM3  EXCEPTION;
        DTDATEMSG  DATE;
        DTDATEMSG2 DATE;
        NAADORDRE ACTUNITE.AADORDRE%TYPE;
        DTDATERIB    DATE;
        DTDATERIBFIN DATE;
        NACTID ACTUNITE.ACTID%TYPE;
        BTROUVE BOOLEAN;
        SERRACTCODE ACTEUR.ACTCODE%TYPE;
        NFLAGRIB NUMBER;
        NACTIDGESTION ACTUNITE.ACTIDGESTION%TYPE;
        DTMAXDATE DATE;
    BEGIN
        BTROUVE := FALSE;
        FOR CACTUNITE_REC IN CACTUNITE
        LOOP
            BEGIN
                IF SCTL = 'A2101' AND CACTUNITE_REC.AUNDTDEB IS NULL THEN
                    RAISE ERR_PARM1;
                    -- CV-21032002 FSA-8111 ( 5918 )
                ELSIF SCTL = 'A2102' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL THEN
                    BEGIN
                        SELECT APHDTDEB INTO DTDATEMSG FROM ACTPHASE WHERE ACTID = ACTEUR_REC.ACTID AND PHACODE = 'ACTIVE' AND APHDTFIN IS NULL;
                    EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                        DTDATEMSG := NULL;
                    END;
                    IF DTDATEMSG   IS NOT NULL AND CACTUNITE_REC.AUNDTDEB < DTDATEMSG THEN
                        DTDATEMSG2 := CACTUNITE_REC.AADDTDEB;
                        RAISE ERR_PARM2;
                    END IF;
                ELSIF SCTL = 'A2103' AND CACTUNITE_REC.AUNPERIODE IS NULL THEN
                    RAISE ERR_PARM1;
                ELSIF SCTL = 'A2104' AND CACTUNITE_REC.AUNMULTIPLE IS NULL THEN
                    RAISE ERR_PARM1;
                    -- Coherence sur la date de debut et la date de fin
                    -- Msg 5459
                ELSIF SCTL                    = 'A2130' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL AND CACTUNITE_REC.AUNDTFIN IS NOT NULL THEN
                    IF CACTUNITE_REC.AUNDTDEB > CACTUNITE_REC.AUNDTFIN THEN
                        DTDATEMSG            := CACTUNITE_REC.AUNDTDEB;
                        DTDATEMSG2           := CACTUNITE_REC.AUNDTFIN;
                        RAISE ERR_PARM2;
                    END IF;
                    -- Msg 5460
                ELSIF SCTL                    = 'A2131' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL AND CACTUNITE_REC.AADDTDEB IS NOT NULL THEN
                    IF CACTUNITE_REC.AUNDTDEB < CACTUNITE_REC.AADDTDEB THEN
                        DTDATEMSG            := CACTUNITE_REC.AUNDTDEB;
                        DTDATEMSG2           := CACTUNITE_REC.AADDTDEB;
                        RAISE ERR_PARM2;
                    END IF;
                    --Msg 5461
                ELSIF SCTL                    = 'A2132' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL AND CACTUNITE_REC.AADDTREMPLACE IS NOT NULL THEN
                    IF CACTUNITE_REC.AUNDTDEB > CACTUNITE_REC.AADDTREMPLACE THEN
                        DTDATEMSG            := CACTUNITE_REC.AUNDTDEB;
                        DTDATEMSG2           := CACTUNITE_REC.AADDTREMPLACE;
                        RAISE ERR_PARM2;
                    END IF;
                    -- Msg 5462
                ELSIF SCTL                    = 'A2133' AND CACTUNITE_REC.AUNDTFIN IS NOT NULL AND CACTUNITE_REC.AADDTREMPLACE IS NOT NULL THEN
                    IF CACTUNITE_REC.AUNDTFIN > CACTUNITE_REC.AADDTREMPLACE THEN
                        DTDATEMSG            := CACTUNITE_REC.AUNDTFIN;
                        DTDATEMSG2           := CACTUNITE_REC.AADDTREMPLACE;
                        RAISE ERR_PARM2;
                    END IF;
                ELSIF SCTL IN('A2134', 'A2135', 'A2136') AND CACTUNITE_REC.RIBID IS NOT NULL THEN
                    BEGIN
                        SELECT ARIDTDEB, ARIDTREMPLACE INTO DTDATERIB, DTDATERIBFIN FROM ACTRIB WHERE RIBID = CACTUNITE_REC.RIBID AND ACTID = ACTEUR_REC.ACTID;
                    EXCEPTION
                    WHEN OTHERS THEN
                        DTDATERIB    := NULL;
                        DTDATERIBFIN := NULL;
                    END;
                    -- Msg 5463
                    IF SCTL                       = 'A2134' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL AND DTDATERIB IS NOT NULL THEN
                        IF CACTUNITE_REC.AUNDTDEB < DTDATERIB THEN
                            DTDATEMSG            := CACTUNITE_REC.AUNDTDEB;
                            DTDATEMSG2           := DTDATERIB;
                            RAISE ERR_PARM2;
                        END IF;
                        -- Msg 5464
                    ELSIF SCTL                    = 'A2135' AND CACTUNITE_REC.AUNDTDEB IS NOT NULL AND DTDATERIBFIN IS NOT NULL THEN
                        IF CACTUNITE_REC.AUNDTDEB > DTDATERIBFIN THEN
                            DTDATEMSG            := CACTUNITE_REC.AUNDTDEB;
                            DTDATEMSG2           := DTDATERIBFIN;
                            RAISE ERR_PARM2;
                        END IF;
                        -- Msg 5465
                    ELSIF SCTL                    = 'A2136' AND CACTUNITE_REC.AUNDTFIN IS NOT NULL AND DTDATERIBFIN IS NOT NULL THEN
                        IF CACTUNITE_REC.AUNDTFIN > DTDATERIBFIN THEN
                            DTDATEMSG            := CACTUNITE_REC.AUNDTFIN;
                            DTDATEMSG2           := DTDATERIBFIN;
                            RAISE ERR_PARM2;
                        END IF;
                    END IF;
                    -- Coherence sur le numero d'unite --   AUNUNITE
                    -- 5468
                ELSIF SCTL = 'A2137' AND CACTUNITE_REC.AUNUNITE IS NOT NULL THEN
                    SELECT
                        MIN(ACT.ACTCODE)
                    INTO
                        SERRACTCODE
                    FROM
                        ACTUNITE AUN
                      , ACTEUR ACT
                    WHERE
                        AUN.AUNUNITE    = CACTUNITE_REC.AUNUNITE
                        AND AUN.ACTID  != ACTEUR_REC.ACTID
                        AND ACT.ACTID   = AUN.ACTID
                        AND ACT.UGECODE = ACTEUR_REC.UGECODE;
                    IF(SERRACTCODE     IS NOT NULL) THEN
                        RAISE ERR_PARM3;
                    END IF;
                    -- 5466
                    /* ELSIF sCtl = 'A2138' AND cActUnite_rec.AADORDRE IS NOT NULL
                    AND cActUnite_rec.ACOORDRE IS NOT NULL
                    AND cActUnite_rec.TMPCODE IS NOT NULL THEN */
                ELSIF SCTL                  = 'A2138' THEN
                    IF CACTUNITE_REC.RIBID IS NOT NULL THEN
                        SELECT
                            COUNT(1)
                        INTO
                            NCOUNT
                        FROM
                            ACTUNITE
                        WHERE
                            ACTID            = ACTEUR_REC.ACTID
                            AND AADORDRE     = CACTUNITE_REC.AADORDRE
                            AND ACTIDGESTION = CACTUNITE_REC.ACTIDGESTION
                            AND
                            (
                                (
                                    CACTUNITE_REC.ACOORDRE IS NOT NULL
                                    AND ACOORDRE            = CACTUNITE_REC.ACOORDRE
                                )
                                OR
                                (
                                    CACTUNITE_REC.ACOORDRE IS NULL
                                    AND ACOORDRE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNDELAIPMT IS NOT NULL
                                    AND AUNDELAIPMT            = CACTUNITE_REC.AUNDELAIPMT
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNDELAIPMT IS NULL
                                    AND AUNDELAIPMT           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNJOUR IS NOT NULL
                                    AND AUNJOUR            = CACTUNITE_REC.AUNJOUR
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNJOUR IS NULL
                                    AND AUNJOUR           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRECAPFAC IS NOT NULL
                                    AND AUNRECAPFAC            = CACTUNITE_REC.AUNRECAPFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRECAPFAC IS NULL
                                    AND AUNRECAPFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNFACAVANCE IS NOT NULL
                                    AND AUNFACAVANCE            = CACTUNITE_REC.AUNFACAVANCE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNFACAVANCE IS NULL
                                    AND AUNFACAVANCE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNQUANTIEMEFAC IS NOT NULL
                                    AND AUNQUANTIEMEFAC            = CACTUNITE_REC.AUNQUANTIEMEFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNQUANTIEMEFAC IS NULL
                                    AND AUNQUANTIEMEFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNPERIODE IS NOT NULL
                                    AND AUNPERIODE            = CACTUNITE_REC.AUNPERIODE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNPERIODE IS NULL
                                    AND AUNPERIODE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNMULTIPLE IS NOT NULL
                                    AND AUNMULTIPLE            = CACTUNITE_REC.AUNMULTIPLE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNMULTIPLE IS NULL
                                    AND AUNMULTIPLE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNGRPFAC IS NOT NULL
                                    AND AUNGRPFAC            = CACTUNITE_REC.AUNGRPFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNGRPFAC IS NULL
                                    AND AUNGRPFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNGRPPRL IS NOT NULL
                                    AND AUNGRPPRL            = CACTUNITE_REC.AUNGRPPRL
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNGRPPRL IS NULL
                                    AND AUNGRPPRL           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRAISON1 IS NOT NULL
                                    AND AUNRAISON1            = CACTUNITE_REC.AUNRAISON1
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRAISON1 IS NULL
                                    AND AUNRAISON1           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRAISON2 IS NOT NULL
                                    AND AUNRAISON2            = CACTUNITE_REC.AUNRAISON2
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRAISON2 IS NULL
                                    AND AUNRAISON2           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNCOMMENT IS NOT NULL
                                    AND AUNCOMMENT            = CACTUNITE_REC.AUNCOMMENT
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNCOMMENT IS NULL
                                    AND AUNCOMMENT           IS NULL
                                )
                            )
                            AND TMPCODE = CACTUNITE_REC.TMPCODE
                            AND RIBID   = CACTUNITE_REC.RIBID;
                    ELSE
                        SELECT
                            COUNT(1)
                        INTO
                            NCOUNT
                        FROM
                            ACTUNITE
                        WHERE
                            ACTID            = ACTEUR_REC.ACTID
                            AND AADORDRE     = CACTUNITE_REC.AADORDRE
                            AND ACTIDGESTION = CACTUNITE_REC.ACTIDGESTION
                            AND
                            (
                                (
                                    CACTUNITE_REC.ACOORDRE IS NOT NULL
                                    AND ACOORDRE            = CACTUNITE_REC.ACOORDRE
                                )
                                OR
                                (
                                    CACTUNITE_REC.ACOORDRE IS NULL
                                    AND ACOORDRE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNDELAIPMT IS NOT NULL
                                    AND AUNDELAIPMT            = CACTUNITE_REC.AUNDELAIPMT
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNDELAIPMT IS NULL
                                    AND AUNDELAIPMT           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNJOUR IS NOT NULL
                                    AND AUNJOUR            = CACTUNITE_REC.AUNJOUR
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNJOUR IS NULL
                                    AND AUNJOUR           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRECAPFAC IS NOT NULL
                                    AND AUNRECAPFAC            = CACTUNITE_REC.AUNRECAPFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRECAPFAC IS NULL
                                    AND AUNRECAPFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNFACAVANCE IS NOT NULL
                                    AND AUNFACAVANCE            = CACTUNITE_REC.AUNFACAVANCE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNFACAVANCE IS NULL
                                    AND AUNFACAVANCE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNQUANTIEMEFAC IS NOT NULL
                                    AND AUNQUANTIEMEFAC            = CACTUNITE_REC.AUNQUANTIEMEFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNQUANTIEMEFAC IS NULL
                                    AND AUNQUANTIEMEFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNPERIODE IS NOT NULL
                                    AND AUNPERIODE            = CACTUNITE_REC.AUNPERIODE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNPERIODE IS NULL
                                    AND AUNPERIODE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNMULTIPLE IS NOT NULL
                                    AND AUNMULTIPLE            = CACTUNITE_REC.AUNMULTIPLE
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNMULTIPLE IS NULL
                                    AND AUNMULTIPLE           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNGRPFAC IS NOT NULL
                                    AND AUNGRPFAC            = CACTUNITE_REC.AUNGRPFAC
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNGRPFAC IS NULL
                                    AND AUNGRPFAC           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNGRPPRL IS NOT NULL
                                    AND AUNGRPPRL            = CACTUNITE_REC.AUNGRPPRL
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNGRPPRL IS NULL
                                    AND AUNGRPPRL           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRAISON1 IS NOT NULL
                                    AND AUNRAISON1            = CACTUNITE_REC.AUNRAISON1
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRAISON1 IS NULL
                                    AND AUNRAISON1           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNRAISON2 IS NOT NULL
                                    AND AUNRAISON2            = CACTUNITE_REC.AUNRAISON2
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNRAISON2 IS NULL
                                    AND AUNRAISON2           IS NULL
                                )
                            )
                            AND
                            (
                                (
                                    CACTUNITE_REC.AUNCOMMENT IS NOT NULL
                                    AND AUNCOMMENT            = CACTUNITE_REC.AUNCOMMENT
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNCOMMENT IS NULL
                                    AND AUNCOMMENT           IS NULL
                                )
                            )
                            AND TMPCODE = CACTUNITE_REC.TMPCODE;
                    END IF;
                    IF NCOUNT > 1 THEN
                        RAISE ERR_PARM1;
                    END IF;
                    -- Msg 5473
                ELSIF SCTL IN('A2139', 'A2141') THEN
                    IF SCTL                            = 'A2139' THEN
                        IF BTROUVE                     = FALSE THEN
                            IF CACTUNITE_REC.AUNDTFIN IS NULL THEN
                                SELECT COUNT(1) INTO NCOUNT FROM ACTUNITE WHERE ACTID = ACTEUR_REC.ACTID AND AUNDTFIN IS NULL AND AUNORDRE = CACTUNITE_REC.AUNORDRE;
                                IF NCOUNT    > 1 THEN
                                    BTROUVE := TRUE;
                                    LOK     := 0;
                                END IF;
                            END IF;
                        END IF;
                        -- Msg 5472
                    ELSIF SCTL                     = 'A2141' THEN
                        IF(CACTUNITE_REC.AUNDTDEB IS NOT NULL) AND
                            (
                                (
                                    CACTUNITE_REC.AUNDTFIN IS NULL
                                )
                                OR
                                (
                                    CACTUNITE_REC.AUNDTFIN != CACTUNITE_REC.AUNDTDEB
                                )
                            )
                            THEN
                            DTMAXDATE := TO_DATE('31129999', 'DDMMYYYY') ;
                            SELECT
                                COUNT(1)
                            INTO
                                NCOUNT
                            FROM
                                ACTUNITE
                            WHERE
                                ACTID            = ACTEUR_REC.ACTID
                                AND ACTIDGESTION = CACTUNITE_REC.ACTIDGESTION
                                AND AUNUNITE     = CACTUNITE_REC.AUNUNITE
                                AND AUNORDRE    != CACTUNITE_REC.AUNORDRE
                                AND AUNDTDEB    != NVL(AUNDTFIN, DTMAXDATE) -- FB 22/08/02 pour substitution le jour de la creation.
                                AND
                                (
                                    (
                                        AUNDTDEB BETWEEN CACTUNITE_REC.AUNDTDEB AND NVL(CACTUNITE_REC.AUNDTFIN, DTMAXDATE)
                                    )
                                    OR
                                    (
                                        NVL(AUNDTFIN, DTMAXDATE) BETWEEN CACTUNITE_REC.AUNDTDEB AND NVL(CACTUNITE_REC.AUNDTFIN, DTMAXDATE)
                                    )
                                ) ;
                            IF NCOUNT       > 0 THEN
                                DTDATEMSG  := CACTUNITE_REC.AUNDTDEB;
                                DTDATEMSG2 := CACTUNITE_REC.AUNDTFIN;
                                RAISE ERR_PARM2;
                            END IF;
                        END IF;
                    END IF;
                    -- Moyen de paiement
                    -- 5467
                ELSIF SCTL = 'A2140' AND CACTUNITE_REC.TMPCODE IS NOT NULL AND CACTUNITE_REC.RIBID IS NULL THEN
                    SELECT NVL(TMPFLAGRIB, 0) INTO NFLAGRIB FROM TMOYENPMT WHERE TMPCODE = CACTUNITE_REC.TMPCODE;
                    IF(NFLAGRIB             = 1) THEN
                        NB_ELEMENT         := NB_ELEMENT + 1;
                        NORDRE             := NB_ELEMENT;
                        AORDRE(NB_ELEMENT) := NORDRE;
                        AMSG(NB_ELEMENT)   := CACTUNITE_REC.AUNUNITE;
                        ATYPE(NB_ELEMENT)  := 'N';
                        ADEC(NB_ELEMENT)   := NULL;
                        NB_ELEMENT         := NB_ELEMENT + 1;
                        AORDRE(NB_ELEMENT) := NORDRE;
                        AMSG(NB_ELEMENT)   := CACTUNITE_REC.AADORDRE;
                        ATYPE(NB_ELEMENT)  := 'N';
                        ADEC(NB_ELEMENT)   := NULL;
                        NB_ELEMENT         := NB_ELEMENT + 1;
                        AORDRE(NB_ELEMENT) := NORDRE;
                        AMSG(NB_ELEMENT)   := 'LANTMOYENPMT|TMPCODE|' || CACTUNITE_REC.TMPCODE;
                        ATYPE(NB_ELEMENT)  := 'S';
                        ADEC(NB_ELEMENT)   := NULL;
                        LOK                := 0;
                    END IF;
                ELSIF SCTL = 'A2143' AND CACTUNITE_REC.AUNDELAIPMT IS NOT NULL AND CACTUNITE_REC.AUNJOUR IS NULL THEN
                    IF(CACTUNITE_REC.AUNDELAIPMT IN('FIXE', 'FMFIXE')) THEN
                        RAISE ERR_PARM1;
                    ELSIF(SUBSTR(CACTUNITE_REC.AUNDELAIPMT, 1, 2) = 'FM') AND
                        (
                            INSTR(CACTUNITE_REC.AUNDELAIPMT, 'FX') != 0
                        )
                        THEN
                        RAISE ERR_PARM1;
                    END IF;
                ELSIF SCTL = 'A2144' AND CACTUNITE_REC.AUNDELAIPMT IS NOT NULL AND CACTUNITE_REC.AUNDELAIPMT = 'DECAL' AND
                    (
                        CACTUNITE_REC.AUNJOUR IS NULL OR CACTUNITE_REC.AUNJOUR < 1
                    )
                    THEN
                    RAISE ERR_PARM1;
                ELSIF SCTL = 'A2145' AND CACTUNITE_REC.AUNDELAIPMT IS NOT NULL AND CACTUNITE_REC.AUNJOUR IS NOT NULL AND
                    (
                        CACTUNITE_REC.AUNJOUR < 1 OR CACTUNITE_REC.AUNJOUR > 31
                    )
                    THEN
                    IF(CACTUNITE_REC.AUNDELAIPMT IN('FIXE', 'FMFIXE')) THEN
                        RAISE ERR_PARM1;
                    ELSIF(SUBSTR(CACTUNITE_REC.AUNDELAIPMT, 1, 2) = 'FM') AND
                        (
                            INSTR(CACTUNITE_REC.AUNDELAIPMT, 'FX') != 0
                        )
                        THEN
                        RAISE ERR_PARM1;
                    END IF;
                ELSIF SCTL = 'A2146' AND CACTUNITE_REC.AUNUNITE IS NOT NULL AND CACTUNITE_REC.ACTIDGESTION IS NOT NULL THEN
                    SELECT
                        AUN.ACTIDGESTION
                      , ACT.ACTCODE
                    INTO
                        NACTIDGESTION
                      , SERRACTCODE
                    FROM
                        ACTUNITE AUN
                      , ACTEUR ACT
                    WHERE
                        AUN.ACTID        = ACTEUR_REC.ACTID
                        AND AUN.AUNORDRE =
                        (
                            SELECT MIN(AUNORDRE) FROM ACTUNITE WHERE ACTID = ACTEUR_REC.ACTID AND AUNUNITE = CACTUNITE_REC.AUNUNITE
                        )
                        AND ACT.ACTID = AUN.ACTIDGESTION;
                    IF(NACTIDGESTION IS NOT NULL) AND
                        (
                            CACTUNITE_REC.ACTIDGESTION != NACTIDGESTION
                        )
                        THEN
                        RAISE ERR_PARM3;
                    END IF;
                    -- Verification en table interne et externe --
                    -- 5469
                ELSIF SCTL                                                                                                              = 'A2170' AND CACTUNITE_REC.AUNRECAPFAC IS NOT NULL THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE('I', CACTUNITE_REC.AUNRECAPFAC, 'RECAPFAC', NB_ELEMENT, AORDRE, AMSG, ATYPE, ADEC) = 0 THEN
                        LOK                                                                                                            := 0;
                    END IF;
                    -- 5471
                ELSIF SCTL                                                                                                               = 'A2171' AND CACTUNITE_REC.AUNDELAIPMT IS NOT NULL THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE('I', CACTUNITE_REC.AUNDELAIPMT, 'CONDREGLT', NB_ELEMENT, AORDRE, AMSG, ATYPE, ADEC) = 0 THEN
                        LOK                                                                                                             := 0;
                    END IF;
                    --5470
                ELSIF SCTL                                                                                                                   = 'A2172' AND CACTUNITE_REC.AUNQUANTIEMEFAC IS NOT NULL THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE('U', CACTUNITE_REC.AUNQUANTIEMEFAC, 'QUANTIEME', NB_ELEMENT, AORDRE, AMSG, ATYPE, ADEC) = 0 THEN
                        LOK                                                                                                                 := 0;
                    END IF;
                ELSIF SCTL                                                                                                            = 'A2173' AND CACTUNITE_REC.AUNPERIODE IS NOT NULL THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE('I', CACTUNITE_REC.AUNPERIODE, 'PERIODE', NB_ELEMENT, AORDRE, AMSG, ATYPE, ADEC) = 0 THEN
                        LOK                                                                                                          := 0;
                    END IF;
                ELSIF SCTL                                                                                                                   = 'A2174' AND CACTUNITE_REC.AUNGRPFAC IS NOT NULL THEN
                    IF PA_FUNGENCONTROLE.OCCODETABLE('I', TO_CHAR(CACTUNITE_REC.AUNGRPFAC), 'GRPFAC', NB_ELEMENT, AORDRE, AMSG, ATYPE, ADEC) = 0 THEN
                        LOK                                                                                                                 := 0;
                    END IF;
                END IF;
            EXCEPTION
            WHEN ERR_PARM1 THEN
                LOK                := 0;
                NB_ELEMENT         := NB_ELEMENT + 1;
                NORDRE             := NB_ELEMENT;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := CACTUNITE_REC.AUNUNITE;
                ATYPE(NB_ELEMENT)  := 'N';
                ADEC(NB_ELEMENT)   := NULL;
            WHEN ERR_PARM2 THEN
                LOK                := 0;
                NB_ELEMENT         := NB_ELEMENT + 1;
                NORDRE             := NB_ELEMENT;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := CACTUNITE_REC.AUNUNITE;
                ATYPE(NB_ELEMENT)  := 'N';
                ADEC(NB_ELEMENT)   := NULL;
                /*nb_element         := nb_element + 1;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element)   := cActUnite_rec.AADORDRE;
                aType(nb_element)  := 'N';
                aDec(nb_element)   := NULL;                     */
                NB_ELEMENT         := NB_ELEMENT + 1;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := TO_CHAR(DTDATEMSG, 'YYYYMMDD') ;
                ATYPE(NB_ELEMENT)  := 'D';
                ADEC(NB_ELEMENT)   := NULL;
                NB_ELEMENT         := NB_ELEMENT + 1;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := TO_CHAR(DTDATEMSG2, 'YYYYMMDD') ;
                ATYPE(NB_ELEMENT)  := 'D';
                ADEC(NB_ELEMENT)   := NULL;
            WHEN ERR_PARM3 THEN
                LOK                := 0;
                NB_ELEMENT         := NB_ELEMENT + 1;
                NORDRE             := NB_ELEMENT;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := CACTUNITE_REC.AUNUNITE;
                ATYPE(NB_ELEMENT)  := 'N';
                ADEC(NB_ELEMENT)   := NULL;
                NB_ELEMENT         := NB_ELEMENT + 1;
                AORDRE(NB_ELEMENT) := NORDRE;
                AMSG(NB_ELEMENT)   := F_STDTRIMALL(SERRACTCODE) ;
                ATYPE(NB_ELEMENT)  := 'C';
                ADEC(NB_ELEMENT)   := NULL;
            END;
        END LOOP;
        RETURN LOK;
    END;
END OCACTUNITE;
END PA_ACTCONTROLE2;
/