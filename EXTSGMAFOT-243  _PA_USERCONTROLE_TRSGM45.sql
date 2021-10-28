create or replace PACKAGE BODY pa_usercontrole
AS
    -- JJ updated 2014-02-19 JIRA 8420 and 8630 CHECK IF THE LOAN DISBURSEMENT HAS BEEN PAID OR NOT
    FUNCTION OCREGLOANDIS (NDOSID   IN DOSSIER.DOSID%TYPE,
                           NCREID   IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK             NUMBER := 1;
            NREGDTREJET     REGLEMENT.REGDTREJET%TYPE;
            NREGFLAGVALID   REGLEMENT.REGFLAGVALID%TYPE;
            NTPGCODE        DOSSIER.TPGCODE%TYPE;
        BEGIN
            SELECT REG.REGDTREJET
              INTO NREGDTREJET
              FROM REGLEMENT REG
             WHERE REG.CREID = NCREID;

            SELECT REG.REGFLAGVALID
              INTO NREGFLAGVALID
              FROM REGLEMENT REG
             WHERE REG.CREID = NCREID;

            IF NREGDTREJET IS NULL AND NREGFLAGVALID = 1
            THEN
                NOK := 0;
            END IF;

            RETURN NOK;
        END;
    END OCREGLOANDIS;

    -- JJ 2013-12-03 JIRA 8420 and 8630 CHECK IF THE LOAN DISBURSEMENT HAS BEEN PAID OR NOT
    FUNCTION F_GETDEPENSEUTICODE (NDEPID IN DEPPHASE.DEPID%TYPE)
        RETURN DEPPHASE.UTICODE%TYPE
    IS
    BEGIN
        DECLARE
            SUTICODE   DEPPHASE.UTICODE%TYPE;
        BEGIN
            BEGIN
                SELECT UTICODE
                  INTO SUTICODE
                  FROM DEPPHASE
                 WHERE     DEPID = NDEPID
                       AND PHACODE = 'VALIDE'
                       AND JALCODE = 'BPF';
            EXCEPTION
                WHEN OTHERS
                THEN
                    SUTICODE := NULL;
            END;

            RETURN SUTICODE;
        END;
    END F_GETDEPENSEUTICODE;

    FUNCTION F_ISRUBFINTYPEFINANTAXE (
        SDOSTYPEFINANCEMENT   IN DOSSIER.DOSTYPEFINANCEMENT%TYPE,
        SAUNREGLECLIENT       IN ACTUNITE.AUNREGLECLIENT%TYPE,
        STAXCODE              IN TAXE.TAXCODE%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NISTFTAXE   NUMBER := 0;
        BEGIN
            IF (SDOSTYPEFINANCEMENT = 'LSBACK')
            THEN
                IF (STAXCODE IN ('V20',
                                 '015',
                                 '006',
                                 '033',
                                 'VNI9',
                                 '009',
                                 '039',
                                 '040',
                                 '011',
                                 '014'))
                THEN
                    NISTFTAXE := 1;
                END IF;
            ELSIF (SDOSTYPEFINANCEMENT = 'ACHAT')
            THEN
                IF (STAXCODE IN ('V20',
                                 '015',
                                 '006',
                                 '033',
                                 'VNI9',
                                 '009',
                                 '039',
                                 '040',
                                 '011',
                                 '014'))
                THEN
                    NISTFTAXE := 1;
                END IF;
            END IF;

            RETURN NISTFTAXE;
        END;
    END F_ISRUBFINTYPEFINANTAXE;

    FUNCTION F_ISRUBIMMOTYPEFINANTAXE (
        SDOSTYPEFINANCEMENT   IN DOSSIER.DOSTYPEFINANCEMENT%TYPE,
        STAXCODE              IN TAXE.TAXCODE%TYPE,
        NDEPID                IN DEPPHASE.DEPID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NISTFTAXE   NUMBER := 0;
            SUTICODE    DEPPHASE.UTICODE%TYPE;
        BEGIN
            IF (SDOSTYPEFINANCEMENT = 'LSBACK')
            THEN
                IF (STAXCODE IN ('018',
                                 '024',
                                 '025',
                                 '026',
                                 '037'))
                THEN
                    NISTFTAXE := 1;
                ELSIF (STAXCODE IN ('TVA19', '005'))
                THEN
                    SUTICODE := F_GETDEPENSEUTICODE (NDEPID);

                    IF (SUTICODE = 'REPRISE')
                    THEN
                        NISTFTAXE := 1;
                    END IF;
                END IF;
            ELSIF (SDOSTYPEFINANCEMENT = 'ACHAT')
            THEN
                IF (STAXCODE IN ('008',
                                 '018',
                                 '023',
                                 '038'))
                THEN
                    NISTFTAXE := 1;
                ELSIF (STAXCODE IN ('TVA19', '005'))
                THEN
                    SUTICODE := F_GETDEPENSEUTICODE (NDEPID);

                    IF (SUTICODE = 'REPRISE')
                    THEN
                        NISTFTAXE := 1;
                    END IF;
                END IF;
            END IF;

            RETURN NISTFTAXE;
        END;
    END F_ISRUBIMMOTYPEFINANTAXE;

    FUNCTION F_ISDEPIMMOTYPEFINANTAXE (
        SDOSTYPEFINANCEMENT   IN DOSSIER.DOSTYPEFINANCEMENT%TYPE,
        NLIGNEIMMO            IN NUMBER,
        STAXCODE              IN TAXE.TAXCODE%TYPE,
        NDEPID                IN DEPPHASE.DEPID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NISTFTAXE   NUMBER := 0;
            SUTICODE    DEPPHASE.UTICODE%TYPE;
        BEGIN
            IF (NLIGNEIMMO = 1)
            THEN
                IF (SDOSTYPEFINANCEMENT = 'LSBACK')
                THEN
                    IF (STAXCODE IN ('018',
                                     '024',
                                     '025',
                                     '026',
                                     '037'))
                    THEN
                        NISTFTAXE := 1;
                    END IF;
                ELSIF (SDOSTYPEFINANCEMENT = 'ACHAT')
                THEN
                    IF (STAXCODE IN ('008',
                                     '018',
                                     '023',
                                     '038'))
                    THEN
                        NISTFTAXE := 1;
                    END IF;
                END IF;
            ELSE
                IF (STAXCODE IN ('009', '010'))
                THEN
                    NISTFTAXE := 1;
                ELSIF (STAXCODE IN ('TVA19', '005'))
                THEN
                    SUTICODE := F_GETDEPENSEUTICODE (NDEPID);

                    IF (SUTICODE = 'REPRISE')
                    THEN
                        NISTFTAXE := 1;
                    END IF;
                END IF;
            END IF;

            RETURN NISTFTAXE;
        END;
    END F_ISDEPIMMOTYPEFINANTAXE;

    FUNCTION F_CONTROLECOMGAR (NDOSID      IN DOSSIER.DOSID%TYPE,
                               NDRUORDRE   IN DOSRUBRIQUE.DRUORDRE%TYPE,
                               NDACORDRE   IN DOSRUBRIQUE.DACORDRE%TYPE,
                               NOK         IN NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NLOK              NUMBER := NOK;
            NACTIDGESTION     DOSSIER.ACTID%TYPE;
            SDOSPOOL          DOSSIER.DOSPOOL%TYPE;
            NDR1NOMBRE        NUMBER;
            NDR1NOMBREVALUE   NUMBER;
            NQP               NUMBER;
            NDR1BASE          NUMBER;
            NDR1BASEVALUE     NUMBER;
            NBASE             NUMBER;
            NPOOIDRISQUE      NUMBER;
            NPOOID            NUMBER;
            NCOUNT1           NUMBER;
            NCOUNT2           NUMBER;

            CURSOR CDOSACTEUR
            IS
                SELECT ACTID, DACORDRE, ROLCODE
                  FROM DOSACTEUR
                 WHERE DOSID = NDOSID;

            CURSOR CL1DOSRUBRIQUE
            IS
                SELECT DR1NOMBRE, DR1BASE
                  FROM L1DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND DRUORDRE = NDRUORDRE
                       AND DRUORDREASSIETTE IN
                               (SELECT DRUORDRE
                                  FROM DOSRUBRIQUE
                                 WHERE     DOSID = NDOSID
                                       AND F_ISRUBIDONFILTRE (RUBID,
                                                              'LOYASSI') =
                                           1);
        BEGIN
            NLOK := 1;

            SELECT ACTID, DOSPOOL
              INTO NACTIDGESTION, SDOSPOOL
              FROM DOSSIER
             WHERE DOSID = NDOSID;

            FOR CDOSACTEUR_REC IN CDOSACTEUR
            LOOP
                IF     NDACORDRE = CDOSACTEUR_REC.DACORDRE
                   AND (   F_PLROLEEXTERNE (CDOSACTEUR_REC.ROLCODE) =
                           'GARANT'
                        OR F_PLROLEEXTERNE (CDOSACTEUR_REC.ROLCODE) =
                           'PARTRIS')
                THEN
                    --DBMS_OUTPUT.PUT_LINE('DacOrdre = ' || TO_CHAR(cDosActeur_rec.DACORDRE));
                    --DBMS_OUTPUT.PUT_LINE('DruOrdre = ' || TO_CHAR(nDruOrdre));
                    SELECT MAX (POOID)
                      INTO NPOOIDRISQUE
                      FROM POOL
                     WHERE     DOSID = NDOSID
                           AND POOTYPE = 'RISQUE'
                           AND POODTFIN IS NULL;

                    --DBMS_OUTPUT.PUT_LINE('nPooIdRisque = ' || TO_CHAR(nPooIdRisque));
                    IF NPOOIDRISQUE IS NOT NULL
                    THEN
                        BEGIN
                            SELECT PACQP, PACBASECONVENTION
                              INTO NDR1NOMBREVALUE, NDR1BASEVALUE
                              FROM POOACTEUR
                             WHERE     POOID = NPOOIDRISQUE
                                   AND ACTID = CDOSACTEUR_REC.ACTID;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                NDR1NOMBREVALUE := 0;
                                NDR1BASEVALUE := 0;
                        END;

                        --DBMS_OUTPUT.PUT_LINE('nDr1NombreValue = ' || TO_CHAR(nDr1NombreValue));
                        --DBMS_OUTPUT.PUT_LINE('nDr1BaseValue = ' || TO_CHAR(nDr1BaseValue));
                        -- Verification du pool de financement
                        SELECT MAX (POOID)
                          INTO NPOOID
                          FROM POOL
                         WHERE     DOSID = NDOSID
                               AND POOTYPE = 'FINANCE'
                               AND POODTFIN IS NULL;

                        --DBMS_OUTPUT.PUT_LINE('nPooId = ' || TO_CHAR(nPooId));
                        IF NPOOID IS NOT NULL
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT1
                              FROM POOACTEUR
                             WHERE     POOID = NPOOID
                                   AND NVL (PACFLAGVISIBLEEXPLOIT, 0) = 1;

                            SELECT COUNT (*)
                              INTO NCOUNT2
                              FROM POOACTEUR
                             WHERE POOID = NPOOID AND ACTID != NACTIDGESTION;

                            IF NCOUNT1 != NCOUNT2 AND SDOSPOOL = 'CDF'
                            THEN
                                BEGIN
                                    SELECT PAC.PACQP, POO.POOBASE
                                      INTO NQP, NBASE
                                      FROM POOACTEUR PAC, POOL POO
                                     WHERE     POO.POOID = NPOOID
                                           AND PAC.POOID = POO.POOID
                                           AND PAC.ACTID = NACTIDGESTION;
                                EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                        NQP := 0;
                                        NBASE := 0;
                                END;

                                --DBMS_OUTPUT.PUT_LINE('nQp = ' || TO_CHAR(nQp));
                                --DBMS_OUTPUT.PUT_LINE('nBase = ' || TO_CHAR(nBase));
                                NDR1NOMBREVALUE := NDR1NOMBREVALUE * NQP;
                                NDR1BASEVALUE := NDR1BASEVALUE * NBASE;
                            --DBMS_OUTPUT.PUT_LINE('nDr1NombreValue 2 = ' || TO_CHAR(nDr1NombreValue));
                            --DBMS_OUTPUT.PUT_LINE('nDr1BaseValue 2 = ' || TO_CHAR(nDr1BaseValue));
                            END IF;
                        END IF;

                        FOR CL1DOSRUBRIQUE_REC IN CL1DOSRUBRIQUE
                        LOOP
                            --DBMS_OUTPUT.PUT_LINE('cL1DOSRUBRIQUE_rec.DR1NOMBRE 2 = ' || TO_CHAR(cL1DOSRUBRIQUE_rec.DR1NOMBRE));
                            --DBMS_OUTPUT.PUT_LINE('nDr1BaseValue 2 = ' || TO_CHAR(cL1DOSRUBRIQUE_rec.DR1BASE));
                            -- CV-12112015 NLSCUAT-152
                            -- IF NVL(NDR1NOMBREVALUE, 0) != NVL(CL1DOSRUBRIQUE_REC.DR1NOMBRE, 0) OR NVL(NDR1BASEVALUE, 0) != NVL(CL1DOSRUBRIQUE_REC.DR1BASE, 0) THEN
                            IF (  NVL (NDR1NOMBREVALUE, 0)
                                / NVL (NDR1BASEVALUE, 0)) !=
                               (  NVL (CL1DOSRUBRIQUE_REC.DR1NOMBRE, 0)
                                / NVL (CL1DOSRUBRIQUE_REC.DR1BASE, 0))
                            THEN
                                NLOK := 0;
                            END IF;
                        END LOOP;
                    END IF;
                END IF;
            END LOOP;

            RETURN NLOK;
        END;
    END F_CONTROLECOMGAR;

    /*
    // Controle sur dossier
    */
    FUNCTION OCDOSGENERALITES (NDOSID      IN DOSSIER.DOSID%TYPE,
                               SCONTROLE   IN VARCHAR2,
                               NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK              NUMBER := 1;
            SDEVCODE         DEVISE.DEVCODE%TYPE;
            NSUMDIVERS       NUMBER;
            NSUMCOMPOSANT    NUMBER;
            NDRUORDRE        DOSRUBRIQUE.DRUORDRE%TYPE;
            NMTTOTAL         NUMBER;
            NTOTAL           NUMBER;
            NTROUVE          NUMBER;
            SDOSPOOL         DOSSIER.DOSPOOL%TYPE;
            NACTID           DOSSIER.ACTID%TYPE;
            NPOOID           POOL.POOID%TYPE;
            NCOUNT           NUMBER;
            STMFFONCTION     CREVT.TMFFONCTION%TYPE;
            SPHACODE         DOSPHASE.PHACODE%TYPE;
            NASSURANCE       NUMBER;
            NDOSFLAGVRAUTO   DOSSIER.DOSFLAGVRAUTO%TYPE;
            SUGECODE         UTILISATEUR.UGECODE%TYPE := F_GETCURRENTUGECODE;
            -- CV-10062016 SCFDV-152
            NCREIDRIB        CREVT.CREID%TYPE;
            NACTIDGESTION    ACTEUR.ACTID%TYPE;

            CURSOR CDRU
            IS
                SELECT DRUORDRE, DRUMTORIGINE
                  FROM DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND DRUORDREPREC IS NULL
                       AND DRUCLASSE = 'F'
                       AND DRUTYPE = 'F';

            CURSOR CDMA
            IS
                SELECT ACTID
                  FROM DOSACTEUR
                 WHERE DOSID = NDOSID AND DACDTFIN IS NULL;

            CURSOR CRIR
            IS
                SELECT ITRID, IRUORDRE
                  FROM LKDOSRUBITRRUB
                 WHERE DOSID = NDOSID AND DRUORDRE = NDRUORDRE;

            CURSOR CTIERS
            IS
                SELECT ROLCODE, ACTID, DACORDRE
                  FROM DOSACTEUR
                 WHERE DOSID = NDOSID AND ROLCODE = 'CLIENT';

            CURSOR CACTREL
            IS
                SELECT ACTID
                  FROM DOSACTEUR
                 WHERE DOSID = NDOSID AND ROLCODE = 'CONDUCT';

            CURSOR CEVT
            IS
                SELECT FACID, ROLCODE
                  FROM FACTURE
                 WHERE CREID = NCREID AND FACIDORIGINE IS NULL;

            CURSOR CFACLIGNEEVT
            IS
                SELECT FAC.FACID, FLI.RUBID, FLI.TAXCODE
                  FROM FACTURE FAC, FACLIGNE FLI
                 WHERE     FLI.FACID = FAC.FACID
                       AND FAC.CREID = NCREID
                       AND FAC.FACIDORIGINE IS NULL;

            CURSOR CDRUTOTALE
            IS
                SELECT DAC.ACTID,
                       DRU.DRUORDRE,
                       DRU.RUBID,
                       DRU.DRUMTORIGINE,
                       DRU.DRUCLASSE
                  FROM DOSRUBRIQUE DRU, DOSACTEUR DAC
                 WHERE     DRU.DOSID = NDOSID
                       AND DAC.DOSID = DRU.DOSID
                       AND DAC.DACORDRE = DRU.DACORDRE;

            CURSOR CDOSACTEUR
            IS
                SELECT ROLCODE,
                       ACTID,
                       DACORDRE,
                       DACNBEXFAC
                  FROM DOSACTEUR
                 WHERE DOSID = NDOSID;

            CURSOR CDOSRUBCOMGAR
            IS
                SELECT DACORDRE
                  FROM DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND RUBID = F_GETRUBIDWITHRUBCODE ('COMGAR', SUGECODE);

            NDREMTBASE       NUMBER := 0;
            NMTENGAGEMENT    NUMBER;
            NDECIMAL         NUMBER;
            NECART           NUMBER;
            SDRUTXNATURE     DOSRUBRIQUE.DRUTXNATURE%TYPE;
            DTDATEMAX        DATE;
            DTDATEEVT        DATE;
            NLOGIQUE         TOPPARAM.TPALOGIQUE%TYPE;
            STPATEXT         TOPPARAM.TPATEXTE%TYPE;
            DTDTEFFET        DOSSIER.DOSDTEFFET%TYPE;

            CURSOR CFACTURE
            IS
                SELECT FACDTFACTURE
                  FROM FACTURE
                 WHERE CREID = NCREID;

            NMONTH           NUMBER;
            NYEAR            NUMBER;
            DDATE            DATE;
            SJALCODE         JALON.JALCODE%TYPE;

            -- CV-10062016 SCFDV-152
            CURSOR C1
            IS
                SELECT DAC.ACTID,
                       DAP.RIBID,
                       TMPCODE,
                       DAPTYPE
                  FROM DOSACTPAIEMENT DAP, DOSACTEUR DAC
                 WHERE     DAP.DOSID = NDOSID
                       AND DAC.DOSID = DAP.DOSID
                       AND DAC.DACORDRE = DAP.DACORDRE
                       AND F_PLROLEEXTERNE (ROLCODE) = 'CLIENT'
                       AND DACDTFIN IS NULL
                       AND DAPDTFIN IS NULL;
        BEGIN
            IF (SCONTROLE = 'DRUMT')
            THEN
                NMTTOTAL := 0;

                SELECT DEVCODE
                  INTO SDEVCODE
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                FOR CDRU_REC IN CDRU
                LOOP
                    NDRUORDRE := NVL (CDRU_REC.DRUORDRE, 0);
                    NMTTOTAL := 0;
                    NTOTAL := 0;

                    FOR CRIR_REC IN CRIR
                    LOOP
                        SELECT NVL (
                                   SUM (
                                       F_CONVDEVISE (
                                           ITR.DEVCODE,
                                           SDEVCODE,
                                           IRC.IRCPU * IRC.IRCQUANTITE,
                                           SYSDATE)),
                                   0)
                          INTO NTOTAL
                          FROM ITRRUBCOMPOSANT IRC, IMMOTRANCHE ITR
                         WHERE     ITR.ITRID = IRC.ITRID
                               AND IRC.ITRID = CRIR_REC.ITRID
                               AND IRC.IRUORDRE = CRIR_REC.IRUORDRE;

                        NMTTOTAL := NMTTOTAL + NTOTAL;
                    END LOOP;

                    IF    (CDRU_REC.DRUMTORIGINE IS NOT NULL)
                       OR (NMTTOTAL IS NOT NULL)
                    THEN
                        IF (CDRU_REC.DRUMTORIGINE != 0) OR (NMTTOTAL != 0)
                        THEN
                            IF ABS (
                                   (NVL (CDRU_REC.DRUMTORIGINE, 0) - NMTTOTAL)) >=
                               0.01
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF (SCONTROLE = 'DMA')
            THEN
                FOR CDMA_REC IN CDMA
                LOOP
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM DOCUMENTMANAGEMENT
                     WHERE     ACTID = CDMA_REC.ACTID
                           AND DMAEXTERNALREFERENCE LIKE '%FRAMEWORK%';

                    IF NCOUNT != 0
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            /* 2014-05-18, For loans with ABS issued, the application of loan cancellation must be blocked, by Son */
            ELSIF (SCONTROLE = 'TBSEC')
            THEN
                BEGIN
                    SELECT JALCODE
                      INTO SJALCODE
                      FROM DOSPHASE
                     WHERE     DOSID = NDOSID
                           AND PHACODE = 'ES'
                           AND DPHORDRE =
                               (SELECT MAX (DPHORDRE)
                                  FROM DOSPHASE
                                 WHERE DOSID = NDOSID AND PHACODE = 'ES');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        SJALCODE := NULL;
                END;

                IF SJALCODE = 'SEC'
                THEN
                    NOK := 0;
                END IF;
            /* 2014-05-18 by Son */
            ELSIF (SCONTROLE = 'PENG')
            THEN
                BEGIN
                    SELECT JALCODE
                      INTO SJALCODE
                      FROM DOSPHASE
                     WHERE     DOSID = NDOSID
                           AND PHACODE = 'ENG'
                           AND DPHORDRE =
                               (SELECT MAX (DPHORDRE)
                                  FROM DOSPHASE
                                 WHERE DOSID = NDOSID AND PHACODE = 'ENG');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        SJALCODE := NULL;
                END;

                IF SJALCODE != 'CAPP'
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'ADJ')
            THEN
                BEGIN
                    SELECT CREDTEFFET
                      INTO DTDATEEVT
                      FROM CREVT
                     WHERE     CREVT.DOSID = NDOSID
                           AND CREVT.CREID = NCREID
                           AND CREVT.TMFFONCTION = 'EVD_ADJ'
                           AND CREVT.CREDTSUP IS NULL
                           AND CREVT.UTICODESUP IS NULL;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NULL;
                END;

                BEGIN
                    SELECT DOSDTDEB
                      INTO DTDATEMAX
                      FROM DOSSIER
                     WHERE DOSID = NDOSID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NULL;
                END;

                IF DTDATEEVT IS NOT NULL AND DTDATEMAX IS NOT NULL
                THEN
                    IF TRUNC (DTDATEEVT) = TRUNC (DTDATEMAX)
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF (SCONTROLE = 'FLAT')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSSIER DOS, DOSRUBRIQUE DRU, BAREME BAR
                 WHERE     DOS.DOSID = NDOSID
                       AND DRU.DOSID = DOS.DOSID
                       AND DRU.DRUCLASSE = 'A'
                       AND DRU.DRUTYPE IN ('F', 'R')
                       AND DRU.DRUTYPEMONTAGE = 'ASSIETT'
                       AND BAR.BARID = DRU.BARID(+)
                       AND BAR.RUBID = DRU.RUBID
                       AND NVL (BAR.BARFLAGFLAT, 0) = 1
                       AND (EXISTS
                                (SELECT 1
                                   FROM DOSRUBFLUX DRF
                                  WHERE     DRF.DOSID = DRU.DOSID
                                        AND DRF.DRUORDRE = DRU.DRUORDRE
                                        AND DRF.DRFNBPERIODE != 1
                                        AND DRF.DRFPERIODE NOT IN
                                                ('001', '360')));

                IF NCOUNT != 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'DCLI')
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM DOSACTEUR
                 WHERE     DOSID = NDOSID
                       AND ROLCODE = 'CLIENT'
                       AND DACDTFIN IS NULL;

                IF NCOUNT > 1
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'CHQCO')
            THEN
                SELECT DEVCODE
                  INTO SDEVCODE
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                -- Montant sur la rubrique diverse
                SELECT SUM (DREMTBASE)
                  INTO NSUMDIVERS
                  FROM DOSRUBRIQUE DRU, DOSRUBECHEANCIER DRE, RUBRIQUE RUB
                 WHERE     DRU.DOSID = NDOSID
                       AND DRU.DRUCLASSE = 'A'
                       AND DRU.DRUTYPE = 'D'
                       AND RUB.RUBID = DRU.RUBID
                       AND RUB.RUBCODE = 'LACO'
                       AND RUB.UGECODE = SUGECODE
                       AND DRE.DOSID = DRU.DOSID
                       AND DRE.DRUORDRE = DRU.DRUORDRE;

                -- Montant sur les composants des rubriques d'immobilisations
                SELECT SUM (F_CONVDEVISE (ITR.DEVCODE,
                                          SDEVCODE,
                                          IRC.IRCPU * IRC.IRCQUANTITE,
                                          SYSDATE))
                  INTO NSUMCOMPOSANT
                  FROM DOSRUBRIQUE      DRU,
                       LKDOSRUBITRRUB   LDR,
                       ITRRUBCOMPOSANT  IRC,
                       IMMOTRANCHE      ITR
                 WHERE     DRU.DOSID = NDOSID
                       AND DRU.DRUCLASSE = 'F'
                       AND DRU.DRUTYPE = 'F'
                       AND DRU.DRUORDREPREC IS NULL
                       AND LDR.DOSID = DRU.DOSID
                       AND LDR.DRUORDRE = DRU.DRUORDRE
                       AND IRC.ITRID = LDR.ITRID
                       AND IRC.IRUORDRE = LDR.IRUORDRE
                       AND IRC.IRCPRODUIT = 'APPORT_COLLAB'
                       AND ITR.ITRID = LDR.ITRID;

                IF (NSUMDIVERS IS NOT NULL) OR (NSUMCOMPOSANT IS NOT NULL)
                THEN
                    IF    (NSUMDIVERS IS NULL)
                       OR (NSUMCOMPOSANT IS NULL)
                       OR (NSUMDIVERS + NSUMCOMPOSANT != 0)
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            --hn 15/04/03 verifie que le tiers ayant un ROLCODE = 'CONDUCT' est en relation "est salarie de" avec au moins un des clients preneurs au dossier.
            ELSIF (SCONTROLE = 'DTIER')
            THEN
                NTROUVE := NULL;

                FOR CACTREL_REC IN CACTREL
                LOOP
                    FOR CTIERS_REC IN CTIERS
                    LOOP
                        SELECT COUNT (*)
                          INTO NTROUVE
                          FROM ACTRELATION ATR
                         WHERE     ATR.ACTID = CACTREL_REC.ACTID
                               AND ATR.ACTIDRELATION = CTIERS_REC.ACTID
                               AND ATR.TRECODE = 'SALARIE';

                        IF NTROUVE = 1
                        THEN
                            EXIT;
                        END IF;
                    END LOOP;

                    IF NTROUVE = 0
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            ELSIF (SCONTROLE = 'DTFAC')
            THEN
                FOR CFACTURE_REC IN CFACTURE
                LOOP
                    SELECT TO_NUMBER (TO_CHAR (SYSDATE, 'MM'))
                      INTO NMONTH
                      FROM DUAL;

                    SELECT TO_NUMBER (TO_CHAR (SYSDATE, 'YYYY'))
                      INTO NYEAR
                      FROM DUAL;

                    DDATE :=
                        TO_DATE (
                               '01/'
                            || TO_CHAR (NMONTH, '99')
                            || '/'
                            || TO_CHAR (NYEAR, '9999'),
                            'DD/MM/YYYY');

                    IF CFACTURE_REC.FACDTFACTURE < DDATE
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'ROLE'
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM DOSACTEUR
                 WHERE     DOSID = NDOSID
                       AND (ROLCODE = 'COMMERC' OR ROLCODE = 'AGENCE');

                IF NCOUNT = 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'NORE'
            THEN
                SELECT ACTID, DOSPOOL
                  INTO NACTID, SDOSPOOL
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                IF SDOSPOOL = 'PART'
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM POOACTEUR
                     WHERE     POOID = (SELECT MAX (POOID)
                                          FROM POOL
                                         WHERE DOSID = NDOSID)
                           AND ACTID = NACTID
                           AND PACFLAGCDF = 0;

                    IF NCOUNT > 0
                    THEN
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM DOSRUBRIQUE
                         WHERE     DOSID = NDOSID
                               AND DRUTYPE = 'F'
                               AND DRUCLASSE = 'A'
                               AND RUBID IN (SELECT RUBID
                                               FROM RUBACCES
                                              WHERE RACACCES = 'ASSUR');

                        IF NCOUNT > 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END IF;
            ELSIF SCONTROLE IN ('COMG', 'COMR', 'NBEX')
            THEN
                IF SCONTROLE = 'COMR'
                THEN
                    FOR CDOSRUBCOMGAR_REC IN CDOSRUBCOMGAR
                    LOOP
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM DOSACTEUR
                         WHERE     DOSID = NDOSID
                               AND DACORDRE = CDOSRUBCOMGAR_REC.DACORDRE
                               AND ROLCODE IN ('GARANBP', 'GARANCE');

                        IF NCOUNT = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END LOOP;
                END IF;

                FOR CDOSACTEUR_REC IN CDOSACTEUR
                LOOP
                    IF SCONTROLE = 'COMG'
                    THEN
                        IF CDOSACTEUR_REC.ROLCODE IN ('GARANBP', 'GARANCE')
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM DOSRUBRIQUE
                             WHERE     DOSID = NDOSID
                                   AND DACORDRE = CDOSACTEUR_REC.DACORDRE
                                   AND RUBID IN
                                           (F_GETRUBIDWITHRUBCODE ('COMGAR',
                                                                   SUGECODE),
                                            F_GETRUBIDWITHRUBCODE ('CGARSC',
                                                                   SUGECODE));

                            IF NCOUNT = 0
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    ELSIF     SCONTROLE = 'NBEX'
                          AND (   F_PLROLEEXTERNE (CDOSACTEUR_REC.ROLCODE) =
                                  'CLIENT'
                               OR F_PLROLEEXTERNE (CDOSACTEUR_REC.ROLCODE) =
                                  'PARTEN')
                    THEN
                        IF NVL (CDOSACTEUR_REC.DACNBEXFAC, 0) = 0
                        THEN
                            BEGIN
                                SELECT DRU.DRUTXNATURE
                                  INTO SDRUTXNATURE
                                  FROM DOSRUBRIQUE DRU
                                 WHERE     DRU.DOSID = NDOSID
                                       AND DRU.DACORDRE =
                                           CDOSACTEUR_REC.DACORDRE
                                       AND DRU.DRUORDRE =
                                           (SELECT MAX (DRUORDRE)
                                              FROM DOSRUBRIQUE
                                             WHERE     DOSID = NDOSID
                                                   AND DACORDRE =
                                                       CDOSACTEUR_REC.DACORDRE
                                                   AND F_ISRUBIDONFILTRE (
                                                           RUBID,
                                                           'LOYASSI') =
                                                       1);
                            EXCEPTION
                                WHEN OTHERS
                                THEN
                                    SDRUTXNATURE := 'TF';
                            END;

                            IF SDRUTXNATURE != 'TF'
                            THEN
                                SELECT DOSPOOL
                                  INTO SDOSPOOL
                                  FROM DOSSIER
                                 WHERE DOSID = NDOSID;

                                IF SDOSPOOL = 'PART'
                                THEN
                                    IF F_PLROLEEXTERNE (
                                           CDOSACTEUR_REC.ROLCODE) =
                                       'PARTEN'
                                    THEN
                                        SELECT MAX (POOID)
                                          INTO NPOOID
                                          FROM POOL
                                         WHERE     DOSID = NDOSID
                                               AND POOTYPE = 'FINANCE'
                                               AND POODTFIN IS NULL;

                                        IF NPOOID IS NOT NULL
                                        THEN
                                            SELECT COUNT (1)
                                              INTO NCOUNT
                                              FROM POOACTEUR
                                             WHERE     POOID = NPOOID
                                                   AND CDOSACTEUR_REC.ACTID =
                                                       ACTID
                                                   AND NVL (PACFLAGCDF, 0) =
                                                       1;

                                            IF NCOUNT = 1
                                            THEN
                                                NOK := 0;
                                            END IF;
                                        END IF;
                                    END IF;
                                ELSIF F_PLROLEEXTERNE (
                                          CDOSACTEUR_REC.ROLCODE) =
                                      'CLIENT'
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE IN ('VENT', 'RULI')
            THEN
                SELECT TMFFONCTION
                  INTO STMFFONCTION
                  FROM CREVT
                 WHERE CREID = NCREID;

                IF     SCONTROLE = 'VENT'
                   AND STMFFONCTION IN ('EVD_VENTE', 'EVD_CPART')
                THEN
                    FOR CEVT_REC IN CEVT
                    LOOP
                        IF     CEVT_REC.ROLCODE != 'CLIENT'
                           AND CEVT_REC.ROLCODE != 'RACHAT'
                        THEN
                            NOK := 0;
                        END IF;
                    END LOOP;
                ELSIF SCONTROLE = 'RULI' AND STMFFONCTION IN ('EVD_ADJ')
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM LKDRUCRE
                     WHERE     CREID = NCREID
                           AND DOSID = NDOSID
                           AND DRUORDRE IN
                                   (SELECT DRUORDRE
                                      FROM DOSRUBRIQUE
                                     WHERE     DOSID = NDOSID
                                           AND DRUORDREMAITRE IS NOT NULL);

                    IF NCOUNT = 0
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'PARA'
            THEN
                SELECT DOSPOOL
                  INTO SDOSPOOL
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                IF SDOSPOOL = 'CDF'
                THEN
                    SELECT MAX (POOID)
                      INTO NPOOID
                      FROM POOL
                     WHERE DOSID = NDOSID AND POODTFIN IS NULL;

                    -- Parcour des partenaires --
                    FOR CDRUTOTALE_REC IN CDRUTOTALE
                    LOOP
                        IF CDRUTOTALE_REC.DRUCLASSE = 'A'
                        THEN
                            SELECT F_ISRUBIDONFILTRE (CDRUTOTALE_REC.RUBID,
                                                      'ASSUR')
                              INTO NASSURANCE
                              FROM DUAL;

                            IF NASSURANCE = 1
                            THEN
                                SELECT COUNT (1)
                                  INTO NCOUNT
                                  FROM LKDOSRUBPOOACT
                                 WHERE     DOSID = NDOSID
                                       AND DRUORDRE = CDRUTOTALE_REC.DRUORDRE
                                       AND POOID = NPOOID;

                                IF NCOUNT > 0
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END LOOP;
                END IF;
            ELSIF SCONTROLE = 'MT10'
            THEN
                SELECT DEVCODE
                  INTO SDEVCODE
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                NDECIMAL := F_NBDECIMALDANSDEVISE (SDEVCODE);
                NMTENGAGEMENT := F_PLENGAGEMENTTOTAL (NDOSID);
                DBMS_OUTPUT.PUT_LINE (
                    'nMtEngagement  = ' || TO_CHAR (NMTENGAGEMENT));
                NECART := ROUND (NMTENGAGEMENT * 10 / 100, NDECIMAL);
                DBMS_OUTPUT.PUT_LINE ('necart = ' || TO_CHAR (NECART));

                FOR CDRUTOTALE_REC IN CDRUTOTALE
                LOOP
                    IF CDRUTOTALE_REC.DRUCLASSE = 'A'
                    THEN
                        SELECT MAX (DREMTBASE)
                          INTO NDREMTBASE
                          FROM DOSRUBECHEANCIER
                         WHERE     DOSID = NDOSID
                               AND DRUORDRE = CDRUTOTALE_REC.DRUORDRE
                               AND DRETYPE = 'LOYER';

                        DBMS_OUTPUT.PUT_LINE (
                            'nDreMtBase = ' || TO_CHAR (NDREMTBASE));

                        IF NVL (NDREMTBASE, 0) > NVL (NECART, 0)
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'FORM'
            THEN
                SELECT NVL (DOSFLAGVRAUTO, 0)
                  INTO NDOSFLAGVRAUTO
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                IF NDOSFLAGVRAUTO = 1
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM ADMINISTRATIF
                     WHERE     DOSID = NDOSID
                           AND FORID IN
                                   (SELECT FRM.FORID
                                      FROM FORMALITE FRM, TUSPARAM TUS
                                     WHERE     TUS.TUPCODE = FRM.FORCODE
                                           AND FRM.UGECODE = SUGECODE
                                           AND TUS.TUSNOM = 'FORMCONTROLE');

                    IF NCOUNT = 0
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'DGTA' AND NCREID IS NOT NULL
            THEN
                FOR CFACLIGNE_REC IN CFACLIGNEEVT
                LOOP
                    IF F_ISRUBIDONFILTRE (CFACLIGNE_REC.RUBID, 'AVPR') = 1
                    THEN
                        IF F_GETTAXCODEINTAXTYPE (CFACLIGNE_REC.TAXCODE,
                                                  'EXO') =
                           0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'REMB'
            THEN
                SELECT MAX (CREDTEFFET)
                  INTO DTDATEMAX
                  FROM CREVT
                 WHERE     CREVT.DOSID = NDOSID
                       AND CREVT.CREID != NCREID
                       AND CREVT.TMFFONCTION = 'EVD_REMBSUBV'
                       AND CREVT.CREDTSUP IS NULL
                       AND CREVT.UTICODESUP IS NULL;

                SELECT CREDTEFFET
                  INTO DTDATEEVT
                  FROM CREVT
                 WHERE     CREVT.DOSID = NDOSID
                       AND CREVT.CREID = NCREID
                       AND CREVT.TMFFONCTION = 'EVD_REMBSUBV'
                       AND CREVT.CREDTSUP IS NULL
                       AND CREVT.UTICODESUP IS NULL;

                IF DTDATEMAX IS NOT NULL AND DTDATEEVT < DTDATEMAX
                THEN
                    NOK := 0;
                END IF;
            --ALA 092007
            ELSIF SCONTROLE = 'DTEFF'
            THEN
                PA_COMMON.S_TPALOGIQUE ('DOSSIMPLE', 'DTEFFET', NLOGIQUE);

                IF NLOGIQUE = 1
                THEN
                    PA_COMMON.S_TPALOGIQUE ('DOSSIMPLE', 'TYPEESS', NLOGIQUE);
                    PA_COMMON.S_TPATEXTE ('DOSSIMPLE', 'TYPEESS', STPATEXT);

                    IF NLOGIQUE = 1 AND STPATEXT = 'CBM'
                    THEN
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM LKTTPTACTPG LTT, DOSSIER DOS
                         WHERE     DOS.DOSID = NDOSID
                               AND LTT.TTRNOM = 'DTPREVISIONNEL'
                               AND LTT.TACCODE = DOS.TACCODE
                               AND LTT.TPGCODE = DOS.TPGCODE
                               AND LTT.TTPCODE =
                                   TO_CHAR (DOS.DOSDTEFFET, 'dd/mm');

                        IF NCOUNT = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END IF;
            --ALA 092007
            ELSIF SCONTROLE = 'DTRUB'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSRUBRIQUE DRU, RUBRIQUE RUB
                 WHERE     DRU.DOSID = NDOSID
                       AND RUB.RUBID = DRU.RUBID
                       AND RUB.RUBCODE = 'PARFOU'
                       AND RUB.UGECODE = SUGECODE;

                IF NCOUNT > 0
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM DOSRUBRIQUE DRU, RUBRIQUE RUB, DOSSIER DOS
                     WHERE     DRU.DOSID = NDOSID
                           AND RUB.RUBID = DRU.RUBID
                           AND RUB.RUBCODE = 'PARFOU'
                           AND RUB.UGECODE = SUGECODE
                           AND DOS.DOSID = NDOSID
                           AND DRU.DRUDTDEB = NVL (DOSDTDEB, DOSDTEFFET);

                    IF NCOUNT = 0
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            --ALA 092007
            ELSIF SCONTROLE = 'DTCOM'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSRUBRIQUE DRU, RUBRIQUE RUB
                 WHERE     DRU.DOSID = NDOSID
                       AND RUB.RUBID = DRU.RUBID
                       AND RUB.RUBCODE = 'COMAPPS'
                       AND RUB.UGECODE = SUGECODE;

                IF NCOUNT > 0
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM DOSRUBRIQUE DRU, RUBRIQUE RUB, DOSSIER DOS
                     WHERE     DRU.DOSID = NDOSID
                           AND RUB.RUBID = DRU.RUBID
                           AND RUB.RUBCODE = 'COMAPPS'
                           AND RUB.UGECODE = SUGECODE
                           AND DOS.DOSID = NDOSID
                           AND DRU.DRUDTDEB = NVL (DOSDTDEB, DOSDTEFFET);

                    IF NCOUNT = 0
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            --ALA 092007
            ELSIF SCONTROLE = 'NC'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSRUBRIQUE DRU
                 WHERE DOSID = NDOSID AND DRUCP IS NOT NULL;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            --FIN ALA 092007
            ELSIF SCONTROLE = ('RB')
            THEN
                SELECT DECODE (COUNT (*), 0, 1, 0)
                  INTO NOK
                  FROM DOSSIER DOS
                 WHERE     (DOS.TPGCODE LIKE 'B%' OR DOS.TPGCODE LIKE 'G%')
                       AND DOS.DOSID = NDOSID;
            -- CV-10062016 SCFDV-152
            ELSIF sControle = 'ACRIB'
            THEN
                SELECT ACTID
                  INTO NACTIDGESTION
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                FOR C1R IN C1
                LOOP
                    IF F_ISTMPRIBSTE (C1R.DAPTYPE, C1R.TMPCODE) = 1
                    THEN
                        NACTID := NACTIDGESTION;
                    ELSE
                        NACTID := C1R.ACTID;
                    END IF;

                    BEGIN
                        SELECT CREID
                          INTO NCREIDRIB
                          FROM ACTRIB
                         WHERE ACTID = NACTID AND RIBIDREMPLACE = C1R.RIBID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NCREIDRIB := NULL;
                    END;

                    IF NCREIDRIB IS NOT NULL
                    THEN
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM CREVT
                         WHERE     CREID = NCREIDRIB
                               AND TMFFONCTION LIKE 'EVACT_RIBREM%'
                               AND (   CREDTVALID IS NULL
                                    OR CREDTSUP IS NOT NULL);

                        DBMS_OUTPUT.PUT_LINE (
                            'NCREIDRIB ' || NCREIDRIB || ' NCOUNT ' || NCOUNT);

                        IF NCOUNT > 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCDOSGENERALITES;

    FUNCTION OCDOSRUBGENERALITES (NDOSID      IN DOSSIER.DOSID%TYPE,
                                  SCONTROLE   IN VARCHAR2,
                                  NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK                 NUMBER := 1;
            STYPEFILTRE         RUBACCES.RACACCES%TYPE;
            NRUBIDMAINTENANCE   RUBRIQUE.RUBID%TYPE;
            NASSUR              NUMBER := 0;
            SROLCODE            ROLE.ROLCODE%TYPE;
            NCOUNT              NUMBER;
            NDRUORDRE           DOSRUBRIQUE.DRUORDRE%TYPE;
            NRUBIDCOMAPPS       RUBRIQUE.RUBID%TYPE;
            NRUBIDCOMAPP        RUBRIQUE.RUBID%TYPE;
            SUGECODE            UTILISATEUR.UGECODE%TYPE
                                    := F_GETCURRENTUGECODE;

            CURSOR CDRUREVERS
            IS
                SELECT RUBID,
                       DRUORDRE,
                       DACORDREREVERSEMENT,
                       DRUTXREVERSEMENT,
                       TAXCODEREVERSEMENT
                  FROM DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND (   F_ISRUBIDONFILTRE (RUBID, 'REVERS') = 1
                            OR F_ISRUBIDONFILTRE (RUBID, 'REVFACT') = 1)
                       AND DRUCLASSE = 'A'
                       AND DRUTYPE = 'F';

            CURSOR CDOSRUBRIQUEREDEVANCE
            IS
                SELECT DRUORDRE,
                       DACORDRE,
                       DRUCLASSE,
                       RUBID
                  FROM DOSRUBRIQUE
                 WHERE DOSID = NDOSID AND DRUTYPE = 'F';

            CURSOR CDOSRUBRIQUE
            IS
                SELECT DRUORDRE,
                       DACORDRE,
                       RUBID,
                       DRUFLAGTEG,
                       DRUCLASSE
                  FROM DOSRUBRIQUE
                 WHERE DOSID = NDOSID;

            CURSOR CLKDOSRUBPOOACT
            IS
                SELECT DPAQP
                  FROM LKDOSRUBPOOACT
                 WHERE DOSID = NDOSID AND DRUORDRE = NDRUORDRE;

            -- CV-10022010 CFS40204
            CURSOR CDOSRUBFLUX
            IS
                  SELECT *
                    FROM DOSRUBFLUX
                   WHERE DOSID = NDOSID AND DRUORDRE = NDRUORDRE
                ORDER BY DRFORDRE;
        BEGIN
            IF (SCONTROLE IN ('VEPR', 'VEAS'))
            THEN
                FOR CDRUREVERS_REC IN CDRUREVERS
                LOOP
                    IF CDRUREVERS_REC.DACORDREREVERSEMENT IS NOT NULL
                    THEN
                        IF SCONTROLE = 'VEPR'
                        THEN
                            SELECT COUNT (1)
                              INTO NCOUNT
                              FROM RUBRIQUE
                             WHERE     RUBID = CDRUREVERS_REC.RUBID
                                   AND RUBCODE LIKE 'MAIN%'
                                   AND UGECODE = SUGECODE;

                            IF NCOUNT > 0
                            THEN
                                SELECT ROLCODE
                                  INTO SROLCODE
                                  FROM DOSACTEUR
                                 WHERE     DACORDRE =
                                           CDRUREVERS_REC.DACORDREREVERSEMENT
                                       AND DOSID = NDOSID;

                                IF SROLCODE != 'PRESTA'
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            END IF;
                        ELSIF SCONTROLE = 'VEAS'
                        THEN
                            SELECT COUNT (1)
                              INTO NASSUR
                              FROM RUBACCES
                             WHERE     RUBID = CDRUREVERS_REC.RUBID
                                   AND RACACCES = 'ASSUR';

                            IF NASSUR > 0
                            THEN
                                SELECT ROLCODE
                                  INTO SROLCODE
                                  FROM DOSACTEUR
                                 WHERE     DACORDRE =
                                           CDRUREVERS_REC.DACORDREREVERSEMENT
                                       AND DOSID = NDOSID;

                                IF SROLCODE != 'ASSUR'
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'MAIT'
            THEN
                BEGIN
                    SELECT F_GETRUBIDWITHRUBCODE ('MAINTV', SUGECODE)
                      INTO NRUBIDMAINTENANCE
                      FROM DUAL;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NRUBIDMAINTENANCE := NULL;
                END;

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND RUBID = NRUBIDMAINTENANCE
                       AND TAXCODE NOT IN ('MAINTVD', 'MAINTV');

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'ROLF'
            THEN
                FOR CDOSRUBRIQUEREDEVANCE_REC IN CDOSRUBRIQUEREDEVANCE
                LOOP
                    SELECT ROLCODE
                      INTO SROLCODE
                      FROM DOSACTEUR
                     WHERE     DOSID = NDOSID
                           AND DACORDRE = CDOSRUBRIQUEREDEVANCE_REC.DACORDRE;

                    IF SROLCODE IN ('ASSUR', 'SCASS', 'AGENCE')
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE IN ('QPN',
                                'PTRI',
                                'QPNE',
                                'EVQP')
            THEN
                FOR CDOSRUBRIQUE_REC IN CDOSRUBRIQUE
                LOOP
                    IF     SCONTROLE = 'PTRI'
                       AND CDOSRUBRIQUE_REC.DRUCLASSE = 'A'
                    THEN
                        IF     F_ISRUBIDONFILTRE (CDOSRUBRIQUE_REC.RUBID,
                                                  'PARTTRI') =
                               1
                           AND NVL (CDOSRUBRIQUE_REC.DRUFLAGTEG, 0) = 0
                        THEN
                            NOK := 0;
                        ELSIF     F_ISRUBIDONFILTRE (CDOSRUBRIQUE_REC.RUBID,
                                                     'PARTTRI') !=
                                  1
                              AND NVL (CDOSRUBRIQUE_REC.DRUFLAGTEG, 0) = 1
                        THEN
                            NOK := 0;
                        END IF;
                    ELSIF     SCONTROLE IN ('QPNE', 'EVQP')
                          AND F_ISRUBIDONFILTRE (CDOSRUBRIQUE_REC.RUBID,
                                                 'COMGA') =
                              1
                    THEN
                        NDRUORDRE := CDOSRUBRIQUE_REC.DRUORDRE;
                        NOK :=
                            F_CONTROLECOMGAR (NDOSID,
                                              NDRUORDRE,
                                              CDOSRUBRIQUE_REC.DACORDRE,
                                              NOK);
                    ELSIF SCONTROLE = 'QPN'
                    THEN
                        NDRUORDRE := CDOSRUBRIQUE_REC.DRUORDRE;

                        FOR CLKDOSRUBPOOACT_REC IN CLKDOSRUBPOOACT
                        LOOP
                            IF CLKDOSRUBPOOACT_REC.DPAQP IS NULL
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'APPO'
            THEN
                BEGIN
                    SELECT F_GETRUBIDWITHRUBCODE ('COMAPP', SUGECODE)
                      INTO NRUBIDCOMAPP
                      FROM DUAL;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NRUBIDCOMAPP := NULL;
                END;

                BEGIN
                    SELECT F_GETRUBIDWITHRUBCODE ('COMAPPS', SUGECODE)
                      INTO NRUBIDCOMAPPS
                      FROM DUAL;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NRUBIDCOMAPPS := NULL;
                END;

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSRUBRIQUE
                 WHERE     DOSID = NDOSID
                       AND RUBID IN (NRUBIDCOMAPPS, NRUBIDCOMAPP);

                IF NCOUNT > 0
                THEN
                    FOR CDOSRUBRIQUE_REC IN CDOSRUBRIQUE
                    LOOP
                        IF    CDOSRUBRIQUE_REC.RUBID = NRUBIDCOMAPPS
                           OR CDOSRUBRIQUE_REC.RUBID = NRUBIDCOMAPP
                        THEN
                            SELECT ROLCODE
                              INTO SROLCODE
                              FROM DOSACTEUR
                             WHERE     DOSID = NDOSID
                                   AND DACORDRE = CDOSRUBRIQUE_REC.DACORDRE;

                            IF SROLCODE != 'APPORT'
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END LOOP;
                END IF;
            -- CV-10022010 CFS40204
            ELSIF SCONTROLE IN ('GKFLX', 'GK1DT', 'GKTXA')
            THEN
                FOR CDOSRUBRIQUEREDEVANCE_REC IN CDOSRUBRIQUEREDEVANCE
                LOOP
                    IF     CDOSRUBRIQUEREDEVANCE_REC.DRUCLASSE = 'A'
                       AND F_ISRUBIDONFILTRE (
                               CDOSRUBRIQUEREDEVANCE_REC.RUBID,
                               'GAPRISK') =
                           1
                    THEN
                        NDRUORDRE := CDOSRUBRIQUEREDEVANCE_REC.DRUORDRE;

                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM DOSRUBRIQUE
                         WHERE     DOSID = NDOSID
                               AND DRUCLASSE = 'A'
                               AND DRUTYPE = 'B'
                               AND DRUORDREMAITRE = NDRUORDRE;

                        IF NCOUNT = 0
                        THEN
                            -- Pas de rub passive accessoire esclave
                            NOK := 0;
                        ELSE
                            -- MSG12385
                            IF SCONTROLE = 'GKFLX'
                            THEN
                                FOR CDOSRUBFLUX_REC IN CDOSRUBFLUX
                                LOOP
                                    SELECT COUNT (*)
                                      INTO NCOUNT
                                      FROM DOSRUBRIQUE
                                     WHERE     DRUCLASSE = 'A'
                                           AND DRUTYPE = 'B'
                                           AND DOSID = NDOSID
                                           AND DRUORDREMAITRE = NDRUORDRE
                                           AND EXISTS
                                                   (SELECT 1
                                                      FROM DOSRUBFLUX
                                                     WHERE     DOSID =
                                                               DOSRUBRIQUE.DOSID
                                                           AND DRUORDRE =
                                                               DOSRUBRIQUE.DRUORDRE
                                                           AND DRFPERIODE =
                                                               CDOSRUBFLUX_REC.DRFPERIODE
                                                           AND DRFDTDEB =
                                                               CDOSRUBFLUX_REC.DRFDTDEB
                                                           AND DRFDTFIN =
                                                               CDOSRUBFLUX_REC.DRFDTFIN
                                                           AND DRFMULTIPLE =
                                                               CDOSRUBFLUX_REC.DRFMULTIPLE
                                                           AND DRFNBPERIODE =
                                                               CDOSRUBFLUX_REC.DRFNBPERIODE
                                                           AND DRFPERCEPTION =
                                                               CDOSRUBFLUX_REC.DRFPERCEPTION
                                                           AND DRFTXASSIETTE =
                                                               CDOSRUBFLUX_REC.DRFTXASSIETTE);

                                    IF NCOUNT = 0
                                    THEN
                                        -- Pas rub passive accessoire esclave tel que le contenu de DOSRUBFLUX sur la passive soit equivalent a celui de la redevance.
                                        NOK := 0;
                                        EXIT;
                                    END IF;
                                END LOOP;
                            -- MSG12386
                            ELSIF SCONTROLE = 'GK1DT'
                            THEN
                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM DOSRUBFLUX
                                 WHERE     DRFDTDEB = DRFDTFIN
                                       AND (DOSID, DRUORDRE) =
                                           (SELECT DOSID, DRUORDRE
                                              FROM DOSRUBRIQUE
                                             WHERE     DRUCLASSE = 'A'
                                                   AND DRUTYPE = 'B'
                                                   AND DOSID = NDOSID
                                                   AND DRUORDREMAITRE =
                                                       NDRUORDRE);

                                IF NCOUNT != 1
                                THEN
                                    -- Soit Pas de flux avec 1 jour soit pls flux de rub passive accessoire esclave
                                    NOK := 0;
                                END IF;
                            -- MSG12387
                            ELSIF SCONTROLE = 'GKTXA'
                            THEN
                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM DOSRUBRIQUE
                                 WHERE     DRUCLASSE = 'A'
                                       AND DRUTYPE = 'B'
                                       AND DOSID = NDOSID
                                       AND DRUORDREMAITRE = NDRUORDRE
                                       AND EXISTS
                                               (SELECT 1
                                                  FROM DOSRUBFLUX
                                                 WHERE     DOSID =
                                                           DOSRUBRIQUE.DOSID
                                                       AND DRUORDRE =
                                                           DOSRUBRIQUE.DRUORDRE
                                                       AND NVL (
                                                               DRFTXASSIETTE,
                                                               0) =
                                                           0);

                                IF NCOUNT > 0
                                THEN
                                    -- Il existe rub passive accessoire esclase avec le tx d assiette du flux = null/zero
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCDOSRUBGENERALITES;

    FUNCTION OCDOSSPECITALIE (NDOSID      IN DOSSIER.DOSID%TYPE,
                              SCONTROLE   IN VARCHAR2,
                              NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK                   NUMBER := 1;
            NDACORDRE             DOSACTEUR.DACORDRE%TYPE;
            SDOSTYPEFINANCEMENT   DOSSIER.DOSTYPEFINANCEMENT%TYPE;

            CURSOR CDRU
            IS
                SELECT DISTINCT DRU.TAXCODE, AUN.AUNREGLECLIENT, DRU.DRUDTDEB
                  FROM DOSRUBRIQUE  DRU,
                       DOSACTEUR    DAC,
                       DOSACTUNITE  DAU,
                       ACTUNITE     AUN
                 WHERE     DRU.DOSID = NDOSID
                       AND DRU.TAXCODE IS NOT NULL
                       AND DAC.DOSID = DRU.DOSID
                       AND DAC.DACORDRE = DRU.DACORDRE
                       AND DAU.DOSID = DAC.DOSID
                       AND DAU.DACORDRE = DAC.DACORDRE
                       AND DAU.ACTID = DAC.ACTID
                       AND DAU.DAUDTFIN IS NULL
                       AND AUN.ACTID = DAU.ACTID
                       AND AUN.AUNORDRE = DAU.AUNORDRE
                       AND AUN.AUNREGLECLIENT IS NOT NULL;

            CURSOR CDAC
            IS
                SELECT DISTINCT DAC.DACORDRE
                  FROM DOSRUBRIQUE  DRU,
                       DOSACTEUR    DAC,
                       DOSACTUNITE  DAU,
                       ACTUNITE     AUN
                 WHERE     DRU.DOSID = NDOSID
                       AND DAC.DOSID = DRU.DOSID
                       AND DAC.DACORDRE = DRU.DACORDRE
                       AND DAU.DOSID = DAC.DOSID
                       AND DAU.DACORDRE = DAC.DACORDRE
                       AND DAU.ACTID = DAC.ACTID
                       AND DAU.DAUDTFIN IS NULL
                       AND AUN.ACTID = DAU.ACTID
                       AND AUN.AUNORDRE = DAU.AUNORDRE
                       AND AUN.AUNREGLECLIENT = 'VNI72';

            CURSOR CDRUDAC
            IS
                  SELECT DRUCLASSE,
                         DRUTYPE,
                         RUBID,
                         COUNT (DISTINCT TAXCODE)
                    FROM DOSRUBRIQUE
                   WHERE DOSID = NDOSID AND DACORDRE = NDACORDRE
                GROUP BY DRUCLASSE, DRUTYPE, RUBID
                  HAVING COUNT (DISTINCT TAXCODE) = 1;

            CURSOR CTFTAX
            IS
                SELECT DISTINCT DRU.TAXCODE, AUN.AUNREGLECLIENT
                  FROM DOSRUBRIQUE  DRU,
                       DOSACTEUR    DAC,
                       DOSACTUNITE  DAU,
                       ACTUNITE     AUN
                 WHERE     DRU.DOSID = NDOSID
                       AND DRU.TAXCODE IS NOT NULL
                       AND DAC.DOSID(+) = DRU.DOSID
                       AND DAC.DACORDRE(+) = DRU.DACORDRE
                       AND DAU.DOSID(+) = DRU.DOSID
                       AND DAU.DACORDRE(+) = DRU.DACORDRE
                       AND DAU.DAUDTFIN IS NULL
                       AND AUN.ACTID(+) = DAU.ACTID
                       AND AUN.AUNORDRE(+) = DAU.AUNORDRE;

            CURSOR CFLITAX
            IS
                SELECT DISTINCT TAXCODE
                  FROM FACTURE FAC, CRO, FACLIGNE FLI
                 WHERE     FAC.CREID = NCREID
                       AND FAC.FACIDORIGINE IS NULL
                       AND CRO.FACID = FAC.FACID
                       AND CRO.CREID = NCREID
                       AND CRO.TCRCODE = 'FACVENT'
                       AND FLI.FACID = FAC.FACID
                       AND FLI.TAXCODE IS NOT NULL;

            CURSOR CDLITAX
            IS
                SELECT DISTINCT DLI.TAXCODE
                  FROM LKDOSRUBITRRUB LDR, ITRRUBRIQUE IRU, DEPLIGNE DLI
                 WHERE     LDR.DOSID = NDOSID
                       AND IRU.ITRID = LDR.ITRID
                       AND IRU.IRUORDRE = LDR.IRUORDRE
                       AND DLI.DEPID = IRU.DEPID
                       AND DLI.DLIORDRE = IRU.DLIORDRE
                       AND DLI.TAXCODE IS NOT NULL;
        BEGIN
            IF (SCONTROLE = 'RFTAX')
            THEN
                FOR CDRU_REC IN CDRU
                LOOP
                    IF (CDRU_REC.AUNREGLECLIENT = 'V20')
                    THEN
                        IF (CDRU_REC.DRUDTDEB <=
                            TO_DATE ('01/08/2006', 'DD/MM/YYYY'))
                        THEN
                            IF CDRU_REC.TAXCODE NOT IN ('015', 'V20')
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        ELSE
                            IF (CDRU_REC.TAXCODE != 'V20')
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END IF;
                    ELSIF (CDRU_REC.AUNREGLECLIENT = '006')
                    THEN
                        IF (CDRU_REC.TAXCODE != '006')
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    ELSIF (CDRU_REC.AUNREGLECLIENT = '033')
                    THEN
                        IF (CDRU_REC.TAXCODE != '033')
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    ELSIF (CDRU_REC.AUNREGLECLIENT = 'VNI9')
                    THEN
                        IF (CDRU_REC.TAXCODE != 'VNI9')
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    ELSIF (CDRU_REC.AUNREGLECLIENT = '015')
                    THEN
                        IF (CDRU_REC.TAXCODE != '015')
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    ELSIF (CDRU_REC.AUNREGLECLIENT = '046')
                    THEN
                        IF (CDRU_REC.TAXCODE NOT IN ('011',
                                                     '014',
                                                     '015',
                                                     '033',
                                                     '039',
                                                     '040',
                                                     'V20'))
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF (SCONTROLE = '033')
            THEN
                FOR CDAC_REC IN CDAC
                LOOP
                    NDACORDRE := CDAC_REC.DACORDRE;

                    FOR CDRUDAC_REC IN CDRUDAC
                    LOOP
                        NOK := 0;
                        EXIT;
                    END LOOP;
                END LOOP;
            ELSIF (SCONTROLE = 'TFTAX')
            THEN
                SELECT DOSTYPEFINANCEMENT
                  INTO SDOSTYPEFINANCEMENT
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                FOR CTFTAX_REC IN CTFTAX
                LOOP
                    -- Traitement specifique sur la regle fiscal 046
                    IF    (SDOSTYPEFINANCEMENT IN ('ACHAT', 'LSBACK'))
                       OR (CTFTAX_REC.AUNREGLECLIENT = '046')
                    THEN
                        IF F_ISRUBFINTYPEFINANTAXE (
                               SDOSTYPEFINANCEMENT,
                               CTFTAX_REC.AUNREGLECLIENT,
                               CTFTAX_REC.TAXCODE) !=
                           1
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF (SCONTROLE = 'VTTAX') AND (NCREID IS NOT NULL)
            THEN
                SELECT DOSTYPEFINANCEMENT
                  INTO SDOSTYPEFINANCEMENT
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                IF (SDOSTYPEFINANCEMENT = 'LSBACK')
                THEN
                    FOR CFLITAX_REC IN CFLITAX
                    LOOP
                        FOR CDLITAX_REC IN CDLITAX
                        LOOP
                            IF (CDLITAX_REC.TAXCODE = '024')
                            THEN
                                IF (CFLITAX_REC.TAXCODE NOT IN ('002',
                                                                '021',
                                                                '017',
                                                                '028',
                                                                '043',
                                                                '042',
                                                                '006',
                                                                '019'))
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            ELSIF (CDLITAX_REC.TAXCODE = '025')
                            THEN
                                IF (CFLITAX_REC.TAXCODE NOT IN ('022', '047'))
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            ELSIF (CDLITAX_REC.TAXCODE = '026')
                            THEN
                                IF (CFLITAX_REC.TAXCODE NOT IN ('002',
                                                                '021',
                                                                '017',
                                                                '028',
                                                                '043',
                                                                '042',
                                                                '006',
                                                                '019'))
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            ELSIF (CDLITAX_REC.TAXCODE = '018')
                            THEN
                                IF (CFLITAX_REC.TAXCODE NOT IN ('002',
                                                                '021',
                                                                '017',
                                                                '028',
                                                                '043',
                                                                '042',
                                                                '006',
                                                                '019'))
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            ELSIF (CDLITAX_REC.TAXCODE = '037')
                            THEN
                                IF (CFLITAX_REC.TAXCODE NOT IN ('022', '047'))
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;
                            END IF;
                        END LOOP;

                        IF (NOK = 0)
                        THEN
                            EXIT;
                        END IF;
                    END LOOP;
                END IF;
            END IF;

            RETURN NOK;
        END;
    END OCDOSSPECITALIE;

    FUNCTION OCDOSBORROWINGBASE (NDOSID IN DOSSIER.DOSID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK              NUMBER := 1;
            NMAXCREID        CREVT.CREID%TYPE;
            NBORROWINGBASE   NUMBER;
            NFUNDS           NUMBER;
        BEGIN
            SELECT MAX (CRE.CREID)
              INTO NMAXCREID
              FROM CREVT CRE, CREDATA CDA
             WHERE     CRE.DOSID = NDOSID
                   AND CRE.CREDTSUP IS NULL
                   AND CDA.CREID = CRE.CREID
                   AND CDA.TSGCODE = 'BASE';

            IF (NMAXCREID IS NOT NULL)
            THEN
                SELECT NVL (CDADATANUMBER, 0)
                  INTO NBORROWINGBASE
                  FROM CREDATA
                 WHERE CREID = NMAXCREID AND TSGCODE = 'BASE';

                SELECT NVL (SUM (CDA.CDADATANUMBER), 0)
                  INTO NFUNDS
                  FROM CREVT CRE, CREDATA CDA
                 WHERE     CRE.DOSID = NDOSID
                       AND CRE.CREDTSUP IS NULL
                       AND CDA.CREID = CRE.CREID
                       AND CDA.CDATABLE = 'DEPENSE'
                       AND CDA.CDACOLONNE = 'FONDS';

                IF (NFUNDS > NBORROWINGBASE)
                THEN
                    NOK := 0;
                END IF;
            END IF;

            RETURN NOK;
        END;
    END OCDOSBORROWINGBASE;

    -----------------AIL:D?but Controls FO------------
    --------------------------------------------------
    --controls on DOSSIERPROSPECT
    FUNCTION OCDOSSIERFRONT (NDOSID      IN DOSSIER.DOSID%TYPE,
                             SCONTROLE   IN VARCHAR2,
                             NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK             NUMBER := 1;
            L_COUNT         NUMBER;
            -- LMI 20131217 Common data frequently searched
            STPGCODE        DOSSIERPROSPECT.TPGCODE%TYPE;
            SVERSION        DOSSIERPROSPECT.DPRVERSION%TYPE;
            NACTIDGESTION   DOSSIERPROSPECT.ACTID%TYPE;
        -- LMI 20131217 Common data frequently searched


        BEGIN
            SELECT TPGCODE, DPRVERSION, ACTID
              INTO STPGCODE, SVERSION, NACTIDGESTION
              FROM DOSSIERPROSPECT
             WHERE     DOSID = NDOSID
                   AND DPRVERSION =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (NDOSID);

            ----HMN:D?but contole date de mise en service
            IF SCONTROLE IN ('DTMEP')
            THEN                                       -- control name U_XXXXX
                DECLARE
                    nCount        NUMBER := 0;
                    sPerception   VARCHAR2 (5);
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM dprdatecle
                     WHERE     dosid = nDosid
                           AND DCLCODE = 'DTMEP'
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = nDosid)
                           AND TO_CHAR (DDCDT, 'DD') NOT IN
                                   (SELECT tupcode
                                      FROM tusparam
                                     WHERE tusnom = 'QUANTIEMEFACT');

                    IF nCount > 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ELSIF SCONTROLE IN ('DPCA')
            THEN --- MBN  05-12-2017 Control sur la checklist "Checklist du jalon "Dossier pris en charge par lanalyste"
                DECLARE
                    nCount   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM ADMINISTRATIF ADM
                     WHERE     DOSIDPROSPECT = nDosid
                           AND ADMIDPARENT IS NOT NULL
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM ADMSTATUS ADS
                                     WHERE     ADS.ADMID = ADM.ADMID
                                           AND ADS.ASTSTATUS IN
                                                   ('TER', 'WAIVE', 'NOA')
                                           AND ASTORDRE =
                                               (SELECT MAX (ASTORDRE)
                                                  FROM admstatus ADS2
                                                 WHERE ADS.admid = ADS2.admid))
                           AND (SELECT forid
                                  FROM ADMINISTRATIF ADM2
                                 WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                                   (SELECT FORID
                                      FROM FORMALITE
                                     WHERE     fortype = 'CHECKLIST'
                                           AND FORCODE IN ('LSTCOM')) -----ICH 225/09/2020  CHECKLIST SGML                                                 ))
                           AND dosidprospect =
                               (SELECT MAX (dosidprospect)
                                  FROM administratif ADM2
                                 WHERE ADM.admidparent = ADM2.admid);


                    IF nCount = 0
                    THEN
                        NOK := 1;
                    ELSE
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                --  NOK := 1;
                END;
            ---MTR 20180508 Control sur les amortissements negatifs

            ELSIF SCONTROLE = 'AMORT'
            THEN
                DECLARE
                    nCount   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM pfirubecheancier
                     WHERE     pfiid IN (SELECT pfiid
                                           FROM dprpropfinance
                                          WHERE dosid = nDosid)
                           AND PECMTAMORTIZATION < 0;

                    IF nCount > 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                --  NOK := 1;
                END;
            --MTR 20180508 Controle sur la presence de materiel ES dans le cas d'une relocation front
            ELSIF (SCONTROLE = 'RELOC')
            THEN
                DECLARE
                    CURSOR curRELTNL
                    IS
                        SELECT dosid, itrid, iruordre
                          FROM dprmateriel
                         WHERE     dosid = nDOSID
                               AND dprversion = (SELECT DPRVERSION
                                                   FROM V_DEAL VDE
                                                  WHERE VDE.DOSID = nDosid);

                    sDEALTYPE   VARCHAR (15);
                    nCount      NUMBER;
                BEGIN
                    SELECT dcodealtype
                      INTO sDEALTYPE
                      FROM dprcomplement
                     WHERE     dosid = nDOSID
                           AND dprversion = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = nDOSID);

                    IF sDEALTYPE = 'RELOCATION'
                    THEN
                        FOR C IN curRELTNL
                        LOOP
                            SELECT COUNT (*)
                              INTO nCount
                              FROM dprmateriel dpm, itrrubphase itr
                             WHERE     irpdtfin IS NULL
                                   AND dpm.itrid = itr.itrid
                                   AND dpm.iruordre = itr.iruordre
                                   AND dosid = C.DOSID
                                   AND itr.itrid = C.itrid
                                   AND itr.iruordre = C.iruordre
                                   AND itr.phacode <> 'TNL'
                                   AND dprversion =
                                       (SELECT DPRVERSION
                                          FROM V_DEAL VDE
                                         WHERE VDE.DOSID = nDOSID);

                            IF nCount > 0
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ------- MTR 18/04/2018
            ELSIF SCONTROLE IN ('DUPLI')
            THEN
                DECLARE
                    nCount   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM dpmdetail        dpd,
                           dprmateriel      dpm,
                           dossierprospect  dpr,
                           dprphase         dp
                     WHERE     dpd.dosid = nDosid
                           AND dpd.DPRVERSION = (SELECT DPRVERSION
                                                   FROM V_DEAL VDE
                                                  WHERE VDE.DOSID = nDosid)
                           AND dpd.dosid = dpm.dosid
                           AND dpd.dosid = dpr.dosid
                           AND dp.dosid = dpr.dosid
                           AND dp.JALCODE IN ('DCTL',
                                              'DOSINC',
                                              'DRC',
                                              'DRF',
                                              'DOSINC')
                           AND dp.DPHDTEND IS NULL
                           AND dpd.DPRVERSION = dpm.DPRVERSION
                           AND dpr.tpgcode NOT IN ('CBELB',
                                                   'CBESPL',
                                                   'CBESPLI',
                                                   'CBIBC',
                                                   'CBIEG',
                                                   'CBIAD',
                                                   'CBILB')
                           AND dpm.DPMQUANTITY > 1;

                    IF (nCount > 0)
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ELSIF SCONTROLE IN ('DEFAL')
            THEN
                DECLARE
                    nCount   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM dpmdetail        dpd,
                           dprmateriel      dpm,
                           dossierprospect  dpr
                     WHERE     dpd.dosid = nDosid
                           AND dpd.DPRVERSION = (SELECT DPRVERSION
                                                   FROM V_DEAL VDE
                                                  WHERE VDE.DOSID = nDosid)
                           AND dpd.dosid = dpm.dosid
                           AND dpd.dosid = dpr.dosid
                           AND dpd.DPRVERSION = dpm.DPRVERSION
                           AND dpr.tpgcode IN ('CBELB',
                                               'CBESPL',
                                               'CBESPLI',
                                               'CBIBC',
                                               'CBIEG',
                                               'CBIAD',
                                               'CBILB')
                           AND dpm.DPMQUANTITY > 1;


                    IF (nCount > 0)
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ------------END MTR 18/04/2018
            ----HMN -AIL :D?but controle moyen de paiement

            ELSIF SCONTROLE IN ('MPMT')
            THEN
                DECLARE
                    Rolea        dpracteur.rolcode%TYPE;
                    Rolef        dpracteur.rolcode%TYPE;
                    Roleg        dpracteur.rolcode%TYPE;
                    Mpaiementf   tmoyenpmt.TMPCODE%TYPE;
                    Mpaiementa   tmoyenpmt.TMPCODE%TYPE;
                    Mpaiementg   tmoyenpmt.TMPCODE%TYPE;

                    Ncount       NUMBER := 0;
                BEGIN
                    SELECT rolcode, TMPCODEDEC
                      INTO Rolef, Mpaiementf
                      FROM dpracteur
                     WHERE     rolcode IN ('FOURN')
                           AND dosid = nDosid
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = nDosid)
                           AND TMPCODEDEC IS NOT NULL;


                    SELECT rolcode, TMPCODEENC
                      INTO Rolea, Mpaiementa
                      FROM dpracteur
                     WHERE     rolcode IN ('CLIENT')
                           AND dosid = nDosid
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = nDosid)
                           AND TMPCODEENC IS NOT NULL;

                    SELECT COUNT (TMPCODEENC)
                      INTO Ncount
                      FROM dpracteur
                     WHERE     rolcode IN ('GARANT')
                           AND dosid = nDosid
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = nDosid);


                    IF (Ncount <> 0)
                    THEN
                        SELECT rolcode, TMPCODEENC
                          INTO Roleg, Mpaiementg
                          FROM dpracteur
                         WHERE     rolcode IN ('GARANT')
                               AND dosid = nDosid
                               AND DPRVERSION = (SELECT DPRVERSION
                                                   FROM V_DEAL VDE
                                                  WHERE VDE.DOSID = nDosid)
                               AND TMPCODEENC IS NOT NULL;

                        IF (    Mpaiementa = 'PRLAUTO'
                            AND Mpaiementg = 'PRLAUTO'
                            AND Mpaiementf IN ('CHQ', 'EFFEM', 'VIRMAN'))
                        THEN
                            Nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSE
                        IF (    Mpaiementa = 'PRLAUTO'
                            AND Mpaiementf IN ('CHQ', 'EFFEM', 'VIRMAN'))
                        THEN
                            Nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ----HMN -AIL :Fin controle moyen de paiement

            --HMN-AIL:D?but controle TEG<TAUX MAX:20170620


            ELSIF SCONTROLE IN ('TEGT')
            THEN
                DECLARE
                    P_Dosid   Dossierprospect.Dosid%TYPE;
                    Tauxf     Pvetable.Ptatxfixe%TYPE;
                    Margem    Pvetable.Ptatxmargemaxi%TYPE;
                    Teg       NUMBER;
                    Tauxm     Pvetable.Ptatxmargemaxi%TYPE;
                BEGIN
                    SELECT F_CALCUL_TEG_FO (nDosid, NULL, NULL)
                      INTO Teg
                      FROM DUAL;

                    SELECT DISTINCT (Ptatxfixe), Ptatxmargemaxi
                      INTO Tauxf, Margem
                      FROM Pvetable
                     WHERE Pelid IN (404287, 404288, 404289);

                    Tauxm := Tauxf + Margem;

                    IF Teg > Tauxm
                    THEN
                        Nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        Nok := 1;
                END;
            --- MMN Controle sur la quantite des composants

            ELSIF SCONTROLE = 'DPQTE'
            THEN
                DECLARE
                    test   NUMBER;
                BEGIN
                    SELECT COUNT (dpm.dosid)
                      INTO test
                      FROM dprmateriel dpm, dpmdetail dmd
                     WHERE     dpm.dosid = dmd.dosid
                           AND dpm.dprversion = dmd.dprversion
                           AND dpm.dpmordre = dmd.dpmordre
                           AND dpm.dprversion = (SELECT dprversion
                                                   FROM v_deal
                                                  WHERE dosid = dpm.dosid)
                           AND dpm.dosid = NDOSID
                           AND dpm.dpmquantity != dmd.DMDQUANTITE;

                    IF test > 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MMN quantite des composants 29082017

            --- MMN Controle 29082017: chaque materiel doit etre rattacher a un fournisseur

            ELSIF SCONTROLE = 'MTFR'
            THEN
                DECLARE
                    test1   NUMBER;
                    test2   NUMBER;
                BEGIN
                    SELECT COUNT (dpmordre)
                      INTO test1
                      FROM lkarodpm ARO
                     WHERE     ARO.dosid = ndosid
                           AND ARO.dprversion IN (SELECT dprversion
                                                    FROM V_deal
                                                   WHERE dosid = ndosid);

                    SELECT COUNT (dpmordre)
                      INTO test2
                      FROM dpmdetail
                     WHERE     dosid = ndosid
                           AND dprversion IN (SELECT dprversion
                                                FROM V_deal
                                               WHERE dosid = ndosid)
                           AND DMDORDRE = 1;

                    IF test1 <> test2
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MMN 29082017

            ---- MMN 19092017 controle sur la quantite du materiel principal ( obligation de proceder a l'eclatement materiel avant generation numero de contrat)

            ELSIF SCONTROLE = 'ECMT'
            THEN
                DECLARE
                    test   NUMBER;
                BEGIN
                    SELECT COUNT (DMDORDRE)
                      INTO test
                      FROM dpmdetail
                     WHERE     dosid = ndosid
                           AND dmdordre = 1
                           AND DMDQUANTITE <> 1
                           AND dprversion IN (SELECT dprversion
                                                FROM V_deal
                                               WHERE dosid = ndosid);

                    IF test > 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MMN 19092017

            --HMN-AIL:FIN controle TEG<TAUX MAX:20170620
            /*
            ---- Control on non achieved checklist items------------
            --Control created 09/05/2016-HBB (ALC Project)
           ELSIF SCONTROLE IN ('NOITM')
            THEN                                          -- control name U_XXXXX
               DECLARE
                  nCount   NUMBER := 0;
               BEGIN
                  SELECT COUNT (*)
                    INTO nCount
                    FROM ADMINISTRATIF ADM
                   WHERE     DOSIDPROSPECT = nDosid
                         AND ADMIDPARENT IS NOT NULL
                         AND NOT EXISTS
                                (SELECT 1
                                   FROM ADMSTATUS ADS
                                  WHERE     ADS.ADMID = ADM.ADMID
                                        AND ADS.ASTSTATUS IN
                                               ('TER', 'WAIVE', 'NOA')
                                        AND ASTORDRE =
                                               (SELECT MAX (ASTORDRE)
                                                  FROM admstatus ADS2
                                                 WHERE ADS.admid = ADS2.admid))
                         AND (SELECT forid
                                FROM ADMINISTRATIF ADM2
                               WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                                (SELECT FORID
                                   FROM FORMALITE
                                  WHERE fortype = 'CHECKLIST' AND forid <> 5106)
                         AND dosidprospect =
                                (SELECT MAX (dosidprospect)
                                   FROM administratif ADM2
                                  WHERE ADM.admidparent = ADM2.admid);

                  IF nCount = 0
                  THEN
                     NOK := 1;
                  ELSE
                     NOK := 0;
                  END IF;
               EXCEPTION
                  WHEN OTHERS
                  THEN
                     NOK := 0;
               --  NOK := 1;
               END;*/
            ---- Control on non achieved checklist items------------
            --Control created 09/05/2016-HBB (ALC Project)


            --HMN--Control created 26/09/2017-HMN (AIL Project)
            ELSIF SCONTROLE IN ('NAPM')
            THEN                                       -- control name U_XXXXX
                DECLARE
                    Nnature   Dprmateriel.Napcode%TYPE;
                BEGIN
                    SELECT COUNT (*)                 --, Dco.Dosidautorisation
                      INTO nNature                                    --,nDaut
                      FROM Dprcomplement Dco, Dprmateriel Dma
                     WHERE     Dma.Dosid = Dco.Dosid
                           AND Dma.Dosid = nDosid
                           AND dco.dprversion IN (SELECT dprversion
                                                    FROM V_deal
                                                   WHERE dosid = Ndosid)
                           AND dco.dprversion = Dma.dprversion
                           AND Napcode IS NULL
                           AND dco.Dosidautorisation IS NOT NULL;

                    IF nNature <> 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            --HMN --Control created 26/09/2017-HMN (AIL Project)

            --- MMN 04102017 CONTROLE SUR LE MONTANT TIRe
            ELSIF SCONTROLE IN ('MTTR')
            THEN
                DECLARE
                    CURSOR Cmtglobal
                    IS
                        SELECT DPMORDRE, DMDORDRE
                          FROM DPMDETAIL
                         WHERE     Dosid = nDosid
                               AND dprversion IN (SELECT dprversion
                                                    FROM V_deal
                                                   WHERE dosid = Ndosid);

                    Mtglobal      DPMDETAIL.DMDPRIXUNITAIRE%TYPE := 0;
                    MT_TIRE       CREDATA.CDADATANUMBER%TYPE;
                    MTCOMPOSANT   DPMDETAIL.DMDPRIXUNITAIRE%TYPE := 0;
                    COMPTEUR      NUMBER;
                BEGIN
                    SELECT CDADATANUMBER
                      INTO MT_TIRE
                      FROM CREDATA
                     WHERE     CREID IN
                                   (SELECT CREID
                                      FROM CREVT
                                     WHERE     TMFFONCTION = 'EVF_TIRAGE'
                                           AND dosidprospect IN
                                                   (SELECT dosidautorisation
                                                      FROM dprcomplement
                                                     WHERE     dosid = Ndosid
                                                           AND dprversion IN
                                                                   (SELECT dprversion
                                                                      FROM V_deal
                                                                     WHERE dosid =
                                                                           Ndosid))
                                           AND creid IN
                                                   (SELECT creid
                                                      FROM CREDATA
                                                     WHERE     CDADATANUMBER =
                                                               Ndosid
                                                           AND CDAORDRE = 4))
                           AND CDAORDRE = 5;

                    COMPTEUR := 0;

                    FOR Cmtmateriel IN Cmtglobal
                    LOOP
                        COMPTEUR := COMPTEUR + 1;

                        SELECT DMDQUANTITE * DMDPRIXUNITAIRE
                          INTO MTCOMPOSANT
                          FROM DPMDETAIL
                         WHERE     Dosid = nDosid
                               AND dprversion IN (SELECT dprversion
                                                    FROM V_deal
                                                   WHERE dosid = Ndosid)
                               AND DPMDETAIL.DMDORDRE = COMPTEUR;

                        Mtglobal := Mtglobal + MTCOMPOSANT;
                    END LOOP;

                    IF Mtglobal != MT_TIRE
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ---------MTR 17/04/2018 TLG: le Taux nominal et le TEG ne doivent pas d?passer le taux d'usure
            ---------MTR 17/04/2018 TLG: le Taux nominal et le TEG ne doivent pas d?passer le taux d'usure
            ELSIF scontrole IN ('DPTU')             --D?passement taux d'usure
            THEN
                DECLARE
                    l_taux   NUMBER;
                    -- l_Tx_Nom   NUMBER;--MTR 20/06/2018 MAJ du ctrl pour ne taper que sur le teg
                    l_TXEX   NUMBER;
                    S_TYPE   dprcomplement.DCODEALTYPE%TYPE;
                BEGIN
                    SELECT DCODEALTYPE
                      INTO S_TYPE
                      FROM dprcomplement
                     WHERE     DOSID = ndosid
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = ndosid);

                    SELECT F_CALCUL_TEG_FO (ndosid, 'FR', 'ORFI')
                      INTO l_taux
                      FROM DUAL;

                    DBMS_OUTPUT.PUT_LINE ('l_taux ' || l_taux);

                    /*  SELECT PFIA.PFADOUBLE
                        INTO l_Tx_Nom
                        FROM Pfiattribut PFIA, DPRPROPFINANCE DPR
                       WHERE     PFIA.pfacode = 'CUSTOMERRATE'
                             AND PFIA.Pfiid = Dpr.Pfiid
                             AND DPR.DOSID = ndosid
                             AND DPR.DPRVERSION = (SELECT DPRVERSION
                                                     FROM V_DEAL VDE
                                                    WHERE VDE.DOSID = DPR.DOSID)
                             AND Dpr.Dpfflagretenue = 1;

                      DBMS_OUTPUT.PUT_LINE ('l_Tx_Nom ' || l_Tx_Nom);*/

                    SELECT TVAVAL
                      INTO l_TXEX
                      FROM tauvaleur
                     WHERE taucode = 'TXEX' AND tvadtfin IS NULL;

                    DBMS_OUTPUT.PUT_LINE ('l_TXEX ' || l_TXEX);


                    IF     (S_TYPE != 'TRANSFERT')
                       AND (S_TYPE != 'RENEGOFROMBEGIN')
                       AND                                               /*(*/
                           (l_taux > l_TXEX)       /*OR (l_Tx_Nom > l_TXEX))*/
                    THEN
                        nok := 0;   -- Le TEG n'a pas depasse le taux excessif
                        DBMS_OUTPUT.PUT_LINE ('TEST1');
                    ELSE
                        nok := 1;     -- Le taux excessif a ete depasse ==> KO
                        DBMS_OUTPUT.PUT_LINE ('TEST2');
                    END IF;

                    DBMS_OUTPUT.PUT_LINE ('NOK ' || nok);
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                        DBMS_OUTPUT.PUT_LINE ('nok EXCEPTION ' || nok);
                END;
            ---END MTR 17/04/2018
            ---- 20181010 MTR TLG: contr?le pr?sence mat?riel
            ELSIF SCONTROLE = 'PMAT'
            THEN
                DECLARE
                    nCOUNT   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCOUNT
                      FROM dprmateriel
                     WHERE     dosid = ndosid
                           AND dprversion IN (SELECT dprversion
                                                FROM V_deal
                                               WHERE dosid = ndosid);

                    IF nCOUNT > 0
                    THEN
                        NOK := 1;
                    ELSE
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- 20181010 MTR
            ---------------20181012 MTR TLG Controle quantit? obligatoire mis en place pour les demandes migr?es 4.0
            ELSIF SCONTROLE = 'NQTTE'
            THEN
                DECLARE
                    nCount   NUMBER;
                BEGIN
                    SELECT COUNT (1)
                      INTO nCount
                      FROM dprmateriel
                     WHERE     DPMQUANTITY IS NULL
                           AND dosid = NDOSID
                           AND dprversion = (SELECT dprversion
                                               FROM v_deal
                                              WHERE dosid = NDOSID);

                    IF nCount <> 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---------------20181012 MTR TLG Controle Bar?me non autoris? mis en place pour les demandes migr?es 4.0

            ---------------------------20181012 MTR TLG Controle bar?me indisponible pour demandes migr?es
            ---------------------------20181017 MTR TLG Controle bar?me indisponible pour demandes migr?es
            ---------------------------20181012 MTR TLG Controle bar?me indisponible pour demandes migr?es
            ---------------------------20181017 MTR TLG Controle bar?me indisponible pour demandes migr?es
            ELSIF SCONTROLE = 'NOSCA'
            THEN
                DECLARE
                    nCount1     NUMBER;
                    nCount2     NUMBER;
                    nCount3     NUMBER;
                    nDEALORIG   NUMBER;
                BEGIN
                    --RBS 20181217 Exclure les dossiers Amen Bank de la population avec bareme
                    SELECT NVL (COUNT (*), 0)
                      INTO nDEALORIG
                      FROM dossierprospect
                     WHERE uticodecreation = 'AMENBANK' AND dosid = nDOSID;

                    IF nDEALORIG = 0
                    THEN
                        --RBS 20181217 Exclure les dossiers Amen Bank de la population avec bareme
                        SELECT NVL (COUNT (*), 0)
                          INTO nCount1
                          FROM v_deal
                         WHERE     dosid = nDOSID
                               AND DPRDTTRANSFERTCASSIOPEE IS NULL
                               AND dprdtcreation <
                                   TO_DATE ('13/10/2018', 'DD/MM/YYYY');

                        SELECT NVL (COUNT (*), 0)
                          INTO nCount2
                          FROM propositionfinanciere
                         WHERE     pcrid IS NOT NULL
                               AND pfiid IN
                                       (SELECT pfiid
                                          FROM DPRPROPFINANCE
                                         WHERE     DOSID = nDOSID
                                               AND DPFFLAGRETENUE = 1
                                               AND dprversion IN
                                                       (SELECT dprversion
                                                          FROM v_deal
                                                         WHERE DOSID = nDOSID));

                        SELECT NVL (COUNT (*), 0)
                          INTO nCount3
                          FROM dprcomplement dpr, v_deal dpt
                         WHERE     dpr.dosid = nDOSID
                               AND dpr.dosidautorisation = dpt.dosid
                               AND dpr.dprversion = dpt.dprversion
                               AND dpt.dprdtcreation <
                                   TO_DATE ('13/10/2018', 'DD/MM/YYYY');

                        IF (   (nCount1 > 0 AND nCount2 > 0)
                            OR (nCount2 > 0 AND nCount3 > 0)
                            OR (nCount1 = 0 AND nCount2 = 0 AND nCount3 = 0))
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ----
            ----

            ELSIF SCONTROLE = 'CRDFR'
            THEN
                DECLARE
                    nMtAssiette   NUMBER;
                    nPfvMt        NUMBER;
                BEGIN
                    SELECT ROUND (MAX (pfa2.pfadouble), 3),
                           ROUND (SUM (pfv.PFVMTCUSTOMER), 3)
                      INTO nMtAssiette, nPfvMt
                      FROM v_deal                 dpr,
                           dprpropfinance         dpf,
                           pfiversement           pfv,
                           propositionfinanciere  pfi,
                           pfiattribut            pfa1,
                           pfiattribut            pfa2
                     WHERE     dpr.dosid = dpf.dosid
                           AND dpf.dpfflagretenue = 1
                           AND dpr.dprversion = dpf.Dprversion
                           AND pfv.pfiid = dpf.pfiid
                           AND NVL (pfv.PFVNBDAYSCUSTOMER, 0) > 0
                           AND pfv.PFVTYPEVERSEMENT <> 'DOWNPAYMENTARR'
                           AND pfi.PFIIMPACTMULTIVERS = 'TAF'
                           AND pfi.pfiid = dpf.pfiid
                           AND pfa1.pfiid = dpf.pfiid
                           AND pfa2.pfiid = dpf.pfiid
                           AND dpr.dosid = NDOSID
                           AND pfa1.pfacode = 'PFITXREMUVERS'
                           AND pfa2.pfacode = 'PFIMTBASECALC'
                           AND NVL (pfa1.pfadouble, 0) <> 0
                           AND NVL (pfa2.pfadouble, 0) <> 0;

                    IF (nMtAssiette <> nPfvMt)
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF sControle = 'DSIRT'
            THEN ----------MTR 30/10/2018:  CONTROLE MATRICULE FISCAL ERRONNe POUR LE CLIENT SUR LA DEMANDE
                DECLARE
                    nLengthSiret   NUMBER := 0;
                    nPart11        VARCHAR2 (10) := NULL;
                    nPart12        VARCHAR2 (10) := NULL;
                    nPart21        VARCHAR2 (10) := NULL;
                    nPart22        VARCHAR2 (10) := NULL;
                    nPart31        VARCHAR2 (10) := NULL;
                    nPart32        VARCHAR2 (10) := NULL;
                    Sjalcode       VARCHAR2 (10) := NULL;
                    nACTSIRET      VARCHAR (50) := NULL;
                    nACTID         NUMBER := 0;
                BEGIN
                    SELECT JALCODE
                      INTO Sjalcode
                      FROM dprphase
                     WHERE     dosid = nDOSID
                           AND DPHDTEND IS NULL
                           AND dprversion IN (SELECT dprversion
                                                FROM v_deal
                                               WHERE dosid = nDOSID);

                    IF (Sjalcode = 'OAC') OR (Sjalcode = 'DCN')
                    THEN
                        SELECT ACTID
                          INTO nActid
                          FROM DPRACTEUR
                         WHERE     ROLCODE = 'CLIENT'
                               AND DOSID = NDOSID
                               AND DPRVERSION = (SELECT DPRVERSION
                                                   FROM V_DEAL
                                                  WHERE DOSID = NDOSID);

                        SELECT LENGTH (f_StdTrimAll (ACTSIRET)), --nLengthSiret
                               --PART1
                               f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7)), --nPart11
                               F_StrTo_Number (
                                   f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7))), --nPart12
                               --PART2
                               f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3)), --nPart21
                               UPPER (f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3))), --nPart22
                               --PART3
                               f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3)), --nPart31
                               F_StrTo_Number (
                                   f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3))) --nPart32
                          INTO nLengthSiret,
                               nPart11,
                               nPart12,
                               nPart21,
                               nPart22,
                               nPart31,
                               nPart32
                          FROM ACTEUR
                         WHERE ACTID = nActid;

                        SELECT ACTSIRET
                          INTO nACTSIRET
                          FROM ACTEUR
                         WHERE ACTID = nActid;


                        IF nACTSIRET IS NULL
                        THEN
                            nok := 1;
                        ELSE
                            IF (nLengthSiret = 13)
                            THEN
                                IF    (    LENGTH (nPart11) = 7
                                       AND nPart11 = '0000000')
                                   OR (LENGTH (nPart11) = 7 AND nPart12 > 0)
                                THEN
                                    IF (    nPart21 = nPart22
                                        AND LENGTH (nPart21) = 3)
                                    THEN
                                        IF    (    LENGTH (nPart31) = 3
                                               AND nPart31 = '000')
                                           OR (    LENGTH (nPart31) = 3
                                               AND nPart32 > 0)
                                        THEN
                                            nok := 1;
                                        ELSE                          ---Part3
                                            nok := 0;
                                        END IF;
                                    ELSE                              ---Part2
                                        nok := 0;
                                    END IF;
                                ELSE
                                    nok := 0;                         ---Part1
                                END IF;
                            ELSE                           --(nLengthSiret=13)
                                nok := 0;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ---------------20181113 MTR TLG Controle le montant de la proposition doit etre egale au montant du materiel
            ELSIF SCONTROLE = 'MTMP'
            THEN
                DECLARE
                    nMtfin   NUMBER;
                    nMtMat   NUMBER;
                    SType    dprcomplement.DCODEALTYPE%TYPE;
                BEGIN
                    SELECT DCODEALTYPE
                      INTO SType
                      FROM dprcomplement
                     WHERE     DOSID = ndosid
                           AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = ndosid);

                    IF (SType != 'NORMAL')
                    THEN
                        NOK := 1;
                    ELSIF (SType = 'NORMAL')
                    THEN
                        SELECT PFIINVESTISSEMENT
                          INTO nMtMat
                          FROM PROPOSITIONFINANCIERE
                         WHERE pfiid IN
                                   (SELECT PFIID
                                      FROM DPRPROPFINANCE
                                     WHERE     dprversion =
                                               (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE dosid = ndosid)
                                           AND DPFFLAGRETENUE = 1
                                           AND dosid = ndosid);

                        SELECT SUM (DPMMTINVEST)
                          INTO nMtfin
                          FROM dprmateriel
                         WHERE     dprversion = (SELECT dprversion
                                                   FROM v_deal
                                                  WHERE dosid = ndosid)
                               AND dosid = ndosid;


                        IF nMtMat <> nMtfin
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ---------------20181113 MTR TLG
            ELSIF SCONTROLE = 'MTFIN'
            THEN
                DECLARE
                    MT_INVEST   NUMBER;
                    nCOMP       NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCOMP
                      FROM utitsm
                     WHERE     uticode = F_GETCURRENTUTICODE
                           AND TSMMETIER = 'COMCDM';

                    SELECT PFIINVESTISSEMENT
                      INTO MT_INVEST
                      FROM PROPOSITIONFINANCIERE
                     WHERE pfiid IN
                               (SELECT pfiid
                                  FROM dprpropfinance
                                 WHERE     DOSID = nDOSID
                                       AND dprversion IN
                                               (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = nDOSID)
                                       AND DPFFLAGRETENUE = 1);

                    IF (MT_INVEST > 2000000) AND (nCOMP > 0)
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;

               ----------------SGM -Existence d'une caution bancaire en cas de matériel d'importation

                  ELSIF SCONTROLE = 'MIMP'
            THEN
                DECLARE
                     ncount1   NUMBER := 0;
					 ncount2   NUMBER := 0;
                BEGIN

					  SELECT NVL (COUNT (CVABOOLEANVALUE), 0)
                      INTO ncount1
                      FROM cchvalue
                     WHERE CCHSID = 'CHKCCHSID451' AND dosidprospect = ndosid;

					  IF (ncount1 = 0)
                    THEN
                        NOK := 1;
                    ELSIF (ncount1 > 0)
                    THEN


                   SELECT count (*) 
                      INTO ncount2
                      FROM pfiguarantee
                     WHERE pfiid IN
                               (SELECT pfiid
                                  FROM dprpropfinance
                                 WHERE     DOSID = nDOSID
                                       AND dprversion IN
                                               (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = nDOSID)
                                       AND DPFFLAGRETENUE = 1)
									   and TGACODE='SGM1';

                    IF ncount2= 0 
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
					  END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;  

       ------SGLM --absence de garantie proposee de type aval 
          ELSIF SCONTROLE IN ('ABSG')
            THEN
                DECLARE
                    ncount   NUMBER := 0;
                BEGIN
         SELECT count (*)
                     INTO ncount
                     FROM pfiguarantee
                     WHERE pfiid IN
                               (SELECT pfiid
                                  FROM dprpropfinance
                                 WHERE     DOSID = NDOSID
                                       AND dprversion IN
                                               (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = NDOSID)
                                       AND DPFFLAGRETENUE = 1)
									  and TGACODE in ('SGM5' ,'SGM12','SGM10') ;
					  IF (ncount = 0)
                    THEN
                          nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                 EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;  


            --------------AKA CONTROLES KYC EFFECTUE SOGELEASE

            ELSIF SCONTROLE IN ('ASSU')
            THEN
                DECLARE
                    ncount   NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (CVABOOLEANVALUE), 0)
                      INTO ncount
                      FROM cchvalue
                     WHERE CCHSID = 'CHKCCHSID450' AND dosidprospect = ndosid;

                    IF (ncount = 0)
                    THEN
                        NOK := 0;
                    ELSIF (ncount > 0)
                    THEN
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF SCONTROLE IN ('MTTF')
            THEN
                DECLARE
                    mt_fin   NUMBER := 0;
                    mt_aut   NUMBER := 0;
                    mt_enc   NUMBER := 0;
                    mt_eng   NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (CVASTRINGVALUE), 0)
                      INTO mt_aut
                      FROM cchvalue
                     WHERE     CCHSID = 'TFDCCHVAL3000'
                           AND dosidprospect = ndosid;

                    IF (mt_aut = 0)
                    THEN
                        NOK := 1;
                    ELSE
                        SELECT TO_NUMBER (CVASTRINGVALUE)
                          INTO mt_aut
                          FROM cchvalue
                         WHERE     CCHSID = 'TFDCCHVAL3000'
                               AND dosidprospect = ndosid;


                        SELECT PFIINVESTISSEMENT
                          INTO mt_fin
                          FROM PROPOSITIONFINANCIERE
                         WHERE PFIID IN
                                   (SELECT pfiid
                                      FROM dprpropfinance
                                     WHERE     dosid = ndosid
                                           AND dprversion IN
                                                   (SELECT dprversion
                                                      FROM v_deal
                                                     WHERE dosid = ndosid));



                        SELECT NVL ((F_GET_ENC_CLT (NDOSID, 'FR', 'ORFI')),
                                    0)
                          INTO mt_enc
                          FROM DUAL;

                        SELECT NVL (
                                   (F_GET_ENG_CLT_DOS (NDOSID, 'FR', 'ORFI')),
                                   0)
                          INTO mt_eng
                          FROM DUAL;

                        IF (mt_fin > (mt_aut - (mt_enc + mt_eng)))
                        THEN
                            nok := 0;
                        ELSIF (mt_fin <= (mt_aut - (mt_enc + mt_eng)))
                        THEN
                            nok := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF SCONTROLE IN ('DETI')
            THEN
                DECLARE
                    ncount    NUMBER := 0;
                    TITFONC   NUMBER := 0;
                    ADRCBI    NUMBER := 0;
                    SUPERF    NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (*), 0)
                      INTO nCOUNT
                      FROM DOSSIERPROSPECT
                     WHERE DOSID = NDOSID AND taccode = 'CBI';

                    IF (nCOUNT = 0)
                    THEN
                        NOK := 1;
                    ELSIF (nCOUNT > 0)
                    THEN
                        SELECT NVL (COUNT (CVASTRINGVALUE), 0)
                          INTO TITFONC
                          FROM cchvalue
                         WHERE     CCHSID = 'TFDCCHVAL356'
                               AND dosidprospect = ndosid;

                        IF (TITFONC = 0)
                        THEN
                            NOK := 0;
                        ELSIF (TITFONC > 0)
                        THEN
                            SELECT NVL (COUNT (CVASTRINGVALUE), 0)
                              INTO ADRCBI
                              FROM cchvalue
                             WHERE     CCHSID = 'TFDCCHVAL323'
                                   AND dosidprospect = ndosid;

                            IF (ADRCBI = 0)
                            THEN
                                NOK := 0;
                            ELSIF (ADRCBI > 0)
                            THEN
                                SELECT NVL (COUNT (CVASTRINGVALUE), 0)
                                  INTO SUPERF
                                  FROM cchvalue
                                 WHERE     CCHSID = 'TFDCCHVAL701'
                                       AND dosidprospect = ndosid;

                                IF (SUPERF = 0)
                                THEN
                                    NOK := 0;
                                ELSE
                                    NOK := 1;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ELSIF SCONTROLE IN ('DURD')
            THEN
                DECLARE
                    ncount     NUMBER := 0;
                    nDUR       NUMBER := 0;
                    nTACCODE   VARCHAR2 (3) := NULL;
                BEGIN
                    SELECT DISTINCT TACCODE
                      INTO nTACCODE
                      FROM DOSSIERPROSPECT
                     WHERE DOSID = nDosId;

                    IF nTACCODE = 'CBM'
                    THEN
                        SELECT NVL (COUNT (*), 0)
                          INTO nCOUNT
                          FROM UTITSM
                         WHERE     UTICODE = F_GETCURRENTUTICODE
                               AND TSMMETIER = 'COMCDM';

                        IF (nCOUNT = 0)
                        THEN
                            nok := 1;
                        ELSIF (nCOUNT > 0)
                        THEN
                            SELECT PFINBPERIODES
                              INTO nDUR
                              FROM dprpropfinance D, PROPOSITIONFINANCIERE P
                             WHERE     D.PFIID = P.PFIID
                                   AND D.DOSID = NDOSID
                                   AND D.DPFFLAGRETENUE=1
                                   AND DPRVERSION =
                                       (SELECT DPRVERSION
                                          FROM V_DEAL VDE
                                         WHERE VDE.DOSID = nDosid);

                            IF nDUR NOT IN ('36', '48', '60')
                            THEN
                                nok := 0;
                            ELSE
                                nok := 1;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ------
            ELSIF SCONTROLE IN ('PRO')
            THEN-- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30100'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;

            -----------
             ELSIF SCONTROLE IN ('PRO2')
         THEN --- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30300'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;
            ----------------------------------------------------------
            ELSIF SCONTROLE IN ('PRO3')
         THEN --- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30400'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;
            --------HME CHECKLIST CLICOM SOGELEASE
              ELSIF SCONTROLE IN ('COM')
         THEN --- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30000'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;

---------------------------------------------------------------------------------------------
			  ELSIF SCONTROLE IN ('COM2')
         THEN --- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30200'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;
------------------------------------------------------------------------------------------------

 ELSIF SCONTROLE IN ('COM3')
         THEN --- HME  Control sur la checklist 
            DECLARE
               nCount   NUMBER := 0;
            BEGIN
               SELECT COUNT (*)
                 INTO nCount
                 FROM ADMINISTRATIF ADM
                WHERE     DOSIDPROSPECT = nDosid
                      AND ADMIDPARENT IS NOT NULL
                      AND NOT EXISTS
                             (SELECT 1
                                FROM ADMSTATUS ADS
                               WHERE     ADS.ADMID = ADM.ADMID
                                     AND ADS.ASTSTATUS IN
                                            ('TER', 'WAIVE', 'NOA')
                                     AND ASTORDRE =
                                            (SELECT MAX (ASTORDRE)
                                               FROM admstatus ADS2
                                              WHERE ADS.admid = ADS2.admid))
                      AND (SELECT forid
                             FROM ADMINISTRATIF ADM2
                            WHERE ADM2.ADMID = ADM.ADMIDPARENT) IN
                             (SELECT FORID
                                FROM FORMALITE
                               WHERE     fortype = 'CHECKLIST'
                                     AND FORCODE IN
                                            ('30500','30400'))                                                  
                      AND dosidprospect =
                             (SELECT MAX (dosidprospect)
                                FROM administratif ADM2
                               WHERE ADM.admidparent = ADM2.admid);


               IF nCount = 0
               THEN
                  NOK := 1;
               ELSE
                  NOK := 0;
               END IF;
            EXCEPTION
               WHEN OTHERS
               THEN
                  NOK := 0;
            END;

            --------------------Controle SUR workflow CLICPRO HME SOGELEASE------------
            ELSIF SCONTROLE IN ('WFP')
            THEN
                DECLARE

                    nactid      VARCHAR (50) := NULL;
					nseg        VARCHAR (50) := NULL;
					nCA         NUMBER;
					nTACCODE    VARCHAR (50) := NULL;
					ninv        NUMBER;
					nnbr        NUMBER;

                BEGIN
				 SELECT actid INTO nactid from dpracteur where dosid=ndosid 
				 AND  dprversion IN (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = NDOSID)
				 AND ROLCODE='CLIENT';								  

				 select CVASTRINGVALUE INTO nseg from cchvalue where cchsid='CMBCCHSID82' and actid=nactid;
				 select CVANUMERICVALUE into nCA from cchvalue where CCHSID='TFDCCHSID681' and dosidprospect=ndosid
				 AND dprversion IN (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = NDOSID) ;

                        IF (nseg IN (10201,10202) AND nCA <=10000000) THEN 

									SELECT TACCODE into nTACCODE from dossierprospect where dosid=ndosid 
								    and  dprversion IN (SELECT dprversion
																  FROM v_deal
																 WHERE DOSID = NDOSID);

										IF (nTACCODE ='CBM') THEN 

										    select PFIINVESTISSEMENT,PFINBPERIODES INTO ninv,nnbr from propositionfinanciere where pfiid IN
												   (SELECT pfiid
													  FROM dprpropfinance
													 WHERE     DOSID = NDOSID
														   AND dprversion IN
																   (SELECT dprversion
																	  FROM v_deal
																	 WHERE DOSID = NDOSID));

												   IF ( ninv>5000000 OR nnbr>60) THEN
													  nok := 0;
												   ELSE 
													nok :=1;
												   END IF;
										END IF;

										IF (nTACCODE ='CBI') THEN 

										    IF ( ninv > 10000000 OR nnbr > 144) THEN
													  nok := 0;
											ELSE 
													nok :=1;
										    END IF;
										END IF;


						ELSE 
						   nok :=0;

                        END IF;
				END;

            ---------CONTROLE SUR WORFFLOW CLICOM HME-----
            ELSIF SCONTROLE IN ('WFC')
            THEN
                DECLARE

                    nactid      VARCHAR (50) := NULL;
					nseg        VARCHAR (50) := NULL;
					nCA         NUMBER;
					nTACCODE    VARCHAR (50) := NULL;
					ninv        NUMBER;
					nnbr        NUMBER;

                BEGIN
				 SELECT actid INTO nactid from dpracteur where dosid=ndosid 
				 AND  dprversion IN (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = NDOSID)
				 AND ROLCODE='CLIENT';								  

				 select CVASTRINGVALUE INTO nseg from cchvalue where cchsid='CMBCCHSID82' and actid=nactid;
				 select CVANUMERICVALUE into nCA from cchvalue where CCHSID='TFDCCHSID681' and dosidprospect=ndosid
				 AND dprversion IN (SELECT dprversion
                                                  FROM v_deal
                                                 WHERE DOSID = NDOSID) ;

                        IF (nseg  IN (10201,10202) AND nCA <= 10000000) THEN 

									SELECT TACCODE into nTACCODE from dossierprospect where dosid=ndosid 
								    and  dprversion IN (SELECT dprversion
																  FROM v_deal
																 WHERE DOSID = NDOSID);

										IF (nTACCODE ='CBM') THEN 

										    select PFIINVESTISSEMENT,PFINBPERIODES INTO ninv,nnbr from propositionfinanciere where pfiid IN
												   (SELECT pfiid
													  FROM dprpropfinance
													 WHERE     DOSID = NDOSID
														   AND dprversion IN
																   (SELECT dprversion
																	  FROM v_deal
																	 WHERE DOSID = NDOSID));

												   IF ( ninv>5000000 OR nnbr>60) THEN
													  nok := 1;
												   ELSE 
													nok :=0;
												   END IF;
										END IF;

										IF (nTACCODE ='CBI') THEN 

										    IF ( ninv > 10000000 OR nnbr > 144) THEN
													  nok := 1;
											ELSE 
													nok :=0;
										    END IF;
										END IF;


						ELSE 
						   nok :=1;

                        END IF;
				END;

   ----------------ICH SOGELEASE controle sur lad renseigne
    ELSIF SCONTROLE IN ('LAD')
        THEN
        DECLARE
        ncount NUMBER := 0;
        ncount1 NUMBER := 0;
        staccode VARCHAR2(20);
        Sworcode VARCHAR2(20);
        BEGIN
        select taccode into staccode from dossierprospect where dosid=ndosid;
        select max (WORCODE) into Sworcode from dprworstep where dosid=ndosid;
        
        if Sworcode='WFCLIPRO' and staccode='CBM'
        then
        SELECT NVL (COUNT (anaid), 0) --exitence du sf
        INTO ncount
        FROM analysis
        WHERE dosid = ndosid
        AND anmid =5;
        
         IF (ncount>0) THEN
        
         SELECT NVL (COUNT (RATVALUE), 0)
        INTO ncount1
        FROM lkanarat
        WHERE ratid IN (SELECT ratid
        FROM ratio
        WHERE ratcode = 'LADSGM')
        AND anaid = (select max(anaid) from analysis where dosid=nDosId and ANMID=5 );---champ lad renseigné
        
        
        IF ( ncount1 = 0) THEN
        
         NOK :=0;
        ELSE
        nok := 1; --OK
        END IF;
        END IF;
        end if;
        
         END;

    ---------------------------

    		  ELSIF SCONTROLE IN ('EXTCA')
            THEN
                DECLARE
                     ncount    NUMBER := 0;

                BEGIN
                     SELECT NVL (COUNT (CVANUMERICVALUE), 0)
                      INTO ncount
                      FROM cchvalue
                      WHERE CCHSID = 'TFDCCHSID91' 
					  AND dosidprospect = ndosid;	

					IF (ncount > 0)

                    THEN
                          nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                 EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END; 


    --------ICH

	  ELSIF SCONTROLE IN ('CLTPR')
            THEN
                DECLARE
                      ncount    NUMBER := 0;


                BEGIN

				    SELECT  COUNT (*)           
                      INTO ncount
                      FROM dpracteur
                      WHERE  dosid=NDOSID 
					  AND DPRVERSION =PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (NDOSID)
					  AND rolcode='CLIENT';

                     IF   (ncount>1) THEN 


				  NOK :=0;                                      
                  ELSE
                    nok := 1;  --OK
                      END IF;

                END; 
---------------


      ELSIF SCONTROLE IN ('DVAL')
            THEN
                DECLARE
                     SDMASTATUS    VARCHAR2(20);

                BEGIN
                     SELECT distinct (DMASTATUS)
                      INTO SDMASTATUS
                      FROM documentmanagement   
					  where dosidprospect = ndosid;
				

					IF (SDMASTATUS in ('BLANK' ,'ACCCON' ,'ATTACHED'  ,'REJEC'  ))
                    
                    THEN
                          nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                 EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END; 
							
				
        			
  --------
    ELSIF SCONTROLE IN ('DAUT')           THEN
               DECLARE
                ncount  NUMBER := 0;
				ncount1   NUMBER := 0;
				ncount2   NUMBER := 0;



                BEGIN


					 SELECT NVL (COUNT (DPMDTRELEASE), 0)            
                      INTO ncount
                     FROM DPRMATERIEL
                    WHERE dosid = ndosid  ;

					 SELECT NVL (COUNT (DPMEXTERNALREF), 0)            
                      INTO ncount1
                      FROM DPRMATERIEL
                    WHERE dosid = ndosid  ;

				  SELECT NVL (COUNT (AADORDRESUPPLIER), 0)            
                     INTO ncount2
                    FROM DPRMATERIEL
                     WHERE dosid = ndosid  ;


                   IF ncount = 0 OR ncount1 = 0 OR  ncount2 = 0  
                   THEN
                      NOK :=0; 
                   ELSE
                           nok := 1;  --OK
                   END IF;


               EXCEPTION
                   WHEN OTHERS
             THEN
                       NOK := 0;
              END;   




     ------ICH ---SGM45---controle sur existence d un client rattache 

      ELSIF SCONTROLE IN ('CLEXT')
            THEN
                DECLARE
                      ncount    NUMBER := 0;

                BEGIN

					  SELECT  COUNT (*)           
                      INTO ncount
                      FROM dpracteur
                      WHERE  dosid=NDOSID 
					  AND DPRVERSION =PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (NDOSID)
					  AND rolcode='CLIENT';

                    IF (ncount=0) THEN


				  NOK :=0;                                      
                  ELSE
                    nok := 1;  --OK
                      END IF;

                END; 




                -----HME CONTROLE SUR WF

                ELSIF SCONTROLE IN ('WF')
            THEN
                DECLARE

                    nseg        VARCHAR2 (10);
                    STPGCODE   DOSSIERPROSPECT.TPGCODE%TYPE;
                    npfiid     VARCHAR2 (10); 
                    nmtinv     NUMBER :=0;
                    nbp        NUMBER :=0;

                BEGIN
				 select PAV4_CRITERIA.F_GET_SEGMENTATION_CLIENT (Ndosid,'ORFI')INTO  nseg from dual;

				 if (nseg in ('10201','10202')) THEN 
				    select TPGCODE INTO STPGCODE from dossierprospect where DOSID=Ndosid;
                    select pfiid INTO  npfiid from dprpropfinance where dosid=Ndosid AND DPRVERSION =
                                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (NDOSID);
                    select PFIINVESTISSEMENT, PFINBPERIODES into nmtinv,nbp from propositionfinanciere where pfiid=npfiid;

				        IF ((STPGCODE='CBM01' AND (nmtinv>5000000 OR nbp>60)) OR (STPGCODE='CBI01' AND (nmtinv>10000000 OR nbp>144 ))) THEN 
                             NOK :=0;
                        ELSE 
                             NOK :=1;
                        END IF;

				 END IF;
				END;
                -------------HME SOGELEASE CONTROLE SUR somme des tirages
                ELSIF SCONTROLE IN ('MTSMA')
            THEN
                DECLARE

                    ndosaut      VARCHAR (50) := NULL;
					ncount       NUMBER := 0;
					NSUMT        VARCHAR (50) := NULL;
					--npfiid       VARCHAR (50) := NULL;
                    pfiidchild    VARCHAR (50) := NULL;
					nmtinv       VARCHAR (50) := NULL;
                    NMTINVCHILD  VARCHAR (50) := NULL;
					MTDISPO      VARCHAR (50) := NULL;

                BEGIN
                select PFIID into pfiidchild from dprpropfinance where dosid=ndosid;
                select PFIINVESTISSEMENT into NMTINVCHILD from propositionfinanciere where pfiid=pfiidchild;
				 SELECT count(*) into ncount

                         FROM dprcomplement
                         WHERE     Dosid = Ndosid
                         AND DPRVERSION = (SELECT Dprversion
                                                   FROM V_Deal Vde
                                                  WHERE Vde.Dosid = Ndosid);

		             IF (ncount>0) THEN

					   SELECT DOSIDAUTORISATION into ndosaut

                         FROM dprcomplement
                         WHERE     Dosid = Ndosid
                         AND DPRVERSION = (SELECT Dprversion
                                                   FROM V_Deal Vde
                                                  WHERE Vde.Dosid = Ndosid);

					 SELECT SUM(DM1MTINVESTCHILD) INTO NSUMT from l1dprmateriel where dosid=ndosaut;

					 select pav4_selectenveloppe.F_GET_MONTANTDISPO(ndosaut) into MTDISPO from dual;

					    IF (nmtinv<NSUMT OR MTDISPO<0 ) THEN
						  NOK :=0;
						ELSE
                          nok :=1;	
                        END IF;
                      END IF;  
				END;


            --------------existence de Marque, Energie, Modele, Puissance fiscale saisis dans "Materiel"
            ELSIF SCONTROLE IN ('DETM')
            THEN
                DECLARE
                    ncount    NUMBER := 0;
                    ncount1   NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (ACACODE), 0)              --Marque : ok
                      INTO nCOUNT1
                      FROM DPRMATERIEL
                     WHERE dosid = ndosid AND ACACODE = 'ROUT';

                    IF (ncount1 = 0)
                    THEN
                        nok := 1;
                    ELSE
                        SELECT NVL (COUNT (MAKID), 0)            --Marque : ok
                          INTO nCOUNT
                          FROM DPRMATERIEL
                         WHERE dosid = ndosid;

                        IF (nCOUNT = 0)
                        THEN
                            nok := 0;
                        ELSIF nCOUNT > 0
                        THEN --Si la marque est selectionnee, on verifie les autres
                            SELECT NVL (COUNT (MMOCODE), 0)           --Modele
                              INTO nCOUNT
                              FROM DPRMATERIEL
                             WHERE dosid = ndosid;

                            IF (nCOUNT = 0)
                            THEN
                                nok := 0;
                            ELSIF nCOUNT > 0
                            THEN --si le modele est selectionne, on verifie ceux qui restent
                                SELECT NVL (COUNT (DPMENERGIE), 0) --Energie / OK
                                  INTO nCOUNT
                                  FROM DPRMATERIEL
                                 WHERE dosid = ndosid;

                                IF (nCOUNT = 0)
                                THEN
                                    nok := 0;
                                ELSIF nCOUNT > 0
                                THEN --si l'energie est saisie, on verifie la puissance fiscale
                                    SELECT NVL (COUNT (DPMPUISSANCE), 0) --Puissance fiscale : ok
                                      INTO nCOUNT
                                      FROM DPRMATERIEL
                                     WHERE dosid = ndosid;

                                    IF (nCOUNT = 0)
                                    THEN
                                        nok := 0;
                                    ELSIF nCOUNT > 0
                                    THEN
                                        nok := 1;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            -----------20191127 JJO UAT-297 FIN
            ---- HMN debut controle  ( si vehicule particulier et apport inf 70% et puissance sup 9 cv, controle bloquant) 20170623
            ELSIF SCONTROLE IN ('VPAP')
            THEN
                DECLARE
                    categorie        dprmateriel.dpmgenre%TYPE;
                    L_Premiermin     Propositionfinanciere.Pfiinvestissement%TYPE;
                    L_Premierloyer   Propositionfinanciere.Pfimtpremierloyer%TYPE;
                    Puissance        Dprmateriel.Dpmpuissance%TYPE;
                    MontantVPTotal   PROPOSITIONFINANCIERE.PFIINVESTISSEMENT%TYPE;
                    MontantVP        PROPOSITIONFINANCIERE.PFIINVESTISSEMENT%TYPE;

                    CURSOR MATVP9
                    IS
                        SELECT Dpmpuissance, dpmgenre
                          INTO puissance, categorie
                          FROM Dprmateriel
                         WHERE     Dosid = Ndosid
                               AND dpmgenre = 'VP'
                               AND DPMPUISSANCE > 9
                               AND DPRVERSION = (SELECT Dprversion
                                                   FROM V_Deal Vde
                                                  WHERE Vde.Dosid = Ndosid);
                BEGIN
                    SELECT Pro.Pfimtpremierloyer
                      INTO L_Premierloyer
                      FROM DPRPROPFINANCE DPR, PROPOSITIONFINANCIERE PRO
                     WHERE     Pro.Pfiid = Dpr.Pfiid
                           AND DPR.DOSID = nDosid
                           AND DPR.DPRVERSION =
                               (SELECT DPRVERSION
                                  FROM V_DEAL VDE
                                 WHERE VDE.DOSID = DPR.DOSID)
                           AND Dpr.Dpfflagretenue = 1;


                    MontantVP := 0;
                    MontantVPTotal := 0;

                    FOR MATVP9_CPT IN MATVP9
                    LOOP
                        SELECT DPMMTINVEST INTO MontantVP FROM dprmateriel;

                        MontantVPTotal := MontantVPTotal + MontantVP;
                    END LOOP;

                    L_Premiermin := MontantVPTotal * 0.7;

                    IF L_Premierloyer < L_Premiermin
                    THEN
                        Nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        Nok := 1;
                END;
            ---- HMN debut controle  ( si vehicule particulier et apport inf 70% et puissance sup 9 cv, controle bloquant) 20170623

            END IF;

            RETURN NOK;
        END;



    END OCDOSSIERFRONT;

    FUNCTION OCDOSSIER (NDOSID      IN DOSSIER.DOSID%TYPE,
                        SCONTROLE   IN VARCHAR2,
                        NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK             NUMBER := 1;
            NIMMOCONTRACT   NUMBER;
            NDFIORDRE       DOSFINANCE.DFIORDRE%TYPE;
            NMTDOSSIER      NUMBER;
            NCOUNT          NUMBER := 0;
            SSTRINGVALUE    CCHVALUE.CVASTRINGVALUE%TYPE;
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION
            SRUBCODE        RUBRIQUE.RUBCODE%TYPE;
            NTXASSIETTE     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
            NRESULT         DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
            NLIMIT          DOSRUBFLUX.DRFTXASSIETTE%TYPE := 2.5;
            -- LMI  2013-09-10 EVD_MEL DELIQUENCY EXISTS
            NACTIDGESTION   DOSSIER.ACTID%TYPE;
            NACTIDCLIENT    DOSACTEUR.ACTID%TYPE;
            SDEVCODEDOS     DOSSIER.DEVCODE%TYPE;
            STPGCODE        DOSSIER.TPGCODE%TYPE;

            CURSOR DRU
            IS
                SELECT DRF.DRFTXASSIETTE, DRU.DRUORDRE, DRF.DRFORDRE
                  FROM DOSRUBRIQUE DRU, DOSRUBFLUX DRF, RUBRIQUE RUB
                 WHERE     DRU.DOSID = NDOSID
                       AND DRU.DRUTYPE = 'R'
                       AND DRU.DOSID = DRF.DOSID
                       AND DRU.DRUORDRE = DRF.DRUORDRE
                       AND DRU.RUBID = RUB.RUBID
                       AND RUB.RUBCODE = SRUBCODE;
        -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION

        BEGIN
            DBMS_OUTPUT.PUT_LINE ('1528 ' || SCONTROLE);

            IF (SCONTROLE IN ('PENG',
                              'DRUMT',
                              'CHQCO',
                              'DTIER',
                              'DTFAC',
                              'DTEFF',
                              'DTRUB',
                              'DTCOM',
                              'DMA',
                              'TBSEC',
                              'RB',
                              'ACRIB'))
            THEN
                NOK := OCDOSGENERALITES (NDOSID, SCONTROLE, NCREID);
            ELSIF SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1) IN ('DCLI',
                                                                    'ADJ',
                                                                    'NC',
                                                                    'ROLE',
                                                                    'FLAT',
                                                                    'NORE',
                                                                    'COMG',
                                                                    'COMR',
                                                                    'VENT',
                                                                    'RULI',
                                                                    'PARA',
                                                                    'MT10',
                                                                    'FORM',
                                                                    'DGTA',
                                                                    'REMB',
                                                                    'NBEX')
            THEN
                NOK :=
                    OCDOSGENERALITES (
                        NDOSID,
                        SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1),
                        NCREID);
            ELSIF (SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1) IN ('EVQP',
                                                                     'QPNE',
                                                                     'PTRI',
                                                                     'VEAS',
                                                                     'VEPR',
                                                                     'APPO',
                                                                     'MAIT',
                                                                     'ROLF',
                                                                     'QPN'))
            THEN
                NOK :=
                    OCDOSRUBGENERALITES (
                        NDOSID,
                        SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1),
                        NCREID);
            ELSIF (SCONTROLE IN ('RFTAX',
                                 'VNI72',
                                 'TFTAX',
                                 'VTTAX'))
            THEN
                NOK := OCDOSSPECITALIE (NDOSID, SCONTROLE, NCREID);
            ELSIF SCONTROLE = 'BBASE'
            THEN
                NOK := OCDOSBORROWINGBASE (NDOSID);
            -- LMI 2014-01-22 Check Guarantee Execution document for a special contract during EVD_MEL
            ELSIF SCONTROLE = ('GUADO')
            THEN
                BEGIN
                    SELECT 1
                      INTO NOK
                      FROM DOCUMENTMANAGEMENT DMA
                     WHERE     DMA.DOSID = NDOSID
                           AND DMATYPE = 'EVID'
                           AND DMASTATUS = 'FULLYRECEIVED';
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 0;
                    WHEN TOO_MANY_ROWS
                    THEN
                        NOK := 1;
                END;
            -- LMI 2013-10-11 Check Evidence document and Roles for owner of a collateral



            ELSIF SCONTROLE = 'FEA'
            THEN
                SELECT MAX (CVASTRINGVALUE)
                  INTO SSTRINGVALUE
                  FROM CCHVALUE
                 WHERE CCHSID = 'TFDCCHSID45' AND DOSID = NDOSID;

                IF SSTRINGVALUE IS NOT NULL
                THEN
                    SELECT COUNT (1)
                      INTO NCOUNT
                      FROM CCHVALUE
                     WHERE     CCHSID = 'TFDCCHSID45'
                           AND CVASTRINGVALUE = SSTRINGVALUE;

                    IF NCOUNT != 1
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'MFCRB'
            THEN
                SELECT F_PLGETCUSTOMCHARBOOLEAN ('DOSSIER',
                                                 'CHKCCHVAL116',
                                                 DOSIDREFINANCE,
                                                 'EN')
                  INTO NCOUNT
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                IF NCOUNT = 1
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'SAME'
            THEN
                SELECT SUM (NVL (IRUMTCONTRACTCURRENCY, 0))
                  INTO NIMMOCONTRACT
                  FROM ITRRUBRIQUE
                 WHERE (ITRID, IRUORDRE) IN (SELECT DISTINCT ITRID, IRUORDRE
                                               FROM LKDOSRUBITRRUB A
                                              WHERE A.DOSID = NDOSID);

                SELECT MIN (DFIORDRE)
                  INTO NDFIORDRE
                  FROM DOSFINANCE
                 WHERE DOSID = NDOSID;

                IF NDFIORDRE IS NULL
                THEN
                    SELECT NVL (DOSMTOUVERT, 0)
                      INTO NMTDOSSIER
                      FROM DOSSIER
                     WHERE DOSID = NDOSID;
                ELSE
                    SELECT NVL (DFIMTTOTAL, 0)
                      INTO NMTDOSSIER
                      FROM DOSFINANCE
                     WHERE DOSID = NDOSID AND DFIORDRE = NDFIORDRE;
                END IF;

                IF NMTDOSSIER != NIMMOCONTRACT
                THEN
                    NOK := 0;
                END IF;
            -- CV-10022010 CFS40204
            ELSIF SCONTROLE IN ('GKFLX', 'GK1DT', 'GKTXA')
            THEN
                NOK := OCDOSRUBGENERALITES (NDOSID, SCONTROLE, NCREID);
            -- LMI  2013-09-10 EVD_MEL DELIQUENCY EXISTS
            ELSIF (SCONTROLE = 'D01')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSSIER
                 WHERE DOSID = NDOSID AND TPGCODE IN ('A');

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF scontrole IN ('DURES') -- 20120503  ------Controle duree restante lors de la cession a VR
            THEN
                DECLARE
                    l_dtdeb      dossier.dosdtdeb%TYPE;
                    l_dtfin      dossier.dosdtfin%TYPE;
                    l_dtderloy   dosrubecheancier.dredtdeb%TYPE;
                    l_duree      NUMBER := 0;
                BEGIN
                    --date debut du dossier :
                    SELECT MIN (dphdteffet)
                      INTO l_dtdeb
                      FROM dosphase
                     WHERE dosid = ndosid AND phacode = 'ES';


                    -- date fin du dossier :
                    SELECT MAX (dredtfin)
                      INTO l_dtfin
                      FROM dosrubecheancier
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17)
                           AND dretype = 'LOYER';

                    IF l_dtfin IS NULL
                    THEN
                        SELECT dosdtfin
                          INTO l_dtfin
                          FROM dossier
                         WHERE dosid = ndosid;
                    END IF;


                    -- date derniere echeance facturee :
                    SELECT NVL (MIN (dredtdeb), l_dtfin)
                      INTO l_dtderloy
                      FROM dosrubecheancier
                     WHERE     dosid = ndosid
                           AND druordre IN
                                   (SELECT druordre
                                      FROM dosrubrique
                                     WHERE     dosid = ndosid
                                           AND druclasse = 'F'
                                           AND drutype = 'F'
                                           AND rubid = 17)
                           AND dretype = 'LOYER'
                           AND facid IS NULL;


                    SELECT ROUND ((TO_DATE (l_dtfin)) - TO_DATE (l_dtderloy))
                      INTO l_duree
                      FROM DUAL;


                    IF (l_duree > 0)
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CLOIS') -- 20120123 controle sur le cloisonnement des agences
            -- Erreur donc KO.
            --MTR 20130711
            --extraire le dernier utilisateur ayant acc?d? au dossier
            --Tester le groupe du dernier utilisateur
            --S?il est dans : ('IT','GRPORFI','BOC', 'FC', 'RA','RC','JU','DBO') ALORS : contr?le ne se lance pas
            --Sinon
            --   Test si l'utilisateur a la meme agence que le dossier
            --   si oui: le controle ne se lance pas
            --   sinon:
            --       Tester si l?utilisateur est intervenant sur le dossier
            --       Si oui : Contr?le ne se lance pas
            --       Sinon : contr?le se lance.

            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_uticonnect   DOSSIER.UTICODE%TYPE := NULL;
                    l_grocode      DOSSIER.UTICODE%TYPE := NULL;
                    l_count1       NUMBER := 0;
                    l_count2       NUMBER := 0;
                BEGIN
                    SELECT MAX (UTICODE)
                      INTO l_uticonnect
                      FROM UTIRECENTENTITY
                     WHERE     uenid = nDosid
                           AND entcode = 'DOSSIER'
                           AND uendtlastaccessed IN
                                   (SELECT MAX (uendtlastaccessed)
                                      FROM UTIRECENTENTITY
                                     WHERE     uenid = nDosid
                                           AND entcode = 'DOSSIER')
                           AND uenid IN (SELECT dosid
                                           FROM dossier
                                          WHERE dossectgestion = 'PROD'); --20120206 exclure contentieux

                    IF (l_uticonnect IS NOT NULL)
                    THEN                        --Tester le groupe utilisateur
                        SELECT MAX (grocode)
                          INTO l_grocode
                          FROM utilisateur
                         WHERE     grocode IN ('IT',
                                               'GRPORFI',
                                               'BOC',
                                               'FC',
                                               'RA',
                                               'RC',
                                               'JU',
                                               'DBO')
                               AND uticode = l_uticonnect;
                    END IF;

                    IF (l_grocode IS NOT NULL)
                    THEN                                      --Utilisateur OK
                        nOk := 1;
                    ELSE
                        IF (l_uticonnect IS NOT NULL)
                        THEN
                            SELECT COUNT (*)
                              INTO l_count1
                              FROM LKUTIARO
                             WHERE     actid IN
                                           (SELECT actid
                                              FROM dosacteur
                                             WHERE     dosid = nDosid
                                                   AND rolcode = 'AGENCE'
                                                   AND dacdtfin IS NULL)
                                   AND uticode = l_uticonnect;

                            IF (l_count1 < 1)
                            THEN                 --ce n'est pas la meme agence
                                SELECT COUNT (*)
                                  INTO l_count2
                                  FROM lkdosutitsm
                                 WHERE     dosid = nDosid
                                       AND uticode = l_uticonnect;

                                IF (l_count2 < 1)
                                THEN   -- n'est pas intervenant sur le dossier
                                    nOk := 0;
                                ELSE
                                    nOk := 1;
                                END IF;
                            ELSE
                                nOk := 1;
                            END IF;
                        ELSE
                            nOk := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOk := 0;
                END;
            ELSIF scontrole IN ('INFFA') -- 11102011 EVOL controle sur exoneration TVA
            ----MAJ RBA 2018-02-09 - rajout DACDTTAXFREEEND obligatoire
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_DACREFTAXFREE           DOSACTEUR.DACREFTAXFREE%TYPE;
                    l_DACDTTAXFREEAGREEMENT   DOSACTEUR.DACDTTAXFREEAGREEMENT%TYPE;
                    l_DACDTTAXFREEBEGIN       DOSACTEUR.DACDTTAXFREEBEGIN%TYPE;
                    l_DACDTTAXFREEEND         DOSACTEUR.DACDTTAXFREEEND%TYPE;
                BEGIN
                    SELECT DACREFTAXFREE,
                           DACDTTAXFREEAGREEMENT,
                           DACDTTAXFREEBEGIN,
                           DACDTTAXFREEEND
                      INTO l_DACREFTAXFREE,
                           l_DACDTTAXFREEAGREEMENT,
                           l_DACDTTAXFREEBEGIN,
                           l_DACDTTAXFREEEND
                      FROM DOSACTEUR
                     WHERE     dosid = nDosid
                           AND rolcode = 'CLIENT'
                           AND dacordre =
                               (SELECT MAX (dacordre)
                                  FROM dosacteur
                                 WHERE dosid = nDosid AND rolcode = 'CLIENT');


                    IF (l_DACREFTAXFREE IS NOT NULL)
                    THEN
                        IF     (l_DACDTTAXFREEAGREEMENT IS NOT NULL)
                           AND (l_DACDTTAXFREEBEGIN IS NOT NULL)
                           AND (l_DACDTTAXFREEEND IS NOT NULL)
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSIF (l_DACDTTAXFREEAGREEMENT IS NOT NULL)
                    THEN
                        IF     (l_DACREFTAXFREE IS NOT NULL)
                           AND (l_DACDTTAXFREEBEGIN IS NOT NULL)
                           AND (l_DACDTTAXFREEEND IS NOT NULL)
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSIF (l_DACDTTAXFREEEND IS NOT NULL)
                    THEN
                        IF     (l_DACREFTAXFREE IS NOT NULL)
                           AND (l_DACDTTAXFREEBEGIN IS NOT NULL)
                           AND (l_DACDTTAXFREEAGREEMENT IS NOT NULL)
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSIF (l_DACDTTAXFREEBEGIN IS NOT NULL)
                    THEN
                        IF     (l_DACDTTAXFREEAGREEMENT IS NOT NULL)
                           AND (l_DACREFTAXFREE IS NOT NULL)
                           AND (l_DACDTTAXFREEEND IS NOT NULL)
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSIF     (l_DACREFTAXFREE IS NULL)
                          AND (l_DACDTTAXFREEAGREEMENT IS NULL)
                          AND (l_DACDTTAXFREEBEGIN IS NULL)
                          AND (l_DACDTTAXFREEEND IS NULL)
                    THEN
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            /*           --20180918 RBS
                        ELSIF scontrole IN ('DGMON') -- Controle que le montage (AMOFIX) et mode de calcul (IPA) du depot de garantie est correct
                       THEN
                          -- nOk:=0 ==> Le retour du CTRL est KO
                          -- nOk:=1 ==> Le retour du CTRL est OK.
                          DECLARE
                             l_count   NUMBER;
                          BEGIN
                             SELECT Nvl(COUNT (*),0)
                               INTO l_count
                               FROM dosrubrique
                              WHERE dosid = ndosid
                                AND rubid = 193
                                AND (drumontage<>'AMOFIX' OR drumodcalint<>'IPA');
                           IF l_count > 0
                             THEN
                                nok := 0;       -- Le montage ou le mode de calcul ne sont pas correct ==> KO
                             ELSE
                                nok := 1; -- Montage correct ==> OK
                             END IF;
                          EXCEPTION
                             WHEN OTHERS
                             THEN
                                nok := 0;                                 -- Erreur donc KO.
                          END;      */
            ELSIF scontrole IN ('DGMON') -- Controle que le montage (AMOFIX) et mode de calcul (IPA) du depot de garantie est correct
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_count   NUMBER;
                    dEFFET    DATE;
                BEGIN
                    IF nDOSID IN (11129,
                                  110810,
                                  133622,
                                  133623,
                                  144323,
                                  144950,
                                  154481)
                    THEN
                        RETURN 1;
                    ELSE
                        SELECT MAX (credteffet)
                          INTO dEFFET
                          FROM crevt
                         WHERE     tmffonction IN ('EVD_NEGO', 'EVD_CNEGO')
                               AND dosid = nDOSID
                               AND creid =
                                   (SELECT MAX (creid)
                                      FROM crevt
                                     WHERE     tmffonction IN
                                                   ('EVD_NEGO', 'EVD_CNEGO')
                                           AND dosid = nDOSID
                                           AND credtcreat LIKE SYSDATE)
                               AND credtcreat LIKE SYSDATE;

                        IF dEFFET IS NULL
                        THEN
                            SELECT NVL (COUNT (*), 0)
                              INTO l_count
                              FROM dosrubrique
                             WHERE     dosid = ndosid
                                   AND rubid = 193
                                   AND (   drumontage <> 'AMOFIX'
                                        OR drumodcalint <> 'IPA')
                                   AND druordre IN
                                           (SELECT MAX (druordre)
                                              FROM dosrubrique
                                             WHERE     dosid = ndosid
                                                   AND rubid = 193)
                                   AND drudtfin >= SYSDATE;
                        ELSE
                            SELECT NVL (COUNT (*), 0)
                              INTO l_count
                              FROM dosrubrique
                             WHERE     dosid = ndosid
                                   AND rubid = 193
                                   AND (   drumontage <> 'AMOFIX'
                                        OR drumodcalint <> 'IPA')
                                   AND druordre IN
                                           (SELECT MAX (druordre)
                                              FROM dosrubrique
                                             WHERE     dosid = ndosid
                                                   AND rubid = 193)
                                   AND drudtfin >= dEFFET;
                        END IF;
                    END IF;

                    IF l_count > 0
                    THEN
                        nok := 0; -- Le montage ou le mode de calcul ne sont pas correct ==> KO
                    ELSE
                        nok := 1;                    -- Montage correct ==> OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 1;                           -- Erreur donc OK.
                END;
            --26012011
            ELSIF scontrole IN ('DPROM') -- le code du CTRL dans la table est U_DPROM, dans le PKG, il faut mettre DPROM
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_count   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM dosacteur
                     WHERE dosid = ndosid AND rolcode = 'PROMOTE';

                    IF (l_count = 1)
                    THEN
                        nok := 1; -- un seul tiers avec le role PROMOTE ==> OK
                    ELSE
                        nok := 0; -- pas de tiers ou plusieurs avec le role PROMOTE ==> KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TTVA') -- BMI 02082011 controle sur le taux de TVA de la rubrique financiere
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_TaxCode   VARCHAR2 (10);

                    CURSOR c_tax
                    IS
                        SELECT DISTINCT taxcode
                          FROM dosrubrique
                         WHERE dosid = ndosid AND rubid IN (17, 235);
                BEGIN
                    OPEN c_tax;

                    LOOP
                        FETCH c_tax INTO l_TaxCode;

                        EXIT WHEN c_tax%NOTFOUND;

                        IF (l_TaxCode = 'TVATN')
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                            EXIT;
                        END IF;
                    END LOOP;

                    CLOSE c_tax;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DGESC') -- BMI 02082011 controle sur le depot de garantie
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_drumaitre   NUMBER;
                    l_count       NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM dosrubrique
                     WHERE dosid = ndosid AND rubid = 193;

                    IF (l_count >= 1)
                    THEN
                        SELECT MAX (dr1.druordremaitre)
                          INTO l_drumaitre                          --23092011
                          FROM dosrubrique dr1
                         WHERE     dr1.dosid = ndosid
                               AND dr1.rubid = 193
                               AND dr1.druordremaitre IN
                                       (SELECT MAX (dr2.druordre)
                                          FROM dosrubrique dr2
                                         WHERE     dr2.dosid = ndosid
                                               AND dr2.rubid = 17
                                               AND dr1.drudtdeb =
                                                   dr2.drudtdeb);


                        IF (l_drumaitre IS NOT NULL)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF (scontrole = 'TASB')
            THEN -- EBM Controle bloquant pour les champs Numero titre foncier, Adresse du bien et superficie  (au niveau du dossier info generales) pour les CBI
                DECLARE
                    nCount1   NUMBER;
                    nCount2   NUMBER;
                    nCount3   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount1
                      FROM CCHVALUE
                     WHERE     dosid = ndosid
                           AND CCHSID = 'TFDCCHSID721' --,'TFDCCHSID722','TFDCCHSID723')
                           AND ndosid IN (SELECT ndosid
                                            FROM dossier
                                           WHERE taccode = 'CBI')
                           AND CVASTRINGVALUE IS NOT NULL;

                    SELECT COUNT (*)
                      INTO nCount2
                      FROM CCHVALUE
                     WHERE     dosid = ndosid
                           AND CCHSID = 'TFDCCHSID722' --,'TFDCCHSID722','TFDCCHSID723')
                           AND ndosid IN (SELECT ndosid
                                            FROM dossier
                                           WHERE taccode = 'CBI')
                           AND CVASTRINGVALUE IS NOT NULL;


                    SELECT COUNT (*)
                      INTO nCount3
                      FROM CCHVALUE
                     WHERE     dosid = ndosid
                           AND CCHSID = 'TFDCCHSID723' --,'TFDCCHSID722','TFDCCHSID723')
                           AND ndosid IN (SELECT ndosid
                                            FROM dossier
                                           WHERE taccode = 'CBI')
                           AND CVASTRINGVALUE IS NOT NULL;


                    IF nCount1 <= 0 OR nCount2 <= 0 OR nCount3 <= 0
                    THEN
                        nOK := 0;
                    END IF;
                END;
            ELSIF (scontrole = 'BESA') --EBM controle sur materiel en service et tableau d'amortissement
            THEN
                DECLARE
                    nCount   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO ncount
                      FROM lkdosrubitrrub  lk,
                           dosphase        ph,
                           itrrubrique     rub,
                           itrrubphase     itr
                     WHERE     lk.dosid = ndosid
                           AND lk.dosid = ph.dosid
                           AND rub.itrid = itr.ITRID
                           AND lk.itrid = rub.itrid
                           AND itr.iruordre = rub.iruordre
                           AND lk.iruordre = rub.iruordre
                           AND itr.irpdtfin IS NULL
                           AND itr.phacode = 'ES'
                           AND ph.phacode = 'ES'
                           AND EXISTS
                                   (SELECT 1
                                      FROM itrrubfluamort a);

                    IF nCount <= 0
                    THEN
                        nOK := 0;
                    END IF;
                END;
            ELSIF scontrole IN ('RIBCI')                   -- EBM controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    s_ribclient      VARCHAR2 (50);
                    s_banqueclient   RIB.RIBCOMPTE%TYPE;
                    s_ribcible       NUMBER;
                    s_banquecible    RIB.RIBCOMPTE%TYPE;
                BEGIN
                    SELECT SUBSTR (TO_CHAR (ribcompte), 1, 3)
                      INTO s_ribclient
                      FROM DOSACTEUR DAC, RIB RIB, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAP.DAPTYPE = 'E'
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.DAPDTFIN IS NULL
                           AND DAP.RIBID = RIB.RIBID;

                    SELECT ribid
                      INTO s_ribcible
                      FROM DOSACTPAIEMENT DAP
                     WHERE DAP.DAPTYPE = 'C' AND DAP.DOSID = ndosid;

                    IF    (    s_ribclient NOT IN ('011', '021')
                           AND s_ribcible = 2)
                       OR (s_ribclient = '021' AND s_ribcible = 1)
                       OR (s_ribclient = '011' AND s_ribcible = 3)
                    THEN
                        nok := 1;                                        -- OK
                    ELSE
                        nok := 0;                                        -- KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DEPCLAS')
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_count   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM DEPLIGNE DLI
                     WHERE     DLI.DOSID = ndosid
                           AND DLI.depid NOT IN
                                   (SELECT depid
                                      FROM crevt
                                     WHERE     tmffonction = 'EVDEP_ANNUL'
                                           AND credtvalid IS NOT NULL
                                           AND credtsup IS NOT NULL);

                    IF (l_count = 0)
                    THEN
                        nok := 1;
                    ELSE
                        nok := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            --MTR RIBCP: Controle migre vers D09831 dans PA_DOSSCONTROLE2
            ELSIF scontrole IN ('RIBCP')
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_count   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count                                 -- 20120406
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAP.DAPTYPE = 'E'
                           AND (   (    DAP.TMPCODE IN ('PRLAUTO')
                                    AND DAP.RIBID IS NOT NULL --EMI 2012/08/27
                                    AND EXISTS
                                            (SELECT DAPTYPE
                                               FROM DOSACTPAIEMENT DAP2
                                              WHERE     DAP2.DAPTYPE = 'C'
                                                    AND DAP2.DOSID = ndosid
                                                    AND DAP2.DACORDRE =
                                                        DAP.DACORDRE
                                                    AND dap2.tmpcode =
                                                        'PRLAUTO')) --EMI 2012/06/17 pour controler coherence tmpcode client vs cible
                                OR DAP.TMPCODE NOT IN ('PRLAUTO'))
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.DAPDTFIN IS NULL
                           AND DAC.DACDTFIN IS NULL;

                    IF (l_count > 0)
                    THEN
                        nOk := 1;
                    ELSE
                        nOk := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOk := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('MIMFC') -- BMI 19072011 controle sur la Mise en Force (Partielle ou Totale)
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_DtEffet    CREVT.CREDTEFFET%TYPE;
                    l_MtAssets   ITRRUBRIQUE.IRUMTORIGINE%TYPE;
                    l_MtRubFi    DOSRUBRIQUE.DRUMTORIGINE%TYPE;
                BEGIN
                    SELECT CREDTEFFET
                      INTO l_DtEffet
                      FROM CREVT
                     WHERE     DOSID = nDosId
                           AND CREID = nCreId
                           AND TMFFONCTION = 'EVD_MEL';         --BMI 24082011

                    SELECT SUM (IRUMTORIGINE) IRUMTORIGINE
                      INTO l_MtAssets
                      FROM ITRRUBRIQUE a, ITRRUBPHASE b, CREVT c
                     WHERE     a.ITRID = b.ITRID
                           AND a.IRUORDRE = b.IRUORDRE
                           AND b.CREID = c.CREID
                           AND b.IRPDTFIN IS NULL
                           AND b.PHACODE = 'ES'
                           AND c.CREID = nCreId
                           AND c.DOSID = nDosId;


                    SELECT SUM (DRUMTORIGINE)
                      INTO l_MtRubFi                -- MNG 20140508 ticket 476
                      FROM DOSRUBRIQUE
                     WHERE     RUBID = 17
                           AND DOSID = nDosId
                           AND DRUDTDEB = l_DtEffet;

                    IF (l_MtRubFi IS NOT NULL) AND (l_MtAssets IS NOT NULL)
                    THEN
                        IF (l_MtRubFi = l_MtAssets)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            --MTR controle MICMF migre dans PA_DOSRUBCONTROLE

            ELSIF scontrole IN ('MICMF') -- BMI 19072011 controle sur l'ajout d'un bien
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_DtEffet    CREVT.CREDTEFFET%TYPE;
                    l_MtAssets   ITRRUBRIQUE.IRUMTORIGINE%TYPE;
                    l_MtRubFi    DOSRUBRIQUE.DRUMTORIGINE%TYPE;
                BEGIN
                    SELECT CREDTEFFET
                      INTO l_DtEffet
                      FROM CREVT a
                     WHERE     DOSID = nDosId
                           AND a.CREID =
                               (SELECT MAX (b.CREID)
                                  FROM CREVT b
                                 WHERE     b.TMFFONCTION = 'EVD_ADJ' --AND b.CREDTSUP IS NULL
                                       AND a.DOSID = b.DOSID);


                    SELECT SUM (IRUMTORIGINE) IRUMTORIGINE
                      INTO l_MtAssets
                      FROM ITRRUBRIQUE a, ITRRUBPHASE b, CREVT c
                     WHERE     a.ITRID = b.ITRID
                           AND a.IRUORDRE = b.IRUORDRE
                           AND b.CREID = c.CREID
                           AND b.IRPDTFIN IS NULL
                           AND b.PHACODE = 'ES'
                           AND c.CREID = nCreId
                           AND c.DOSID = nDosId;


                    SELECT DRUMTORIGINE
                      INTO l_MtRubFi
                      FROM DOSRUBRIQUE
                     WHERE     RUBID = 17
                           AND DOSID = nDosId
                           AND DRUDTDEB = l_DtEffet
                           AND druordreprec IS NULL;        -- BMI20120123 TLG

                    IF (l_MtRubFi IS NOT NULL) AND (l_MtAssets IS NOT NULL)
                    THEN
                        IF (l_MtRubFi = l_MtAssets)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            --10022011 BRI
            ELSIF scontrole IN ('RUBDF')
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDos   DATE;
                    l_dateRub   DATE;
                BEGIN
                    SELECT drudtfin
                      INTO l_dateRub
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druclasse = 'F'
                           AND drutype = 'F'
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F');

                    SELECT dosdtfin
                      INTO l_dateDos
                      FROM dossier
                     WHERE dosid = ndosid;

                    IF (l_dateDos = l_dateRub)
                    THEN
                        nok := 1; -- date fin de la rubrique = date fin du dossier ==> OK
                    ELSE
                        nok := 0; --  ==> date fin de la rubrique differente date fin du dossierKO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            --11022011  RIC

            ELSIF scontrole IN ('RUBDT')
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDos   DATE;
                    l_dateRub   DATE;
                BEGIN
                    SELECT drudtfin
                      INTO l_dateRub
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druclasse = 'F'
                           AND drutype = 'F'
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F');

                    SELECT dosdtfin
                      INTO l_dateDos
                      FROM dossier
                     WHERE dosid = ndosid;

                    IF (l_dateDos = l_dateRub)
                    THEN
                        nok := 1; -- date fin de la rubrique = date fin du dossier ==> OK
                    ELSE
                        nok := 0; --  ==> date fin de la rubrique differente date fin du dossierKO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ---RIC 20110303

            ELSIF scontrole IN ('TAUX')                     --EBM controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_taux          NUMBER;
                    l_Ancien_taux   NUMBER;
                    l_TXEX          NUMBER;
                BEGIN
                    SELECT drutauxfixe
                      INTO l_taux
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 177);

                    SELECT drutauxfixe
                      INTO l_Ancien_taux
                      FROM imadosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM imadosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 177)
                           AND imaid =
                               (SELECT MAX (imaid)
                                  FROM imadosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);


                    SELECT TVAVAL
                      INTO l_TXEX
                      FROM tauvaleur
                     WHERE taucode = 'TAUXEXC' AND tvadtfin IS NULL;

                    IF (l_taux = l_Ancien_taux)
                    THEN
                        nok := 1;
                    ELSE                                 -- changement de taux
                        IF (l_taux <= l_TXEX)
                        THEN
                            nok := 1; -- Le taux excessif n'as pas ete depasse
                        ELSE
                            nok := 0; -- Le taux excessif a ete depasse ==> KO
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TEXE') -- le code du CTRL dans la table est U_TAUX, dans le PKG, il faut mettre TAUX
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_phase   VARCHAR2 (20);
                    l_dos     VARCHAR2 (20);
                BEGIN
                    SELECT PHACODE
                      INTO l_phase
                      FROM dosphase
                     WHERE dosid = nDosid AND dphdtfin IS NULL;

                    IF (l_phase = 'ENG')
                    THEN
                        SELECT dostypefinancement
                          INTO l_dos
                          FROM dossier
                         WHERE dosid = nDosid;

                        IF (l_dos = 'RIC')
                        THEN
                            nok := 1; -- Le taux excessif n'as pas ete depasse
                        ELSE
                            nok := 0; -- Le taux excessif a ete depasse ==> KO
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTDEB')                   -- EBM controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    d_datedeb   DATE;
                    s_jour      VARCHAR2 (2);
                BEGIN
                    SELECT dosdtdeb
                      INTO d_datedeb
                      FROM dossier
                     WHERE dosid = ndosid;

                    IF (d_datedeb IS NOT NULL)
                    THEN
                        s_jour := SUBSTR (TO_CHAR (d_datedeb), 1, 2);

                        IF s_jour NOT IN ('05', '15', '25')
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RIBEF') -- 12102011 contr?le sur la saisie du RIB cible EFFET
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_RIBC     NUMBER;
                    l_RIBDOS   NUMBER;

                    CURSOR c_RIBC
                    IS
                        SELECT DISTINCT TO_NUMBER (tupcode)
                          FROM tusparam
                         WHERE tusnom = 'RIBCEFFET';
                BEGIN
                    SELECT MAX (DAP.RIBID)
                      INTO l_RIBDOS
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAP.DAPTYPE = 'C'
                           AND DAP.TMPCODE IN ('EFFET')
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.RIBID IS NOT NULL
                           AND DAP.DAPDTFIN IS NULL;

                    IF (l_RIBDOS IS NOT NULL)
                    THEN
                        OPEN c_RIBC;

                        LOOP
                            FETCH c_RIBC INTO l_RIBC;

                            EXIT WHEN c_RIBC%NOTFOUND;

                            IF (l_RIBC = l_RIBDOS)
                            THEN
                                nok := 1;                                -- OK
                                EXIT;
                            ELSE
                                nok := 0;                                -- KO
                            END IF;
                        END LOOP;

                        CLOSE c_RIBC;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RIBPR') -- 12102011 contr?le sur la saisie du RIB cible PRLAUTO
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_RIBC     NUMBER;
                    l_RIBDOS   NUMBER;

                    CURSOR c_RIBC
                    IS
                        SELECT DISTINCT TO_NUMBER (tupcode)
                          FROM tusparam
                         WHERE tusnom = 'RIBCPRLAUTO';
                BEGIN
                    SELECT MAX (DAP.RIBID)
                      INTO l_RIBDOS
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAP.DAPTYPE = 'C'
                           AND DAC.DACDTFIN IS NULL --MSB 2810 cas de transfert, prendre uniquement le dernier client
                           AND DAP.TMPCODE IN ('PRLAUTO')
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.RIBID IS NOT NULL
                           AND DAP.DAPDTFIN IS NULL;

                    IF (l_RIBDOS IS NOT NULL)
                    THEN
                        OPEN c_RIBC;

                        LOOP
                            FETCH c_RIBC INTO l_RIBC;

                            EXIT WHEN c_RIBC%NOTFOUND;

                            IF (l_RIBC = l_RIBDOS)
                            THEN
                                nok := 1;                                -- OK
                                EXIT;
                            ELSE
                                nok := 0;                                -- KO
                            END IF;
                        END LOOP;

                        CLOSE c_RIBC;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CESIM') -- BMI 21032011 controle sur impayes lors de la cession
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_SumImp   NUMBER;
                BEGIN
                    SELECT NVL (SUM (f_plrestantfacture (FAC.FACID, NULL)),
                                0)
                      INTO l_SumImp
                      FROM facreference  fac,
                           dossier       dos,
                           crevt         cr,
                           facture       fact,
                           dosacteur     DAC ----ticket TLBO-1603 filtrage sur factures d'avoir
                     WHERE     fac.fredosid = nDosid
                           AND fac.fredosid = dos.dosid
                           AND dac.dosid = dos.dosid
                           AND dac.rolcode = 'CLIENT'
                           AND dac.dacdtfin IS NULL
                           AND dac.actid = fact.actidclient
                           AND fac.facid = fact.facid ----ticket TLBO-1603 filtrage sur factures d'avoir
                           AND fact.facidorigine IS NULL ----ticket TLBO-1603 filtrage sur factures d'avoir
                           AND dos.dossectgestion != 'CONT' -- BMI 12092011 exclure les dossiers en CTX
                           AND cr.creid = nCreId
                           AND cr.dosid = dos.dosid
                           AND cr.uticodecreat NOT IN
                                   (SELECT DISTINCT uticode
                                      FROM utilisateur
                                     WHERE    grocode IN ('IT', 'DBO')
                                           OR UTICODE IN ('AMIRH', 'HELAH')); --FER 13022013 AJOUTER LE GROUPE DBO

                    --or uticode='AMIRH' ==> mention rajout?e par  RBA 31/07/2014 - Ticket TLBO-551
                    ------ or UTICODE IN ('AMIRH','HELAH') ISA ---TLBO-623 ISA 07/11/2014
                    IF (l_SumImp IS NULL)
                    THEN
                        l_SumImp := 0;
                    END IF;

                    IF (l_SumImp > 0)
                    THEN
                        nok := 0; --0 instead of 1 kilani                       -- KO existance d'impayes
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('MSUBS') -- BMI 20042011 controle sur modification tiers avant MEF
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_imaid        NUMBER;
                    l_actid        NUMBER;
                    l_rolcode      VARCHAR2 (15);
                    l_imaactid     NUMBER;
                    l_imarolcode   VARCHAR2 (15);
                    l_phacode      VARCHAR2 (10);

                    CURSOR c_imadosact
                    IS
                        SELECT actid, rolcode
                          FROM imadosacteur
                         WHERE dosid = nDosId AND imaid = l_imaid;
                BEGIN
                    SELECT phacode
                      INTO l_phacode
                      FROM dosphase
                     WHERE dosid = ndosid AND dphdtfin IS NULL;

                    IF l_phacode IN ('ES', 'TER')
                    THEN
                        nok := 1; -- le contr?le doit se lancer uniquement avant MEF
                    ELSE
                        SELECT imaid
                          INTO l_imaid
                          FROM crevt
                         WHERE creid = nCreId AND dosid = nDosId;

                        IF (l_imaid IS NOT NULL)
                        THEN
                            OPEN c_imadosact;

                            LOOP
                                FETCH c_imadosact
                                    INTO l_imaactid, l_imarolcode;

                                EXIT WHEN c_imadosact%NOTFOUND;

                                SELECT actid, rolcode
                                  INTO l_actid, l_rolcode
                                  FROM dosacteur
                                 WHERE     dosid = nDosId
                                       AND rolcode = l_imarolcode
                                       AND actid = l_imaactid;

                                IF     (l_actid = l_imaactid)
                                   AND (l_rolcode = l_imarolcode)
                                THEN
                                    nok := 1;
                                ELSE
                                    nok := 0;
                                    EXIT;
                                END IF;
                            END LOOP;

                            CLOSE c_imadosact;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('ASUBS') -- BMI 20042011 controle sur ajout tiers CLIENT avant MEF
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_imaid        NUMBER;
                    l_actid        NUMBER;
                    l_uticonnect   utirecententity.uticode%TYPE;
                    l_imaactid     NUMBER;

                    CURSOR c_dosact
                    IS
                        SELECT actid
                          FROM dosacteur
                         WHERE dosid = nDosId AND rolcode = 'CLIENT';
                BEGIN
                    SELECT imaid
                      INTO l_imaid
                      FROM crevt
                     WHERE creid = nCreId AND dosid = nDosId;

                    SELECT MAX (UTICODE)
                      INTO l_uticonnect            -- teh 13022013-- extlgbo71
                      FROM UTIRECENTENTITY
                     WHERE     uenid = nDosid
                           AND entcode = 'DOSSIER'
                           AND uendtlastaccessed IN
                                   (SELECT MAX (uendtlastaccessed)
                                      FROM UTIRECENTENTITY
                                     WHERE     uenid = nDosid
                                           AND entcode = 'DOSSIER')
                           AND uticode NOT IN
                                   (SELECT uticode
                                      FROM utilisateur
                                     WHERE grocode IN
                                               ('IT', 'GRPORFI', 'DBO'));

                    IF (l_imaid IS NOT NULL AND l_uticonnect IS NOT NULL)
                    THEN
                        OPEN c_dosact;

                        LOOP
                            FETCH c_dosact INTO l_actid;

                            EXIT WHEN c_dosact%NOTFOUND;

                            SELECT actid
                              INTO l_imaactid
                              FROM imadosacteur
                             WHERE     dosid = nDosId
                                   AND rolcode = 'CLIENT'
                                   AND imaid = l_imaid
                                   AND actid = l_actid;


                            IF (l_imaactid IS NOT NULL)
                            THEN
                                nok := 1;
                            ELSE
                                nok := 0;
                                EXIT;
                            END IF;
                        END LOOP;

                        CLOSE c_dosact;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('BSUBS') -- BMI 19042011 bloquer la substitution de tiers apres la premiere facturation
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_facid       NUMBER;
                    l_phacode     VARCHAR2 (10);
                    l_count_ima   NUMBER;
                    l_count       NUMBER;
                    l_count2      NUMBER := 0;
                    l_imaid       NUMBER;
                    l_sum         NUMBER := 0;

                    CURSOR l_actid
                    IS
                        SELECT actid
                          FROM dosacteur
                         WHERE dosid = ndosid AND dacdtfin IS NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM dosacteur
                     WHERE dosid = ndosid AND dacdtfin IS NULL;

                    BEGIN
                        SELECT MAX (imaid)
                          INTO l_imaid
                          FROM crevt
                         WHERE dosid = ndosid AND creid = ncreid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            l_imaid := NULL;
                    END;

                    IF l_imaid IS NOT NULL
                    THEN
                        SELECT COUNT (*)
                          INTO l_count_ima
                          FROM imadosacteur
                         WHERE imaid = l_imaid AND dacdtfin IS NULL;
                    ELSE
                        l_count_ima := l_count;
                    END IF;

                    IF l_count > l_count_ima
                    THEN
                        nok := 1; -- Ne pas bloquer, il s'agit d'un ajout de tiers et non de substitution
                    ELSE
                        SELECT phacode
                          INTO l_phacode
                          FROM dosphase
                         WHERE dosid = ndosid AND dphdtfin IS NULL;

                        IF l_phacode = 'TER'
                        THEN
                            nok := 1;
                        ELSE
                            FOR c_actid IN l_actid
                            LOOP
                                SELECT COUNT (*)
                                  INTO l_count2
                                  FROM imadosacteur
                                 WHERE     imaid = l_imaid
                                       AND dacdtfin IS NULL
                                       AND actid = c_actid.actid;

                                IF l_count2 <> 0
                                THEN
                                    l_sum := l_sum + 1;
                                END IF;
                            END LOOP;

                            IF l_sum = l_count
                            THEN
                                nok := 1; --MSB 20120106 tester si aucune substitution n'a ?t? faite (m?me acteurs sur dossier apr?s evt) (Chgt d'adr ou de moyen de pmt)
                            ELSE
                                SELECT MAX (facid)
                                  INTO l_facid
                                  FROM dosrubecheancier
                                 WHERE     dosid = nDosId
                                       AND dretype = 'LOYER'
                                       AND druordre IN
                                               (SELECT druordre
                                                  FROM dosrubrique
                                                 WHERE     dosid = nDosId
                                                       AND druclasse = 'F'
                                                       AND drutype = 'F'
                                                       AND rubid = 17)
                                       AND dredtech IN
                                               (SELECT MIN (dredtech)
                                                  FROM dosrubecheancier
                                                 WHERE     dosid = nDosId
                                                       AND dretype = 'LOYER'
                                                       AND druordre IN
                                                               (SELECT druordre
                                                                  FROM dosrubrique
                                                                 WHERE     dosid =
                                                                           nDosId
                                                                       AND druclasse =
                                                                           'F'
                                                                       AND drutype =
                                                                           'F'
                                                                       AND rubid =
                                                                           17));

                                IF (l_facid IS NULL)
                                THEN
                                    nok := 1;                            -- OK
                                ELSE
                                    nok := 0;                            -- KO
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBDFA') -- BMI 21032011 controle sur date fin des rubriques accesoires
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateRubF    DATE;
                    l_dateRubA1   DATE;
                    l_dateRubA2   DATE;
                BEGIN
                    SELECT drudtfin
                      INTO l_dateRubF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtfin)
                      INTO l_dateRubA1
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 197);

                    SELECT MAX (drudtfin)
                      INTO l_dateRubA2
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 22);

                    IF     (l_dateRubA2 IS NOT NULL)
                       AND (l_dateRubA1 IS NOT NULL)
                    THEN
                        IF    (l_dateRubA1 > l_dateRubF)
                           OR (l_dateRubA2 > l_dateRubF)
                        THEN
                            nok := 0;                 -- KO existance d'impaye
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTDG') -- BMI 20111213 controle sur date debut et date fin des DG
            THEN
                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebDG   DATE;
                    l_dateFinF    DATE;
                    l_dateFinDG   DATE;
                    l_druordre    NUMBER (4);
                BEGIN
                    SELECT MIN (drudtdeb), MAX (drudtfin), MAX (druordre)
                      INTO l_dateDebF, l_dateFinF, l_druordre
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17
                                       AND druordreprec IS NOT NULL); --20111115

                    SELECT MIN (drudtdeb), MAX (drudtfin)
                      INTO l_dateDebDG, l_dateFinDG
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordremaitre = l_druordre
                           AND rubid = 193;

                    IF     (l_dateDebDG IS NOT NULL)
                       AND (l_dateFinDG IS NOT NULL)
                    THEN
                        IF    (l_dateDebF > l_dateDebDG)
                           OR (l_dateFinF < l_dateFinDG)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTDGA') -- BMI 21032011 controle alerte sur date debut et date fin des DG
            THEN
                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebDG   DATE;
                    l_dateFinF    DATE;
                    l_dateFinDG   DATE;
                    l_druordre    NUMBER (4);
                BEGIN
                    SELECT MIN (drudtdeb), MAX (drudtfin), MAX (druordre)
                      INTO l_dateDebF, l_dateFinF, l_druordre
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17
                                       AND druordreprec IS NOT NULL); --20111115

                    SELECT MIN (drudtdeb), MAX (drudtfin)
                      INTO l_dateDebDG, l_dateFinDG
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordremaitre = l_druordre
                           AND rubid = 193;

                    IF     (l_dateDebDG IS NOT NULL)
                       AND (l_dateFinDG IS NOT NULL)
                    THEN
                        IF    (l_dateDebDG > l_dateDebF)
                           OR (l_dateFinDG < l_dateFinF)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBFT') -- BMI 30032011 controle sur dates debut et fin des rubriques de frais de timbre
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 22);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 22);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebF <> l_dateDebRA)
                           OR (l_dateFinF <> l_dateFinRA)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBLCB') -- BMI 30032011 controle bloquant sur dates debut et fin des rubriques de frais de lettre de change
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 197);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 197);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA < l_dateDebF)
                           OR (l_dateFinRA > l_dateFinF)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBLCA') -- BMI 30032011 controle alerte sur dates debut et fin des rubriques de frais de lettres de change
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 197);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 197);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA > l_dateDebF)
                           OR (l_dateFinRA < l_dateFinF)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBASB') -- BMI 30032011 controle bloquant sur dates debut et fin des rubriques d'assurance
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 6);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 6);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA < l_dateDebF)
                           OR (l_dateFinRA > l_dateFinF)
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBASA') -- BMI 30032011 controle alerte sur dates debut et fin des rubriques d'assurance
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 6);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 6);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA > l_dateDebF)
                           OR (l_dateFinRA < l_dateFinF)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBPRB') -- BMI 05042011 controle bloquant sur dates debut et fin des rubriques promotion
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MIN (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MIN (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 219);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 219);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA < l_dateDebF)
                           OR (l_dateFinRA > l_dateFinF)
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RBPRA') -- BMI 05042011 controle alerte sur dates debut et fin des rubriques promotion
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_dateDebF    DATE;
                    l_dateDebRA   DATE;
                    l_dateFinF    DATE;
                    l_dateFinRA   DATE;
                BEGIN
                    SELECT drudtdeb
                      INTO l_dateDebF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT drudtfin
                      INTO l_dateFinF
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (drudtdeb)
                      INTO l_dateDebRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 219);

                    SELECT MAX (drudtfin)
                      INTO l_dateFinRA
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'A'
                                       AND drutype = 'F'
                                       AND rubid = 219);

                    IF     (l_dateDebRA IS NOT NULL)
                       AND (l_dateFinRA IS NOT NULL)
                    THEN
                        IF    (l_dateDebRA > l_dateDebF)
                           OR (l_dateFinRA < l_dateFinF)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RTNL') -- BMI 21042011 bloquer le passage en phase realisation si bien en TNL non adosse
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_itrid                NUMBER;
                    l_dostypefinancement   VARCHAR2 (15);
                    l_count                NUMBER;
                BEGIN
                    SELECT dostypefinancement
                      INTO l_dostypefinancement
                      FROM dossier
                     WHERE dosid = nDosId;

                    IF (l_dostypefinancement = 'TTNL')
                    THEN
                        SELECT COUNT (*)
                          INTO l_count                             -- 20111028
                          FROM itrrubphase i, lkdosrubitrrub l
                         WHERE     l.itrid = i.itrid
                               AND i.irpdtfin IS NULL
                               AND i.phacode = 'TER'
                               AND l.dosid = nDosId;

                        IF (l_count > 0)
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TAXVE') -- 20111017 controle sur la taxe lors de la vente
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_taxcode      VARCHAR2 (20);
                    l_dphdteffet   DATE;
                BEGIN
                    SELECT MAX (taxcode)
                      INTO l_taxcode
                      --from facligne fl, facture fa
                      FROM sasfacligne fl, sasfacture fa            --20120401
                     WHERE     fl.rubid = 11         --and fl.facid = fa.facid
                           AND fl.sfaid = fa.sfaid
                           AND fa.creid = nCreId;

                    IF (l_taxcode IS NOT NULL)
                    THEN
                        SELECT MAX (dphdteffet)
                          INTO l_dphdteffet
                          FROM dosphase
                         WHERE     dosid = nDosid
                               AND phacode = 'ES'
                               AND jalcode IN ('ES', 'PART')
                               AND DPHDTMAJ =
                                   (SELECT MAX (DPHDTMAJ)
                                      FROM dosphase
                                     WHERE     dosid = nDosid
                                           AND phacode = 'ES'
                                           AND jalcode IN ('ES', 'PART'));

                        IF (l_dphdteffet <= '31/12/2007')
                        THEN
                            IF (l_taxcode = 'EXOTN')
                            THEN
                                nok := 1;                                -- KO
                            ELSE
                                nok := 0;
                            END IF;
                        ELSE
                            IF (l_taxcode = 'TVATN')
                            THEN
                                nok := 1;                                -- KO
                            ELSE
                                nok := 0;
                            END IF;
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('ENCVT') -- BMI 21032011 controle sur le montant totale de la vente par rapport au montant des encours
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_Fac_MtHT    NUMBER;
                    l_Mt_calc     NUMBER;
                    l_TacCode     DOSSIER.TACCODE%TYPE;
                    l_Taux        NUMBER;
                    l_DtEvt       DATE;
                    l_DtDruDeb    DATE;
                    l_druordre    NUMBER;
                    l_EncoursHT   NUMBER;
                    l_penal       NUMBER;
                    l_VR          NUMBER;
                    l_duree       NUMBER;
                    l_imaid       NUMBER;
                BEGIN
                    /*        select max(flimthtbase) into l_Fac_MtHT
                            from sasfacligne fl, sasfacture fa
                            where fa.creid =nCreId
                            and fl.fliordre = 1 and fl.sfaid =fa.sfaid;

                            if (l_Fac_MtHT is null) then l_Fac_MtHT:=0;
                            end if;

                            select max(credteffet) into l_DtEvt
                            from crevt
                            where creid =nCreId;

                            select max(druordre) into l_druordre
                            from dosrubrique
                            where dosid=ndosid
                            and druclasse='F' and drutype='F' and rubid = 17;

                            select max(drudtdeb) into l_DtDruDeb
                            from dosrubrique
                            where dosid =ndosid
                            and druordre =l_druordre;

                            select max(TACCODE) into l_TacCode
                            from dossier
                            where dosid =nDosid;

                            select max(DREMTAMO) into l_VR
                            from dosrubecheancier
                            where dosid =nDosid
                            and dretype ='VR'
                            and druordre =l_druordre;


                            l_duree :=round(to_date(l_DtEvt,'DD/MM/YYYY')-to_date(l_DtDruDeb,'DD/MM/YYYY'));

                            if (l_TacCode != 'CBI') then

                              if (l_duree <= 365) then
                                   l_Taux:= 0.07;
                            elsif  (l_duree> 365) and (l_duree <= 730) then
                                   l_Taux:= 0.05;
                            elsif (l_duree > 730) then
                                   l_Taux:= 0.03;
                              end if;

                            else
                                   l_Taux:= 0.03;
                            end if;

                            l_EncoursHT :=f_plecfrub(nDosid,l_druordre,'TND','TND',l_DtEvt) ;*/
                    SELECT cremt, imaid
                      INTO l_Fac_MtHT, l_imaid
                      FROM crevt
                     WHERE     creid = nCreId
                           AND TMFFONCTION = 'EVD_VENTE'
                           AND CREDTSUP IS NULL;

                    /*
                       select max(DREDTECH)into l_DtEvt
                     from dosrubecheancier
                     where dosid =ndosid
                       and druordre in (select druordre from dosrubrique
                             where dosid=ndosid
                             and druclasse='F' and drutype IN('F','M') and rubid IN(17,235) ) and dretype ='LOYER'
                     and facid is not null;


                     if (l_DtEvt is not null) then
                       select sum(dremtecf) into l_EncoursHT
                       from dosrubecheancier
                       where dosid =ndosid
                       and druordre in (select druordre from dosrubrique
                             where dosid=ndosid
                             and druclasse='F' and drutype in('F','M') and rubid in (17,235) )
                       and dretype ='LOYER'
                       and DREDTECH= l_DtEvt;
                     end if;    */
                    --        l_penal := l_EncoursHT * (1 + l_Taux);

                    --      l_Mt_calc := l_EncoursHT + l_VR + l_penal;

                    BEGIN
                        SELECT pav4_tlg_reporting2.f_plgetdossierencours_ima (
                                   l_imaid)
                          INTO l_EncoursHT
                          FROM DUAL;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            l_EncoursHT := 0;
                    END;

                    IF (l_Fac_MtHT < l_EncoursHT)
                    THEN
                        nok := 0;                                        -- KO
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('PRIVI') -- le code du CTRL si existe privilege sur Immo
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_count   NUMBER;
                BEGIN
                    SELECT SUM (cvabooleanvalue)
                      INTO l_count
                      FROM cchvalue
                     WHERE     cchsid = 'IMMOPRIVILEGE'
                           AND itrid IN (SELECT itrid
                                           FROM lkdosrubitrrub
                                          WHERE dosid = nDosid);

                    IF (l_count > 0)
                    THEN
                        nok := 0; -- existe au moin un bien avec privilege sur dossier ==> KO
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TIIM') -- Controle sur taux d'interet intercalaire: si taux du contrat < au taux min --> taux i inter =9%
            THEN
                DECLARE
                    l_taux_rf   NUMBER;
                    l_taux_ii   NUMBER;
                    l_TMIN      NUMBER;
                BEGIN
                    SELECT MAX (drutauxfixe)
                      INTO l_taux_rf
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MIN (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    SELECT MAX (TVAVAL)
                      INTO l_TMIN
                      FROM tauvaleur
                     WHERE taucode = 'TMIN';

                    SELECT MAX (drutauxfixe)
                      INTO l_taux_ii
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'P'
                                       AND rubid = 150);

                    IF (l_taux_rf IS NOT NULL) AND (l_taux_ii IS NOT NULL)
                    THEN
                        IF (l_taux_rf > l_TMIN)
                        THEN
                            nok := 1;
                        ELSE
                            IF (l_taux_ii = l_TMIN)
                            THEN
                                nok := 1;                                   --
                            ELSE
                                nok := 0;                                   --
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TIIC')
            -- Controle sur taux d'interet intercalaire: si taux du contrat > au taux min --> taux i inter = taux du contrat
            THEN
                DECLARE
                    --l_taux_rf   NUMBER;
                    l_taux_TEG   NUMBER;
                    l_taux_ii    NUMBER;
                    l_TMIN       NUMBER;
                BEGIN
                    /*                 SELECT MAX (ROUND (drutauxfixe, 3))
                                       INTO l_taux_rf
                                       FROM dosrubrique
                                      WHERE     dosid = ndosid
                                            AND druordre =
                                                   (SELECT MIN (druordre)
                                                      FROM dosrubrique
                                                     WHERE     dosid = ndosid
                                                           AND druclasse = 'F'
                                                           AND drutype = 'F'
                                                           AND rubid = 17);*/

                    SELECT F_CALCUL_TEG_BO (ndosid, 'FR', 'ORFI')
                      INTO l_taux_TEG
                      FROM DUAL;


                    SELECT MAX (ROUND (TVAVAL, 3))
                      INTO l_TMIN
                      FROM tauvaleur
                     WHERE taucode = 'TMIN';

                    SELECT MAX (ROUND (drutauxfixe, 3))
                      INTO l_taux_ii
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'P'
                                       AND rubid = 150);

                    IF (l_taux_TEG IS NOT NULL) AND (l_taux_ii IS NOT NULL)
                    THEN
                        IF (l_taux_TEG <= l_TMIN)
                        THEN
                            nok := 1;
                        ELSE
                            IF (l_taux_ii = l_taux_TEG)
                            THEN
                                nok := 1;                                   --
                            ELSE
                                nok := 0;                                   --
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TIIE') -- Controle sur taux d'interet intercalaire: si taux i inter =9% > TEXE --> KO
            THEN
                DECLARE
                    l_taux          NUMBER;
                    l_Ancien_taux   NUMBER;
                    l_TXEX          NUMBER;
                BEGIN
                    SELECT MAX (drutauxfixe)
                      INTO l_taux
                      FROM dosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'P'
                                       AND rubid = 150);

                    SELECT MAX (drutauxfixe)
                      INTO l_Ancien_taux
                      FROM imadosrubrique
                     WHERE     dosid = ndosid
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM imadosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'P'
                                       AND rubid = 150)
                           AND imaid =
                               (SELECT MAX (imaid)
                                  FROM imadosrubrique
                                 WHERE     dosid = ndosid
                                       AND druclasse = 'F'
                                       AND drutype = 'P'
                                       AND rubid = 150);


                    SELECT MAX (TVAVAL)
                      INTO l_TXEX
                      FROM tauvaleur
                     WHERE taucode = 'TXEX';

                    IF (l_taux IS NOT NULL) AND (l_Ancien_taux IS NOT NULL)
                    THEN
                        IF (l_taux = l_Ancien_taux)
                        THEN
                            nok := 1;
                        ELSE                             -- changement de taux
                            IF (l_taux > l_TXEX)
                            THEN
                                nok := 0; --  modification par rapport ? la V4.0                                      --
                            ELSE
                                nok := 1; --  modification par rapport ? la V4.0                                          --
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            --15032011 RIC CONTROLE SUR CONDITION PVL TLG

            /*        ELSIF scontrole IN ('PVL') -- le code du CTRL dans la table est U_PVL, dans le PKG, il faut mettre TAUX
                 THEN
                    -- nOk:=0 ==> Le retour du CTRL est KO
                    -- nOk:=1 ==> Le retour du CTRL est OK.
                    DECLARE
                       l_res   NUMBER;
                       l_val   varchar2(20) :=0;
                    BEGIN

                       select count(*)
                       into l_res
                       from cchvalue
                        where dosid=ndosid;

                       SELECT cvastringvalue
                       INTO l_val
                       from cchvalue
                       WHERE dosid =ndosid;


                       IF (l_val='CONFOR' or l_val='NAPPLI' or l_res=1)

                       THEN
                          nok := 1;            -- La condition est conforme

                       ELSE
                          nok := 0;            -- La condition est non conforme ou non renseignee==> KO

                 END IF;
                    EXCEPTION
                       WHEN OTHERS
                       THEN
                          nok := 0;                                      -- Erreur donc KO.
                    END;

            */
            ELSIF scontrole IN ('PVL') --EBM controle 4.0 -- BRI 23032011 Conditions prealables:PVL signe et date
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID580';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseign?e==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('FACD') -- BRI 23032011 Conditions prealables:Facture definitive avec toutes les mentions obligatoires
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2736';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CVSV') -- BRI 23032011 Conditions prealables:Contrat de vente signe par le vendeur
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2737';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CCPR') -- BRI 23032011 Conditions prealables:Copie conforme de la procuration
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2738';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RVME') -- BRI 23032011 Conditions prealables:Rapport de visite materiel effectue par TL
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2739';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            -----------------MTR 20180220
            --------EMI 21-02-2018
            ELSIF SCONTROLE = ('CLBO')
            THEN
                DECLARE
                    nCount          NUMBER := 0;
                    l_tmffonction   VARCHAR2 (50) := '';
                BEGIN
                    SELECT tmffonction
                      INTO l_tmffonction
                      FROM crevt
                     WHERE creid = ncreid;

                    IF (l_tmffonction = 'EVD_MEL')
                    THEN
                        SELECT COUNT (1)
                          INTO nCount
                          FROM ADMINISTRATIF ADM, ADMSTATUS AST
                         WHERE     DOSID = nDosid
                               AND forid IN
                                       (SELECT lf.foridliee
                                          FROM formalite    f,
                                               tevfor       tf,
                                               l1formalite  lf
                                         WHERE     fortype IN
                                                       ('CHECKLIST', 'ITEM')
                                               AND f.forid = lf.forid
                                               AND tf.forid = f.forid
                                               AND tf.tmffonction =
                                                   'EVD_REAL') --RBS TLG45 20180329
                               AND ADMIDPARENT IS NOT NULL
                               AND adm.admid = ast.admid
                               AND Ast.Astdtend IS NULL
                               AND ast.aststatus NOT IN
                                       ('TER', 'WAIVE', 'NOA');
                    ELSE
                        SELECT COUNT (1)
                          INTO nCount
                          FROM ADMINISTRATIF ADM, ADMSTATUS AST
                         WHERE     DOSID = nDosid
                               AND forid IN
                                       (SELECT lf.foridliee
                                          FROM formalite    f,
                                               tevfor       tf,
                                               l1formalite  lf
                                         WHERE     fortype IN
                                                       ('CHECKLIST', 'ITEM')
                                               AND f.forid = lf.forid
                                               AND tf.forid = f.forid
                                               AND tf.tmffonction = 'EVD_MEL') --RBS TLG45 20180329
                               AND ADMIDPARENT IS NOT NULL
                               AND adm.admid = ast.admid
                               AND Ast.Astdtend IS NULL
                               AND ast.aststatus NOT IN
                                       ('TER', 'WAIVE', 'NOA');
                    END IF;


                    IF nCount = 0
                    THEN
                        NOK := 1;
                    ELSE
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                --  NOK := 1;
                END;
            ---Widad / Ramla / Oumaima 20190909
            ELSIF SCONTROLE = ('CLTST')
            THEN
                DECLARE
                    nCount          NUMBER := 0;
                    l_tmffonction   VARCHAR2 (50) := '';
                BEGIN
                    SELECT tmffonction
                      INTO l_tmffonction
                      FROM crevt
                     WHERE creid = ncreid;

                    IF (l_tmffonction = 'EVD_VPART')
                    THEN
                        SELECT COUNT (1)
                          INTO nCount
                          FROM ADMINISTRATIF ADM, ADMSTATUS AST
                         WHERE     DOSID = nDosid
                               AND forid IN (10384, 10385) ---Widad / Ramla / Oumaima 20190909
                               AND ADMIDPARENT IS NOT NULL
                               AND adm.admid = ast.admid
                               AND Ast.Astdtend IS NULL
                               AND ast.aststatus NOT IN ('TER');
                    END IF;


                    IF nCount = 0
                    THEN
                        NOK := 1;
                    ELSE
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                --  NOK := 1;
                END;
            ---Widad / Ramla / Oumaima 20190909



            ELSIF scontrole IN ('RADE') -- BRI 23032011 Conditions prealables:Reception des avis de debit par notre banque
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2740';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RFAF') -- BRI 23032011 Conditions prealables: Reception des factures des autres frais
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2741';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CLGG') -- BRI 23032011 Conditions prealables: Copie lisible de la carte grise
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2742';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('FASA') -- BRI 23032011 Conditions prealables: Formulaire d?assurance rempli et signe par l?assureur
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2743';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DASP') -- BRI 23032011 Conditions prealables: Demande adhesion signe par les trois parties
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2744';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RFTB') -- BRI 23032011 Conditions prealables: Reception de la facture du transitaire
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2745';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('NCES') -- BRI 23032011 Conditions prealables: Nouvelle chaine d?effets signee par le client
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2746';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('ASLG') -- BRI 23032011 Conditions prealables: Nouvel aval signe legalise par le garant
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2747';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CEQE') -- BRI 23032011 Conditions prealables: Contrat de leasing enregistre
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2748';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RDCVL') -- BRI 23032011 Conditions prealables: Recu de depot du contrat de vente legalise enregistre a la CPF
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2749';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DPCV') -- BRI 23032011 Conditions prealables: Decharge promoteur du contrat de vente
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2750';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('NPVHN') -- BRI 23032011 Conditions prealables: PV de notification etabli par l?huissier notaire
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2751';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DSVE') -- BRI 23032011 Conditions prealables: Demande de cession
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2752';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('CCSC') -- BRI 23032011 Conditions prealables: Contrat de cession signe par le client
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2753';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('MASA') -- BRI 23032011 Conditions prealables: Message de l?assureur
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2754';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DDRE') -- BRI 23032011 Conditions prealables: Decharge de la demande de retrait des effets de la banque
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2755';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ---RBT TLG_EVD_CNEGO : controle sur le Montant HT d'un Frais
            ELSIF (sControle = 'FRAL')
            THEN
                DECLARE
                    nMtHt    NUMBER := 0;
                    nNorme   NUMBER := 0;
                BEGIN
                    --D?tection de la valeur du Frais dans le dossier
                    SELECT NVL (MAX (DRF.DRFMT), -1)
                      INTO nMtHt
                      FROM DOSRUBRIQUE DRU, DOSRUBFLUX DRF
                     WHERE     DRU.RUBID = 102
                           AND DRU.DOSID = DRF.DOSID
                           AND DRU.DRUORDRE IN
                                   (SELECT MAX (druordre)
                                      FROM dosrubrique dru1
                                     WHERE     dru1.dosid = dru.dosid
                                           AND dru1.rubid = dru.rubid)
                           AND DRU.DRUORDRE = DRF.DRUORDRE
                           AND dru.DRUORDREGRPFAC IS NOT NULL
                           AND dru.dosid = nDosid;

                    --D?tection de la valeur de r?f?rence du Frais
                    --select to_number(replace(DTR.DDEHOSTVALUE,',','.'))
                    SELECT TTP.TPANOMBRE
                      INTO nNorme
                      FROM TOPPARAM TTP
                     WHERE TOPTABLE = 'MTRUBDEF' AND tpaparam = 'FRAL'; ----MSB EXTTLGBO-2713


                    IF nMtHt != nNorme
                    THEN
                        nOK := 0;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOK := 0;                                -- Erreur OK.
                END;
            ----END RBT

            ---MSB TLG : controle sur le flag facturation separe pour frai etude
            ELSIF (sControle = 'FACSP')
            THEN
                DECLARE
                    ncount   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO ncount
                      FROM DOSRUBRIQUE DRU
                     WHERE     DRU.RUBID = 101
                           AND DRU.DOSID = nDosid
                           AND dru.DRUORDREGRPFAC IS NULL;

                    IF ncount <> 0
                    THEN
                        nOK := 0;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOK := 0;                                -- Erreur OK.
                END;
            ----END MSB

            ---L4345-------------------------- Start DBS  18042016 TLBO-1392 -----------------------------------


            ELSIF sControle IN ('RBASE')
            THEN
                DECLARE
                    nCount   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM dosrubecheancier
                     WHERE     DOSID = nDosId
                           AND dremtbase < 0
                           AND druordre NOT IN
                                   (SELECT druordre
                                      FROM dosrubrique
                                     WHERE dosid = nDosId AND rubid = 150);


                    IF nCount > 0
                    THEN
                        nOk := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOk := 0;
                END;
            ----------------------------- End DBS  18042016 TLBO-1392 -----------------------------------


            ELSIF scontrole IN ('AASC') -- BRI 23032011 Conditions prealables: Avenant, aval, et nouvelle chaine d?effets signes par le client en cas de cession partielle
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    l_val   VARCHAR2 (20) := 0;
                BEGIN
                    SELECT MAX (cvastringvalue)
                      INTO l_val
                      FROM cchvalue
                     WHERE dosid = nDosid AND cchsid = 'CMBCCHSID2756';


                    IF (l_val IS NULL) OR (l_val = 'NCONFOR')
                    THEN
                        nok := 0; -- La condition est nonconforme ou non renseignee==> KO
                    ELSE
                        nok := 1;                 -- La condition est conforme
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTDMP')                           -- 20120503
            THEN
                DECLARE
                    l_dtdebMP      dosactpaiement.dapdtdeb%TYPE;
                    l_dteffetMEF   crevt.credteffet%TYPE;
                /*l_tmffonction crevt.tmffonction%type;*/

                BEGIN
                    SELECT MAX (dapdtdeb)
                      INTO l_dtdebMP                                --20120612
                      FROM dosactpaiement adr, dosacteur act
                     WHERE     adr.dosid = nDosId
                           AND act.dosid = adr.dosid
                           AND act.rolcode = 'CLIENT'
                           AND daptype IN ('E', 'C', 'D')
                           AND act.dacordre = adr.dacordre
                           AND act.dacdtfin IS NULL;

                    SELECT credteffet
                      INTO l_dteffetMEF                         --EMI 20121017
                      FROM crevt
                     WHERE creid = ncreid;

                    /*        select tmffonction into l_tmffonction
                            from crevt where creid=ncreid;

                            if(l_tmffonction='EVD_MEL')then
                               select max(credteffet) into l_dteffetMEF
                               from crevt
                               where creid = nCreId;
                            else
                               select credteffet into l_dteffetMEF
                               from crevt
                               where creid= (select max(creid) from crevt where dosid=ndosid and tmffonction='EVD_MEL');
                            end if;*/

                    IF (l_dtdebMP > l_dteffetMEF)
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTAFA')                           -- 20120503
            THEN
                DECLARE
                    l_dtdebAdress   dosactadresse.daadtdeb%TYPE;
                    l_dteffetMEF    crevt.credteffet%TYPE;
                BEGIN
                    SELECT MAX (daadtdeb)
                      INTO l_dtdebAdress
                      FROM dosactadresse adr, dosacteur act
                     WHERE     adr.dosid = nDosId
                           AND act.dosid = adr.dosid
                           AND act.rolcode = 'CLIENT'
                           AND act.dacordre = adr.dacordre;

                    SELECT MAX (credteffet)
                      INTO l_dteffetMEF
                      FROM crevt
                     WHERE creid = nCreId;

                    IF (l_dtdebAdress IS NULL)
                    THEN                                        --EMI 20121010
                        nok := 0;
                    ELSIF (    (l_dtdebAdress IS NOT NULL)
                           AND (l_dtdebAdress > l_dteffetMEF))
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DTGEN')                       -- EMI 20121011
            THEN
                DECLARE
                    l_dosdtdeb      dossier.dosdtdeb%TYPE;
                    l_drudtdeb      dosrubrique.drudtdeb%TYPE;
                    l_imadosdtdeb   dossier.dosdtdeb%TYPE;
                BEGIN
                    SELECT MIN (dosdtdeb)
                      INTO l_dosdtdeb
                      FROM dossier
                     WHERE dosid = nDosId;

                    SELECT MIN (drudtdeb)
                      INTO l_drudtdeb
                      FROM dosrubrique
                     WHERE     dosid = nDosId
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = nDosId
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND rubid = 17);

                    /*cas ou dosdtdeb n'est pas null dans dossier */
                    IF (l_dosdtdeb IS NOT NULL)
                    THEN
                        IF (l_drudtdeb != l_dosdtdeb)
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;
                        END IF;
                    ELSE
                        /*cas ou dosdtdeb est  null  dans dossier*/
                        SELECT MAX (dosdtdeb)
                          INTO l_dosdtdeb
                          FROM imadossier
                         WHERE dosid = ndosid;

                        /*cas ou dosdtdeb n'est pas null dans imadossier*/
                        IF (l_dosdtdeb IS NOT NULL)
                        THEN
                            SELECT dosdtdeb
                              INTO l_dosdtdeb
                              FROM imadossier
                             WHERE     dosid = nDosId
                                   AND dosdtdeb IS NOT NULL
                                   AND imaid = (SELECT MIN (imaid)
                                                  FROM imadossier
                                                 WHERE dosid = ndosid);

                            IF (l_drudtdeb != l_dosdtdeb)
                            THEN
                                nok := 0;
                            ELSE
                                nok := 1;
                            END IF;
                        /*cas ou dosdtdeb est null dans imadossier*/
                        ELSE
                            /*recalculer dosdtdeb ? partir de dosdtfin dans dossier*/

                            SELECT   ADD_MONTHS (
                                         dosdtfin - dosdureejour,
                                         -dosdureemois - 12 * dosdureean)
                                   + 1
                              INTO l_dosdtdeb
                              FROM dossier
                             WHERE dosid = ndosid;


                            IF (l_drudtdeb != l_dosdtdeb)
                            THEN
                                nok := 0;
                            ELSE
                                nok := 1;
                            END IF;
                        END IF;
                    END IF;

                    RETURN (nok);
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('UNIMP')
            THEN
                DECLARE
                    l_countE   NUMBER := 0;
                    l_countD   NUMBER := 0;
                    l_countC   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_countE                                -- 20120406
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAC.DACORDRE =
                               (SELECT MAX (DACORDRE)
                                  FROM dosacteur
                                 WHERE ROLCODE = 'CLIENT' AND dosid = ndosid)
                           AND DAP.DAPTYPE = 'E'
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.DAPDTFIN IS NULL;

                    SELECT COUNT (*)
                      INTO l_countD                                -- 20120406
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAC.DACORDRE =
                               (SELECT MAX (DACORDRE)
                                  FROM dosacteur
                                 WHERE ROLCODE = 'CLIENT' AND dosid = ndosid)
                           AND DAP.DAPTYPE = 'D'
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.DAPDTFIN IS NULL;

                    SELECT COUNT (*)
                      INTO l_countC                                -- 20120406
                      FROM DOSACTEUR DAC, DOSACTPAIEMENT DAP
                     WHERE     DAC.DOSID = ndosid
                           AND DAC.ROLCODE = 'CLIENT'
                           AND DAC.DACORDRE = DAP.DACORDRE
                           AND DAC.DACORDRE =
                               (SELECT MAX (DACORDRE)
                                  FROM dosacteur
                                 WHERE ROLCODE = 'CLIENT' AND dosid = ndosid)
                           AND DAP.DAPTYPE = 'C'
                           AND DAP.DOSID = DAC.DOSID
                           AND DAP.DAPDTFIN IS NULL;

                    IF (l_countE > 1 OR l_countE > 1 OR l_countE > 1)
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('VRCTR') -- AZN 13122012 Contr?le bloquant sur la VR du contrat
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    nVR   VARCHAR2 (10);
                BEGIN
                    SELECT DRUMTRESIDUEL
                      INTO nVR
                      FROM dosrubrique
                     WHERE     dosid = nDosId
                           AND rubid = 17
                           AND druclasse = 'F'
                           AND drutype = 'F'
                           AND DRUORDRE IN
                                   (SELECT MAX (DRUORDRE)
                                      FROM DOSRUBRIQUE
                                     WHERE     dosid = nDosId
                                           AND DRUORDREPREC IS NULL
                                           AND rubid = 17
                                           AND druclasse = 'F'
                                           AND drutype = 'F');


                    IF (nVR IS NOT NULL)
                    THEN
                        nok := 1;              -- La VR n'est pas nulle ==> OK
                    ELSE
                        nok := 0;                    -- La VR est nulle ==> KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RIBD') -- MTR-12062013 Controle bloquant sur RIB Front <> RB Back
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    nRIBB           VARCHAR2 (100);
                    nRIBF           VARCHAR2 (100);
                    DATETRANSFERT   DATE;
                    DATESUBS        DATE;
                /*nRIBCHANGE NUMBER:=0;*/
                BEGIN
                    --Test sur l'evenement 'EVD_SUBS' s'il est passe apres le transfert, meme si les RIB sont different on declenche pas le controle// demande SOFIEN le 04102013
                    SELECT MIN (DPH.DPHDTEFFET), MAX (CRE.CREDTCREAT)
                      INTO DATETRANSFERT, DATESUBS
                      FROM DOSPHASE        DPH,
                           DOSSIER         DOS,
                           DOSACTPAIEMENT  DAP,
                           DOSACTEUR       DAC,
                           CREVT           CRE
                     WHERE     DOS.DOSID = nDosId
                           AND DPH.DOSID = DOS.DOSID
                           AND DPH.PHACODE = 'INI'
                           AND DAP.DOSID = DOS.DOSID
                           AND DAP.tmpcode IN ('PRLAUTO', 'EFFET')
                           AND DAP.daptype = 'E'
                           AND DOS.dosid = DAP.dosid
                           AND DAC.dacordre = DAP.dacordre
                           AND DAC.dosid = DOS.dosid
                           AND DAC.dacdtfin IS NULL
                           AND DAC.rolcode = 'CLIENT'
                           AND DAP.DAPDTFIN IS NULL
                           AND CRE.CREID = DAP.CREID
                           AND CRE.TMFFONCTION IN ('EVD_SUBS');


                    IF (DATETRANSFERT IS NOT NULL AND DATESUBS IS NOT NULL)
                    --  AND DATESUBS >= DATETRANSFERT)--MTR 20181005
                    THEN
                        nOk := 1;
                    ELSE
                        SELECT DAP.ribid, DPA.ribidenc
                          INTO nRIBB, nRIBF
                          FROM and_dossierprospect  DPR,
                               and_dpracteur        DPA,
                               dossier              DOS,
                               dosacteur            DAC,
                               dosactpaiement       DAP
                         WHERE     DAC.DOSID = nDosId
                               AND DOS.dosnum = DPR.dprnumcassiopee
                               AND DPR.dprversion = 'PROD'
                               AND DPA.tmpcodeenc IN ('PRLAUTO', 'EFFET')
                               AND DAP.tmpcode IN ('PRLAUTO', 'EFFET')
                               AND DAP.daptype = 'E'
                               AND DPA.rolcode = 'CLIENT'
                               AND DAC.dosid = DOS.dosid
                               AND DAC.dacdtfin IS NULL
                               AND DAC.rolcode = 'CLIENT'
                               AND DPA.rolcode = DAC.rolcode
                               AND DOS.dosid = DAP.dosid
                               AND DAC.dacordre = DAP.dacordre
                               AND DPA.dosid = DPR.dosid
                               AND DPA.ribidenc IS NOT NULL
                               AND DPA.dprversion = DPR.dprversion
                               AND DAC.dacdtfin IS NULL
                               AND (   (    DAP.DAPDTFIN IS NULL
                                        AND DAP.CREID IS NULL)           --new
                                    OR (    DAP.DAPDTFIN IS NULL         --new
                                        AND DAP.CREID IN
                                                (SELECT CRE.CREID
                                                   FROM CREVT CRE
                                                  WHERE     CRE.TMFFONCTION IN
                                                                ('EVD_DOMICIL')
                                                        AND CRE.DOSID =
                                                            nDosId)));   --new

                        IF (    nRIBB IS NOT NULL
                            AND nRIBF IS NOT NULL
                            AND nRIBB = nRIBF)
                        THEN
                            nOk := 1;                 -- La nRIBB=nRIBF ==> OK
                        ELSE
                            nOk := 0; -- Le RIB existant sur le BACK et diff?rent de celui sur le front office
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOk := 0;
                END;
            ------Controle commente suite ticket TLBO-827: il faut revoir le controle
            /*ELSIF scontrole IN ('NEGO') THEN -- AZN 26122012 Simulation de renegociation pour les dossiers CBI


                  -- nOk:=0 ==> Le retour du CTRL est KO
                  -- nOk:=1 ==> Le retour du CTRL est OK.


                  DECLARE
                     l_count   NUMBER;
                     l_druordre  NUMBER;


                     CURSOR c_renego IS


                     select dru.druordre
                     from dosrubrique dru, dosphase dph
                     where dru.dosid=nDosId
                     and dru.dosid=dph.dosid
                     and DRU.rubid=17
                     and DRU.druclasse='F'
                     and DRU.drutype='F'
                     and dph.dphmotif <> 'CODE7';


                  BEGIN




                  OPEN c_renego;
                        LOOP
                        FETCH c_renego
                        INTO l_druordre;
                        EXIT WHEN c_renego%NOTFOUND;


                  IF(l_druordre is not null)  THEN


                        SELECT COUNT(*) INTO l_count FROM LKDOSRUBITRRUB WHERE dosid=nDosId AND druordre=l_druordre;


                     IF (l_count > 0)
                     THEN
                        nok := 1;            -- Le materiel est adosse a la rubrique ==> OK

                     ELSE
                        nok := 0;            -- Le materiel n'est pas adosse a la rubrique ==> KO
                     END IF;


                 ELSE
                    nok := 0;                      -- Erreur donc KO.
                 END IF;
                        END LOOP;
                   CLOSE c_renego;




                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        nok := 0;                       -- Erreur donc KO.
                  END;
            */
            --MSB 1410 TLBO1247
            ELSIF sControle IN ('TMP')
            THEN
                SELECT COUNT (*)
                  INTO nCount
                  FROM dosactpaiement dap, dosacteur dac
                 WHERE     dap.dosid = nDosId
                       AND dap.dosid = dac.dosid
                       AND dac.rolcode = 'CLIENT'
                       AND dacdtfin IS NULL
                       AND dap.dacordre = dac.dacordre
                       AND dap.daptype IN ('E', 'D', 'C')
                       AND dap.dapdtfin IS NULL;

                IF nCount < 3
                THEN
                    nOK := 0;
                END IF;
            ELSIF sControle IN ('RBTIM')
            THEN
                SELECT COUNT (*)
                  INTO nCount
                  FROM dosrubrique dru
                 WHERE     dru.dosid = nDosId
                       AND dru.rubid = 22
                       AND (DRUFLAGSUSPEND IS NULL OR DRUFLAGSUSPEND != 1);

                IF nCount <> 0
                THEN
                    nOK := 0;
                END IF;
            ELSIF sControle IN ('GRFAC')
            THEN
                /*SELECT DRUORDREGRPFAC INTO nCount
                              FROM dosrubrique dru
                              WHERE dru.dosid=nDosId
                              AND dru.rubid in (6,22);

                              IF nCount is not null
                              THEN
                                nOK := 0;
                              END IF;      */

                SELECT COUNT (*)
                  INTO nCount
                  FROM dosrubrique dru
                 WHERE     dru.dosid = nDosId
                       AND dru.rubid IN (6, 22)
                       AND DRUORDREGRPFAC IS NOT NULL;

                IF nCount <> 0
                THEN
                    nOK := 0;
                END IF;
            ELSIF sControle IN ('ADRC')
            THEN
                SELECT COUNT (*)
                  INTO nCount
                  FROM dosactadresse daa, dosacteur dac
                 WHERE     daa.dosid = nDosId
                       AND daa.dosid = dac.dosid
                       AND dac.rolcode = 'CLIENT'
                       AND dacdtfin IS NULL
                       AND daa.dacordre = dac.dacordre
                       AND daadtfin IS NULL;

                IF nCount = 0
                THEN
                    nOK := 0;
                END IF;
            --TLG45 RBS 20180417 Controle sur date effet vente partielle/totale
            ELSIF (SCONTROLE = 'DTNFA')
            THEN
                DECLARE
                    dECHNFA       VARCHAR2 (50);
                    dECHNFE       VARCHAR2 (50);
                    dDTEFF        VARCHAR2 (50);
                    sPERCEPTION   VARCHAR2 (10);
                BEGIN
                    SELECT TO_CHAR ( --RBS 21062018 Date derniere echeance pour perception "avance"
                                    ADD_MONTHS (FACDTEXIGIBLE, 1),
                                    'dd/mm/yyyy')
                      INTO dECHNFA
                      FROM facture
                     WHERE facid IN
                               (SELECT facid
                                  FROM DOSRUBECHEANCIER DRE
                                 WHERE     DRE.DOSID = nDOSID
                                       AND DRE.FACID IS NOT NULL
                                       AND DRE.FACID <> 0
                                       AND DREDTECH =
                                           (SELECT MAX (DREDTECH)
                                              FROM DOSRUBECHEANCIER DRE
                                             WHERE     DRE.DOSID = nDOSID
                                                   AND DRE.FACID IS NOT NULL
                                                   AND DRE.FACID <> 0
                                                   AND EXISTS
                                                           (SELECT 1
                                                              FROM DOSRUBRIQUE
                                                                   DRU
                                                             WHERE     DRU.DOSID =
                                                                       DRE.DOSID
                                                                   AND DRU.DRUORDRE =
                                                                       DRE.DRUORDRE
                                                                   AND DRU.DRUTYPE =
                                                                       'F'
                                                                   AND DRU.DRUCLASSE =
                                                                       'F'))
                                       AND EXISTS
                                               (SELECT 1
                                                  FROM DOSRUBRIQUE DRU
                                                 WHERE     DRU.DOSID =
                                                           DRE.DOSID
                                                       AND DRU.DRUORDRE =
                                                           DRE.DRUORDRE
                                                       AND DRU.DRUTYPE = 'F'
                                                       AND DRU.DRUCLASSE =
                                                           'F'));

                    SELECT TO_CHAR ( --RBS 21062018 Date derniere echeance pour perception "echu"
                                    (FACDTEXIGIBLE + 1), 'dd/mm/yyyy')
                      INTO dECHNFE
                      FROM facture
                     WHERE facid IN
                               (SELECT facid
                                  FROM DOSRUBECHEANCIER DRE
                                 WHERE     DRE.DOSID = nDOSID
                                       AND DRE.FACID IS NOT NULL
                                       AND DRE.FACID <> 0
                                       AND DREDTECH =
                                           (SELECT MAX (DREDTECH)
                                              FROM DOSRUBECHEANCIER DRE
                                             WHERE     DRE.DOSID = nDOSID
                                                   AND DRE.FACID IS NOT NULL
                                                   AND DRE.FACID <> 0
                                                   AND EXISTS
                                                           (SELECT 1
                                                              FROM DOSRUBRIQUE
                                                                   DRU
                                                             WHERE     DRU.DOSID =
                                                                       DRE.DOSID
                                                                   AND DRU.DRUORDRE =
                                                                       DRE.DRUORDRE
                                                                   AND DRU.DRUTYPE =
                                                                       'F'
                                                                   AND DRU.DRUCLASSE =
                                                                       'F'))
                                       AND EXISTS
                                               (SELECT 1
                                                  FROM DOSRUBRIQUE DRU
                                                 WHERE     DRU.DOSID =
                                                           DRE.DOSID
                                                       AND DRU.DRUORDRE =
                                                           DRE.DRUORDRE
                                                       AND DRU.DRUTYPE = 'F'
                                                       AND DRU.DRUCLASSE =
                                                           'F'));


                    SELECT TO_CHAR (credteffet, 'dd/mm/yyyy')
                      INTO dDTEFF
                      FROM crevt
                     WHERE creid IN
                               (SELECT MAX (creid)
                                  FROM CREVT
                                 WHERE     dosid = nDOSID
                                       AND tmffonction IN
                                               ('EVD_VENTE', 'EVD_VPART'));

                    SELECT MAX (DRFPERCEPTION) --RBS 21062018 Mode de perception du flux financier
                      INTO sPERCEPTION
                      FROM dosrubflux
                     WHERE     DRFDTDEB <= f_getderniereechfacture (nDOSID)
                           AND DRFDTFIN >=
                               (f_getderniereechfacture (nDOSID) - 1)
                           AND dosid = nDOSID
                           AND druordre =
                               (SELECT MAX (druordre)
                                  FROM dosrubrique
                                 WHERE     dosid = nDOSID
                                       AND druclasse = 'F'
                                       AND drutype = 'F'
                                       AND DRUDTDEB <=
                                           f_getderniereechfacture (nDOSID)
                                       AND DRUDTFIN >=
                                           (  f_getderniereechfacture (
                                                  nDOSID)
                                            - 1));

                    --Se declanche si Date effet differente de date derniere echeance
                    IF    (sPERCEPTION = 'A' AND dECHNFA <> dDTEFF)
                       OR (sPERCEPTION = 'E' AND dECHNFE <> dDTEFF) --ABH 20180420
                    THEN
                        nOK := 0;
                    END IF;
                END;
            --TLG45 RBS 20180411 Controle sur les donnees relatives au certificat d'exemption

            -- IEB 03/07/2018 : CONTOLE DU TVA 19% SUR EVD_CESSION
            ELSIF sControle IN ('VAT')
            THEN
                DECLARE
                    --nROLCODE SASFACTURE.ROLCODE%TYPE := NULL;
                    nTAXCODE      SASFACTAXE.TAXCODE%TYPE := NULL;
                    ncountLigne   NUMBER;                      --20180720 kila
                BEGIN
                    --20180720 kila


                    ---RBA 20181017 - POINT 962
                    SELECT NVL (COUNT (*), 0)
                      INTO ncountLigne
                      FROM cchvalue cch
                     WHERE     cch.dosid = nDOSID
                           AND cch.cchsid = 'CHKCCHSID1815'
                           AND cch.cvabooleanvalue = 1;

                    ---RBA 20181017 - POINT 962
                    IF ncountLigne > 0
                    THEN
                        RETURN 1;


                        SELECT COUNT (*)
                          INTO ncountLigne
                          FROM SASFACTURE SFA, SASFACTAXE SFT, CREVT C -- DOSSIER D-- FACREFERENCE FRE
                         WHERE     SFA.SFAID = SFT.SFAID
                               AND SFA.FACTYPE = 'DP'
                               AND SFA.CREID = C.CREID
                               AND SFA.SFAID =
                                   (SELECT MAX (SFAID)
                                      FROM SASFACTURE
                                     WHERE CREID IN
                                               (SELECT creid
                                                  FROM crevt
                                                 WHERE     tmffonction =
                                                           'EVD_CESSION'
                                                       AND DOSID = nDOSID));


                        IF ncountLigne = 0
                        THEN                                   --20180720 kila
                            NOK := 1;
                        ELSE
                            SELECT SFT.TAXCODE
                              INTO nTAXCODE
                              FROM SASFACTURE SFA, SASFACTAXE SFT, CREVT C -- DOSSIER D-- FACREFERENCE FRE
                             WHERE     SFA.SFAID = SFT.SFAID
                                   AND SFA.FACTYPE = 'DP'
                                   AND SFA.CREID = C.CREID
                                   AND SFA.SFAID =
                                       (SELECT MAX (SFAID)
                                          FROM SASFACTURE
                                         WHERE CREID IN
                                                   (SELECT creid
                                                      FROM crevt
                                                     WHERE     tmffonction =
                                                               'EVD_CESSION'
                                                           AND DOSID = nDOSID));

                            IF nTAXCODE <> 'TVATN'
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END IF;                                    --20180720 kila
                END;
            ELSIF (SCONTROLE = 'CEXEM')
            THEN
                DECLARE
                    CURSOR curActExem
                    IS
                        SELECT dosid, dacordre
                          FROM dosacteur
                         WHERE dosid = ndosid AND dacdtfin IS NULL;

                    nReturn   NUMBER;
                BEGIN
                    FOR C IN curActExem
                    LOOP
                        SELECT dacdttaxfreeend - dacdttaxfreebegin
                          INTO nReturn
                          FROM dosacteur
                         WHERE dosid = C.dosid AND dacordre = C.dacordre;

                        IF nReturn <= 0                        -- ABH 20180420
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            --TLG45 ABH 20180420 Controle sur la modification d?un fournisseur affect? sur un mat?riel en phase ES sur un contrat MEF par un autre fournisseur.
            ELSIF (SCONTROLE = 'FRACT')
            THEN
                DECLARE
                    CURSOR curMatDos
                    IS
                        SELECT ITRID, IRUORDRE
                          FROM LKDOSRUBITRRUB
                         WHERE dosid = ndosid;

                    nReturn   NUMBER;
                    nFOURN    NUMBER;
                    sPHASE    itrrubphase.phacode%TYPE;
                    sPHADOS   dosphase.phacode%TYPE;
                    nORDRE    dosacteur.DACORDRE%TYPE;
                    nACTEUR   dosacteur.ACTID%TYPE;
                    sROLE     dosacteur.ROLCODE%TYPE;
                    nIMAGE    crevt.IMAID%TYPE;
                BEGIN
                    /*      SELECT DACORDRE, ACTID, ROLCODE
                            INTO nORDRE, nACTEUR, sROLE
                            FROM dosacteur
                           WHERE dosid = nDOSID
                          MINUS
                          SELECT DACORDRE, ACTID, ROLCODE
                            FROM imadosacteur
                           WHERE imaid IN
                                    (SELECT imaid
                                       FROM crevt cre, credata crd
                                      WHERE     cre.creid = crd.creid
                                            AND cre.creid IN (SELECT MAX (creid)
                                                                FROM crevt
                                                               WHERE dosid = ndosid));

                          IF sROLE = 'FOURN'
                          THEN
                             SELECT phacode
                               INTO sPHADOS
                               FROM dosphase
                              WHERE dosid = nDOSID AND dphdtfin IS NULL;

                             IF sPHADOS = 'ES'
                             THEN
                                FOR C IN curMatDos
                                LOOP
                                   SELECT ACTIDFRS, phacode
                                     INTO nFOURN, sPHASE
                                     FROM lkdosrubitrrub idr,
                                          itrrubrique itr,
                                          itrrubphase itp
                                    WHERE     itr.itrid = C.itrid
                                          AND itr.iruordre = C.iruordre
                                          AND itp.itrid = itr.itrid
                                          AND itp.iruordre = itp.iruordre
                                          AND idr.itrid = itr.itrid
                                          AND idr.IRUORDRE = itr.IRUORDRE
                                          AND IRPDTFIN IS NULL
                                          AND ACTIDFRS=nACTEUR;

                                   IF sPHASE = 'ES'
                                   THEN
                                      NOK := 0;
                                      EXIT;
                                   END IF;
                                END LOOP;
                             END IF;
                          END IF;                                            */
                    --RBS 13/10/2018
                    SELECT COUNT (*)
                      INTO nCount
                      FROM lkdosrubitrrub  idr,
                           itrrubrique     itr,
                           itrrubphase     itp
                     WHERE     idr.dosid = nDOSID
                           AND itp.itrid = itr.itrid
                           AND itp.iruordre = itp.iruordre
                           AND idr.itrid = itr.itrid
                           AND idr.IRUORDRE = itr.IRUORDRE
                           AND itp.phacode = 'ES'
                           AND itr.actidfrs NOT IN
                                   (SELECT actid
                                      FROM dosacteur dac
                                     WHERE     dac.dosid = nDOSID
                                           AND rolcode = 'FOURN'
                                           AND dacdtfin IS NULL);

                    IF nCount > 0
                    THEN
                        nOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --TLG45 RBS 20180426 contr?le pour annuler la d?pense avant le classement sans suite
            ELSIF (SCONTROLE = 'SSDEP')
            THEN
                DECLARE
                    CURSOR curDepDos
                    IS
                        SELECT dep.depid
                          FROM depense dep, depligne dpl, depphase dpp
                         WHERE     dpl.depid = dep.depid
                               AND dpp.depid = dep.depid
                               AND depidorigine IS NULL
                               AND dosid = nDOSID
                               AND phacode = 'VALIDE'
                               AND dphdtfin IS NULL;

                    nCount1   NUMBER;
                    nCount2   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount1
                      FROM depense dep, depligne dpl, depphase dpp
                     WHERE     dpl.depid = dep.depid
                           AND dpp.depid = dep.depid
                           AND depidorigine IS NULL
                           AND dosid = nDOSID
                           AND phacode = 'VALIDE'
                           AND dphdtfin IS NULL;

                    IF nCount1 > 0
                    THEN
                        FOR C IN curDepDos
                        LOOP
                            SELECT COUNT (*)
                              INTO nCount2
                              FROM depense
                             WHERE depidorigine = C.depid;

                            IF nCount2 = 0
                            THEN
                                nOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ELSIF (SCONTROLE = 'D02')
            THEN
                --EVD_VPART
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSSIER DOS, CREVT CR, CREDATA CRD
                 WHERE     CR.CREID = NCREID
                       AND CR.CREID = CRD.CREID
                       AND CR.DOSID = DOS.DOSID
                       AND (       (    (   DOS.TPGCODE LIKE 'L%'
                                         OR DOS.TPGCODE LIKE 'S%')
                                    AND SUBSTR (DOS.TPGCODE, 2, 3) NOT IN
                                            ('493',
                                             '495',
                                             '497',
                                             '499'))
                               AND NVL (
                                       (SELECT CDADATASTRING
                                          FROM CREDATA
                                         WHERE     CREID = CRD.CREID
                                               AND CDACOLONNE = 'PAYOFFTYPE'),
                                       0) =
                                   'PART'
                            OR DOS.TPGCODE LIKE 'T%'
                            OR DOS.TPGCODE LIKE 'F%');

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'D03')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSSIER DOS, CREVT CR, CREDATA CRD
                 WHERE     CR.CREID = NCREID
                       AND CRD.CREID = CR.CREID
                       AND CR.DOSID = DOS.DOSID
                       AND (       (    (   DOS.TPGCODE LIKE 'L%'
                                         OR DOS.TPGCODE LIKE 'S%')
                                    AND SUBSTR (DOS.TPGCODE, 2, 3) NOT IN
                                            ('493',
                                             '495',
                                             '497',
                                             '499'))
                               AND NVL (
                                       (SELECT CDADATASTRING
                                          FROM CREDATA
                                         WHERE     CREID = CRD.CREID
                                               AND CDACOLONNE = 'PAYOFFTYPE'),
                                       0) =
                                   'TOT'
                            OR DOS.TPGCODE LIKE 'T%');

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'EXDEL'
            THEN
                SELECT ACTID, DEVCODE
                  INTO NACTIDGESTION, SDEVCODEDOS
                  FROM DOSSIER
                 WHERE DOSID = NDOSID;

                SELECT MAX (ACTID)
                  INTO NACTIDCLIENT
                  FROM DOSACTEUR DAC
                 WHERE     DOSID = NDOSID
                       AND ROLCODE IN ('EMPRUNT', 'CLIENT', 'LOCAT')
                       AND (DAC.DACDTFIN IS NULL OR DAC.DACDTFIN > SYSDATE);

                -- RECOVER AMOUNT OF DELIQUENCY FOR THE CLIENT
                BEGIN
                    SELECT NVL (
                               SUM (
                                     F_PLFACTIMP (FAC.FACID, SYSDATE)
                                   * PA_COMMON_BATCH.F_TAUXCONVDEV (
                                         FAC.DEVCODE,
                                         SDEVCODEDOS,
                                         SYSDATE)),
                               0)
                      INTO NMTDOSSIER
                      FROM FACTURE FAC
                     WHERE     FAC.ACTIDCLIENT = NACTIDCLIENT
                           AND FAC.ACTIDGESTION = NACTIDGESTION;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NMTDOSSIER := 0;
                END;

                IF NMTDOSSIER > 0
                THEN
                    NOK := 0;
                END IF;
            -- LMI  2013-09-10 EVD_MEL DELIQUENCY EXISTS
            --CASNT-7482
            ELSIF SCONTROLE = 'UNPAS' AND NCREID IS NOT NULL
            THEN
                BEGIN
                    DECLARE
                        STPGCODE   DOSSIER.TPGCODE%TYPE;
                        NUNPAID    ITRRUBRIQUE.IRUMTORIGINE%TYPE := 0;

                        CURSOR C1
                        IS
                              SELECT IRU.ITRID,
                                     IRU.IRUORDRE,
                                     IRU.IRUMTORIGINE,
                                     IRU.IRUFLAGWDISB
                                FROM ITRRUBRIQUE IRU, LKDOSRUBITRRUB LK
                               WHERE     LK.DOSID = NDOSID
                                     AND LK.ITRID = IRU.ITRID
                                     AND LK.IRUORDRE = IRU.IRUORDRE
                            ORDER BY IRU.ITRID, IRU.IRUORDRE;
                    BEGIN
                        NOK := 1;

                        SELECT NVL (TPGCODE, 'xXx')
                          INTO STPGCODE
                          FROM DOSSIER
                         WHERE DOSID = NDOSID;

                        IF UPPER (STPGCODE) LIKE 'G410%'
                        THEN
                            FOR C1R IN C1
                            LOOP
                                IF NVL (C1R.IRUFLAGWDISB, 0) = 0
                                THEN
                                    NUNPAID :=
                                          NUNPAID
                                        + F_PLMTASSETELUNPAID (
                                              C1R.ITRID,
                                              C1R.IRUORDRE,
                                              C1R.IRUMTORIGINE,
                                              SYSDATE);
                                END IF;

                                EXIT WHEN NUNPAID != 0;
                            END LOOP;

                            IF NUNPAID != 0
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END;
                END;
            --CASNT-8643
            ELSIF SCONTROLE = 'CANCA'
            THEN
                BEGIN
                    DECLARE
                        NEXISTS      NUMBER := 0;
                        NACTIVE      TOPPARAM.TPALOGIQUE%TYPE := 0;
                        SLISTOEM     TOPPARAM.TPATEXTE%TYPE := NULL;
                        SUGECODE     ACTEUR.UGECODE%TYPE := NULL;
                        SOEMCODE     ACTEUR.ACTCODE%TYPE := NULL;
                        NNOCONTROL   NUMBER := 0;
                        NRUBID       RUBRIQUE.RUBID%TYPE := 0;
                    BEGIN
                        -- Get management unit
                        SELECT ACT.UGECODE
                          INTO SUGECODE
                          FROM DOSSIER DOS, ACTEUR ACT
                         WHERE DOS.DOSID = NDOSID AND DOS.ACTID = ACT.ACTID;

                        -- Get option activation
                        PA_COMMON.S_TPALOGIQUE ('DOSSIER',
                                                'LISTOEM',
                                                NACTIVE,
                                                SUGECODE);

                        IF NACTIVE != 0
                        THEN
                            -- Get list of OEM exempted
                            PA_COMMON.S_TPATEXTE ('DOSSIER',
                                                  'LISTOEM',
                                                  SLISTOEM,
                                                  SUGECODE);

                            IF SLISTOEM IS NOT NULL
                            THEN
                                SELECT MAX (ACTCODE)
                                  INTO SOEMCODE
                                  FROM DOSACTEUR DAC, ACTEUR ACT
                                 WHERE     DAC.DOSID = NDOSID
                                       AND DAC.ROLCODE = 'OEM'
                                       AND DAC.DACDTFIN IS NULL
                                       AND DAC.ACTID = ACT.ACTID;

                                IF (    SOEMCODE IS NOT NULL
                                    AND INSTR (SLISTOEM, SOEMCODE) != 0)
                                THEN
                                    NNOCONTROL := 1;
                                END IF;
                            ELSE
                                NOK := 0;
                            END IF;
                        END IF;

                        IF (NNOCONTROL = 0 AND NOK = 1)
                        THEN
                            -- Check if cancellation amount exists
                            SELECT RUBID
                              INTO NRUBID
                              FROM RUBRIQUE
                             WHERE RUBCODE = 'CANCAMT' AND UGECODE = SUGECODE;

                            IF NRUBID != 0
                            THEN
                                SELECT NVL (RIM.RIMMTCOMPTA, 0)
                                  INTO NEXISTS
                                  FROM REGIMPUTATION RIM, REGLEMENT REG
                                 WHERE     RIM.DOSID = NDOSID
                                       AND RIM.RUBID = NRUBID
                                       AND RIM.REGIDCANCEL IS NULL
                                       AND RIM.REGID = REG.REGID
                                       AND REG.REGDTREJET IS NULL;

                                IF NEXISTS = 0
                                THEN
                                    NOK := 0;
                                END IF;
                            ELSE
                                NOK := 0;
                            END IF;
                        END IF;
                    END;
                END;
            -- LMI  2013-11-25 JIRA 8643 CHECK FOR CANCELLATION AMOUNT
            ELSIF SCONTROLE = 'DDCK'
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM CREDATA CDA, CREVT CVT
                 WHERE     CVT.CREID = CDA.CREID
                       AND CDA.CDACOLONNE = 'TYPENEGO'
                       AND CDA.CDADATASTRING = 'EXIDTCHG'
                       AND CVT.DOSID = NDOSID
                       AND CDA.CREID = NCREID;

                IF NCOUNT > 0
                THEN
                    SELECT   TO_CHAR (NVL (CDA.CDADATADATE, CVT.CREDTEFFET),
                                      'DD')
                           - TO_CHAR (CVT.CREDTEFFET, 'DD')
                      INTO NOK
                      FROM CREDATA CDA, CREVT CVT
                     WHERE     CVT.CREID = CDA.CREID
                           AND CDA.CDACOLONNE = 'TYPENEGO'
                           AND CDA.CDADATASTRING = 'EXIDTCHG'
                           AND CVT.DOSID = NDOSID
                           AND CDA.CREID = NCREID;

                    IF NOK < 0
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ---30-05-2014 -START-- Control for Approval line Change Cap. Type ISF
            ELSIF SCONTROLE = 'CAPTYP'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM DOSACTEUR
                 WHERE     (   DACIRFLOORTYPE = 'FIXEDRATE'
                            OR DACIRCAPTYPE = 'FIXEDRATE')
                       AND DOSID = NDOSID;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ---30-05-2014 -END-- Control for Approval line Change Cap. Type ISF
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION (BIDDR)
            ELSIF SCONTROLE = 'BIDDR'
            THEN
                BEGIN
                    DECLARE
                        NTXASSIETTE   DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMIN     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMAX     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0.5;
                    BEGIN
                        NOK := 1;
                        SRUBCODE := 'BIDDR';

                        FOR DRUR IN DRU
                        LOOP
                            IF (   DRUR.DRFTXASSIETTE < NLIMITMIN
                                OR DRUR.DRFTXASSIETTE > NLIMITMAX)
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END;
                END;
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION (INRET)
            ELSIF SCONTROLE = 'INRET'
            THEN
                BEGIN
                    DECLARE
                        NTXASSIETTE   DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMIN     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMAX     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0.3;
                    BEGIN
                        NOK := 1;
                        SRUBCODE := 'INRET';

                        FOR DRUR IN DRU
                        LOOP
                            IF (   DRUR.DRFTXASSIETTE < NLIMITMIN
                                OR DRUR.DRFTXASSIETTE > NLIMITMAX)
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END;
                END;
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION (BHAPP)
            ELSIF SCONTROLE = 'BHAPP'
            THEN
                BEGIN
                    DECLARE
                        NTXASSIETTE   DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMIN     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMAX     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0.2;
                    BEGIN
                        NOK := 1;
                        SRUBCODE := 'BHAPP';

                        FOR DRUR IN DRU
                        LOOP
                            IF (   DRUR.DRFTXASSIETTE < NLIMITMIN
                                OR DRUR.DRFTXASSIETTE > NLIMITMAX)
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END;
                END;
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION (KAHTH)
            ELSIF SCONTROLE = 'KAHTH'
            THEN
                BEGIN
                    DECLARE
                        NTXASSIETTE   DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMIN     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 0;
                        NLIMITMAX     DOSRUBFLUX.DRFTXASSIETTE%TYPE := 2.5;
                    BEGIN
                        NOK := 1;
                        SRUBCODE := 'KAHTH';

                        FOR DRUR IN DRU
                        LOOP
                            IF (   DRUR.DRFTXASSIETTE < NLIMITMIN
                                OR DRUR.DRFTXASSIETTE > NLIMITMAX)
                            THEN
                                NOK := 0;
                                EXIT;
                            END IF;
                        END LOOP;
                    END;
                END;
            -- LMI  2013-09-12 CREATION FOR DISBURSEMENT ELEMENT BETWEEN RANGE LIMITS PER PROCESS ALLOCATION
            -- LMI  2013-09-12 SUM OF DISBURSEMENT ELEMENTS SHOULD NOT EXCEED A LIMIT
            /*                           ELSIF sControle = 'SDISB' THEN
            nOk := 1;
            SELECT  SUM(DRF.DRFTXASSIETTE)
            INTO    nResult
            FROM    DOSRUBRIQUE DRU, DOSRUBFLUX DRF, RUBRIQUE RUB
            WHERE   DRU.DOSID = nDosid
            AND     DRU.DRUTYPE = 'R'
            AND     DRU.DOSID = DRF.DOSID
            AND     DRU.DRUORDRE = DRF.DRUORDRE
            AND     DRU.RUBID = RUB.RUBID
            AND     RUB.RUBCODE IN ('BIDDR', 'INRET', 'BHAPP', 'KAHTH');
            IF nResult > nLimit THEN
            nOk := 0;
            END IF; */
            -- LMI  2013-09-12 SUM OF DISBURSEMENT ELEMENTS SHOULD NOT EXCEED A LIMIT
            -- JJ 2013-12-03 JIRA 8420 and 8630 CHECK IF THE LOAN DISBURSEMENT HAS BEEN PAID OR NOT
            ELSIF SCONTROLE = 'LNDIS'
            THEN
                NOK := OCREGLOANDIS (NDOSID, NCREID);
            -- JJ 2013-12-03 JIRA 8420 and 8630 CHECK IF THE LOAN DISBURSEMENT HAS BEEN PAID OR NOT
            ELSIF SCONTROLE = ('CODQA')
            THEN
                DECLARE
                    WCINDTCREATED   DATE;
                    NACTID          ACTEUR.ACTID%TYPE;
                    WPROID          NUMBER;
                    WCINID          NUMBER;
                    NBDELQ          NUMBER;
                    NACTIDGESTION   ACTEURGESTION.ACTID%TYPE;
                    SDEVCODEDOS     DEVISE.DEVCODE%TYPE;
                BEGIN
                    NOK := 1;

                    --GET PROID
                    BEGIN
                        SELECT PROID
                          INTO WPROID
                          FROM PROCESS
                         WHERE PROCODE = 'RLH_DTRLH0';
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            WPROID := NULL;
                    END;

                    --- Get ACTID
                    BEGIN
                        SELECT ACTID, DEVCODE
                          INTO NACTIDGESTION, SDEVCODEDOS
                          FROM DOSSIER
                         WHERE DOSID = NDOSID;

                        SELECT MAX (ACTID)
                          INTO NACTID
                          FROM DOSACTEUR DAC
                         WHERE     DOSID = NDOSID
                               AND ROLCODE IN ('EMPRUNT', 'CLIENT', 'LOCAT')
                               AND (   DAC.DACDTFIN IS NULL
                                    OR DAC.DACDTFIN > SYSDATE);
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NACTID := NULL;
                    END;

                    ------
                    BEGIN
                        SELECT MAX (CRE.CINID),
                               TRUNC (MAX (CRE.CINDTCREATED))
                          INTO WCINID, WCINDTCREATED
                          FROM CREDITINFO CRE
                         WHERE CRE.PROID = WPROID AND CRE.ACTID = NACTID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NULL;
                    END;

                    IF WCINDTCREATED = TRUNC (SYSDATE)
                    THEN
                        BEGIN
                            SELECT COUNT (*)
                              INTO NBDELQ
                              FROM (SELECT *
                                      FROM CINTCRGRID
                                     WHERE     CTGGCOCODE = 'RLH_CRGN_A'
                                           AND CINID = WCINID
                                           AND PROID = WPROID)
                             WHERE     CTGVALUE > 0
                                   AND CTGLINE IN
                                           (SELECT CTGLINE
                                              FROM (SELECT *
                                                      FROM CINTCRGRID
                                                     WHERE     CTGGCOCODE =
                                                               'RLH_SBJT_CD'
                                                           AND CINID = WCINID
                                                           AND PROID = WPROID)
                                             WHERE SUBSTR (CTGVALUE, 4, 1) =
                                                   '7');

                            IF (NBDELQ > 0)
                            THEN
                                NOK := 0;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                NOK := 1;
                        END;
                    END IF;
                END;
            -- LMI  2013-11-25 JIRA 8643 CHECK FOR CANCELLATION AMOUNT
            ELSIF SCONTROLE = 'CANCA'
            THEN
                BEGIN
                    DECLARE
                        NEXISTS      NUMBER := 0;
                        NACTIVE      TOPPARAM.TPALOGIQUE%TYPE := 0;
                        SLISTOEM     TOPPARAM.TPATEXTE%TYPE := NULL;
                        SUGECODE     ACTEUR.UGECODE%TYPE := NULL;
                        SOEMCODE     ACTEUR.ACTCODE%TYPE := NULL;
                        NNOCONTROL   NUMBER := 0;
                        NRUBID       RUBRIQUE.RUBID%TYPE := 0;
                    BEGIN
                        -- Get management unit
                        SELECT ACT.UGECODE
                          INTO SUGECODE
                          FROM DOSSIER DOS, ACTEUR ACT
                         WHERE DOS.DOSID = NDOSID AND DOS.ACTID = ACT.ACTID;

                        -- Get option activation
                        PA_COMMON.S_TPALOGIQUE ('DOSSIER',
                                                'LISTOEM',
                                                NACTIVE,
                                                SUGECODE);

                        IF NACTIVE != 0
                        THEN
                            -- Get list of OEM exempted
                            PA_COMMON.S_TPATEXTE ('DOSSIER',
                                                  'LISTOEM',
                                                  SLISTOEM,
                                                  SUGECODE);

                            IF SLISTOEM IS NOT NULL
                            THEN
                                SELECT MAX (ACTCODE)
                                  INTO SOEMCODE
                                  FROM DOSACTEUR DAC, ACTEUR ACT
                                 WHERE     DAC.DOSID = NDOSID
                                       AND DAC.ROLCODE = 'OEM'
                                       AND DAC.DACDTFIN IS NULL
                                       AND DAC.ACTID = ACT.ACTID;

                                IF (    SOEMCODE IS NOT NULL
                                    AND INSTR (SLISTOEM, SOEMCODE) != 0)
                                THEN
                                    NNOCONTROL := 1;
                                END IF;
                            ELSE
                                NOK := 0;
                            END IF;
                        END IF;

                        IF (NNOCONTROL = 0 AND NOK = 1)
                        THEN
                            -- Check if cancellation amount exists
                            SELECT RUBID
                              INTO NRUBID
                              FROM RUBRIQUE
                             WHERE RUBCODE = 'CANCAMT' AND UGECODE = SUGECODE;

                            IF NRUBID != 0
                            THEN
                                SELECT NVL (RIM.RIMMTCOMPTA, 0)
                                  INTO NEXISTS
                                  FROM REGIMPUTATION RIM, REGLEMENT REG
                                 WHERE     RIM.DOSID = NDOSID
                                       AND RIM.RUBID = NRUBID
                                       AND RIM.REGIDCANCEL IS NULL
                                       AND RIM.REGID = REG.REGID
                                       AND REG.REGDTREJET IS NULL;

                                IF NEXISTS = 0
                                THEN
                                    NOK := 0;
                                END IF;
                            ELSE
                                NOK := 0;
                            END IF;
                        END IF;
                    END;
                END;
            ---- MAH - AIL : Debut controle (Taux des Int?r?ts intercalaires doit ?tre ?gal au Taux Loyer)
            ELSIF SCONTROLE IN ('TXIIC')
            THEN
                BEGIN
                    DECLARE
                        nDosTx   NUMBER := 0;
                        nPreTx   NUMBER := 0;
                        nPre     NUMBER := 0;
                    BEGIN
                        SELECT COUNT (*)
                          INTO nPre
                          FROM dosrubrique
                         WHERE     dosid = nDosid
                               AND druclasse = 'F'
                               AND drutype = 'P';

                        IF (nPre <> 0)
                        THEN
                            SELECT ROUND (
                                       MAX (
                                           NVL (F_PLGETINTERESTRATE (nDosid),
                                                0)),
                                       2)
                              INTO nDosTx
                              FROM DUAL;

                            SELECT ROUND (
                                       MAX (NVL (dosrubrique.drutauxfixe, 0)),
                                       2)
                              INTO nPreTx
                              FROM dosrubrique
                             WHERE     dosid = nDosid
                                   AND druclasse = 'F'
                                   AND drutype = 'P';

                            IF (nDosTx = nPreTx)
                            THEN
                                nOk := 1;
                            ELSE
                                nOk := 0;
                            END IF;
                        END IF;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            nok := 1;
                    END;
                END;
            ---- MAH - AIL : Fin controle (Taux des Int?r?ts intercalaires doit ?tre ?gal au Taux Loyer)
            ---- RSM - AIL : Debut controle (Type de taxe pour "Repreneur" 'EVD_CESSION)
            ELSIF SCONTROLE IN ('TAXE')
            THEN
                DECLARE
                    nROLCODE   SASFACTURE.ROLCODE%TYPE := NULL;
                    nTAXCODE   SASFACTAXE.TAXCODE%TYPE := NULL;
                BEGIN
                    SELECT SFA.ROLCODE, SFT.TAXCODE
                      INTO nROLCODE, nTAXCODE
                      FROM SASFACTURE SFA, SASFACTAXE SFT, CREVT C -- DOSSIER D-- FACREFERENCE FRE
                     WHERE     SFA.SFAID = SFT.SFAID
                           AND SFA.FACTYPE = 'DP'
                           AND SFA.CREID = C.CREID
                           AND SFA.SFAID =
                               (SELECT MAX (SFAID)
                                  FROM SASFACTURE
                                 WHERE CREID IN
                                           (SELECT creid
                                              FROM crevt
                                             WHERE     tmffonction =
                                                       'EVD_CESSION'
                                                   AND DOSID = nDOSID));

                    IF nROLCODE = 'REP' AND nTAXCODE <> 'TAXAIL'
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- RSM - AIL : Fin controle (Type de taxe pour "Repreneur" 'EVD_CESSION)
            ---- MAH - AIL : D?but al?te FACTURATION FRAIS DE DOSSIER (Sur Sauvegarde Dossier et Classement Sans Suite EVD_SSUITE)
            ELSIF SCONTROLE IN ('FACFD')
            THEN
                DECLARE
                    nDosPhase   DOSPHASE.PHACODE%TYPE := NULL;
                    nFacture    FACTURE.FACID%TYPE := NULL;
                BEGIN
                    SELECT RE.FACID
                      INTO nFacture
                      FROM DOSRUBRIQUE DR, DOSRUBECHEANCIER RE
                     WHERE     DR.RUBID = 464
                           AND DR.DOSID = RE.DOSID
                           AND DR.DRUORDRE = RE.DRUORDRE
                           AND DR.DOSID = nDosId;

                    IF nFacture IS NULL
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin al?rte FACTURATION FRAIS DE DOSSIER (Sur Sauvegarde Dossier et Classement Sans Suite EVD_SSUITE)
            ---- MAH - AIL : Debut controle EGALITE BASE LOCATIVE ET VALEUR MATERIEL (Sur Modification Engagement EVD_ENGM)
            ELSIF SCONTROLE IN ('BLOVM')
            THEN
                DECLARE
                    nBaseLocative     DOSRUBRIQUE.DRUMTORIGINE%TYPE := NULL;
                    nITRId            ITRRUBRIQUE.ITRID%TYPE := NULL;
                    nValeurMateriel   ITRRUBRIQUE.IRUMTORIGINE%TYPE := NULL;
                BEGIN
                    SELECT ITR.ITRID, DR.DRUMTORIGINE
                      INTO nITRId, nBaseLocative
                      FROM ITRRUBRIQUE     ITR,
                           LKDOSRUBITRRUB  LKDR,
                           DOSRUBRIQUE     DR
                     WHERE     ITR.ITRID = LKDR.ITRID
                           AND DR.DOSID = LKDR.DOSID
                           AND LKDR.DRUORDRE = 1
                           AND DR.DOSID = nDosId
                           AND DR.RUBID = 3;

                    SELECT SUM (IRUMTORIGINE)
                      INTO nValeurMateriel
                      FROM ITRRUBRIQUE
                     WHERE ITRID = nITRId;

                    IF nBaseLocative != nValeurMateriel
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle EGALITE BASE LOCATIVE ET VALEUR MATERIEL (Sur Modification Engagement EVD_ENGM)
            ---- MAH - AIL : Debut controle sur le JOUR DE LA DATE D'EFFET DE MISE EN SERVICE (Sur Mise en Service EVD_MEL)
            ELSIF SCONTROLE IN ('DTMES')
            THEN
                DECLARE
                    nJourEffetMES   NUMBER := NULL;
                    nCREDTEFFET     DATE := NULL;            -- RSM 10/10/2017
                    nCREDTCREAT     DATE := NULL;            -- RSM 10/10/2017
                BEGIN
                    SELECT EXTRACT (DAY FROM CREDTEFFET)
                      INTO nJourEffetMES
                      FROM CREVT
                     WHERE     TMFFONCTION = 'EVD_MEL'
                           AND CREDTSUP IS NULL
                           AND DOSID = nDosId;

                    --RSM AIL 10/10/2017

                    SELECT CREDTEFFET
                      INTO nCREDTEFFET
                      FROM crevt
                     WHERE     TMFFONCTION = 'EVD_MEL'
                           AND CREDTSUP IS NULL
                           AND DOSID = nDosId;

                    SELECT CREDTCREAT
                      INTO nCREDTCREAT
                      FROM crevt
                     WHERE     TMFFONCTION = 'EVD_MEL'
                           AND CREDTSUP IS NULL
                           AND DOSID = nDosId;

                    IF    nJourEffetMES NOT IN (1, 15)
                       OR (    nJourEffetMES IN (1, 15)
                           AND nCREDTEFFET < nCREDTCREAT) /*RSM AIL 10/10/2017*/
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur le JOUR DE LA DATE D'EFFET DE MISE EN SERVICE (Sur Mise en Service EVD_MEL)
            ---- MAH - AIL : Debut controle sur l'EXISTANCE D'INTERETS INTERCALAIRES EN PRESENCE D'UN DECAISSEMENT FOURNISSEUR (Sur Mise en Service EVD_MEL)
            ELSIF SCONTROLE IN ('RUBII')
            THEN
                DECLARE
                    nCountReg     NUMBER := 0;
                    nCountRubII   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCountReg
                      FROM REGIMPUTATION REGIMP, REGLEMENT REG, DOSACTEUR ACT
                     WHERE     REGIMP.REGID = REG.REGID
                           AND ACT.ACTID = REG.ACTID
                           AND ACT.DOSID = REGIMP.DOSID
                           AND ACT.ROLCODE = 'FOURN'
                           AND REGIMP.DOSID = nDosId
                           AND REG.REGTYPEMVT = 'D';

                    IF nCountReg >= 1
                    THEN
                        SELECT COUNT (*)
                          INTO nCountRubII
                          FROM DOSRUBRIQUE
                         WHERE DOSID = nDosId AND RUBID = 4;

                        IF nCountRubII = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur l'EXISTANCE D'INTERETS INTERCALAIRES EN PRESENCE D'UN DECAISSEMENT FOURNISSEUR (Sur Mise en Service EVD_MEL)
            ---- MAH - AIL : Debut controle sur l'EXISTANCE DE FACTURES IMPAYEES POUR UN DOSSIER DONNE (Sur EVD_VENTE, EVD_VPART et EVD_CESSION)
            ELSIF SCONTROLE IN ('FACIM')
            THEN
                DECLARE
                    nCountImp   NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCountImp
                      FROM FACREFERENCE
                     WHERE     FREDOSID = NDOSID
                           AND F_PLFACTIMP (FACID, SYSDATE) > 0;

                    IF nCountImp > 0
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur l'EXISTANCE DE FACTURES IMPAYEES POUR UN DOSSIER DONNE (Sur EVD_VENTE, EVD_VPART et EVD_CESSION)
            ---- MAH - AIL : Debut controle sur la CHECKLIST "Avant Envoi bon de commande" (Sur EVD_MJALON)
            ELSIF SCONTROLE IN ('CHKBC')
            THEN
                DECLARE
                    nUncheckedItems   NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO nUncheckedItems
                      FROM ADMSTATUS
                     WHERE     ADMID IN
                                   (SELECT ADMID
                                      FROM ADMINISTRATIF
                                     WHERE     DOSID = nDosId
                                           AND ADMIDPARENT IN
                                                   (SELECT ADMID
                                                      FROM ADMINISTRATIF
                                                     WHERE     DOSID = nDosId
                                                           AND FORID IN
                                                                   (1001,
                                                                    1002)))
                           AND ASTSTATUS IN ('EC', 'IN')
                           AND ASTDTEND IS NULL;

                    IF nUncheckedItems >= 1
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur la CHECKLIST "Avant Envoi bon de commande" (Sur EVD_MJALON)
            ---- MAH - AIL : Debut controle sur la CHECKLIST "Avant Mise en service" (Sur EVD_MEL)
            ELSIF SCONTROLE IN ('CHKMS')
            THEN
                DECLARE
                    nUncheckedItems   NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO nUncheckedItems
                      FROM ADMSTATUS
                     WHERE     ADMID IN
                                   (SELECT ADMID
                                      FROM ADMINISTRATIF
                                     WHERE     DOSID = nDosId
                                           AND ADMIDPARENT IN
                                                   (SELECT ADMID
                                                      FROM ADMINISTRATIF
                                                     WHERE     DOSID = nDosId
                                                           AND FORID = 5475))
                           AND ASTSTATUS IN ('EC', 'IN')
                           AND ASTDTEND IS NULL;

                    IF nUncheckedItems >= 1
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur la CHECKLIST "Avant Mise en service" (Sur EVD_MEL)
            ---- MAH - AIL : Debut controle si les numeros d'immatriculation des materiels du dossier sont renseignes (Sur EVD_MEL)
            ELSIF SCONTROLE IN ('IMMES')
            THEN
                DECLARE
                    ---> Determiner la liste des materiels a parcourir sur le dossier
                    CURSOR curMaterielDos
                    IS
                        SELECT LK.ITRID, LK.IRUORDRE
                          FROM LKDOSRUBITRRUB  LK
                               INNER JOIN DOSRUBRIQUE DR
                                   ON     LK.DOSID = DR.DOSID
                                      AND LK.DRUORDRE = DR.DRUORDRE
                                      AND DR.RUBID = 3
                                      AND DR.DOSID = nDosId;

                    nCount   NUMBER := NULL;
                BEGIN
                    FOR cMateriel IN curMaterielDos
                    LOOP
                        SELECT COUNT (*)
                          INTO nCount
                          FROM ITRRUBCOMIMMAT
                         WHERE     ITRID = cMateriel.ITRID
                               AND IRUORDRE = cMateriel.IRUORDRE;

                        IF nCount = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle si les numeros d'immatriculation des materiels du dossier sont renseignes (Sur EVD_MEL)
            ---- MAH - AIL : Debut controle sur LA PRESENCE D'ENCAISSEMENTS NON VALIDES LIES LE DOSSIER A METTRE EN COURS (Sur EVD_REAL)
            ELSIF SCONTROLE IN ('ENCNV')
            THEN
                DECLARE
                    nCount   NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM REGLEMENT R
                     WHERE     R.REGTYPEMVT = 'E'
                           AND R.REGFLAGVALID = 0
                           AND R.REGID IN (SELECT RI.REGID
                                             FROM REGIMPUTATION RI
                                            WHERE RI.DOSID = nDosId);

                    IF nCount <> 0
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur LA PRESENCE D'ENCAISSEMENTS NON VALIDES LIES LE DOSSIER A METTRE EN COURS (Sur EVD_REAL)
            ---- MAH - AIL : Debut controle sur LE TAUX D'IMNISATION LORS D'UNE VENTE ANTICIPEE TOTALE (Sur EVD_VENTE)
            ELSIF SCONTROLE IN ('TXIND')
            THEN
                DECLARE
                    nTauxIndem   CREDATA.CDADATANUMBER%TYPE := NULL;
                    nCount       NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM DOSPHASE
                     WHERE     DOSID = nDosId
                           AND JALCODE = 'SUSPEND'
                           AND DPHMOTIF IN ('Vol', 'Accident (epave)');

                    IF nCount = 0
                    THEN ---> Dans le cas ou il ne s'agit pas d'un vol ou d'une epave, le taux d'indemnisation doit etre de 6%
                        ---> Tester sur le taux d'indeminisation
                        SELECT CDADATANUMBER
                          INTO nTauxIndem
                          FROM CREDATA
                         WHERE CREID = NCREID AND CDACOLONNE = 'PENALTYRATE';

                        IF nTauxIndem <> 6
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur LE TAUX D'IMNISATION LORS D'UNE VENTE ANTICIPEE TOTALE (Sur EVD_VENTE)
            ---- MAH - AIL : Debut controle sur le minimum de 6 mois de facturation de loyer pour le CBM et 24 mois pour le CBI, exige pour pouvoir effectuer une vente anticipee totale (Sur EVD_VENTE)
            ELSIF SCONTROLE IN ('FACVN')
            THEN
                DECLARE
                    nEcheancesMin   NUMBER := NULL;
                    nMultiple       DOSRUBFLUX.DRFMULTIPLE%TYPE := NULL;
                    nPeriode        DOSRUBFLUX.DRFPERIODE%TYPE := NULL;
                    nBase           NUMBER := 6;
                    nCount          NUMBER := NULL;
                    nTACCODE        VARCHAR2 (3) := NULL;
                    nNbMois         NUMBER := NULL;
                BEGIN
                    ---> Recuperer le nombre d'echeances de loyer facturees
                    SELECT COUNT (*)
                      INTO nCount
                      FROM DOSRUBECHEANCIER  RE
                           INNER JOIN DOSRUBRIQUE R
                               ON     RE.DOSID = R.DOSID
                                  AND RE.DRUORDRE = R.DRUORDRE
                                  AND R.RUBID = 3
                                  AND RE.DOSID = nDosId
                                  AND RE.DRETYPE = 'LOYER'
                                  AND RE.FACID IS NOT NULL;

                      ---> Duree de paiement en mois
                      SELECT SUM (RF.DRFNBPERIODE),
                             RF.DRFMULTIPLE,
                             DECODE (RF.DRFPERIODE,
                                     '030', 1,
                                     '090', 3,
                                     '180', 6,
                                     '360', 12)
                        INTO nNbMois, nMultiple, nPeriode
                        FROM DOSRUBFLUX RF
                             INNER JOIN DOSRUBRIQUE R
                                 ON     RF.DOSID = R.DOSID
                                    AND RF.DRUORDRE = R.DRUORDRE
                                    AND R.RUBID = 3
                                    AND RF.DOSID = nDosId
                    GROUP BY RF.DRFMULTIPLE, RF.DRFPERIODE;

                    SELECT TACCODE
                      INTO nTACCODE
                      FROM DOSSIER
                     WHERE DOSID = nDosId;

                    IF nTACCODE = 'CBI'
                    THEN ----------> CBI alors minimum de 24 mois, CBM alors minimum de 6 mois (declare ci-haut)
                        nBase := 24;
                    END IF;

                    IF nPeriode < 6
                    THEN                            ---> Mensuel / Trimestriel
                        nEcheancesMin := nMultiple * nBase / nPeriode;
                    ELSIF nPeriode = 6
                    THEN                                       ---> Semestriel
                        nEcheancesMin := nMultiple;
                    ELSIF nPeriode = 12
                    THEN                                           ---> Annuel
                        nEcheancesMin := 1;
                    END IF;

                    IF nCount < nEcheancesMin
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Debut controle sur le minimum de 6 mois de facturation de loyer pour le CBM et 24 mois pour le CBI, exige pour pouvoir effectuer une vente anticipee totale (Sur EVD_VENTE)
            ---- MAH - AIL : Debut controle sur TOUTES LES CHECKLISTS POUR LE CAS DE RELOCATION (Sur EVD_MEL)
            ELSIF SCONTROLE IN ('CHRLC')
            THEN
                DECLARE
                    nDosTypeFin       DOSSIER.DOSTYPEFINANCEMENT%TYPE := NULL;
                    nUncheckedItems   NUMBER := NULL;
                BEGIN
                    SELECT DOSTYPEFINANCEMENT
                      INTO nDosTypeFin
                      FROM DOSSIER
                     WHERE DOSID = nDosId;

                    IF nDosTypeFin = 'TTNL'
                    THEN
                        SELECT COUNT (*)
                          INTO nUncheckedItems
                          FROM ADMSTATUS
                         WHERE     ADMID IN
                                       (SELECT ADMID
                                          FROM ADMINISTRATIF
                                         WHERE     DOSID = nDosId
                                               AND ADMIDPARENT IN
                                                       (SELECT ADMID
                                                          FROM ADMINISTRATIF
                                                         WHERE     DOSID =
                                                                   nDosId
                                                               AND FORID IN
                                                                       (1001,
                                                                        1002,
                                                                        5447,
                                                                        5461,
                                                                        5475,
                                                                        5481)))
                               AND ASTSTATUS IN ('EC', 'IN')
                               AND ASTDTEND IS NULL;

                        IF nUncheckedItems >= 1
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur TOUTES LES CHECKLISTS POUR LE CAS DE RELOCATION (Sur EVD_MEL)

            -- LMI  2013-11-25 JIRA 8643 CHECK FOR CANCELLATION AMOUNT
            -- JLA 2013-11-28 Serial number
            /*ELSIF sControle IN ('DVIN') THEN
            DECLARE
            l_Count        NUMBER := 0;
            BEGIN
            SELECT  COUNT(1)
            INTO     l_Count
            FROM   LKDOSRUBITRRUB, DOSSIER, ITRRUBRIQUE, CAS_DOSPHASE
            WHERE  IRUSERIALNUMBER IN (SELECT     DPMNUMSERIE
            FROM   DPRMATERIEL
            WHERE  DOSID = nDosId
            AND                DPRVERSION = 'PROD'
            AND                DPMNUMSERIE IS NOT NULL)
            AND                ITRRUBRIQUE.ITRID = LKDOSRUBITRRUB.ITRID
            AND                DOSSIER.DOSID = LKDOSRUBITRRUB.DOSID
            AND                CAS_DOSPHASE.DOSID = DOSSIER.DOSID
            AND                CAS_DOSPHASE.PHACODE <> 'TER'
            AND                CAS_DOSPHASE.DPHDTFIN IS NULL;
            IF l_Count > 0 THEN
            nOk := 0;
            END IF;
            EXCEPTION
            WHEN OTHERS THEN
            nOk := 0;
            END;*/

            ---- RSM 20170623 AIL_comparaison TEG et Taux d'usure
            ---MBN  05-12-2017 Control sur la checklist "Checklist du jalon "Dossier pris en charge par lanalyste"


            ELSIF (sControle = 'TTEG')
            THEN
                DECLARE
                    nTEG      NUMBER := 0;
                    nTMAX     Pvetable.Ptatxmargemaxi%TYPE;
                    n_Dosid   DOSSIER.Dosid%TYPE;
                    Tauxf     Pvetable.Ptatxfixe%TYPE;
                    Margem    Pvetable.Ptatxmargemaxi%TYPE;
                    Teg       Pvetable.Ptatxmargemaxi%TYPE;
                BEGIN
                    --Detection de la valeur du TEG

                    nTEG := F_CALCUL_TEG_FO (nDosid, 'FR', '');

                    -- nTEG := p_calcul_teg_bo (nDosid, 'FR', '');

                    --Detection de la valeur du Taux d'usure
                    SELECT DISTINCT (Ptatxfixe), Ptatxmargemaxi
                      INTO Tauxf, Margem
                      FROM Pvetable
                     WHERE Pelid IN (404287, 404288, 404289);

                    nTMAX := Tauxf + Margem;

                    IF nTEG > nTMAX
                    THEN
                        nOK := 0;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOK := 1;                                -- Erreur OK.
                END;
            ELSIF sControle = 'DOSPF'
            THEN ----------MTR 31/10/2018:  Un seul responsable PF actif sur le dossier
                DECLARE
                    nCount   NUMBER := 0;
                BEGIN
                    SELECT COUNT (*)
                      INTO nCount
                      FROM lkdosutitsm
                     WHERE     TSMMETIER = 'PORTFL'
                           AND uticode IN
                                   (SELECT uticode
                                      FROM utilisateur
                                     WHERE NVL (UTIFLAGINACTIF, 0) != 1)
                           AND DOSID = nDosId;


                    IF (nCount = 1)
                    THEN
                        nok := 1;
                    ELSE
                        nok := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ---- RSM 20170623 AIL_comparaison TEG et Taux d'usure
            ---RSM - AIL Controle sur notification du modification de l'engagement
            ELSIF SCONTROLE = ('NTENG')
            THEN
                DECLARE
                    nFORID     ADMINISTRATIF.FORID%TYPE := NULL;
                    l_result   NUMBER := 0;
                BEGIN
                    /*SELECT FORID
                      INTO nFORID
                      FROM ADMINISTRATIF
                      WHERE CREID=NCREID;*/

                    L_result := APL_NOTI_MONTANT_ENGA_AFTER (nDosid);

                    IF /* l_result = 1 and nFORID = 7373 THEN
                       NOK := 0;
                     Elsif */
                       l_result = 2                       /*and nFORID=10116*/
                    THEN
                        -- NOK := 0;

                        NOK := 1;
                    ELSE
                        -- NOK:=1;
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---RSM -AIL Fin Controle sur notification du modification de l'engagement

            ----------- Debut des controles OCDOSSIER CDMLF ajoutes :

            ELSIF scontrole IN ('ITRAM')                   -- EBM controle 4.0
            THEN
                DECLARE
                    l_itrid      ITRRUBRIQUE.ITRID%TYPE;
                    l_iruordre   ITRRUBRIQUE.IRUORDRE%TYPE;
                    l_NbrFl      NUMBER;
                BEGIN
                    SELECT itrid, iruordre
                      INTO l_itrid, l_iruordre
                      FROM itrrubphase
                     WHERE creid = ncreid;

                    IF (l_itrid IS NOT NULL)
                    THEN
                        SELECT COUNT (*)
                          INTO l_NbrFl
                          FROM ITRRUBFLUX
                         WHERE itrid = l_itrid AND iruordre = l_iruordre;

                        IF (l_NbrFl = 0)
                        THEN
                            nok := 0;                                    -- KO
                        ELSE
                            nok := 1;                                    -- OK
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('SSTER')
            THEN                                           -- EBM CONTROLE 4.0
                DECLARE
                    l_reloc      dossier.dostypefinancement%TYPE;
                    l_itrphase   itrrubphase.phacode%TYPE;
                    l_count      NUMBER;
                BEGIN
                    SELECT dostypefinancement
                      INTO l_reloc
                      FROM dossier
                     WHERE dosid = ndosid;

                    SELECT COUNT (*)
                      INTO l_count
                      FROM lkdosrubitrrub
                     WHERE dosid = ndosid;

                    IF (l_reloc = 'TTNL' OR l_count < 1)
                    THEN
                        nok := 1;
                    ELSE
                        SELECT irp.phacode
                          INTO l_itrphase
                          FROM imalkdosrubitrrub  lkd,
                               itrrubphase        irp,
                               dossier            dos
                         WHERE     dos.dosid = lkd.dosid
                               AND irp.itrid = lkd.itrid
                               AND irp.irpdtfin IS NULL
                               AND irp.iruordre = lkd.iruordre
                               AND lkd.imaid = (SELECT MAX (imaid)
                                                  FROM imalkdosrubitrrub
                                                 WHERE dosid = ndosid)
                               AND dos.dosid = ndosid
                               AND ROWNUM = 1;

                        IF (l_itrphase = 'TER')
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('CGPCT')
            THEN                                       -- EBM: controle de 4.0
                DECLARE
                    CURSOR c_dosgarantie
                    IS
                        SELECT dag.tgacode, dag.dagpctgarantieutilise
                          FROM dosactgarantie dag
                         WHERE dosid = ndosid;
                BEGIN
                    FOR dosgarantie_rec IN c_dosgarantie
                    LOOP
                        IF (    dosgarantie_rec.tgacode = 'GT_CGar'
                            AND dosgarantie_rec.dagpctgarantieutilise IS NULL)
                        THEN
                            nok := 0;
                            EXIT;
                        ELSE
                            nok := 1;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('APPAF')
            THEN                                       -- EBM: controle de 4.0
                DECLARE
                    CURSOR c_rolacteur
                    IS
                        SELECT rolcode
                          FROM dosacteur
                         WHERE dosid = ndosid AND dacdtfin IS NULL;
                BEGIN
                    FOR rolacteur_rec IN c_rolacteur
                    LOOP
                        IF (rolacteur_rec.rolcode = 'APPORT')
                        THEN
                            nok := 1;
                            EXIT;
                        ELSE
                            nok := 0;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('AMTFI')
            THEN                                       -- EBM: controle de 4.0
                DECLARE
                    l_mtInv     NUMBER := 0;
                    l_mtAss     NUMBER := 0;
                    l_methAmt   VARCHAR2 (50);

                    CURSOR c_immotranche
                    IS
                        SELECT DISTINCT (itrid)
                          FROM lkdosrubitrrub
                         WHERE dosid = ndosid;
                BEGIN
                    FOR immotranche_rec IN c_immotranche
                    LOOP
                        SELECT iru.irumtorigine,
                               irf.irfmtassiette,
                               irf.tlfcode
                          INTO l_mtInv, l_mtAss, l_methAmt
                          FROM itrrubrique     iru,
                               itrrubflux      irf,
                               lkdosrubitrrub  lkd
                         WHERE     iru.itrid = immotranche_rec.itrid
                               AND iru.itrid = irf.itrid
                               AND iru.iruordre = irf.iruordre
                               AND lkd.itrid = iru.itrid
                               AND lkd.druordre =
                                   (SELECT MAX (druordre)
                                      FROM lkdosrubitrrub
                                     WHERE     dosid = lkd.dosid
                                           AND itrid = immotranche_rec.itrid)
                               AND lkd.iruordre = iru.iruordre
                               AND irf.irfordre = (SELECT MAX (irfordre)
                                                     FROM itrrubflux
                                                    WHERE itrid = lkd.itrid)
                               AND lkd.dosid = ndosid;

                        IF (    (l_methAmt = 'LINAP_FVRQP')
                            AND (   (l_mtAss IS NULL)
                                 OR (    (l_mtAss IS NOT NULL)
                                     AND (l_mtAss <> l_mtInv))))
                        THEN
                            nok := 0;
                            EXIT;
                        ELSE
                            nok := 1;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ------ MAH : Debut controle sur item (Assurance re?ue) dans le checklist (Reglement fournisseur)
            ELSIF SCONTROLE = ('CHKAR')
            THEN
                DECLARE
                    nUncheckedItems   NUMBER := NULL;
                BEGIN
                    --dbms_output.put_line('inside');
                    SELECT COUNT (*)
                      INTO nUncheckedItems
                      FROM ADMSTATUS
                     WHERE     ADMID IN
                                   (SELECT ADMID
                                      FROM ADMINISTRATIF
                                     WHERE     DOSID = ndosid
                                           AND ADMIDPARENT IN
                                                   (SELECT ADMID
                                                      FROM ADMINISTRATIF
                                                     WHERE     DOSID = ndosid
                                                           AND FORID IN (410))
                                           AND FORID = 414)
                           AND ASTSTATUS IN ('EC', 'IN')
                           AND ASTDTEND IS NULL;

                    --dbms_output.put_line('nUncheckedItems = '||nUncheckedItems);
                    IF nUncheckedItems >= 1
                    THEN
                        NOK := 0;
                    END IF;
                --dbms_output.put_line('NOK = '||NOK);
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        --dbms_output.put_line('Exception');
                        NOK := 0;
                END;
            ------ MAH : Fin controle sur item (Assurance re?ue) dans le checklist (Reglement fournisseur)



            ELSIF scontrole IN ('BAM')
            THEN                                       -- EBM: controle de 4.0
                DECLARE
                    l_intBAM   VARCHAR2 (50);
                BEGIN
                    SELECT cvastringvalue
                      INTO l_intBAM
                      FROM cchvalue
                     WHERE cchsid = 'TFDCCHSID208' AND dosid = ndosid;

                    IF (l_intBAM IS NULL)
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF scontrole IN ('LIAU')                    -- EBM controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.
                DECLARE
                    nactid        VARCHAR2 (50);
                    l_mtutilise   NUMBER;
                    l_mtdispo     NUMBER;
                    l_mtauto      NUMBER;
                    l_mtfinance   NUMBER;
                BEGIN
                    SELECT actid
                      INTO nactid
                      FROM dosacteur
                     WHERE     dosid = ndosid
                           AND rolcode = 'CLIENT'
                           AND dacdtfin IS NULL;

                    SELECT cvanumericvalue
                      INTO l_mtauto
                      FROM cchvalue
                     WHERE cchsid = 'TFDCCHSID100' AND actid = nactid;

                    SELECT dosmtproduct
                      INTO l_mtfinance
                      FROM dossier
                     WHERE dosid = ndosid;

                    l_mtutilise :=
                        pav4_cdml_reporting.f_get_mtUtilise_ligne_client (
                            nactid);

                    l_mtdispo := l_mtauto - l_mtutilise;


                    IF l_mtauto IS NULL OR l_mtdispo >= L_mtfinance
                    THEN
                        nok := 1;                                        -- OK
                    ELSE
                        nok := 0;                                        -- KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('ACTG')                  --EBM: added from 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_RES   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_res
                      FROM dosacteur
                     WHERE     actid IN (SELECT actid
                                           FROM acteur
                                          WHERE actcode LIKE '%999-%')
                           AND dosid = ndosid;



                    IF l_res >= 1
                    THEN
                        nok := 0;                                        -- OK
                    ELSE
                        nok := 1;                                        -- KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('TRI')                      -- EBM contole 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_mtfinance   NUMBER;
                    l_TRI         NUMBER;
                    l_tauxref     NUMBER;
                BEGIN
                    l_TRI := f_cdml_get_tri (nDosid, 'FR', 'ORFI');

                    SELECT dosmtproduct
                      INTO l_mtfinance
                      FROM dossier
                     WHERE dosid = ndosid;


                    IF (l_mtfinance BETWEEN 0 AND 100000)
                    THEN
                        SELECT tvaval
                          INTO l_tauxref
                          FROM tauvaleur
                         WHERE taucode = 'TXTRI1';


                        IF (l_TRI >= l_tauxref)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_mtfinance BETWEEN 100001 AND 200000)
                    THEN
                        SELECT tvaval
                          INTO l_tauxref
                          FROM tauvaleur
                         WHERE taucode = 'TXTRI2';


                        IF (l_TRI >= l_tauxref)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_mtfinance > 200000)
                    THEN
                        SELECT tvaval
                          INTO l_tauxref
                          FROM tauvaleur
                         WHERE taucode = 'TXTRI2';


                        IF (l_TRI >= l_tauxref)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('DONC')                    -- EBM CONTROLE 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_jalcode   VARCHAR2 (50);
                    l_res       NUMBER;
                BEGIN
                    SELECT jalcode
                      INTO l_jalcode
                      FROM dosphase
                     WHERE creid = ncreid;


                    IF (l_jalcode IN ('DONCEDI', 'DOCEDI'))
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'OFACCOM' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CTEDITE')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'DOCEDI' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CTBRSCL')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'DOCEDI' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CRSCTSN')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'CTBRSCL' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CBDSCDM')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'CTBDSCD' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    END IF;
                -- ? reprendre le m?me bloc pour les autres groupes avec les jalons autoris?s

                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('JACBM')                  -- EBM  controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_uti       VARCHAR2 (50);
                    l_jalcode   VARCHAR2 (50);
                    l_groupe    VARCHAR2 (50);
                BEGIN
                    SELECT uticodecreat
                      INTO l_uti
                      FROM crevt
                     WHERE creid = ncreid;

                    SELECT grocode
                      INTO l_groupe
                      FROM utilisateur
                     WHERE uticode = l_uti;

                    SELECT jalcode
                      INTO l_jalcode
                      FROM dosphase
                     WHERE creid = nCreid;

                    IF    (    l_jalcode IN ('OFPREP',
                                             'SIGFIX',
                                             'OFREFC',
                                             'ACCEPTE',
                                             'ACCORD',
                                             'CRSCTSN')
                           AND l_groupe IN ('GRPORFI', 'COM')) --? compl?ter avec les jalons autoris?s de FO
                       OR (    l_jalcode IN ('ACCORD',
                                             'DONCEDI',
                                             'DOCEDI',
                                             'CTEDITE',
                                             'CTNCFMO',
                                             'CTBRSCL',
                                             'CRSCTSN')
                           AND l_groupe IN ('GRPORFI', 'MO')) --? compl?ter avec les jalons autoris?s de MO
                       OR (    l_jalcode IN ('CTSICRE',
                                             'CTSICNC',
                                             'CSCCDBO',
                                             'CSCNCDB',
                                             'CTBDSCD',
                                             'CBDSCDM',
                                             'DOSCOM',
                                             'DOSNCO',
                                             'DOSCON')
                           AND l_groupe IN ('GRPORFI', 'BO')) --? compl?ter avec les jalons autoris?s de BO
                       OR (    l_jalcode IN ('DOSRANA',
                                             'OFAJCOM',
                                             'OFRFCOM',
                                             'OFACCOM')
                           AND l_groupe IN ('GRPORFI', 'ANALY')) --? compl?ter avec les jalons autoris?s de AC
                    THEN
                        nok := 1;                                        -- OK
                    ELSE
                        nok := 0;                                        -- KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('JACBI')                  --EBM : controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_uti       VARCHAR2 (50);
                    l_jalcode   VARCHAR2 (50);
                    l_groupe    VARCHAR2 (50);
                BEGIN
                    SELECT uticodecreat
                      INTO l_uti
                      FROM crevt
                     WHERE creid = ncreid;

                    SELECT grocode
                      INTO l_groupe
                      FROM utilisateur
                     WHERE uticode = l_uti;

                    SELECT jalcode
                      INTO l_jalcode
                      FROM dosphase
                     WHERE creid = nCreid;

                    IF    (    l_jalcode IN ('OFPREP',
                                             'SIGFIX',
                                             'OFREFC',
                                             'ACCEPTE',
                                             'ACCORD',
                                             'CRSCTSN')
                           AND l_groupe IN ('GRPORFI', 'COM')) --? compl?ter avec les jalons autoris?s de FO
                       OR (    l_jalcode IN ('ACCORD',
                                             'LENGCC',
                                             'LENGRC',
                                             'LENGAC',
                                             'CTCONMO',
                                             'CTBRSCL',
                                             'CRSCTSN')
                           AND l_groupe IN ('GRPORFI', 'MO')) --? compl?ter avec les jalons autoris?s de MO
                       OR (    l_jalcode IN ('CTEDITE',
                                             'CTNCFMO',
                                             'CONEDTC',
                                             'CTSICRE',
                                             'CTSICNC',
                                             'CSCCDBO',
                                             'CSCNCDB',
                                             'CBDSCDM',
                                             'CSNOT',
                                             'MINTRE',
                                             'MINTNC',
                                             'MINTES',
                                             'MINSCDM',
                                             'SICHQR',
                                             'MINRNOT')
                           AND l_groupe IN ('GRPORFI', 'BO')) --? compl?ter avec les jalons autoris?s de BO
                       OR (    l_jalcode IN ('DOSRANA',
                                             'OFAJCOM',
                                             'OFRFCOM',
                                             'OFACCOM')
                           AND l_groupe IN ('GRPORFI', 'ANALY')) --? compl?ter avec les jalons autoris?s de AC
                    THEN
                        nok := 1;                                        -- OK
                    ELSE
                        nok := 0;                                        -- KO
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            -- 20110710 RIC Mise ? jour jalon non autoris?

            ELSIF scontrole IN ('CCBI')                   --EBM : controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    l_jalcode   VARCHAR2 (50);
                    l_res       NUMBER;
                BEGIN
                    SELECT jalcode
                      INTO l_jalcode
                      FROM dosphase
                     WHERE creid = ncreid;


                    IF (l_jalcode = 'CTEDITE')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'DOSCRBO' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 1; -- KO --20111014 RIC A Verifier ce jalon ??
                        END IF;
                    ELSIF (l_jalcode = 'CONEDTC')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'CTEDITE' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CRSCTSN')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'CTBRSCL' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'CSNOT')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'CTESCDM' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    ELSIF (l_jalcode = 'MINRNOT')
                    THEN
                        SELECT COUNT (*)
                          INTO l_res
                          FROM dosphase
                         WHERE jalcode = 'SICHQR' AND dosid = ndosid;

                        IF (l_res >= 1)
                        THEN
                            nok := 1;                                    -- OK
                        ELSE
                            nok := 0;                                    -- KO
                        END IF;
                    END IF;
                -- ? reprendre le m?me bloc pour les autres groupes avec les jalons autoris?s

                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('RCDM')                    -- EBM controle 4.0
            THEN
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.

                DECLARE
                    nactid   VARCHAR2 (50);
                    l_res    NUMBER;
                    l_gar    NUMBER;
                    l_cdm    NUMBER;
                BEGIN
                    SELECT actid
                      INTO nactid
                      FROM dosacteur
                     WHERE     dosid = ndosid
                           AND rolcode = 'CLIENT'
                           AND dacdtfin IS NULL;


                    SELECT COUNT (*)
                      INTO l_res
                      FROM rib
                     WHERE     bgubanque = '021'
                           AND ribid IN (SELECT ribid
                                           FROM actrib
                                          WHERE actid = nactid);

                    SELECT COUNT (*)
                      INTO l_gar
                      FROM dosactgarantie
                     WHERE tgacode = 'GT_CGar' AND dosid = ndosid;

                    /*  select count(*) into l_cdm from dosacteur
                      where rolcode='APPORT' and actid='51256'
                      and dosid=ndosid;
                    */

                    SELECT COUNT (*)
                      INTO l_cdm
                      FROM dosacteur                          --mod 19/08/2011
                     WHERE     rolcode = 'APPORT'
                           AND actid = (SELECT actid
                                          FROM acteur
                                         WHERE actcode = '107')
                           AND dosid = ndosid;


                    /*   IF ( ( l_res >=1 or l_cdm >=1) and  l_gar >=1)

                        THEN
                                      nok := 1;            -- OK
                       ELSE
                                      nok := 0;            -- KO
                       END IF;
                    */

                    IF (l_cdm >= 1 AND l_gar >= 1)
                    THEN                                      --mod 19/08/2011
                        IF l_res = 0
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;
                        END IF;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF SCONTROLE = ('ROLN')
            THEN
                DECLARE
                    nCount          NUMBER;
                    l_tmffonction   VARCHAR2 (50);
                BEGIN
                    SELECT tmffonction
                      INTO l_tmffonction
                      FROM crevt
                     WHERE creid = ncreid;

                    IF (l_tmffonction = 'EVD_MEL')
                    THEN
                        SELECT COUNT (*)
                          INTO nCount
                          FROM dosacteur dos, ACTTELECOM com
                         WHERE     dos.DOSID = nDosid
                               AND dos.actid = com.actid
                               AND dos.rolcode = 'NOTAIRE'
                               AND dos.DACDTFIN IS NOT NULL
                               AND com.ATENUM IS NOT NULL;
                    END IF;

                    IF nCount = 0
                    THEN
                        NOK := 0;
                    END IF;
                END;
            --DBT--UAT-221: ne pas permettre la confirmation de l?evenement< Cession anticipee totale >,  si le Client (pas seulement le Dossier objet de cession) a des factures impayees (tout type de factures)
            ELSIF (SCONTROLE = 'IMCLT')
            THEN
                SELECT COUNT (DISTINCT FAC.FACID)
                  INTO NCOUNT
                  FROM FACTURE FAC, DOSACTEUR DAC
                 WHERE     FAC.ACTIDCLIENT = DAC.ACTID
                       AND F_PLRESTANTFACTURE (FAC.FACID, NULL) > 0
                       AND F_PLROLEEXTERNE (FAC.ROLCODE) = 'CLIENT'
                       AND DAC.DOSID = NDOSID
                       AND DAC.ROLCODE = 'CLIENT'
                       AND DAC.DACDTFIN IS NULL;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                ELSE
                    NOK := 1;
                END IF;
            --DBT--UAT-220: Manque du controle bloquant la MEF si le premier loyer est strictement superieur au deuxieme loyer (1er loyer majore) et qu?il n?y a aucun encaissement sur compte d?attente (imputation analytique < Avance Client >)
            ELSIF SCONTROLE IN ('LOYNE')
            THEN
                DECLARE
                    nDREMTBASE1   NUMBER;
                    nDREMTBASE2   NUMBER;
                    nCount        NUMBER := NULL;
                BEGIN
                    SELECT DREMTBASE
                      INTO nDREMTBASE1
                      FROM DOSRUBECHEANCIER
                     WHERE dosid = nDOSID AND DRUORDRE = 1 AND DREORDRE = 2;

                    SELECT DREMTBASE
                      INTO nDREMTBASE2
                      FROM DOSRUBECHEANCIER
                     WHERE dosid = nDOSID AND DRUORDRE = 1 AND DREORDRE = 3;

                    SELECT COUNT (*)
                      INTO nCount
                      FROM REGIMPUTATION
                     WHERE     dosid = nDOSID
                           AND RUBID = 257
                           AND RIMLETTRAGE IS NULL;

                    IF nDREMTBASE1 > nDREMTBASE2 AND nCount = 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH : Debut controle sur l'EXISTENCE D'INTERETS DE RETARD NON FACTURES POUR UN CLIENT DONNE (Sur EVD_vente )
            ELSIF SCONTROLE IN ('IRNOF')
            THEN
                DECLARE
                    nClientId   ACTEUR.ACTID%TYPE := NULL;
                    nCount      NUMBER := NULL;
                BEGIN
                    SELECT ACTID
                      INTO nClientId
                      FROM DOSACTEUR
                     WHERE     DOSID = nDosId
                           AND ROLCODE = 'CLIENT'
                           AND DACDTFIN IS NULL;

                    SELECT COUNT (*)
                      INTO nCount
                      FROM FACIR IR, FACTURE F
                     WHERE     IR.FACID = F.FACID
                           AND F.ACTIDCLIENT = nClientId
                           AND FACIDCALCULE IS NULL
                           AND (firflagdiffere != 1 OR firflagdiffere IS NULL);

                    IF nCount > 0
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH : Fin controle sur l'EXISTENCE D'INTERETS DE RETARD NON FACTURES POUR UN CLIENT DONNE (Sur EVD_VENTE )
            ----------- Fin des controles OCDOSSIER CDMLF ajoutes
            END IF;


            RETURN NOK;
        END;
    END OCDOSSIER;

    --08/04/03 Controle sur tranche somme des composants egal a l'assiette de la rubrique financiere
    FUNCTION OCITRGENERALITES (NITRID      IN IMMOTRANCHE.ITRID%TYPE,
                               SCONTROLE   IN VARCHAR2,
                               NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK         NUMBER := 1;
            NDOSID      DOSRUBRIQUE.DOSID%TYPE;
            SDEVCODE    DEVISE.DEVCODE%TYPE;
            NDRUORDRE   DOSRUBRIQUE.DRUORDRE%TYPE;
            NMTTOTAL    NUMBER;
            NTOTAL      NUMBER;

            CURSOR CDRU
            IS
                SELECT DRU.DRUORDRE, DRU.DRUMTORIGINE, LKD.DOSID
                  FROM LKDOSRUBITRRUB LKD, DOSRUBRIQUE DRU
                 WHERE     LKD.ITRID = NITRID
                       AND DRU.DRUORDRE = LKD.DRUORDRE
                       AND DRU.DOSID = LKD.DOSID
                       AND DRU.DRUORDREPREC IS NULL
                       AND DRU.DRUCLASSE = 'F'
                       AND DRU.DRUTYPE = 'F';

            CURSOR CRIR
            IS
                SELECT IRUORDRE, ITRID
                  FROM LKDOSRUBITRRUB
                 WHERE DOSID = NDOSID AND DRUORDRE = NDRUORDRE;
        BEGIN
            IF (SCONTROLE = 'DRUMT')
            THEN
                NMTTOTAL := 0;

                FOR CDRU_REC IN CDRU
                LOOP
                    NDRUORDRE := NVL (CDRU_REC.DRUORDRE, 0);
                    NDOSID := CDRU_REC.DOSID;
                    NMTTOTAL := 0;
                    NTOTAL := 0;

                    SELECT DEVCODE
                      INTO SDEVCODE
                      FROM DOSSIER
                     WHERE DOSID = NDOSID;

                    FOR CRIR_REC IN CRIR
                    LOOP
                        SELECT NVL (
                                   SUM (
                                       F_CONVDEVISE (
                                           ITR.DEVCODE,
                                           SDEVCODE,
                                           IRC.IRCPU * IRC.IRCQUANTITE,
                                           SYSDATE)),
                                   0)
                          INTO NTOTAL
                          FROM ITRRUBCOMPOSANT IRC, IMMOTRANCHE ITR
                         WHERE     ITR.ITRID = IRC.ITRID
                               AND IRC.ITRID = CRIR_REC.ITRID
                               AND IRC.IRUORDRE = CRIR_REC.IRUORDRE;

                        NMTTOTAL := NMTTOTAL + NTOTAL;
                    END LOOP;

                    IF    (CDRU_REC.DRUMTORIGINE IS NOT NULL)
                       OR (NMTTOTAL IS NOT NULL)
                    THEN
                        IF (CDRU_REC.DRUMTORIGINE != 0) OR (NMTTOTAL != 0)
                        THEN
                            IF ABS (
                                   (NVL (CDRU_REC.DRUMTORIGINE, 0) - NMTTOTAL)) >=
                               0.01
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCITRGENERALITES;

    FUNCTION OCITRSPECITALIE (NITRID      IN IMMOTRANCHE.ITRID%TYPE,
                              SCONTROLE   IN VARCHAR2,
                              NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;

            CURSOR CTFTAX
            IS
                SELECT DISTINCT
                       DOS.DOSTYPEFINANCEMENT, IRU.TAXCODE, DLI.DEPID
                  FROM ITRRUBRIQUE     IRU,
                       LKDOSRUBITRRUB  LDR,
                       DOSSIER         DOS,
                       DEPLIGNE        DLI
                 WHERE     IRU.ITRID = NITRID
                       AND IRU.TAXCODE IS NOT NULL
                       AND LDR.ITRID = IRU.ITRID
                       AND LDR.IRUORDRE = IRU.IRUORDRE
                       AND DOS.DOSID = LDR.DOSID
                       AND DOS.DOSTYPEFINANCEMENT IN ('LSBACK', 'ACHAT')
                       AND DLI.ITRID(+) = IRU.ITRID
                       AND DLI.IRUORDRE(+) = IRU.IRUORDRE;
        BEGIN
            IF (SCONTROLE = 'TFTAX')
            THEN
                FOR CTFTAX_REC IN CTFTAX
                LOOP
                    IF F_ISRUBIMMOTYPEFINANTAXE (
                           CTFTAX_REC.DOSTYPEFINANCEMENT,
                           CTFTAX_REC.TAXCODE,
                           CTFTAX_REC.DEPID) !=
                       1
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCITRSPECITALIE;

    FUNCTION OCTRANCHE (NITRID      IN IMMOTRANCHE.ITRID%TYPE,
                        SCONTROLE   IN VARCHAR2,
                        NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;
        BEGIN
            IF (SCONTROLE = 'DRUMT')
            THEN
                NOK := OCITRGENERALITES (NITRID, SCONTROLE, NCREID);
            -- LMI  2013-10-10 SIRET OF SUPPLYER WHEN DOMESTIC PRODUCTION FOR ASSET ELEMENTS FOR ALL G410 JIRA 7482
            ELSIF SCONTROLE = 'SIRDO'
            THEN
                DECLARE
                    NFOUNDFRS   NUMBER := 1;

                    CURSOR C1
                    IS
                          SELECT IRUORDRE,
                                 TPGCODE,
                                 IRUCATEGORY,
                                 ACTIDFRS
                            FROM ITRRUBRIQUE
                           WHERE ITRID = NITRID
                        ORDER BY IRUORDRE;
                BEGIN
                    NOK := 1;

                    FOR C1R IN C1
                    LOOP
                        IF (    UPPER (NVL (C1R.TPGCODE, 'xXx')) LIKE 'G410%'
                            AND NVL (C1R.IRUCATEGORY, 'xXx') = 'CATDOMP')
                        THEN
                            SELECT COUNT (ACTSIRET)
                              INTO NFOUNDFRS
                              FROM ACTEUR
                             WHERE     ACTID = C1R.ACTIDFRS
                                   AND ACTSIRET IS NOT NULL;

                            EXIT WHEN NFOUNDFRS = 0;
                        END IF;
                    END LOOP;

                    IF NFOUNDFRS = 0
                    THEN
                        NOK := 0;
                    END IF;
                END;
            -- LMI  2013-10-10 SIRET OF SUPPLYER WHEN DOMESTIC PRODUCTION FOR ASSET ELEMENTS FOR ALL G410 JIRA 7482
            ELSIF (sControle = 'TAXVI')
            THEN                                                    --20102011
                DECLARE
                    l_taxcode      VARCHAR2 (20);
                    l_irpdteffet   DATE;
                BEGIN
                    /*select taxcode into l_taxcode
                from facligne fl, facture fa
                where fl.rubid =11
                and fl.facid = fa.facid
                and fa.creid = nCreId;*/
                    SELECT MAX (taxcode)
                      INTO l_taxcode
                      FROM sasfacligne fl, sasfacture fa
                     WHERE     fl.rubid = 11
                           AND fl.sfaid = fa.sfaid
                           AND fa.creid = nCreId;

                    IF (l_taxcode IS NOT NULL)
                    THEN
                        /*select max(irpdteffet) into l_irpdteffet
                         from itrrubphase
                         where itrid =nItrid
                         and phacode in ( 'EC','ES' )
                         and IRPDTMAJ = (select max(IRPDTMAJ) from itrrubphase where itrid = nItrid and phacode in ( 'EC','ES' ));
                         */
                        --20120210
                        SELECT MAX (dphdteffet)
                          INTO l_irpdteffet
                          FROM dosphase
                         WHERE     dosid IN (SELECT dosid
                                               FROM lkdosrubitrrub
                                              WHERE itrid = nItrid)
                               AND phacode = 'ES'
                               AND jalcode IN ('ES', 'PART')
                               AND DPHDTMAJ =
                                   (SELECT MAX (DPHDTMAJ)
                                      FROM dosphase
                                     WHERE     dosid IN
                                                   (SELECT dosid
                                                      FROM lkdosrubitrrub
                                                     WHERE itrid = nItrid)
                                           AND phacode = 'ES'
                                           AND jalcode IN ('ES', 'PART'));

                        /*ajout EMI 20121004*/
                        IF (l_irpdteffet IS NULL)
                        THEN
                            SELECT MIN (drudtdeb)
                              INTO l_irpdteffet
                              FROM dosrubrique
                             WHERE (dosid || druordre) IN
                                       (  SELECT dosid || MIN (druordre)
                                            FROM lkdosrubitrrub
                                           WHERE itrid = nItrid
                                        GROUP BY dosid);
                        END IF;

                        /*ajout EMI 20121004*/
                        IF (l_irpdteffet <=
                            TO_DATE ('31/12/2007', 'DD/MM/YYYY'))
                        THEN
                            IF (l_taxcode = 'EXOTN')
                            THEN
                                nok := 1;                                -- KO
                            ELSE
                                nok := 0;
                            END IF;
                        ELSE
                            IF (l_taxcode = 'TVATN')
                            THEN
                                nok := 1;                                -- KO
                            ELSE
                                nok := 0;
                            END IF;
                        END IF;
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF scontrole IN ('ENCVM')                            --20120416
            THEN
                DECLARE
                    l_Fac_MtHT    NUMBER;
                    l_EncoursHT   NUMBER;
                BEGIN
                    SELECT cremt
                      INTO l_Fac_MtHT
                      FROM crevt
                     WHERE creid = nCreId AND TMFFONCTION = 'EVD_VENTE';

                    BEGIN
                        SELECT f_plitroutstanding (itrid,
                                                   iruordre,
                                                   0,
                                                   SYSDATE)
                          INTO l_EncoursHT
                          FROM itrrubphase
                         WHERE itrid = nItrid AND creid = nCreId;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            l_EncoursHT := 0;
                    END;

                    IF (l_Fac_MtHT < l_EncoursHT)
                    THEN
                        nok := 0;                                        -- KO
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF (scontrole = 'GBUDG')
            THEN             -- AZN 26042013 GESTION BUGDET SUR TRANCHE D'IMMO
                -- nOk:=0 ==> Le retour du CTRL est KO
                -- nOk:=1 ==> Le retour du CTRL est OK.


                DECLARE
                    l_cc    NUMBER;
                    l_sum   NUMBER;
                BEGIN
                    SELECT SUM (IRUMTORIGINE)
                      INTO l_sum
                      FROM ITRRUBRIQUE
                     WHERE     ITRID = nItrId
                           AND IRUORDRE IN (SELECT IRUORDRE
                                              FROM LKDOSRUBITRRUB
                                             WHERE ITRID = nItrId);

                    SELECT CVANUMERICVALUE
                      INTO l_cc
                      FROM CCHVALUE
                     WHERE itrid = nItrId AND CCHSID = 'TFDCCHSID978';

                    IF (l_sum > l_cc)
                    THEN
                        nOk := 0;                                    -- ==> KO
                    ELSE
                        nOk := 1;                                    -- ==> OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOk := 0;                           -- Erreur donc KO.
                END;
            ELSIF (SCONTROLE = 'TFTAX')
            THEN
                NOK := OCITRSPECITALIE (NITRID, SCONTROLE, NCREID);
            END IF;

            RETURN NOK;
        END;
    END OCTRANCHE;

    FUNCTION OCDEPGENERALITES (NDEPID      IN DEPENSE.DEPID%TYPE,
                               SCONTROLE   IN VARCHAR2,
                               SUTICODE    IN UTILISATEUR.UTICODE%TYPE,
                               NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK                   NUMBER := 1;
            NIMMO                 NUMBER;
            SDOSTYPEFINANCEMENT   DOSSIER.DOSTYPEFINANCEMENT%TYPE;
            SSTATISTIQUE          CCHVALUE.CVASTRINGVALUE%TYPE;
            DTDATEINIACHAT        DATE;
            NMOIS                 NUMBER;
            NFILTRE               NUMBER;
            NDEPMTTTC             NUMBER;
            STMFFONCTION          CREVT.TMFFONCTION%TYPE;
            NCOUNT                NUMBER;
            NDEPIDORIGINE         DEPENSE.DEPID%TYPE;
            SUSER                 UTILISATEUR.UTICODE%TYPE := SUTICODE;

            CURSOR CDEPRUB
            IS
                SELECT DOSID,
                       IRUORDRE,
                       TACCODE,
                       RUBID,
                       DEPDTCOMPTA,
                       ITRID
                  FROM DEPENSE DEP, DEPLIGNE DLI
                 WHERE DEP.DEPID = NDEPID AND DLI.DEPID = DEP.DEPID;
        BEGIN
            IF (SCONTROLE = 'DTBAC')
            THEN
                FOR CDEPRUB_REC IN CDEPRUB
                LOOP
                    IF CDEPRUB_REC.TACCODE NOT IN ('CBM', 'PROPRE', 'LOCFIN')
                    THEN
                        IF CDEPRUB_REC.IRUORDRE IS NOT NULL
                        THEN
                            NIMMO := 1;
                        END IF;
                    ELSE
                        IF (CDEPRUB_REC.IRUORDRE IS NOT NULL)
                        THEN
                            PA_SELECTIMMOTRANCHE2.P_ISFILTRESURRUBRIQUE (
                                CDEPRUB_REC.RUBID,
                                'DEPMOB',
                                NFILTRE);

                            IF NFILTRE = 1
                            THEN
                                NIMMO := 1;
                            END IF;
                        END IF;
                    END IF;

                    IF NIMMO = 1
                    THEN
                        IF CDEPRUB_REC.DOSID IS NOT NULL
                        THEN
                            SELECT DOSTYPEFINANCEMENT
                              INTO SDOSTYPEFINANCEMENT
                              FROM DOSSIER
                             WHERE DOSID = CDEPRUB_REC.DOSID;

                            IF     (SDOSTYPEFINANCEMENT IS NOT NULL)
                               AND (SDOSTYPEFINANCEMENT = 'LEASEBA')
                            THEN
                                BEGIN
                                    SELECT IRSLIBRE
                                      INTO SSTATISTIQUE
                                      FROM ITRRUBSTATISTIQUE
                                     WHERE     ITRID = CDEPRUB_REC.ITRID
                                           AND IRUORDRE =
                                               CDEPRUB_REC.IRUORDRE
                                           AND SGECODE = 'DATEINIACHAT';
                                EXCEPTION
                                    WHEN OTHERS
                                    THEN
                                        SSTATISTIQUE := NULL;
                                END;

                                IF (SSTATISTIQUE IS NOT NULL)
                                THEN
                                    BEGIN
                                        DTDATEINIACHAT :=
                                            TO_CHAR (TO_DATE (SSTATISTIQUE),
                                                     'DD/MM/YYYY');
                                    EXCEPTION
                                        WHEN OTHERS
                                        THEN
                                            DTDATEINIACHAT := NULL;
                                    END;

                                    IF     (DTDATEINIACHAT IS NOT NULL)
                                       AND (CDEPRUB_REC.DEPDTCOMPTA
                                                IS NOT NULL)
                                    THEN
                                        SELECT MONTHS_BETWEEN (
                                                   CDEPRUB_REC.DEPDTCOMPTA,
                                                   DTDATEINIACHAT)
                                          INTO NMOIS
                                          FROM DUAL;

                                        IF (NMOIS > 6)
                                        THEN
                                            NOK := 0;
                                            EXIT;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            ELSIF SCONTROLE = 'AVRE' AND NCREID IS NOT NULL
            THEN
                SELECT DEPIDORIGINE
                  INTO NDEPIDORIGINE
                  FROM DEPENSE
                 WHERE DEPID = NDEPID;

                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM DEPENSE DEP
                 WHERE     DEP.DEPID = NDEPIDORIGINE
                       AND EXISTS
                               (SELECT 1
                                  FROM DEPLIGNE
                                 WHERE     DEPID = DEP.DEPID
                                       AND F_ISRUBIDONFILTRE (RUBID,
                                                              'REVFACT') =
                                           1)
                       AND DEP.DEPTTFID IS NOT NULL
                       AND DEP.DEPTTFID IN (SELECT TTFID
                                              FROM TTFACTURATION
                                             WHERE TTFTYPEFAC = 'REVERS');

                IF NCOUNT = 1
                THEN
                    NOK := 0;
                END IF;
            -- CV-15122016 NLSCUAT-3324
            --ELSIF SCONTROLE = 'BAPD' AND NCREID IS NOT NULL THEN
            --    DBMS_OUTPUT.PUT_LINE('BAPD') ;
            --    IF F_GETDROITBAPT(NDEPID, SUSER) = 0 THEN
            --        NOK                         := 0;
            --    END IF;
            ELSIF SCONTROLE = 'PLAFO' AND NCREID IS NOT NULL
            THEN
                SELECT TMFFONCTION
                  INTO STMFFONCTION
                  FROM CREVT
                 WHERE CREID = NCREID;

                IF    STMFFONCTION = 'EVDEP_BAPTRESO'
                   OR STMFFONCTION = 'EVDEP_BAPTECH'
                THEN
                    SELECT DEPMTTTC
                      INTO NDEPMTTTC
                      FROM DEPENSE
                     WHERE DEPID = NDEPID;

                    IF SUSER = 'ORFI' AND NDEPMTTTC > 2
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF (sControle = 'ANNDP')
            THEN
                DECLARE
                    nUser        crevt.uticodecreat%TYPE;
                    nCreatUser   depphase.uticode%TYPE;
                    nCreid1      CREVT.CREID%TYPE;
                BEGIN
                    NOK := 1;

                    SELECT MAX (CREID)
                      INTO nCreid1
                      FROM CREVT
                     WHERE TMFFONCTION = 'EVDEP_ANNUL' AND DEPID = nDepid;

                    SELECT UTICODECREAT
                      INTO nUser
                      FROM CREVT
                     WHERE CREID = nCreid1;

                    ---check if the same user

                    SELECT UTICODE
                      INTO nCreatUser
                      FROM DEPPHASE
                     WHERE DEPID = nDepid AND PHACODE = 'INI';

                    IF (nCreatUser <> nUser)
                    THEN
                        NOK := 0;
                    END IF;
                END;
            ELSIF (sControle = 'DPTCH')
            THEN
                DECLARE
                    nUser     crevt.uticodecreat%TYPE;
                    nLimit    utipreference.UPRNUMERICVALUE%TYPE;
                    nAmount   DEPENSE.DEPMTTTC%TYPE;
                    nCreid1   CREVT.CREID%TYPE;
                BEGIN
                    NOK := 1;

                    SELECT MAX (CREID)
                      INTO nCreid1
                      FROM CREVT
                     WHERE TMFFONCTION = 'EVDEP_BAPTECH' AND DEPID = nDepid;

                    SELECT UTICODECREAT
                      INTO nUser
                      FROM CREVT
                     WHERE CREID = nCreid1;


                    ---check the limit of the user

                    SELECT UPRNUMERICVALUE
                      INTO nLimit
                      FROM UTIPREFERENCE
                     WHERE UTICODE = nUser AND UPRCODE = 'LIMITE_TECH';

                    SELECT DEPMTTTC
                      INTO nAmount
                      FROM DEPENSE
                     WHERE DEPID = nDepid;


                    IF (nAmount > nLimit)
                    THEN
                        NOK := 0;
                    END IF;
                END;
            ELSIF (sControle = 'DPBTR')
            THEN
                DECLARE
                    nUser     crevt.uticodecreat%TYPE;
                    nLimit    utipreference.UPRNUMERICVALUE%TYPE;
                    nAmount   DEPENSE.DEPMTTTC%TYPE;
                    nCreid1   CREVT.CREID%TYPE;
                BEGIN
                    NOK := 1;

                    SELECT MAX (CREID)
                      INTO nCreid1
                      FROM CREVT
                     WHERE TMFFONCTION = 'EVDEP_BAPTRESO' AND DEPID = nDepid;

                    SELECT UTICODECREAT
                      INTO nUser
                      FROM CREVT
                     WHERE CREID = nCreid1;


                    ---check the limit of the user

                    SELECT UPRNUMERICVALUE
                      INTO nLimit
                      FROM UTIPREFERENCE
                     WHERE UTICODE = nUser AND UPRCODE = 'LIMITE_TRESO';

                    SELECT DEPMTTTC
                      INTO nAmount
                      FROM DEPENSE
                     WHERE DEPID = nDepid;


                    IF (nAmount > nLimit)
                    THEN
                        NOK := 0;
                    END IF;
                END;
            END IF;

            RETURN NOK;
        END;
    END OCDEPGENERALITES;

    FUNCTION OCDEPSPECITALIE (NDEPID      IN DEPENSE.DEPID%TYPE,
                              SCONTROLE   IN VARCHAR2,
                              NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;

            CURSOR CTFTAX
            IS
                SELECT DISTINCT DOS.DOSTYPEFINANCEMENT,
                                DECODE (RAC.RACACCES, NULL, 0, 1) LIGNEIMMO,
                                DLI.TAXCODE,
                                DLI.DEPID
                  FROM DEPLIGNE DLI, DOSSIER DOS, RUBACCES RAC
                 WHERE     DLI.DEPID = NDEPID
                       AND DLI.TAXCODE IS NOT NULL
                       AND DOS.DOSID = DLI.DOSID
                       AND DOS.DOSTYPEFINANCEMENT IN ('LSBACK', 'ACHAT')
                       AND RAC.RUBID(+) = DLI.RUBID
                       AND RAC.RACACCES(+) = 'DEPMOB';
        BEGIN
            IF (SCONTROLE = 'TFTAX')
            THEN
                FOR CTFTAX_REC IN CTFTAX
                LOOP
                    IF F_ISDEPIMMOTYPEFINANTAXE (
                           CTFTAX_REC.DOSTYPEFINANCEMENT,
                           CTFTAX_REC.LIGNEIMMO,
                           CTFTAX_REC.TAXCODE,
                           CTFTAX_REC.DEPID) !=
                       1
                    THEN
                        NOK := 0;
                        EXIT;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCDEPSPECITALIE;

    FUNCTION OCDEPENSE (NDEPID      IN DEPENSE.DEPID%TYPE,
                        SCONTROLE   IN VARCHAR2,
                        SUTICODE    IN UTILISATEUR.UTICODE%TYPE,
                        NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;
        BEGIN
            IF SCONTROLE IN ('DTBAC',
                             'PLAFO',
                             'AVRE',
                             'BAPD',
                             'ANNDP',
                             'DPTCH',
                             'DPBTR')
            THEN
                NOK :=
                    OCDEPGENERALITES (NDEPID,
                                      SCONTROLE,
                                      SUTICODE,
                                      NCREID);
            ELSIF (SCONTROLE = 'TFTAX')
            THEN
                NOK := OCDEPSPECITALIE (NDEPID, SCONTROLE, NCREID);
            --LMI JIRA 8508 Income Withholding Tax only applicable over 33333 W
            ELSIF SCONTROLE = 'WITHO'
            THEN
                DECLARE
                    CURSOR C1
                    IS
                        SELECT DLIMTHT
                          FROM DEPLIGNE
                         WHERE TAXCODE = 'INC' AND DEPID = NDEPID;
                BEGIN
                    FOR C1R IN C1
                    LOOP
                        IF C1R.DLIMTHT <= 33333
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END LOOP;
                END;
            -- LMI JIRA 8508 Income Withholding Tax only applicable over 33333 W
            ---- MAH - AIL : Debut controle sur L'EXISTANCE D'AU MOINS UNE FACTURE AVEC LA MEME REFERENCE EXTERNE QUE CELLE DE LA DEPENSE
            ELSIF SCONTROLE IN ('CDTDP')
            THEN
                DECLARE
                    nRefExterne   DEPENSE.DEPREFEXTERNE%TYPE := NULL;
                    nCount1       NUMBER := NULL;
                    nCount2       NUMBER := NULL;
                BEGIN
                    -- S??lectionner la r??f??rence externe de la DEPENSE
                    SELECT DEPREFEXTERNE
                      INTO nRefExterne
                      FROM DEPENSE
                     WHERE DEPID = nDepId;

                    SELECT COUNT (*)
                      INTO nCount1
                      FROM DEPLIGNE
                     WHERE DEPID = nDepId AND RUBID NOT IN (180, 495, 466);

                    IF nCount1 > 0
                    THEN
                        -- V??rifier s'il y a des factures avec la m??me r??f??rence externe que la d??pense
                        SELECT COUNT (*)
                          INTO nCount2
                          FROM FACREFERENCE
                         WHERE FREREFCLI = nRefExterne;

                        IF nCount2 = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur L'EXISTANCE D'AU MOINS UNE FACTURE AVEC LA MEME REFERENCE EXTERNE QUE CELLE DE LA DEPENSE
            ---- MAH - AIL : Debut controle EXISTANCE DROIT DE TIMBRE SUR CHAQUE DEPENSE DE TYPE FACTURE
            ELSIF SCONTROLE IN ('DRTIM')
            THEN
                DECLARE
                    CURSOR curRubLignes
                    IS
                        SELECT RUBID
                          FROM DEPLIGNE
                         WHERE DEPID = nDepId;

                    nImputationAna       RUBRIQUE.RUBCODE%TYPE := NULL;
                    nDepType             DEPENSE.DEPTYPEPIECE%TYPE := NULL;
                    nExistanceDroitTim   BOOLEAN := FALSE;
                BEGIN
                    SELECT DEPTYPEPIECE
                      INTO nDepType
                      FROM DEPENSE
                     WHERE DEPID = nDepId;

                    IF nDepType = 'FACT'
                    THEN
                        FOR cRub IN curRubLignes
                        LOOP
                            IF cRub.RUBID = 466
                            THEN
                                nExistanceDroitTim := TRUE;
                            END IF;
                        END LOOP;

                        IF nExistanceDroitTim = FALSE
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle EXISTANCE DROIT DE TIMBRE SUR CHAQUE DEPENSE DE TYPE FACTURE
            ---- RSM - AIL : controle sur approval line du processus de validation de la d?pense
            ELSIF SCONTROLE IN ('ALDEP')
            THEN
                DECLARE
                    nPHACODE   DEPPHASE.PHACODE%TYPE := NULL;
                BEGIN
                    SELECT PHACODE
                      INTO nPHACODE
                      FROM DEPPHASE
                     WHERE DEPID = nDepId;

                    IF nPHACODE = 'VALIDE'
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- RSM - AIL : controle sur approval line du processus de validation de la d?pense

            ---- MAH - AIL : Debut controle sur la CHECKLIST "Avant Confirmation de la d?pense" (Sur EVDEP_BAPTECH)



            ELSIF SCONTROLE IN ('CHKDP')
            THEN
                DECLARE
                    CURSOR CurDossiersDep
                    IS
                        SELECT DISTINCT DOSID
                          FROM DEPLIGNE
                         WHERE DEPID = nDepId;

                    nUncheckedItems   NUMBER := NULL;
                BEGIN
                    FOR everyDosId IN CurDossiersDep
                    LOOP
                        SELECT COUNT (*)
                          INTO nUncheckedItems
                          FROM ADMSTATUS
                         WHERE     ADMID IN
                                       (SELECT ADMID
                                          FROM ADMINISTRATIF
                                         WHERE     DOSID = everyDosId.DOSID
                                               AND ADMIDPARENT IN
                                                       (SELECT ADMID
                                                          FROM ADMINISTRATIF
                                                         WHERE     DOSID =
                                                                   everyDosId.DOSID
                                                               AND FORID IN
                                                                       ('5447',
                                                                        '5461')))
                               AND ASTSTATUS IN ('EC', 'IN')
                               AND ASTDTEND IS NULL;

                        IF nUncheckedItems >= 1
                        THEN
                            NOK := 0;
                        END IF;
                    END LOOP;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur la CHECKLIST "Avant Confirmation de la d?pense" (Sur EVDEP_BAPTECH)
            END IF;

            RETURN NOK;
        END;
    END OCDEPENSE;

    FUNCTION OCREGGENERALITES (NREGID      IN REGLEMENT.REGID%TYPE,
                               SCONTROLE   IN VARCHAR2,
                               SUTICODE    IN UTILISATEUR.UTICODE%TYPE,
                               STUGECODE   IN UTILISATEUR.UGECODE%TYPE,
                               NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK             NUMBER := 1;
            NIMMO           NUMBER;
            SREGTYPEMVT     REGLEMENT.REGTYPEMVT%TYPE;
            NSUMRIMMT       NUMBER := 0;
            SACTLIBCOURT    ACTEUR.ACTLIBCOURT%TYPE;
            NCOUNT          NUMBER;
            NACTIDGESTION   DOSSIER.ACTID%TYPE;
            -- CV-26112013 CASNT-8436
            NFACID          FACTURE.FACID%TYPE;
            NDOSID          DOSSIER.DOSID%TYPE;
            NDRUORDRE       DOSRUBRIQUE.DRUORDRE%TYPE;
            NDREORDRE       DOSRUBECHEANCIER.DREORDRE%TYPE;
            NBOOLEAN        BOOLEAN;
            NCALID          NUMBER := 8;
            NREGDTREGLT     REGLEMENT.REGDTREGLT%TYPE;
            NCOUNT1         NUMBER := 0;
            NPTPTYPE1       NUMBER := NULL;
            NPTPTYPE2       NUMBER := NULL;
            NPTPTYPE3       NUMBER := NULL;
            NPTPTYPE4       NUMBER := NULL;
            SUSER           UTILISATEUR.UTICODE%TYPE := SUTICODE;
            SUGECODE        UTILISATEUR.UGECODE%TYPE := STUGECODE;

            CURSOR CREGIMPUTATION
            IS
                SELECT DISTINCT RIM.RIMLETTRAGE
                  FROM REGLEMENT REG, REGIMPUTATION RIM, RUBACCES RAC
                 WHERE     REG.REGID = NREGID
                       AND REG.REGTYPEMVT = 'D'
                       AND RIM.REGID = REG.REGID
                       AND RIM.RUBID IS NOT NULL
                       AND RAC.RUBID = RIM.RUBID
                       AND RAC.RACACCES = 'ATTENTE'
                       AND RAC.RACACCES != 'ODP';

            -- CV-26112013 CASNT-8436
            CURSOR C1
            IS
                SELECT DISTINCT FACID
                  FROM REGIMPUTATION
                 WHERE REGID = NREGID AND FACID IS NOT NULL;

            CURSOR C2
            IS
                SELECT DOSID, DRUORDRE, DREORDRE
                  FROM DOSRUBECHEANCIER
                 WHERE FACID = NFACID;

            CURSOR C3
            IS
                  SELECT DREORDRE, FACID
                    FROM DOSRUBECHEANCIER
                   WHERE     DOSID = NDOSID
                         AND DRUORDRE = NDRUORDRE
                         AND DREORDRE > NDREORDRE
                         AND DRETYPE = 'LOYER'
                ORDER BY 1;

            CURSOR C4
            IS
                  SELECT DREORDRE, FACID
                    FROM DOSRUBECHEANCIER
                   WHERE     DOSID = NDOSID
                         AND DRUORDRE = NDRUORDRE
                         AND DREORDRE < NDREORDRE
                         AND DRETYPE = 'LOYER'
                ORDER BY 1 DESC;

            CURSOR C5
            IS
                SELECT RUBID
                  FROM FACLIGNE
                 WHERE FACID = NFACID;
        BEGIN
            BEGIN
                SELECT ACTCODE
                  INTO SACTLIBCOURT
                  FROM ACTEUR
                 WHERE ACTID IN (SELECT ACTID
                                   FROM REGLEMENT
                                  WHERE REGID = NREGID);
            EXCEPTION
                WHEN OTHERS
                THEN
                    SACTLIBCOURT := NULL;
            END;

            -- CV-26112013 CASNT-8436
            BEGIN
                SELECT REGTYPEMVT
                  INTO SREGTYPEMVT
                  FROM REGLEMENT
                 WHERE REGID = NREGID;
            EXCEPTION
                WHEN OTHERS
                THEN
                    SREGTYPEMVT := NULL;
            END;

            IF (SCONTROLE = 'ARCA') AND SACTLIBCOURT IS NOT NULL
            THEN
                SELECT ACTIDGESTION
                  INTO NACTIDGESTION
                  FROM REGLEMENT
                 WHERE REGID = NREGID;

                NCOUNT := 0;

                IF NACTIDGESTION IS NOT NULL
                THEN
                    SELECT COUNT (1)
                      INTO NCOUNT
                      FROM TACACTGESTION
                     WHERE ACTID = NACTIDGESTION AND TACCODE = 'CBI';
                END IF;

                IF NCOUNT = 0
                THEN
                    FOR CREGIMPUTATION_REC IN CREGIMPUTATION
                    LOOP
                        IF     SUBSTR (SACTLIBCOURT, 1, 4) != 'ARCA'
                           AND CREGIMPUTATION_REC.RIMLETTRAGE IS NULL
                        THEN
                            NOK := 0;
                        END IF;
                    END LOOP;
                END IF;
            ELSIF (SCONTROLE = 'BKDTE')
            THEN
                SELECT REGDTREGLT
                  INTO NREGDTREGLT
                  FROM REGLEMENT
                 WHERE REGID = NREGID;

                NBOOLEAN := PA_ORFIDATE.ODATEISFERIEX (NREGDTREGLT, 8);

                IF NBOOLEAN
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'R03')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGLEMENT REG, REGIMPUTATION RIMP, UTITSM UTI
                 WHERE     REG.REGID = RIMP.REGID
                       AND RIMP.REGID = NREGID
                       AND REG.REGTYPEMVT = 'O'
                       AND RIMP.DOSID IN (SELECT DOSID
                                            FROM DOSSIER
                                           WHERE TPGCODE IN ('A64232',
                                                             'A64234',
                                                             'A64612',
                                                             'A64712'))
                       AND UTI.TSMSECTGESTION NOT IN ('AN3')
                       AND REG.UTICODE = UTI.UTICODE;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ------29/07/2014 start ISF Control defect --DF_FN_SV010211_0010_002---
            ELSIF (SCONTROLE = 'R21')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM TTTRESORERIE
                 WHERE     TTTTMPCODE = 'PRLAUTO'
                       AND TTTACTIDGESTION = (SELECT ACTIDGESTION
                                                FROM REGLEMENT
                                               WHERE REGID = NREGID)
                       AND TTTRIBID IN
                               (SELECT RIBID
                                  FROM REGLEMENT
                                 WHERE     REGPTPTYPE = 'YES'
                                       AND TMPCODE = 'PRLAUTO'
                                       AND REGID = NREGID)
                       AND NVL (TTTCOUNTADJ, 0) != 0
                       AND NVL (TTTMTOTALADJ, 0) != 0
                       AND TTTDTFINECH = SYSDATE;

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            ------29/07/2014 end ISF Control defect --DF_FN_SV010211_0010_002---
            ELSIF (SCONTROLE = 'R04')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI, FACLIGNE FLI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.FACID = LKI.FACID
                       AND LKI.FACID = FLI.FACID
                       AND LKI.FLIORDRE = FLI.FLIORDRE
                       AND FLI.FLIDRUCLASSE = 'F'
                       AND FLI.FLIDRUTYPE = 'F'
                       AND RIM.REGID = NREGID
                       AND NVL (RFLMTAMORT, 0) != 0
                       AND EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE FRE, DOSSIER DOS
                                 WHERE     DOS.DOSID = FRE.FREDOSID
                                       AND FRE.FACID = RIM.FACID
                                       AND (   DOS.TPGCODE LIKE '493%'
                                            OR DOS.TPGCODE LIKE 'S494%'
                                            OR DOS.TPGCODE LIKE 'L495%'
                                            OR DOS.TPGCODE LIKE 'S495%'
                                            OR DOS.TPGCODE LIKE 'L499%'
                                            OR DOS.TPGCODE LIKE 'S499%'));

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'R05')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI, FACLIGNE FLI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.FACID = LKI.FACID
                       AND LKI.FACID = FLI.FACID
                       AND LKI.FLIORDRE = FLI.FLIORDRE
                       AND FLI.FLIDRUCLASSE = 'F'
                       AND FLI.FLIDRUTYPE = 'F'
                       AND RIM.REGID = NREGID
                       AND NVL (RFLMTAMORT, 0) != 0
                       AND EXISTS
                               (SELECT 1
                                  FROM REGLEMENT REG, UTITSM UTS
                                 WHERE     REG.REGID = RIM.REGID
                                       AND REG.UTICODE = UTS.UTICODE
                                       AND UTS.TSMSECTGESTION NOT IN ('AN3',
                                                                      'AQ3',
                                                                      'AN2',
                                                                      'AE4'));

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'R06')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.FACID = LKI.FACID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.REGID = NREGID
                       AND EXISTS
                               (SELECT 1
                                  FROM RUBRIQUE RUB, FACLIGNE FLI
                                 WHERE     RUB.RUBID = FLI.RUBID
                                       AND FLI.FACID = LKI.FACID
                                       AND FLI.FLIORDRE = LKI.FLIORDRE
                                       AND RUB.RUBCODE IN ('LFJUDSC',
                                                           'LFOPEND',
                                                           'NOTAFEE',
                                                           'LFACTEX')
                                       AND RUB.UGECODE = SUGECODE)
                       AND EXISTS
                               (SELECT 1
                                  FROM REGLEMENT REG, UTITSM UTS
                                 WHERE     REG.REGID = RIM.REGID
                                       AND REG.UTICODE = UTS.UTICODE
                                       AND UTS.TSMSECTGESTION NOT IN ('AN3',
                                                                      'AQ3',
                                                                      'AN2',
                                                                      'AE4'));

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'R07')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI, FACLIGNE FLI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.FACID = LKI.FACID
                       AND LKI.FACID = FLI.FACID
                       AND LKI.FLIORDRE = FLI.FLIORDRE
                       AND FLI.FLIDRUCLASSE = 'F'
                       AND FLI.FLIDRUTYPE = 'F'
                       AND RIM.REGID = NREGID
                       AND NVL (RFLMTAMORT, 0) != 0
                       AND EXISTS
                               (SELECT *
                                  FROM FACREFERENCE FRE, DOSSIER DOS
                                 WHERE     DOS.DOSID = FRE.FREDOSID
                                       AND FRE.FACID = RIM.FACID
                                       AND (   DOS.TPGCODE LIKE 'L%'
                                            OR DOS.TPGCODE LIKE 'S%'
                                            OR DOS.TPGCODE LIKE 'T%'));

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            ELSIF (SCONTROLE = 'R08')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGLEMENT
                 WHERE     REGID = NREGID
                       AND REGDTREGLT < TRUNC (SYSDATE)
                       AND REGPTPTYPE = 'YES';

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            --  end if;
            ELSIF (SCONTROLE = 'R09')
            THEN
                --2. ????????????????????????? : reimputation.rubid??????? PREPAY filter?????? ????????????
                SELECT COUNT (1)
                  INTO NPTPTYPE1
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'PREPAY') = 1;

                -- 0 : ????????????????????????? ???????????????
                -- 1 : ????????????????????????? ??????
                --3. ?????????????? : regimputation.FACID????? FACREFERENCE.FRECOMMENT='Advanced Bills'?????? ????????????
                SELECT COUNT (1)
                  INTO NPTPTYPE2
                  FROM REGIMPUTATION RIM
                 WHERE     REGID = NREGID
                       AND EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE
                                 WHERE     FRECOMMENT = 'ADVANCED BILLS'
                                       AND FACID = RIM.FACID);

                -- 0 : ???????????????? ??"???9??
                -- 1 : ?????????? ??????
                --1. ???????????????????? : ??????????????????????????????? ?????????????????????, ???????????????????? ?????????????????????, FACID??????? ???????????.
                SELECT COUNT (1)
                  INTO NPTPTYPE3
                  FROM REGIMPUTATION RIM
                 WHERE     REGID = NREGID
                       AND FACID IS NOT NULL
                       AND NOT EXISTS
                               (SELECT 1
                                  FROM REGIMPUTATION
                                 WHERE     REGID = RIM.REGID
                                       AND F_ISRUBIDONFILTRE (RUBID,
                                                              'PREPAY') =
                                           1)
                       AND NOT EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE
                                 WHERE     FRECOMMENT = 'ADVANCED BILLS'
                                       AND FACID = RIM.FACID);

                --4. ????????????? : FACID????????????????????,??????????????????????????????? ?????????????????????, ?????????????????????????????????????????, RUBID??????? ???????????.
                SELECT COUNT (1)
                  INTO NPTPTYPE4
                  FROM REGIMPUTATION RIM
                 WHERE     REGID = NREGID
                       AND FACID IS NULL
                       AND RUBID IS NOT NULL
                       AND NOT EXISTS
                               (SELECT 1
                                  FROM REGIMPUTATION
                                 WHERE     REGID = RIM.REGID
                                       AND F_ISRUBIDONFILTRE (RUBID,
                                                              'PREPAY') =
                                           1)
                       AND NOT EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE
                                 WHERE     FRECOMMENT = 'ADVANCED BILLS'
                                       AND FACID = RIM.FACID);

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGLEMENT
                 WHERE     ACTID IN (SELECT ACTID
                                       FROM REGLEMENT
                                      WHERE REGID = NREGID)
                       AND TRUNC (REGDTREGLT) IN
                               (SELECT TRUNC (REGDTREGLT)
                                  FROM REGLEMENT
                                 WHERE REGID = NREGID AND REGPTPTYPE = 'YES') --it should compare PTP vs PTP
                       AND AUNORDRE IN (SELECT AUNORDRE
                                          FROM REGLEMENT
                                         WHERE REGID = NREGID)
                       AND REGPTPTYPE = 'YES';

                IF (NPTPTYPE1 > 0)
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGLEMENT A
                     WHERE     ACTID IN (SELECT ACTID
                                           FROM REGLEMENT
                                          WHERE REGID = NREGID)
                           AND TRUNC (REGDTREGLT) IN
                                   (SELECT TRUNC (REGDTREGLT)
                                      FROM REGLEMENT
                                     WHERE     REGID = NREGID
                                           AND REGPTPTYPE = 'YES') --it should compare PTP vs PTP
                           AND AUNORDRE IN (SELECT AUNORDRE
                                              FROM REGLEMENT
                                             WHERE REGID = NREGID)
                           AND REGPTPTYPE = 'YES'
                           AND EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION
                                     WHERE     REGID = A.REGID
                                           AND F_ISRUBIDONFILTRE (RUBID,
                                                                  'PREPAY') =
                                               1);
                ELSIF (NPTPTYPE2 > 0)
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGLEMENT A
                     WHERE     ACTID IN (SELECT ACTID
                                           FROM REGLEMENT
                                          WHERE REGID = NREGID)
                           AND TRUNC (REGDTREGLT) IN
                                   (SELECT TRUNC (REGDTREGLT)
                                      FROM REGLEMENT
                                     WHERE     REGID = NREGID
                                           AND REGPTPTYPE = 'YES') --it should compare PTP vs PTP
                           AND AUNORDRE IN (SELECT AUNORDRE
                                              FROM REGLEMENT
                                             WHERE REGID = NREGID)
                           AND REGPTPTYPE = 'YES'
                           AND EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION  RIM,
                                           FACREFERENCE   FRE
                                     WHERE     RIM.REGID = A.REGID
                                           AND RIM.FACID = FRE.FACID
                                           AND FRE.FRECOMMENT =
                                               'ADVANCED BILLS');
                ELSIF (NPTPTYPE3 > 0)
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGLEMENT A
                     WHERE     ACTID IN (SELECT ACTID
                                           FROM REGLEMENT
                                          WHERE REGID = NREGID)
                           AND TRUNC (REGDTREGLT) IN
                                   (SELECT TRUNC (REGDTREGLT)
                                      FROM REGLEMENT
                                     WHERE     REGID = NREGID
                                           AND REGPTPTYPE = 'YES') --it should compare PTP vs PTP
                           AND AUNORDRE IN (SELECT AUNORDRE
                                              FROM REGLEMENT
                                             WHERE REGID = NREGID)
                           AND REGPTPTYPE = 'YES'
                           AND EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION
                                     WHERE     REGID = A.REGID
                                           AND FACID IS NOT NULL)
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION
                                     WHERE     REGID = A.REGID
                                           AND F_ISRUBIDONFILTRE (RUBID,
                                                                  'PREPAY') =
                                               1)
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM FACREFERENCE   FRE,
                                           REGIMPUTATION  RIM
                                     WHERE     FRECOMMENT = 'ADVANCED BILLS'
                                           AND FRE.FACID = RIM.FACID
                                           AND RIM.REGID = A.REGID);
                ELSIF (NPTPTYPE4 > 0)
                THEN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGLEMENT A
                     WHERE     ACTID IN (SELECT ACTID
                                           FROM REGLEMENT
                                          WHERE REGID = NREGID)
                           AND TRUNC (REGDTREGLT) IN
                                   (SELECT TRUNC (REGDTREGLT)
                                      FROM REGLEMENT
                                     WHERE     REGID = NREGID
                                           AND REGPTPTYPE = 'YES') --it should compare PTP vs PTP
                           AND AUNORDRE IN (SELECT AUNORDRE
                                              FROM REGLEMENT
                                             WHERE REGID = NREGID)
                           AND REGPTPTYPE = 'YES'
                           AND EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION
                                     WHERE     REGID = A.REGID
                                           AND FACID IS NULL
                                           AND RUBID IS NOT NULL)
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION
                                     WHERE     REGID = A.REGID
                                           AND F_ISRUBIDONFILTRE (RUBID,
                                                                  'PREPAY') =
                                               1)
                           AND NOT EXISTS
                                   (SELECT 1
                                      FROM FACREFERENCE   FRE,
                                           REGIMPUTATION  RIM
                                     WHERE     FRECOMMENT = 'ADVANCED BILLS'
                                           AND FRE.FACID = RIM.FACID
                                           AND RIM.REGID = A.REGID);
                END IF;

                IF (NCOUNT > 1)
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'PERT'
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM UTILISATEUR
                 WHERE     UGECODE = SUGECODE
                       AND UTICODE = SUSER
                       AND GROCODE LIKE 'COMP%';

                IF NCOUNT = 0
                THEN
                    SELECT SUM (NVL (RIMMT, 0))
                      INTO NSUMRIMMT
                      FROM REGIMPUTATION RIM, RUBACCES RAC
                     WHERE     RIM.REGID = NREGID
                           AND RIM.RUBID IS NOT NULL
                           AND RAC.RUBID = RIM.RUBID
                           AND RAC.RACACCES = 'ODP';

                    IF NVL (NSUMRIMMT, 0) > 10
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            --Interest exemption
            ELSIF SCONTROLE = 'R01'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI, FACLIGNE FLI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.FACID = LKI.FACID
                       AND LKI.FACID = FLI.FACID
                       AND LKI.FLIORDRE = FLI.FLIORDRE
                       AND FLI.FLIDRUCLASSE = 'F'
                       AND FLI.FLIDRUTYPE = 'F'
                       AND RIM.REGID = NREGID
                       AND (NVL (RFLMTHTBASE, 0) - NVL (RFLMTAMORT, 0)) != 0
                       AND EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE FRE, DOSSIER DOS
                                 WHERE     DOS.DOSID = FRE.FREDOSID
                                       AND DOS.TPGCODE LIKE 'A%'
                                       AND FRE.FACID = RIM.FACID);

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            --Principal exemption per product
            ELSIF SCONTROLE = 'R02'
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT1
                  FROM REGIMPUTATION RIM, LKRIMFLI LKI, FACLIGNE FLI
                 WHERE     RIM.REGID = LKI.REGID
                       AND RIM.RIMORDRE = LKI.RIMORDRE
                       AND RIM.FACID = LKI.FACID
                       AND LKI.FACID = FLI.FACID
                       AND LKI.FLIORDRE = FLI.FLIORDRE
                       AND FLI.FLIDRUCLASSE = 'F'
                       AND FLI.FLIDRUTYPE = 'F'
                       AND RIM.REGID = NREGID
                       AND (NVL (RFLMTHTBASE, 0) - NVL (RFLMTAMORT, 0)) != 0
                       AND EXISTS
                               (SELECT 1
                                  FROM FACREFERENCE FRE, DOSSIER DOS
                                 WHERE     DOS.DOSID = FRE.FREDOSID
                                       AND DOS.TPGCODE LIKE 'U%'
                                       AND FRE.FACID = RIM.FACID);

                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGIMPUTATION
                 WHERE     REGID = NREGID
                       AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                THEN
                    NOK := 0;
                END IF;
            -- CV-26112013 CASNT-8436
            ELSIF SCONTROLE = 'OLFA' AND SREGTYPEMVT = 'O'
            THEN
                FOR C1R IN C1
                LOOP
                    -- Invoice totally paid
                    IF F_PLRESTANTFACTURE (C1R.FACID, NULL) = 0
                    THEN
                        NFACID := C1R.FACID;

                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM DOSRUBECHEANCIER
                         WHERE FACID = NFACID;

                        -- Invoice associated to a schedule
                        IF NCOUNT > 0
                        THEN
                            FOR C2R IN C2
                            LOOP
                                NDOSID := C2R.DOSID;
                                NDRUORDRE := C2R.DRUORDRE;
                                NDREORDRE := C2R.DREORDRE;

                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM FACTURE FAC
                                 WHERE     F_PLRESTANTFACTURE (FACID, NULL) !=
                                           0
                                       AND EXISTS
                                               (SELECT 1
                                                  FROM DOSRUBECHEANCIER
                                                 WHERE     DOSID = NDOSID
                                                       AND DRUORDRE =
                                                           NDRUORDRE
                                                       AND DREORDRE <
                                                           NDREORDRE
                                                       AND FACID = FAC.FACID);

                                -- Previous invoices have to be totally paid
                                IF NCOUNT > 0
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;

                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM DOSRUBECHEANCIER
                                 WHERE     DOSID = NDOSID
                                       AND DRUORDRE = NDRUORDRE
                                       AND DREORDRE > NDREORDRE
                                       AND DRETYPE = 'LOYER'
                                       AND FACID IS NOT NULL;

                                IF NCOUNT > 0
                                THEN
                                    FOR C3R IN C3
                                    LOOP
                                        IF C3R.FACID IS NOT NULL
                                        THEN
                                            IF F_PLRESTANTFACTURE (C3R.FACID,
                                                                   NULL) !=
                                               0
                                            THEN
                                                -- No following invoice can be totally paid
                                                SELECT COUNT (*)
                                                  INTO NCOUNT
                                                  FROM FACTURE FAC
                                                 WHERE     F_PLRESTANTFACTURE (
                                                               FACID,
                                                               NULL) =
                                                           0
                                                       AND EXISTS
                                                               (SELECT 1
                                                                  FROM DOSRUBECHEANCIER
                                                                 WHERE     DOSID =
                                                                           NDOSID
                                                                       AND DRUORDRE =
                                                                           NDRUORDRE
                                                                       AND DREORDRE >
                                                                           C3R.DREORDRE
                                                                       AND FACID =
                                                                           FAC.FACID);

                                                IF NCOUNT > 0
                                                THEN
                                                    NOK := 0;
                                                    EXIT;
                                                END IF;
                                            END IF;
                                        END IF;
                                    END LOOP;
                                END IF;
                            END LOOP;
                        END IF;
                    -- Invoice not totally paid
                    ELSE
                        NFACID := C1R.FACID;

                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM DOSRUBECHEANCIER
                         WHERE FACID = NFACID;

                        -- Invoice associated to a schedule
                        IF NCOUNT > 0
                        THEN
                            FOR C2R IN C2
                            LOOP
                                NDOSID := C2R.DOSID;
                                NDRUORDRE := C2R.DRUORDRE;
                                NDREORDRE := C2R.DREORDRE;

                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM FACTURE FAC
                                 WHERE     F_PLRESTANTFACTURE (FACID, NULL) =
                                           0
                                       AND EXISTS
                                               (SELECT 1
                                                  FROM DOSRUBECHEANCIER
                                                 WHERE     DOSID = NDOSID
                                                       AND DRUORDRE =
                                                           NDRUORDRE
                                                       AND DREORDRE >
                                                           NDREORDRE
                                                       AND FACID = FAC.FACID);

                                -- Following invoices have to be not totally paid
                                IF NCOUNT > 0
                                THEN
                                    NOK := 0;
                                    EXIT;
                                END IF;

                                SELECT COUNT (*)
                                  INTO NCOUNT
                                  FROM DOSRUBECHEANCIER
                                 WHERE     DOSID = NDOSID
                                       AND DRUORDRE = NDRUORDRE
                                       AND DREORDRE < NDREORDRE
                                       AND DRETYPE = 'LOYER'
                                       AND FACID IS NOT NULL;

                                IF NCOUNT > 0
                                THEN
                                    FOR C4R IN C4
                                    LOOP
                                        IF C4R.FACID IS NOT NULL
                                        THEN
                                            IF F_PLRESTANTFACTURE (C4R.FACID,
                                                                   NULL) =
                                               0
                                            THEN
                                                -- No previous invoice can be not totally paid
                                                SELECT COUNT (*)
                                                  INTO NCOUNT
                                                  FROM FACTURE FAC
                                                 WHERE     F_PLRESTANTFACTURE (
                                                               FACID,
                                                               NULL) !=
                                                           0
                                                       AND EXISTS
                                                               (SELECT 1
                                                                  FROM DOSRUBECHEANCIER
                                                                 WHERE     DOSID =
                                                                           NDOSID
                                                                       AND DRUORDRE =
                                                                           NDRUORDRE
                                                                       AND DREORDRE <
                                                                           C4R.DREORDRE
                                                                       AND FACID =
                                                                           FAC.FACID);

                                                IF NCOUNT > 0
                                                THEN
                                                    NOK := 0;
                                                    EXIT;
                                                END IF;
                                            END IF;
                                        END IF;
                                    END LOOP;
                                END IF;
                            END LOOP;
                        END IF;
                    END IF;
                END LOOP;
            -- payment balance
            ELSIF SCONTROLE = ('REGBL')
            THEN
                BEGIN
                    SELECT DECODE ((SELECT SUM (RIMMT) - REG.REGMT
                                      FROM REGIMPUTATION
                                     WHERE REGID = NREGID),
                                   0, 1,
                                   0)
                      INTO NOK
                      FROM REGLEMENT REG
                     WHERE REG.REGID = NREGID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            -- Clear suspense on diff actor
            ELSIF SCONTROLE = ('R10')
            THEN
                BEGIN
                    DECLARE
                        NREGIDORIGIN   REGLEMENT.REGID%TYPE := NULL;
                        NACTIDORIGIN   ACTEUR.ACTID%TYPE := NULL;
                        NACTIDNEW      ACTEUR.ACTID%TYPE := NULL;
                        NREGREFNEW     REGLEMENT.REGREFERENCE%TYPE := NULL;
                        SUGECODE       UTILISATEUR.UGECODE%TYPE
                                           := F_GETCURRENTUGECODE;
                    BEGIN
                        SELECT ACTID, REGREFERENCE
                          INTO NACTIDNEW, NREGREFNEW
                          FROM REGLEMENT
                         WHERE REGID = NREGID AND REGTYPEMVT = 'O';

                        SELECT MAX (REG.ACTID), MAX (REG.REGID)
                          INTO NACTIDORIGIN, NREGIDORIGIN
                          FROM REGLEMENT REG, REGIMPUTATION RIM
                         WHERE     RIM.REGID = REG.REGID
                               AND RIM.RIMLETTRAGE = NREGREFNEW;

                        IF     NACTIDNEW IS NOT NULL
                           AND NACTIDORIGIN IS NOT NULL
                           AND NACTIDNEW != NACTIDORIGIN
                        THEN
                            NOK := 0;
                        END IF;
                    END;
                END;
            --Refund after 10PM
            ELSIF SCONTROLE = ('R11')
            THEN
                BEGIN
                    SELECT DECODE (COUNT (*), 0, 1, 0)
                      INTO NOK
                      FROM REGLEMENT REG, REGIMPUTATION RIM
                     WHERE     REG.REGID = RIM.REGID
                           AND REG.REGTYPEMVT = 'D'
                           AND RIM.RUBID IN
                                   (SELECT MAX (RUBID)
                                      FROM RUBRIQUE
                                     WHERE     RUBCODE = 'REFUND'
                                           AND UGECODE = SUGECODE)
                           AND REG.REGID = NREGID
                           AND REG.UTICODE NOT IN
                                   (SELECT UTICODE
                                      FROM UTITSM
                                     WHERE TSMSECTGESTION = 'AN3')
                           AND TO_CHAR (SYSDATE, 'HH24:mi') <=
                               TO_CHAR ('22:00', 'HH24:mi');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --Direct debit PTP approval
            ELSIF SCONTROLE = ('R12')
            THEN
                BEGIN
                    SELECT DECODE (COUNT (*), 0, 1, 0)
                      INTO NOK
                      FROM REGLEMENT REG
                     WHERE     REG.TMPCODE = 'PRLAUTO'
                           AND NVL (REG.REGFLAGVALID, NULL) = 0
                           AND REG.REGPTPTYPE = 'YES'
                           AND REG.REGID = NREGID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF SCONTROLE = ('R13')
            THEN
                BEGIN
                    SELECT DECODE (COUNT (*), 0, 1, 0)
                      INTO NOK
                      FROM REGLEMENT REG1, REGLEMENT REG2
                     WHERE     REG1.REGPTPTYPE = 'YES'
                           AND REG2.REGPTPTYPE = 'YES'
                           AND REG1.REGDTREGLT = REG2.REGDTREGLT
                           AND REG1.ACTID = REG2.ACTID
                           AND REG1.TMPCODE = REG2.TMPCODE
                           AND NVL (REG1.AUNORDRE, 0) =
                               NVL (REG2.AUNORDRE, 0)
                           AND REG1.REGID != REG2.REGID
                           AND REG1.REGID = NREGID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF SCONTROLE = ('R14')
            THEN
                BEGIN
                    SELECT COUNT (*)
                      INTO NCOUNT1
                      FROM REGLEMENT REG, COLLECTIONCASE CS
                     WHERE     F_GETDOSSIERREGLEMENTID (REG.REGID) = CS.DOSID
                           AND EXISTS
                                   (SELECT 1
                                      FROM CCAPHASE
                                     WHERE     CCAID = CS.CCAID
                                           AND PHACODE = 'ACTIVE'
                                           AND CPHDTEND IS NULL)
                           AND REG.REGID = NREGID;

                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGIMPUTATION
                     WHERE     REGID = NREGID
                           AND F_ISRUBIDONFILTRE (RUBID, 'ODLOSS') = 1;

                    IF (NCOUNT > 0) AND (NCOUNT1 > 0)
                    THEN
                        NOK := 0;
                    END IF;
                END;
            ELSIF SCONTROLE = 'R15'
            THEN
                BEGIN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM REGIMPUTATION RIM, REGLEMENT REG, FACTURE FAC
                     WHERE     RIM.REGID = REG.REGID
                           AND REG.REGTYPEMVT = 'O'
                           AND RIM.RIMSENS = 'C'
                           AND FAC.FACID = RIM.FACID
                           AND FAC.FACDTEXIGIBLE <= SYSDATE
                           AND RIM.REGID = NREGID
                           AND EXISTS
                                   (SELECT 1
                                      FROM REGIMPUTATION RIM2
                                     WHERE     RIM2.REGID = RIM.REGID
                                           AND RIM2.RIMSENS = 'D'
                                           AND RIM2.FACID IS NOT NULL);

                    IF (NCOUNT > 0)
                    THEN
                        NOK := 0;
                    END IF;
                END;
            ELSIF SCONTROLE = 'R16'
            THEN
                SELECT COUNT (DISTINCT F_REG_PAYMENTDATE (REGID))
                  INTO NCOUNT
                  FROM REGLEMENT
                 WHERE REGREFERENCE IN
                           (SELECT RIM.RIMLETTRAGE
                              FROM REGLEMENT REG, REGIMPUTATION RIM
                             WHERE     REG.REGID = RIM.REGID
                                   AND RIM.RIMSENS = 'D'
                                   AND REG.REGID = NREGID
                                   AND F_ISRUBIDONFILTRE (RIM.RUBID,
                                                          'ATTENTE') =
                                       1);

                IF (NCOUNT > 1)
                THEN
                    NOK := 0;
                END IF;
            --24/07/2014--START -- Target receivable should not be delinqeunt
            ELSIF (SCONTROLE = 'R17')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM REGLEMENT REG, REGIMPUTATION RIM, FACTURE FAC
                 WHERE     FAC.FACDTEXIGIBLE < SYSDATE
                       AND FAC.FACID = RIM.FACID
                       AND RIM.FACID IS NOT NULL
                       AND RIM.RIMSENS = 'C'
                       AND REG.REGTYPEMVT IN ('OL', 'O')
                       AND REG.REGID = RIM.REGID
                       AND REG.REGID = NREGID
                       AND EXISTS
                               (SELECT 1
                                  FROM REGIMPUTATION RIM2
                                 WHERE     RIM2.REGID = NREGID
                                       AND RIM2.RIMSENS = 'D'
                                       AND RIM2.FACID IS NOT NULL);

                IF (NCOUNT > 0)
                THEN
                    NOK := 0;
                END IF;
            --16/07/2014--START -- ISF Control for  Unallocation Priority Late Charge
            ELSIF SCONTROLE = 'R18' AND SREGTYPEMVT = 'O'
            THEN
                FOR C1R IN C1
                LOOP
                    NFACID := C1R.FACID;

                    SELECT FREDOSID
                      INTO NDOSID
                      FROM FACREFERENCE
                     WHERE FACID = NFACID;

                    FOR C5R IN C5
                    LOOP
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM FACTURE F, FACECHEANCE FEC
                         WHERE     EXISTS
                                       (SELECT 1
                                          FROM FACREFERENCE FR
                                         WHERE     FREDOSID = NDOSID
                                               AND FR.FACID = F.FACID)
                               AND NOT EXISTS
                                       (SELECT 1
                                          FROM REGIMPUTATION R
                                         WHERE     R.REGID = NREGID
                                               AND R.FACID IS NOT NULL
                                               AND R.FACID = F.FACID)
                               AND F_PLRESTANTFACTURECOMPTA (F.FACID,
                                                             FEC.FECORDRE,
                                                             0) =
                                   0
                               AND FEC.FACID = F.FACID
                               AND (   (    EXISTS
                                                (SELECT FACID
                                                   FROM FACLIGNE FL
                                                  WHERE     RUBID NOT IN
                                                                (SELECT RUBID
                                                                   FROM ACTALLOCATIONPRIORITY
                                                                        A
                                                                  WHERE A.AAPORDRE <=
                                                                        (SELECT AA.AAPORDRE
                                                                           FROM ACTALLOCATIONPRIORITY
                                                                                AA
                                                                          WHERE     AA.RUBID =
                                                                                    C5R.RUBID
                                                                                AND AA.AAPGRPPRIORLEVEL =
                                                                                    A.AAPGRPPRIORLEVEL))
                                                        AND FL.FACID =
                                                            F.FACID)
                                        AND F.FACDTEXIGIBLE =
                                            (SELECT FACDTEXIGIBLE
                                               FROM FACTURE
                                              WHERE FACID = NFACID))
                                    OR (    EXISTS
                                                (SELECT FACID
                                                   FROM FACLIGNE FL
                                                  WHERE     RUBID NOT IN
                                                                (SELECT RUBID
                                                                   FROM ACTALLOCATIONPRIORITY
                                                                        A
                                                                  WHERE A.AAPORDRE <
                                                                        (SELECT AA.AAPORDRE
                                                                           FROM ACTALLOCATIONPRIORITY
                                                                                AA
                                                                          WHERE     AA.RUBID =
                                                                                    C5R.RUBID
                                                                                AND AA.AAPGRPPRIORLEVEL =
                                                                                    A.AAPGRPPRIORLEVEL))
                                                        AND FL.FACID =
                                                            F.FACID)
                                        AND F.FACDTEXIGIBLE >
                                            (SELECT FACDTEXIGIBLE
                                               FROM FACTURE
                                              WHERE FACID = NFACID)));

                        IF NCOUNT > 0
                        THEN
                            NOK := 0;
                            EXIT;
                        END IF;
                    END LOOP;
                END LOOP;
            --CONTROLE ACCOUNT SERVICING IN UNALLOCATION 22/07/2014 KV
            ELSIF SCONTROLE = ('R19')
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM REGIMPUTATION RIM, FACREFERENCE FRE, REGLEMENT REG
                 WHERE     REG.REGID = RIM.REGID
                       AND RIM.RIMSENS = 'C'
                       AND REG.REGTYPEMVT = 'OL'
                       AND REG.REGID = NREGID
                       AND RIM.FACID = FRE.FACID
                       AND FRE.FREDOSID IN
                               (SELECT DISTINCT FREDOSID
                                  FROM FACREFERENCE FRE1, REGIMPUTATION RIM1
                                 WHERE     RIM1.REGID = NREGID
                                       AND RIM1.RIMSENS = 'D'
                                       AND RIM1.FACID = FRE1.FACID);

                IF (NCOUNT > 0)
                THEN
                    NOK := 0;
                END IF;
            --05-08-2014 -start - ISF --- Controle Acount Servicing :the Cash Flow made from a PtP with DD as payment method or from the first DD batch > unpaid Receivable + late charge
            ELSIF SCONTROLE = 'R23'
            THEN
                SELECT (REGMT - F_CALCULIMPAYETIERS (ACTID, SYSDATE))
                  INTO NCOUNT
                  FROM REGLEMENT
                 WHERE    (TMPCODE = 'PRLAUTO' AND REGPTPTYPE = 'YES')
                       OR     (REGTTTID IS NOT NULL AND TMPCODE = 'PRLAUTO')
                          AND REGID = NREGID;

                IF (NCOUNT > 0)
                THEN
                    NOK := 0;
                END IF;
            --05-08-2014 -END - ISF --- Controle Acount Servicing :the Cash Flow made from a PtP with DD as payment method or from the first DD batch
            ELSIF SCONTROLE = 'R20'
            THEN
                  SELECT NVL (SUM (COUNT (*)), 0)
                    INTO NCOUNT
                    FROM REGIMPUTATION RIM, REGLEMENT REG
                   WHERE     REG.REGID = RIM.REGID
                         AND REG.REGTYPEMVT IN ('E', 'D')
                         AND (FACID IS NOT NULL OR DEPID IS NOT NULL)
                         AND REG.REGID = NREGID
                GROUP BY FACID,
                         FECORDRE,
                         DEPID,
                         DECORDRE
                  HAVING COUNT (*) > 1;                          --CASNT-16600

                IF (NCOUNT > 0)
                THEN
                    NOK := 0;
                END IF;
            ELSIF SCONTROLE = 'R22'
            THEN
                SELECT SUM (CNT)
                  INTO NCOUNT
                  FROM (SELECT COUNT (1) AS CNT
                          FROM REGLEMENT A, REGIMPUTATION X
                         WHERE     A.REGID = X.REGID
                               AND A.REGID = NREGID
                               AND A.TMPCODE IN ('INNDDEB', 'PRLAUTO')
                               AND EXISTS
                                       (SELECT 1
                                          FROM DOSACTPAIEMENT A, DOSACTEUR B
                                         WHERE     A.DOSID = B.DOSID
                                               AND A.DACORDRE = B.DACORDRE
                                               AND B.DOSID = X.DOSID
                                               AND TMPCODE = 'GIRO'
                                               AND DAPTYPE = 'E'
                                               AND F_PLROLEEXTERNE (
                                                       B.ROLCODE) =
                                                   'CLIENT'
                                               AND DAPDTFIN IS NULL)
                        UNION ALL
                        SELECT COUNT (1) AS CNT
                          FROM ACTUNITE A, REGLEMENT B
                         WHERE     A.ACTID = B.ACTID
                               AND A.AUNORDRE = B.AUNORDRE
                               AND REGID = NREGID
                               AND A.TMPCODE = 'GIRO'
                               AND A.AUNDTFIN IS NULL
                               AND B.TMPCODE IN ('INNDDEB', 'PRLAUTO'));

                IF (NCOUNT > 0)
                THEN
                    NOK := 0;
                END IF;
            END IF;

            RETURN NOK;
        END;
    END OCREGGENERALITES;

    FUNCTION OCREGLEMENT (NREGID      IN REGLEMENT.REGID%TYPE,
                          SCONTROLE   IN VARCHAR2,
                          SUTICODE    IN UTILISATEUR.UTICODE%TYPE,
                          SUGECODE    IN UTILISATEUR.UGECODE%TYPE,
                          NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;
        BEGIN
            -- CV-26112013 CASNT-8436 OLFA
            IF SCONTROLE IN ('ARCA',
                             'PERT',
                             'OLFA',
                             'BKDTE',
                             'R01',
                             'R02',
                             'R03',
                             'R04',
                             'R05',
                             'R06',
                             'R07',
                             'R08',
                             'R09',
                             'REGBL',
                             'R10',
                             'R11',
                             'R12',
                             'R13',
                             'R14',
                             'R15',
                             'R16',
                             'R17',
                             'R18',
                             'R19',
                             'R20',
                             'R21',
                             'R22',
                             'R23')
            THEN
                NOK :=
                    OCREGGENERALITES (NREGID,
                                      SCONTROLE,
                                      SUTICODE,
                                      SUGECODE,
                                      NCREID);
            END IF;

            ---- MAH - AIL : Debut controle UNICITE REFERENCE CHEQUE POUR LE TYPE DE PAIEMENT ENCAISSEMENT STANDARD
            IF SCONTROLE = 'UNQCH'
            THEN
                DECLARE
                    nTypeMouvement   REGLEMENT.REGTYPEMVT%TYPE := NULL;
                    nModePaiement    REGLEMENT.TMPCODE%TYPE := NULL;
                    nTypePaiement    CCHVALUE.CVASTRINGVALUE%TYPE := NULL;
                    nRefCheque       REGLEMENT.REGPIECE%TYPE := NULL;
                    nCCHVId          CCHVALUE.CVAID%TYPE := NULL;
                    nCount           NUMBER := NULL;
                BEGIN
                    SELECT REGTYPEMVT, TMPCODE
                      INTO nTypeMouvement, nModePaiement
                      FROM REGLEMENT
                     WHERE REGID = nRegId;

                    IF nTypeMouvement = 'E' AND nModePaiement = 'CHQ'
                    THEN          ----> S'il s'agit d'un ENCAISSEMENT CHEQUE :
                        SELECT REGPIECE
                          INTO nRefCheque
                          FROM REGLEMENT
                         WHERE REGID = nRegId; ----> r?cup?rer la r?f?rence du ch?que

                        SELECT MAX (CVAID)
                          INTO nCCHVId
                          FROM CCHVALUE
                         WHERE CCHSID = 'CMBCCHSID63213' AND REGID = nRegId;

                        IF nCCHVId IS NOT NULL
                        THEN      ----> Si le type de paiement est renseign? :
                            SELECT CVASTRINGVALUE
                              INTO nTypePaiement
                              FROM CCHVALUE
                             WHERE CVAID = nCCHVId; ----> r?cup?rer le type de paiement

                            IF     nRefCheque IS NOT NULL
                               AND nTypePaiement IS NOT NULL
                            THEN ----> Si r?f?rence ch?que et type de paiement ont ?t? saisis, proc?der au contr?le :
                                IF nTypePaiement = 'ENCSTRD'
                                THEN ----> S'il s'agit d'un encaissement standard :
                                    SELECT COUNT (*)
                                      INTO nCount
                                      FROM REGLEMENT
                                     WHERE REGPIECE = nRefCheque; ----> V?rifier l'unicit? de la r?f?rence ch?que

                                    IF nCount > 1
                                    THEN
                                        NOK := 0;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle UNICITE REFERENCE CHEQUE POUR LE TYPE DE PAIEMENT ENCAISSEMENT STANDARD

            ---- MAH - AIL : Debut controle sur le RIB s?lectionn? dans les cas d'ENCAISSEMENT : CHEQUE, VIREMENT MANUEL et VERSEMENT ESPECE
            ELSIF SCONTROLE = 'RIBAI'
            THEN
                DECLARE
                    nTypeMouvement   REGLEMENT.REGTYPEMVT%TYPE := NULL;
                    nModePaiement    REGLEMENT.TMPCODE%TYPE := NULL;
                    nRIBId           REGLEMENT.RIBID%TYPE := NULL;
                BEGIN
                    SELECT REGTYPEMVT, TMPCODE
                      INTO nTypeMouvement, nModePaiement
                      FROM REGLEMENT
                     WHERE REGID = nRegId;

                    IF nTypeMouvement = 'E'
                    THEN
                        IF nModePaiement IN ('CHQ', 'VIRMAN', 'ESVECA')
                        THEN
                            SELECT RIBID
                              INTO nRIBId
                              FROM REGLEMENT
                             WHERE REGID = nRegId;

                            IF nRIBId != 2
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle contr?le sur le RIB s?lectionn? dans les cas d'ENCAISSEMENT : CHEQUE, VIREMENT MANUEL et VERSEMENT ESPECE

            ---- MAH - AIL : Debut controle sur L'EXISTANCE D'UNE RETENUE A LA SOURCE DANS LA DEPENSE RELATIVE AU REGLEMENT DE TYPE 'D'
            ELSIF SCONTROLE = 'RSXST'
            THEN
                DECLARE
                    nTypeMvt   REGLEMENT.REGTYPEMVT%TYPE := NULL;
                    nCount     NUMBER := 0;
                BEGIN
                    SELECT REGTYPEMVT
                      INTO nTypeMvt
                      FROM REGLEMENT
                     WHERE REGID = nRegId;

                    IF nTypeMvt = 'D'
                    THEN
                        SELECT COUNT (*)
                          INTO nCount
                          FROM REGIMPUTATION RI, REGLEMENT R, DEPTAXE DT
                         WHERE     RI.REGID = R.REGID
                               AND RI.DEPID = DT.DEPID
                               AND RI.REGID = nRegId
                               AND R.REGTYPEMVT = 'D'
                               AND DT.DTAMTRETENUE != 0;

                        IF nCount = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur L'EXISTANCE D'UNE RETENUE A LA SOURCE DANS LA DEPENSE RELATIVE AU REGLEMENT DE TYPE 'D'

            ---- MAH - AIL : Debut controle si LE MONTANT DECAISSE AU FOURNISSEUR EST NET DU MONTANT DE LA RETENUE A LA SOURCE
            ELSIF SCONTROLE = 'DECRS'
            THEN
                DECLARE
                    nTypeMvt         REGLEMENT.REGTYPEMVT%TYPE := NULL;
                    nMntReglement    REGLEMENT.REGMT%TYPE := NULL;
                    nMntDepenseTTC   DEPENSE.DEPMTTTC%TYPE := NULL;
                    nMntDepRetenue   DEPTAXE.DTAMTRETENUE%TYPE := NULL;
                BEGIN
                    SELECT REGTYPEMVT
                      INTO nTypeMvt
                      FROM REGLEMENT
                     WHERE REGID = nRegId;

                    IF nTypeMvt = 'D'
                    THEN
                        SELECT R.REGMT, D.DEPMTTTC, DT.DTAMTRETENUE
                          INTO nMntReglement, nMntDepenseTTC, nMntDepRetenue
                          FROM REGIMPUTATION  RI,
                               REGLEMENT      R,
                               DEPTAXE        DT,
                               DEPENSE        D
                         WHERE     RI.REGID = R.REGID
                               AND RI.REGID = nRegId
                               AND RI.DEPID = DT.DEPID
                               AND RI.DEPID = D.DEPID
                               AND DT.TAXCODE = 'TVATN'
                               AND R.REGTYPEMVT = 'D';

                        IF nMntReglement > (nMntDepenseTTC - nMntDepRetenue)
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle si LE MONTANT DECAISSE AU FOURNISSEUR EST NET DU MONTANT DE LA RETENUE A LA SOURCE

            ---- MAH - AIL : Debut controle sur l'EXISTANCE D'IMPAYE SUR L'ACTEUR DE ROLE CLIENT LORS D'UN REMBOURSEMENT DE TROP PERCU
            ELSIF SCONTROLE = 'RMBTP'
            THEN
                DECLARE
                    nRubId          REGIMPUTATION.RUBID%TYPE := NULL;
                    nTypeMvt        REGLEMENT.REGTYPEMVT%TYPE := NULL;
                    nLettrageReg    NUMBER := 0;
                    nActIdReg       ACTEUR.ACTID%TYPE := NULL;
                    nImpayeClient   NUMBER := NULL;
                    nCount          NUMBER := 0;
                BEGIN
                    --- Savoir s'il s'agit d'un d?caissement
                    SELECT REGTYPEMVT
                      INTO nTypeMvt
                      FROM REGLEMENT
                     WHERE REGID = nRegId;

                    --- Savoir s'il y a bien un RUBID et que le lettrage concerne un r?glement et non pas une d?pense ou autre
                    IF nTypeMvt = 'D'
                    THEN
                        SELECT RUBID
                          INTO nRubId
                          FROM REGIMPUTATION
                         WHERE REGID = nRegId;

                        SELECT COUNT (*)
                          INTO nLettrageReg
                          FROM REGIMPUTATION
                         WHERE     REGID = nRegId
                               AND RIMLETTRAGE IN
                                       (SELECT REGREFERENCE FROM REGLEMENT);

                        IF nRubId IS NOT NULL AND nLettrageReg <> 0
                        THEN
                            SELECT ACTID
                              INTO nActIdReg
                              FROM REGLEMENT
                             WHERE REGID = nRegId;

                            SELECT COUNT (*)
                              INTO nCount
                              FROM ACTROLE AR, DOSACTEUR DA
                             WHERE     AR.ROLCODE = 'CLIENT'
                                   AND AR.ACTID = nActIdReg
                                   AND AR.ACTID = DA.ACTID
                                   AND DA.DACDTFIN IS NULL;

                            IF nCount <> 0
                            THEN
                                SELECT NVL (
                                           SUM (
                                               NVL (
                                                   F_PLFACTIMP (
                                                       FACID,
                                                       TO_DATE ('26/09/2017',
                                                                'dd/MM/yyyy')),
                                                   0)),
                                           0)
                                  INTO nImpayeClient
                                  FROM FACTURE
                                 WHERE ACTIDCLIENT = nActIdReg;

                                IF nImpayeClient > 0
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle sur l'EXISTANCE D'IMPAYE SUR L'ACTEUR DE ROLE CLIENT LORS D'UN REMBOURSEMENT DE TROP PERCU
            --         END IF;


            ELSIF (sControle IN ('VALRE'))
            THEN                                                    --13102011
                nOk := 0;

                DECLARE
                    l_count   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM crevt cr, reglement reg
                     WHERE     reg.regid = nRegId
                           AND cr.tmffonction = 'EVREG_VAL'
                           AND reg.regid = cr.regid
                           AND reg.regtypemvt IN ('E', 'D')
                           AND reg.tmpcode IN ('EFFET', 'PRLAUTO')
                           AND cr.uticodecreat NOT IN
                                   (SELECT uticode
                                      FROM utilisateur
                                     WHERE grocode IN ('IT', 'GRPORFI'));

                    IF (l_count > 0)
                    THEN
                        nok := 0;                                        -- KO
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF (sControle IN ('TRPUB'))
            THEN                                                    --20171103
                DECLARE
                    sRegtypemvt   REGLEMENT.REGTYPEMVT%TYPE;
                    nActid        ACTEUR.ACTID%TYPE;
                    sTmpcode      REGLEMENT.TMPCODE%TYPE;
                    nRibid        REGLEMENT.RIBID%TYPE;
                BEGIN
                    SELECT regtypemvt,
                           actid,
                           tmpcode,
                           ribid
                      INTO sRegtypemvt,
                           nActid,
                           sTmpcode,
                           nRibid
                      FROM reglement
                     WHERE regid = nRegId;

                    IF sRegtypemvt = 'D' AND nActid = 45
                    THEN
                        IF    sTmpcode <> 'REGLMAN'
                           OR nRibid IS NULL
                           OR nRibid <> 24917
                        THEN
                            nOk := 0;                                    -- KO
                        ELSE
                            nOk := 1;                                    -- OK
                        END IF;
                    ELSE
                        nOk := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            END IF;

            RETURN NOK;
        END;
    END OCREGLEMENT;

    FUNCTION OCFACGENERALITES (NFACID      IN FACTURE.FACID%TYPE,
                               SCONTROLE   IN VARCHAR2,
                               NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK              NUMBER := 1;
            NRUBIDBONIF      RUBRIQUE.RUBID%TYPE;
            NRUBIDPLAFOND    RUBRIQUE.RUBID%TYPE;
            NMOIS            NUMBER;
            DTDOSDTEFFET     DOSSIER.DOSDTEFFET%TYPE;
            DTDOSDTFIN       DOSSIER.DOSDTFIN%TYPE;
            NDUREEDOSSIER    NUMBER;
            NDUREELIGNE      NUMBER;
            NFREDOSID        NUMBER;
            NMONTH           NUMBER;
            NYEAR            NUMBER;
            DDATE            DATE;
            DTFACDTFACTURE   DATE;
            SUGECODE         UTILISATEUR.UGECODE%TYPE := F_GETCURRENTUGECODE;

            CURSOR CFACLIGNE
            IS
                SELECT FLI.RUBID,
                       FAC.ROLCODE,
                       FLI.FLIDTDEB,
                       FLI.FLIDTFIN
                  FROM FACTURE FAC, FACLIGNE FLI
                 WHERE FAC.FACID = NFACID AND FLI.FACID = FAC.FACID;
        BEGIN
            IF (SCONTROLE = 'FACDT')
            THEN
                BEGIN
                    SELECT FACDTFACTURE
                      INTO DTFACDTFACTURE
                      FROM FACTURE
                     WHERE FACID = NFACID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        DTFACDTFACTURE := NULL;
                END;

                IF DTFACDTFACTURE IS NOT NULL
                THEN
                    SELECT TO_NUMBER (TO_CHAR (SYSDATE, 'MM'))
                      INTO NMONTH
                      FROM DUAL;

                    SELECT TO_NUMBER (TO_CHAR (SYSDATE, 'YYYY'))
                      INTO NYEAR
                      FROM DUAL;

                    DDATE :=
                        TO_DATE (
                               '01/'
                            || TO_CHAR (NMONTH, '99')
                            || '/'
                            || TO_CHAR (NYEAR, '9999'),
                            'DD/MM/YYYY');

                    IF DTFACDTFACTURE < DDATE
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF (SCONTROLE IN ('BONI', 'CUTO'))
            THEN
                IF SCONTROLE = 'BONI'
                THEN
                    BEGIN
                        SELECT F_GETRUBIDWITHRUBCODE ('BONIF', SUGECODE)
                          INTO NRUBIDBONIF
                          FROM DUAL;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NRUBIDBONIF := NULL;
                    END;
                END IF;

                IF SCONTROLE = 'CUTO'
                THEN
                    SELECT MAX (FREDOSID)
                      INTO NFREDOSID
                      FROM FACREFERENCE
                     WHERE FACID = NFACID;

                    BEGIN
                        SELECT F_GETRUBIDWITHRUBCODE ('BONIF', SUGECODE)
                          INTO NRUBIDBONIF
                          FROM DUAL;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NRUBIDBONIF := NULL;
                    END;

                    BEGIN
                        SELECT F_GETRUBIDWITHRUBCODE ('PLAFOND', SUGECODE)
                          INTO NRUBIDPLAFOND
                          FROM DUAL;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NRUBIDBONIF := NULL;
                    END;
                END IF;

                FOR CFACLIGNE_REC IN CFACLIGNE
                LOOP
                    IF     SCONTROLE = 'BONI'
                       AND CFACLIGNE_REC.RUBID = NRUBIDBONIF
                    THEN
                        IF CFACLIGNE_REC.ROLCODE != 'FOURN'
                        THEN
                            NOK := 0;
                        END IF;
                    ELSIF SCONTROLE = 'CUTO'
                    THEN
                        IF    CFACLIGNE_REC.RUBID = NRUBIDBONIF
                           OR CFACLIGNE_REC.RUBID = NRUBIDPLAFOND
                        THEN
                            IF     CFACLIGNE_REC.FLIDTDEB IS NOT NULL
                               AND CFACLIGNE_REC.FLIDTFIN IS NOT NULL
                            THEN
                                SELECT TRUNC (
                                           MONTHS_BETWEEN (
                                               CFACLIGNE_REC.FLIDTFIN,
                                               CFACLIGNE_REC.FLIDTDEB))
                                  INTO NMOIS
                                  FROM DUAL;

                                IF NMOIS = 0
                                THEN
                                    NOK := 0;
                                ELSIF NFREDOSID != 0
                                THEN
                                    BEGIN
                                        SELECT DOSDTEFFET, DOSDTFIN
                                          INTO DTDOSDTEFFET, DTDOSDTFIN
                                          FROM DOSSIER
                                         WHERE DOSID = NFREDOSID;
                                    EXCEPTION
                                        WHEN OTHERS
                                        THEN
                                            DTDOSDTEFFET := NULL;
                                            DTDOSDTFIN := NULL;
                                    END;

                                    IF     DTDOSDTEFFET IS NOT NULL
                                       AND DTDOSDTFIN IS NOT NULL
                                    THEN
                                        NDUREEDOSSIER :=
                                            F_DATEBETWEEN (
                                                TRUNC (DTDOSDTEFFET),
                                                TRUNC (DTDOSDTFIN),
                                                'A',
                                                'A',
                                                0,
                                                360);
                                        NDUREELIGNE :=
                                            F_DATEBETWEEN (
                                                TRUNC (
                                                    CFACLIGNE_REC.FLIDTDEB),
                                                TRUNC (
                                                    CFACLIGNE_REC.FLIDTFIN),
                                                'A',
                                                'A',
                                                0,
                                                360);

                                        IF     TRUNC (CFACLIGNE_REC.FLIDTDEB) >
                                               TRUNC (DTDOSDTEFFET)
                                           AND TRUNC (DTDOSDTFIN) !=
                                               TRUNC (CFACLIGNE_REC.FLIDTFIN)
                                        THEN
                                            NOK := 0;
                                        ELSIF TRUNC (CFACLIGNE_REC.FLIDTDEB) <=
                                              TRUNC (DTDOSDTEFFET)
                                        THEN
                                            IF NDUREELIGNE < NDUREEDOSSIER
                                            THEN
                                                NOK := 0;
                                            END IF;
                                        END IF;
                                    END IF;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END LOOP;
            END IF;

            RETURN NOK;
        END;
    END OCFACGENERALITES;

    FUNCTION OCFACTURE (NFACID      IN FACTURE.FACID%TYPE,
                        SCONTROLE   IN VARCHAR2,
                        NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK   NUMBER := 1;
        BEGIN
            IF SCONTROLE = ('FACDT')
            THEN
                NOK := OCFACGENERALITES (NFACID, SCONTROLE, NCREID);
            ELSIF (SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1) IN
                       ('BONI', 'CUTO'))
            THEN
                NOK :=
                    OCFACGENERALITES (
                        NFACID,
                        SUBSTR (SCONTROLE, 1, LENGTH (SCONTROLE) - 1),
                        NCREID);
            ELSIF sControle = 'FRESC'
            THEN                          -- Added by Anish for EVFAC_MODIFECH
                BEGIN
                    DECLARE
                        CURSOR C1
                        IS
                            SELECT *
                              FROM SASFACECHEANCE
                             WHERE CREID = nCreId;

                        CURSOR C2
                        IS
                            SELECT *
                              FROM FACECHEANCE
                             WHERE FACID IN (SELECT FACID
                                               FROM CREVT
                                              WHERE CREID = nCreId);
                    BEGIN
                        FOR C1R IN C1
                        LOOP
                            FOR C2R IN C2
                            LOOP
                                IF TRUNC (C1R.FECDTEXIGIBLE) <
                                   TRUNC (C2R.FECDTEXIGIBLE)
                                THEN
                                    nOk := 0;
                                    EXIT;
                                END IF;
                            END LOOP;
                        END LOOP;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            nOk := 1;
                    END;
                END;
            ---- MAH - AIL : Debut controle sur LA MODIFICATION DU MONTANT DES INTERETS DE RETARD (Sur Modification IR EVFIR_MAJMONT)
            ELSIF SCONTROLE = ('MNTIR')
            THEN
                DECLARE
                    nIRFacturable   FACIR.FIRMTINTFACTURABLE%TYPE := NULL;
                    nIRCalcule1     FACIR.FIRMTINTCALCULE%TYPE := NULL;
                BEGIN
                    SELECT FIRMTINTCALCULE, FIRMTINTFACTURABLE
                      INTO nIRCalcule1, nIRFacturable
                      FROM FACIR
                     WHERE     FACID = nFacId
                           AND FIRTTFID = (SELECT MIN (FIRTTFID)
                                             FROM FACIR
                                            WHERE FACID = nFacId);

                    IF nIRFacturable / nIRCalcule1 < 0.8
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            /* --   WAS :
            DECLARE
              nIRFacturable FACIR.FIRMTINTFACTURABLE%TYPE := NULL;
              nIRCalcule1 FACIR.FIRMTINTCALCULE%TYPE := NULL;
              nIRCalcule2 FACIR.FIRMTINTCALCULE%TYPE := NULL;
            BEGIN
              SELECT FIRMTINTCALCULE, FIRMTINTFACTURABLE
                INTO nIRCalcule1, nIRFacturable
                FROM FACIR
                WHERE FACID = nFacId
                AND FIRDTFIN = (SELECT MAX(FIRDTFIN) FROM FACIR WHERE FACID = nFacId);
              SELECT FIRMTINTCALCULE
                INTO nIRCalcule2
                FROM FACIR
                WHERE FACID = nFacId
                AND FIRDTFIN = (SELECT MIN(FIRDTFIN) FROM FACIR WHERE FACID = nFacId);
              IF nIRCalcule1 != nIRCalcule2 THEN ---> Si les montants des IR avant et apres sont differents, proceder au test :
                IF nIRFacturable/(nIRCalcule1 -  nIRCalcule2) < 0.8 THEN
                  NOK := 0;
                END IF;
              END IF;
            EXCEPTION
              WHEN OTHERS THEN
                NOK := 0;
            END;*/
            ---- MAH - AIL : Fin controle sur LA MODIFICATION DU MONTANT DES INTERETS DE RETARD (Sur Modification IR EVFIR_MAJMONT)
            -- END IF;



            ---RBT TLG_Facture diverse ponctuelle : controle sur le Montant HT d'un Frais
            ELSIF (sControle = 'FRAMT')
            THEN
                DECLARE
                    nMtHt    NUMBER := 0;
                    nNorme   NUMBER := 0;
                BEGIN
                    --Detection de la valeur du Frais dans la Facture
                    SELECT NVL (SUM (FLI.FLIMTHTBASE), 0)
                      INTO nMtHt
                      FROM RUBRIQUE RUB, FACLIGNE FLI
                     WHERE     RUB.RUBID = FLI.RUBID
                           AND FLI.FACID = nFacid
                           AND rub.rubcode LIKE 'FRA%';    --MSB EXTTLGBO-2652


                    --Detection de la valeur de reference du Frais
                    /*  select nvl(Sum(to_number(DTR.DDEHOSTVALUE)),0)
                     into nNorme
                     from RUBRIQUE RUB, DTRDETAIL DTR, FACLIGNE FLI
                     where RUB.RUBCODE = DTR.DDECASSIOPEEVALUE
                     and DTR.DTRID = 100
                     and RUB.RUBID = FLI.RUBID
                     AND rub.rubcode like 'FRA%'     --MSB EXTTLGBO-2652
                     and FLI.FACID = nFacid ;  */

                    SELECT NVL (SUM (TTP.TPANOMBRE), 0)
                      INTO nNorme
                      FROM RUBRIQUE RUB, TOPPARAM TTP, FACLIGNE FLI
                     WHERE     TTP.TOPTABLE = 'MTRUBDEF'
                           AND RUB.RUBCODE = TTP.TPAPARAM
                           AND RUB.RUBID = FLI.RUBID
                           AND rub.rubcode LIKE 'FRA%'     --MSB EXTTLGBO-2713
                           AND FLI.FACID = nFacid;

                    IF nMtHt != nNorme
                    THEN
                        nOK := 0;
                    ELSE
                        nOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nOK := 0;                                -- Erreur OK.
                END;
            ----END RBT



            ELSIF (SUBSTR (sControle, 1, LENGTH (sControle) - 1) IN
                       ('BONI', 'CUTO'))
            THEN
                nOk :=
                    ocFacGeneralites (
                        nFacId,
                        SUBSTR (sControle, 1, LENGTH (sControle) - 1),
                        nCreId);
            END IF;

            RETURN NOK;
        END;
    END OCFACTURE;

    FUNCTION OCACTEUR (NACTID      IN ACTEUR.ACTID%TYPE,
                       SCONTROLE   IN VARCHAR2,
                       NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
        l_count   NUMBER;

    BEGIN
        DECLARE
            NOK                  NUMBER := 1;
            NCOUNT               NUMBER := 0;
            W_RIB                ACTRIB.RIBID%TYPE;
            W_ARIDTDEB           ACTRIB.ARIDTDEB%TYPE;
            W_CVADTVALUE         CCHVALUE.CVADTVALUE%TYPE;
            sLegalType           VARCHAR2 (7) := NULL;
            Sacttype           VARCHAR2 (7) := NULL;
            sIdentityNum         VARCHAR2 (50) := NULL;
            dIdentityDel         DATE := NULL;
            dIdentityExp         DATE := NULL;
            sIdentityPays        VARCHAR2 (50) := NULL;
            sClientNature        VARCHAR2 (20) := NULL;
            sPatente             VARCHAR2 (50) := NULL;
            sice             VARCHAR2 (50) := NULL;
            sIF             VARCHAR2 (50) := NULL;
            stypeid             VARCHAR2 (50) := NULL;
            sIdentifiantFiscal   VARCHAR2 (20) := NULL;
            sRegComm             VARCHAR2 (20) := NULL;
            dCreationComp        DATE := NULL;
            sVille               VARCHAR2 (255) := NULL;
            sLocaliteCode        VARCHAR2 (255) := NULL;
            sTailleEntreprise    VARCHAR2 (20) := NULL;
            nDureeFisc           NUMBER (10) := 0;
            sTypeResult          VARCHAR2 (255) := NULL;
            dDebExercice         DATE := NULL;
            nsgclt          VARCHAR2 (50):=NULL;
            nadract         DATE:=NULL;
            nACTNUMRCM          VARCHAR2 (50):=NULL;
             NRIBCOMPTE          VARCHAR2 (50):=NULL;
             NRIBCLE          VARCHAR2 (50):=NULL;
             CCstring         VARCHAR2 (50):=NULL;--abh 06/04/2021

        BEGIN
            IF SCONTROLE = 'RIBMA'
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM ACTRIB
                 WHERE     ACTID = NACTID
                       AND ARITYPE = 'RIBMAIN'
                       AND ARIDTREMPLACE IS NULL;

                IF NCOUNT > 1
                THEN
                    NOK := 0;
                END IF;
            --CASNT8997-USer control on Agency branch with flag forced=0 and ATVCOMMENT ='Non Special Agreement' LB 01/13/2014
            ELSIF sControle = 'AGDIV'
            THEN
                DECLARE
                    l_grocode   UTILISATEUR.GROCODE%TYPE;
                BEGIN
                    SELECT MAX (grocode)
                      INTO l_grocode
                      FROM ACTEUR, utilisateur
                     WHERE     ACTID = nActId
                           AND uticodemaj = uticode
                           AND EXISTS
                                   (SELECT 1
                                      FROM actrole
                                     WHERE     actid = nActid
                                           AND rolcode IN
                                                   ('AGENCE', 'PARTEN')); --FER 13022013 AJOUTER LE GROUPE DBO

                    IF    (l_grocode IN ('GRPORFI', 'IT', 'DBO'))
                       OR (l_grocode IS NULL)
                    THEN
                        nok := 1;
                    ELSE
                        nok := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;                           -- Erreur donc KO.
                END;
            ELSIF sControle = 'EXCIN'
            THEN
                DECLARE
                    l_cjucode   VARCHAR2 (25);
                    l_cin       VARCHAR2 (25);
                BEGIN
                    SELECT cjucode
                      INTO l_cjucode
                      FROM acteur
                     WHERE actid = nActid;

                    IF l_cjucode <> '1000'
                    THEN
                        nok := 1;   -- personne morale, donc pas de controle?.
                    ELSE
                        SELECT MAX (cvastringvalue)
                          INTO l_cin
                          FROM cchvalue
                         WHERE     cchsid = 'TFDCCHSID1676'
                               AND SUBSTR (cvapkeyvalue,
                                           INSTR (cvapkeyvalue, '-') + 1,
                                           INSTR (cvapkeyvalue, '|') - 7) =
                                   nActid;

                        IF (l_cin IS NULL)
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            -----------20191127 JJO UAT-297 DEBUT
            ELSIF SCONTROLE IN ('COHE')                -----------20191127 JJO
            THEN
                DECLARE
                    ncount    NUMBER := 0;
                    DEBDEAL   dossierprospect.DPRDTCREATION%TYPE;
                    CREATDT   acteur.ACTDTIMMATRICULATION%TYPE;
                BEGIN
                    SELECT NVL (COUNT (actid), 0)
                      INTO ncount
                      FROM acteur
                     WHERE     actid = nactid
                           AND actid IN (SELECT actid
                                           FROM actrole
                                          WHERE rolcode = 'CLIENT')
                           AND actid IN (SELECT actid
                                           FROM acteur
                                          WHERE acttype = 'E');

                    IF (ncount = 0)
                    THEN
                        NOK := 1; -------le role n est pas client ou le type n est pas personne morale : passage sans controle
                    ELSIF (ncount > 0) -------le role est client et le type est personne morale : verification
                    THEN
                        SELECT MAX (ACTDTIMMATRICULATION)
                          INTO CREATDT
                          FROM acteur
                         WHERE actid = nactid; ----verifier nactid ou p_actid...



                        IF CREATDT > SYSDATE
                        THEN
                            nok := 0;
                        ELSE
                            nok := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ELSIF sControle = 'RESPP'
            THEN
                DECLARE
                    l_count    NUMBER;
                    l_countA   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO l_countA
                      FROM actrole
                     WHERE rolcode = 'CLIENT' AND actid = nActid;

                    IF (l_countA > 0)
                    THEN
                        SELECT COUNT (*)
                          INTO l_count
                          FROM actrole ac, lkactutitsm lk
                         WHERE     ac.rolcode = 'CLIENT'
                               AND lk.tsmmetier = 'PORTFL'
                               AND ac.actid = lk.actid
                               AND ac.actid = nActid;

                        IF (l_count = 1)
                        THEN
                            nok := 1;
                        ELSE
                            nok := 0;
                        END IF;
                    ELSE
                        nok := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF sControle = 'TLCIN'
            THEN                                        --controle migre A0333
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
                                                     INSTR (cvapkeyvalue,
                                                            '-')
                                                   + 1,
                                                     INSTR (cvapkeyvalue,
                                                            '|')
                                                   - 7) =
                                               nActid
                                           AND cchsid = 'TFDCCHSID1676'
                                           AND LTRIM (cvastringvalue)
                                                   IS NOT NULL);



                    IF l_count > 1
                    THEN
                        nok := 0;
                    ELSE
                        nok := 1;
                    END IF;
                END;
            ELSIF sControle = 'TLIMM'
            THEN
                BEGIN
                    SELECT COUNT (*)
                      INTO l_count
                      FROM acteur
                     /*where actsiret in (select distinct actsiret from acteur where  actsiret not in ('9999999ZZZ000','031440SAM000'))*/
                     WHERE     actsiret IN
                                   (SELECT DISTINCT actsiret
                                      FROM acteur
                                     WHERE     actsiret IS NOT NULL
                                           AND actsiret NOT IN
                                                   ('9999999ZZZ000',
                                                    '9999999zzz000')
                                           AND actid != nActid) --MTR-20052013
                           AND actid = nActid;

                    IF l_count > 0
                    THEN
                        nok := 0;                            -- erreur doublon
                    ELSE
                        nok := 1;                                        -- OK
                    END IF;
                END;
            /*   ELSIF sControle = 'SIRET'
               THEN --------MTR 20150226 TLBO-737: un contr?'le bloquant sur le champ du Matricule fiscal pour que le code soit saisi sur 13 caract??res sans espace et sans autre caract??re: Format : 7chiffres 3 lettres en majuscule 3 chiffres.
                  DECLARE
                     nLengthSiret   NUMBER := 0;
                     nPart11        VARCHAR2 (10) := NULL;
                     nPart12        VARCHAR2 (10) := NULL;
                     nPart21        VARCHAR2 (10) := NULL;
                     nPart22        VARCHAR2 (10) := NULL;
                     nPart31        VARCHAR2 (10) := NULL;
                     nPart32        VARCHAR2 (10) := NULL;
                     nACTSIRET      VARCHAR (50) := NULL;
                  BEGIN
                     SELECT LENGTH (f_StdTrimAll (ACTSIRET)),         --nLengthSiret
                            --PART1
                            f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7)),        --nPart11
                            F_StrTo_Number (f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7))), --nPart12
                            --PART2
                            f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3)),        --nPart21
                            UPPER (f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3))), --nPart22
                            --PART3
                            f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3)),       --nPart31
                            F_StrTo_Number (
                               f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3)))    --nPart32
                       INTO nLengthSiret,
                            nPart11,
                            nPart12,
                            nPart21,
                            nPart22,
                            nPart31,
                            nPart32
                       FROM ACTEUR
                      WHERE ACTID = nActid;

                     SELECT ACTSIRET
                       INTO nACTSIRET
                       FROM ACTEUR
                      WHERE ACTID = nActid;

                     IF nACTSIRET IS NULL
                     THEN
                        nok := 1;
                     ELSE
                        IF (nLengthSiret = 13)
                        THEN
                           IF    (LENGTH (nPart11) = 7 AND nPart11 = '0000000')
                              OR (LENGTH (nPart11) = 7 AND nPart12 > 0)
                           THEN
                              IF (nPart21 = nPart22 AND LENGTH (nPart21) = 3)
                              THEN
                                 IF    (LENGTH (nPart31) = 3 AND nPart31 = '000')
                                    OR (LENGTH (nPart31) = 3 AND nPart32 > 0)
                                 THEN
                                    nok := 1;
                                 ELSE                                       ---Part3
                                    nok := 0;
                                 END IF;
                              ELSE                                          ---Part2
                                 nok := 0;
                              END IF;
                           ELSE
                              nok := 0;                                     ---Part1
                           END IF;
                        ELSE                                     --(nLengthSiret=13)
                           nok := 0;
                        END IF;
                     END IF;
                  EXCEPTION
                     WHEN OTHERS
                     THEN
                        nok := 0;
                  END;
               ELSIF sControle = 'TLRC'
               THEN
                  BEGIN
                     SELECT COUNT (*)
                       INTO l_count
                       FROM acteur
                      WHERE     actnumrcm IN
                                   (SELECT DISTINCT actnumrcm
                                      FROM acteur
                                     WHERE     actnumrcm IS NOT NULL
                                           AND actid != nActid)
                            AND actid = nActid;

                     IF l_count > 0
                     THEN                                                 --20120404
                        nok := 0;                                  -- erreur doublon
                     ELSE
                        nok := 1;                                              -- OK
                     END IF;
                  END;
               ELSIF sControle = 'TLCLI'
               THEN
                  BEGIN
                     SELECT COUNT (*)
                       INTO l_count
                       FROM actrole
                      WHERE rolcode = 'CLIENT' AND actid = nActid;

                     IF l_count > 0
                     THEN                                                 --20120404
                        nok := 0;                                  -- erreur doublon
                     ELSE
                        nok := 1;                                              -- OK
                     END IF;
                  END;*/


            ELSIF sControle = 'ACDCT'
            THEN              ----------SAB 01/10/2019:  DOUBLON CODE TRIBUNAL
                DECLARE
                    Nexiste   NUMBER := 0;
                BEGIN
                    SELECT f_get_duplicated_codetribunal (nActid, 'ORFI')
                      INTO Nexiste
                      FROM DUAL;

                    IF Nexiste > 0
                    THEN
                        nok := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 1;
                END;
            ELSIF sControle = 'ACTER'
            THEN ----------RBS 01/11/2018:  CONTROLE pour ne pas changer la phase des acteurs terminer avant le 30/10/2018
                DECLARE
                    nCount   NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (*), 0)
                      INTO nCount
                      FROM imaactphase
                     WHERE     phadest = 'ACTEUR'
                           AND phacode = 'TER'
                           AND aphdtdeb <
                               TO_DATE ('29/10/2018', 'dd/mm/yyyy')
                           AND aphdtfin IS NULL
                           AND actid = nActid
                           AND imaid =
                               (SELECT MAX (imaid)
                                  FROM crevt
                                 WHERE     actid = nActid
                                       AND tmffonction = 'EVACT_PHAREMP');

                    IF nCOUNT > 0
                    THEN
                        nok := 0;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 1;
                END;
            ELSIF sControle = 'SIRET'
            THEN ----------MTR 03/10/2018:  CONTROLE mo NE DECLENCHE PAS POUR LES COMMERCIAUX
                DECLARE
                    nLengthSiret   NUMBER := 0;
                    nPart11        VARCHAR2 (10) := NULL;
                    nPart12        VARCHAR2 (10) := NULL;
                    nPart21        VARCHAR2 (10) := NULL;
                    nPart22        VARCHAR2 (10) := NULL;
                    nPart31        VARCHAR2 (10) := NULL;
                    nPart32        VARCHAR2 (10) := NULL;
                    nACTSIRET      VARCHAR (50) := NULL;
                    nCOUNT         NUMBER := 0;
                BEGIN
                    SELECT NVL (COUNT (*), 0)
                      INTO nCOUNT
                      FROM UTITSM
                     WHERE     UTICODE = F_GETCURRENTUTICODE
                           AND TSMMETIER = 'COM'
                           AND TSMFLAGDEFAUT = 1;             --MTR 03/10/2018

                    SELECT LENGTH (f_StdTrimAll (ACTSIRET)),    --nLengthSiret
                           --PART1
                           f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7)),   --nPart11
                           F_StrTo_Number (
                               f_StdTrimAll (SUBSTR (ACTSIRET, 1, 7))), --nPart12
                           --PART2
                           f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3)),   --nPart21
                           UPPER (f_StdTrimAll (SUBSTR (ACTSIRET, 8, 3))), --nPart22
                           --PART3
                           f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3)),  --nPart31
                           F_StrTo_Number (
                               f_StdTrimAll (SUBSTR (ACTSIRET, 11, 3))) --nPart32
                      INTO nLengthSiret,
                           nPart11,
                           nPart12,
                           nPart21,
                           nPart22,
                           nPart31,
                           nPart32
                      FROM ACTEUR
                     WHERE ACTID = nActid;

                    SELECT ACTSIRET
                      INTO nACTSIRET
                      FROM ACTEUR
                     WHERE ACTID = nActid;

                    IF nCOUNT <> 0
                    THEN
                        nok := 1;
                    ELSIF nACTSIRET IS NULL
                    THEN
                        nok := 1;
                    ELSE
                        IF (nLengthSiret = 13)
                        THEN
                            IF    (    LENGTH (nPart11) = 7
                                   AND nPart11 = '0000000')
                               OR (LENGTH (nPart11) = 7 AND nPart12 > 0)
                               OR (ncount > 0)
                            THEN
                                IF (    nPart21 = nPart22
                                    AND LENGTH (nPart21) = 3)
                                THEN
                                    IF    (    LENGTH (nPart31) = 3
                                           AND nPart31 = '000')
                                       OR (    LENGTH (nPart31) = 3
                                           AND nPart32 > 0)
                                    THEN
                                        nok := 1;
                                    ELSE                              ---Part3
                                        nok := 0;
                                    END IF;
                                ELSE                                  ---Part2
                                    nok := 0;
                                END IF;
                            ELSE
                                nok := 0;                             ---Part1
                            END IF;
                        ELSE                               --(nLengthSiret=13)
                            nok := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        nok := 0;
                END;
            ELSIF sControle = 'CCLT'
            THEN
                DECLARE
                    w_raf     VARCHAR2 (15);
                    w_count   NUMBER;
                BEGIN
                    SELECT NVL (astlibre, 'TOTO')
                      INTO w_raf
                      FROM actstatistique
                     WHERE actid = nActid AND saccode = 'RAF';

                    IF w_raf = 'TOTO'
                    THEN
                        nOk := 0;
                    ELSE
                        nOk := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        -- test si on essaye de creer le role client :
                        SELECT COUNT (*)
                          INTO w_count
                          FROM actrole
                         WHERE actid = nActid AND rolcode = 'CLIENT';

                        IF w_count > 0
                        THEN
                            -- pas RAF mais le roleclient alors  : bloquant
                            nOk := 0;
                        ELSE
                            -- pas de RAF et pas de role CLIENT ==> OK
                            nOk := 1;
                        END IF;
                END;
            ELSIF SCONTROLE = ('AGGRD')
            THEN
                DECLARE
                    NCOUNTR    NUMBER;
                    SUGECODE   UTILISATEUR.UGECODE%TYPE
                                   := F_GETCURRENTUGECODE;
                BEGIN
                    BEGIN
                          SELECT COUNT (MAX (ATVORDRE))
                            INTO NCOUNTR
                            FROM ACTTCOVALEUR ATC, ACTROLE ARL, TCOTATION TCT
                           WHERE     ATC.ATVFLAGFORCE = 0
                                 AND SUBSTR (ATVCOMMENT,
                                               INSTR (ATVCOMMENT,
                                                      '||',
                                                      1,
                                                      1)
                                             + 2) = 'SPECAGRN'
                                 AND TCT.TCOID = ATC.TCOID
                                 AND TCT.TCOCODE = 'FGRAAB'
                                 AND TCT.UGECODE = SUGECODE
                                 AND ARL.ROLCODE = 'AGBRCH'
                                 AND ATC.ACTID = ARL.ACTID
                                 AND ATC.ACTID = NACTID
                                 AND TRUNC (ATC.ATVDTFIN) = TRUNC (SYSDATE)
                        GROUP BY ATVORDRE;

                        IF NCOUNTR > 0
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NOK := 1;
                    END;
                END;
            -- CASNT8997-END USer control on Agency branch with flag forced=0 and ATVCOMMENT ='Non Special Agreement' LB 01/13/2014
            -- Release date must be <= to start date
            ELSIF SCONTROLE = 'RIBCO'
            THEN
                BEGIN
                    SELECT MAX (RIBID)
                      INTO W_RIB
                      FROM ACTRIB
                     WHERE ACTID = NACTID AND ARITYPE = 'DEPACC';
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        W_RIB := NULL;
                END;

                IF W_RIB IS NOT NULL
                THEN
                    SELECT ARIDTDEB
                      INTO W_ARIDTDEB
                      FROM ACTRIB
                     WHERE RIBID = W_RIB;

                    BEGIN
                        SELECT NVL (CVADTVALUE,
                                    TO_DATE ('2100/01/01', 'YYYY/MM/DD'))
                          INTO W_CVADTVALUE
                          FROM CCHVALUE
                         WHERE     ENTCODE = 'ACTEUR'
                               AND CCHSID = 'TFDCCHSID250'
                               AND CVAPKEYVALUE =
                                      'Actid-'
                                   || NACTID
                                   || '||Ribid-'
                                   || W_RIB
                                   || '||';
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            W_CVADTVALUE :=
                                TO_DATE ('2100/01/01', 'YYYY/MM/DD');
                    END;

                    IF W_CVADTVALUE <= W_ARIDTDEB
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            ELSIF (SCONTROLE = 'RTYPE')
            THEN
                SELECT COUNT (1)
                  INTO NCOUNT
                  FROM ACTRIB
                 WHERE     ACTID = NACTID
                       AND ARITYPE = 'INCACC'
                       AND ARIDTREMPLACE IS NULL;

                IF NCOUNT > 1
                THEN
                    NOK := 0;
                ELSE
                    SELECT COUNT (1)
                      INTO NCOUNT
                      FROM ACTRIB
                     WHERE     ACTID = NACTID
                           AND ARITYPE = 'LTA'
                           AND ARIDTREMPLACE IS NULL;

                    IF NCOUNT > 1
                    THEN
                        NOK := 0;
                    ELSE
                        SELECT COUNT (1)
                          INTO NCOUNT
                          FROM ACTRIB
                         WHERE     ACTID = NACTID
                               AND ARITYPE = 'PAYBACC'
                               AND ARIDTREMPLACE IS NULL;

                        IF NCOUNT > 1
                        THEN
                            NOK := 0;
                        ELSE
                            SELECT COUNT (1)
                              INTO NCOUNT
                              FROM ACTRIB
                             WHERE     ACTID = NACTID
                                   AND ARITYPE = 'PROMACC'
                                   AND ARIDTREMPLACE IS NULL;

                            IF NCOUNT > 1
                            THEN
                                NOK := 0;
                            ELSE
                                SELECT COUNT (1)
                                  INTO NCOUNT
                                  FROM ACTRIB
                                 WHERE     ACTID = NACTID
                                       AND ARITYPE = 'SCA'
                                       AND ARIDTREMPLACE IS NULL;

                                IF NCOUNT > 1
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                END IF;
            ELSIF SCONTROLE = 'DSHIP'
            THEN                               -- Added by Raja on 09/Feb/2014
                BEGIN
                    DECLARE
                        NRET   NUMBER := NULL;
                    BEGIN
                        BEGIN
                            SELECT F_ROLEEXISTE ('USEDCAR', NACTID)
                              INTO NRET
                              FROM DUAL;

                            IF NRET = 1
                            THEN
                                NOK := 0;
                            ELSE
                                NOK := 1;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                NOK := 1;
                        END;
                    END;
                END;
            ---- JCH 2013-09-08
            ELSIF SCONTROLE = ('ATXS')
            THEN
                DECLARE
                    SACTREGIMETVA   ACTEUR.ACTREGIMETVA%TYPE;
                BEGIN
                    NOK := 1;

                    --GET Tax Code
                    SELECT ACTREGIMETVA
                      INTO SACTREGIMETVA
                      FROM ACTEUR
                     WHERE ACTID = NACTID AND ACTTYPE <> 'PART';

                    --TEST if OK
                    IF SACTREGIMETVA IS NULL
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                END;
            -- end Chaker
            ---- JCH 2014-07-20
            -- LMI  2013-10-11 REPRESENTATIVE NAME, BUSINESS CATEGORY AND DETAILED BUSINESS CATEGORY MANDATORY EXCEPT FOR PRIVATE JIRA 7564
            ELSIF SCONTROLE = 'ACTMA'
            THEN
                DECLARE
                    SACTTYPE        ACTEUR.ACTTYPE%TYPE := NULL;
                    SCCHBUSCAT      CCHVALUE.CVASTRINGVALUE%TYPE := NULL;
                    SCCHBUSCATDET   CCHVALUE.CVASTRINGVALUE%TYPE := NULL;
                    SCVAPKEY        CCHVALUE.CVAPKEYVALUE%TYPE;
                    SREPNAME        ACTEUR.ACTREPRESENTNAME%TYPE := NULL;
                BEGIN
                    NOK := 1;

                    SELECT NVL (ACTREPRESENTNAME, 'xXx'),
                           NVL (ACTTYPE, 'xXx')
                      INTO SREPNAME, SACTTYPE
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    IF (SACTTYPE != 'xXx' AND SACTTYPE != 'PART')
                    THEN
                        SCVAPKEY := 'Actid-' || NACTID || '|%';

                        SELECT NVL (CVASTRINGVALUE, 'xXx')
                          INTO SCCHBUSCAT
                          FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
                         WHERE     CCH.ENTCODE = CVA.ENTCODE
                               AND CVA.ENTCODE = 'ACTEUR'
                               AND CCH.CCHSID = CVA.CCHSID
                               AND CVAPKEYVALUE LIKE SCVAPKEY
                               AND CCHVALUECODE = 'CC_BUSCAT';

                        SELECT NVL (CVASTRINGVALUE, 'xXx')
                          INTO SCCHBUSCATDET
                          FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
                         WHERE     CCH.ENTCODE = CVA.ENTCODE
                               AND CVA.ENTCODE = 'ACTEUR'
                               AND CCH.CCHSID = CVA.CCHSID
                               AND CVAPKEYVALUE LIKE SCVAPKEY
                               AND CCHVALUECODE = 'CC_DETBUSCAT';

                        IF (   SREPNAME = 'xXx'
                            OR SCCHBUSCAT = 'xXx'
                            OR SCCHBUSCATDET = 'xXx')
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END;
            -- LMI  2013-10-11 REPRESENTATIVE NAME, BUSINESS CATEGORY AND DETAILED BUSINESS CATEGORY MANDATORY EXCEPT FOR PRIVATE JIRA 7564
            --Check duplicate sales channels bank account LB 05/26/2014
            ELSIF SCONTROLE = ('DRIBT')
            THEN
                DECLARE
                    NARITYPECOUNT   NUMBER;
                    SARITYPE        ACTRIB.ARITYPE%TYPE;
                BEGIN
                    NOK := 1;

                    BEGIN
                        --GET THE BANK ACCOUNT TYPE
                        BEGIN
                            SELECT ARITYPE
                              INTO SARITYPE
                              FROM ACTRIB
                             WHERE     RIBID = (SELECT MAX (RIBID)
                                                  FROM ACTRIB
                                                 WHERE ACTID = NACTID)
                                   AND ACTID = NACTID;
                        END;

                        --NOW COUNT ALL THE BANK ACCOUNT
                        --THE TEST OF ARIDTREMPLACE IS USE OF THE CHANGE BANK ACCOUNT
                        IF     (   SARITYPE = 'INCACC'
                                OR SARITYPE = 'SCA'
                                OR SARITYPE = 'LTA'
                                OR SARITYPE = 'PROMACC'
                                OR SARITYPE = 'PAYBACC')
                           AND SARITYPE IS NOT NULL
                        THEN
                            SELECT COUNT (*)
                              INTO NARITYPECOUNT
                              FROM ACTRIB
                             WHERE     ARITYPE = SARITYPE
                                   AND ACTID = NACTID
                                   AND (   ARIDTREMPLACE IS NULL
                                        OR ARIDTREMPLACE > SYSDATE);
                        END IF;

                        --TEST OF THE NUMBER OF THE BANK ACCOUNT
                        IF NARITYPECOUNT > 1
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            NOK := 1;
                    END;
                END;
            -- end Check duplicate sales channels bank account LB 05/26/2014
            -- LMI JIRA8170 Risk Analysis Rating 20131117
            ELSIF (SCONTROLE = 'W00RA')
            THEN
                DECLARE
                    NEXISTWLD00   NUMBER := 0;
                    SUGECODE      UTILISATEUR.UGECODE%TYPE
                                      := F_GETCURRENTUGECODE;
                BEGIN
                    SELECT COUNT (1)
                      INTO NEXISTWLD00
                      FROM ACTEUR        ACT,
                           TCOTATION     TCO,
                           ACTTCOVALEUR  ATV,
                           ACTROLE       ARO
                     WHERE     ACT.ACTID = NACTID
                           AND ACT.UGECODE = TCO.UGECODE
                           AND TCO.TCODEST = 'ACTEUR'
                           AND TCO.TCOCODE = 'WLD'
                           AND TCO.UGECODE = SUGECODE
                           AND TCO.TCOID = ATV.TCOID
                           AND ATV.ACTID = ACT.ACTID
                           AND ATV.TVACODE = '00'
                           AND (   ATV.ATVDTFIN IS NULL
                                OR TRUNC (ATV.ATVDTFIN) > TRUNC (SYSDATE))
                           AND ACT.ACTID = ARO.ACTID
                           AND ARO.ROLCODE = 'SUBROG';

                    IF NEXISTWLD00 > 0
                    THEN
                        NEXISTWLD00 := 0;

                        SELECT COUNT (1)
                          INTO NEXISTWLD00
                          FROM ACTEUR ACT, ACTTCOVALEUR ATV, TCOTATION TCO
                         WHERE     ACT.ACTID = NACTID
                               AND ACT.UGECODE = TCO.UGECODE
                               AND TCO.TCODEST = 'ACTEUR'
                               AND TCO.TCOCODE = 'RA'
                               AND TCO.UGECODE = SUGECODE
                               AND TCO.TCOID = ATV.TCOID
                               AND ATV.ACTID = ACT.ACTID
                               AND ATV.TVACODE IS NOT NULL
                               AND (   ATV.ATVDTFIN IS NULL
                                    OR TRUNC (ATV.ATVDTFIN) >=
                                       TRUNC (SYSDATE));

                        IF NEXISTWLD00 = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END;
            -- LMI JIRA8170 Risk Analysis Rating 20131117
            ELSIF SCONTROLE = ('ACSM')
            THEN
                DECLARE
                    SPHASE           AROAGE.PHACODE%TYPE;
                    NACTIDGESTION    VARCHAR2 (10);
                    SROLE            AROAGE.ROLCODE%TYPE;
                    STYPERIB         ACTRIB.ARITYPE%TYPE;
                    SDATEVALIDE      ACTRIB.ARIDTVALIDATE%TYPE;
                    NB_INACCVALIDE   NUMBER;
                    NB_INACC         NUMBER;
                BEGIN
                    NOK := 1;

                    BEGIN
                        -- TO GET THE ROLE AND COMPANY FOR WHICH THE EVENT HAS BEEN LAUNCHED
                        SELECT CRD.CDADATASTRING, CRD2.CDADATASTRING
                          INTO SROLE, NACTIDGESTION
                          FROM CREDATA CRD, CREDATA CRD2
                         WHERE     CRD.CREID = NCREID
                               AND CRD2.CREID = NCREID
                               AND CRD2.CDACOLONNE = 'ACTIDGESTION'
                               AND CRD.CDACOLONNE = 'ROLCODE';

                        --GET THE CURRENT PHASE AFTER EXECUTION OF THE EVENT
                        IF (SROLE IS NOT NULL AND NACTIDGESTION IS NOT NULL)
                        THEN
                            SELECT PHACODE
                              INTO SPHASE
                              FROM AROAGE
                             WHERE     ACTID = NACTID
                                   AND AAGDTFIN IS NULL
                                   AND ACTIDGESTION =
                                       TO_NUMBER (NACTIDGESTION)
                                   AND ROLCODE = SROLE;
                        END IF;

                        IF SPHASE = 'ES'
                        THEN
                            BEGIN
                                --FOR THE ROLE AG AND CM CHECK IF USER HAS THE INCENTIVE ACCOUNT
                                IF    SROLE = 'AG'
                                   OR SROLE = 'CM'
                                   OR SROLE = 'USECARA'
                                THEN
                                    BEGIN
                                        --COUNT ALL THE INCENTIVE TYPE BANK ACCOUNT AND HAS BEEN VALIDATED(VALIDATION DATE NOT NULL).
                                        SELECT COUNT (*)
                                          INTO NB_INACCVALIDE
                                          FROM ACTRIB
                                         WHERE     ACTID = NACTID
                                               AND ARITYPE = 'INCACC'
                                               AND ARIDTVALIDATE IS NOT NULL;
                                    END;

                                    --NOW COUNT ALL THE INCENTIVE BANK ACCOUNT (VALIDATED OR NOT) THIS WAY WE COMPARE THE NUMBERS
                                    --IF SAME THEN HE HAS ONE AND VALIDATED ELSE AT LEASE ONE IS NOT VALIDATED THEN BLOCK
                                    BEGIN
                                        SELECT COUNT (*)
                                          INTO NB_INACC
                                          FROM ACTRIB
                                         WHERE     ACTID = NACTID
                                               AND ARITYPE = 'INCACC';
                                    END;

                                    IF NB_INACCVALIDE > 0
                                    THEN
                                        IF NB_INACCVALIDE = NB_INACC
                                        THEN
                                            NOK := 1;
                                        ELSE
                                            NOK := 0;
                                        END IF;
                                    ELSE
                                        NOK := 0;
                                    END IF;
                                ELSE
                                    IF    SROLE = 'OEM'
                                       OR SROLE = 'OEMAGEN'
                                       OR SROLE = 'OEMSUBA'
                                       OR SROLE = 'AGCOMP'
                                       OR SROLE = 'AGBRCH'
                                       OR SROLE = 'SUBAGBR'
                                    THEN
                                        BEGIN
                                            --COUNT ALL THE INCENTIVE TYPE BANK ACCOUNT AND HAS BEEN VALIDATED(VALIDATION DATE NOT NULL).
                                            SELECT COUNT (*)
                                              INTO NB_INACCVALIDE
                                              FROM ACTRIB
                                             WHERE     ACTID = NACTID
                                                   AND ARITYPE = 'LTA'
                                                   AND ARIDTVALIDATE
                                                           IS NOT NULL;
                                        END;

                                        --NOW COUNT ALL THE INCENTIVE BANK ACCOUNT (VALIDATED OR NOT) THIS WAY WE COMPARE THE NUMBERS
                                        --IF SAME THEN HE HAS ONE AND VALIDATED ELSE AT LEASE ONE IS NOT VALIDATED THEN BLOCK
                                        BEGIN
                                            SELECT COUNT (*)
                                              INTO NB_INACC
                                              FROM ACTRIB
                                             WHERE     ACTID = NACTID
                                                   AND ARITYPE = 'LTA';
                                        END;

                                        IF NB_INACCVALIDE > 0
                                        THEN
                                            IF NB_INACCVALIDE = NB_INACC
                                            THEN
                                                NOK := 1;
                                            ELSE
                                                NOK := 0;
                                            END IF;
                                        ELSE
                                            NOK := 0;
                                        END IF;
                                    END IF;
                                END IF;
                            END;
                        ELSE
                            NOK := 1;
                        END IF;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            NOK := 1;
                    END;
                END;
            -- LMI JIRA8170 Risk Analysis Rating 20131117
            ELSIF (SCONTROLE = 'W00RA')
            THEN
                DECLARE
                    NEXISTWLD00   NUMBER := 0;
                    SUGECODE      UTILISATEUR.UGECODE%TYPE
                                      := F_GETCURRENTUGECODE;
                BEGIN
                    SELECT COUNT (1)
                      INTO NEXISTWLD00
                      FROM ACTEUR        ACT,
                           TCOTATION     TCO,
                           ACTTCOVALEUR  ATV,
                           ACTROLE       ARO
                     WHERE     ACT.ACTID = NACTID
                           AND ACT.UGECODE = TCO.UGECODE
                           AND TCO.TCODEST = 'ACTEUR'
                           AND TCO.TCOCODE = 'WLD'
                           AND TCO.UGECODE = SUGECODE
                           AND TCO.TCOID = ATV.TCOID
                           AND ATV.ACTID = ACT.ACTID
                           AND ATV.TVACODE = '00'
                           AND (   ATV.ATVDTFIN IS NULL
                                OR TRUNC (ATV.ATVDTFIN) > TRUNC (SYSDATE))
                           AND ACT.ACTID = ARO.ACTID
                           AND ARO.ROLCODE = 'SUBROG';

                    IF NEXISTWLD00 > 0
                    THEN
                        NEXISTWLD00 := 0;

                        SELECT COUNT (1)
                          INTO NEXISTWLD00
                          FROM ACTEUR ACT, ACTTCOVALEUR ATV, TCOTATION TCO
                         WHERE     ACT.ACTID = NACTID
                               AND ACT.UGECODE = TCO.UGECODE
                               AND TCO.TCODEST = 'ACTEUR'
                               AND TCO.TCOCODE = 'RA'
                               AND TCO.UGECODE = SUGECODE
                               AND TCO.TCOID = ATV.TCOID
                               AND ATV.ACTID = ACT.ACTID
                               AND ATV.TVACODE IS NOT NULL
                               AND (   ATV.ATVDTFIN IS NULL
                                    OR TRUNC (ATV.ATVDTFIN) >=
                                       TRUNC (SYSDATE));

                        IF NEXISTWLD00 = 0
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END;
            -- LMI JIRA8170 Risk Analysis Rating 20131117
            ---Actor delinquency detected
            ELSIF SCONTROLE = ('ACTDQ')
            THEN
                DECLARE
                    WCINDTCREATED   DATE;
                    NACTID          NUMBER;
                    WPROID          NUMBER;
                BEGIN
                    NOK := 1;

                    --GET PROID
                    BEGIN
                        SELECT PROID
                          INTO WPROID
                          FROM PROCESS
                         WHERE PROCODE = 'RLH_DTRLH0';
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            WPROID := NULL;
                    END;

                    --- Get Last date execution of Interface
                    BEGIN
                        SELECT TRUNC (MAX (CRE.CINDTCREATED))
                          INTO WCINDTCREATED
                          FROM CREDITINFO CRE
                         WHERE CRE.PROID = WPROID AND CRE.ACTID = NACTID;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            WCINDTCREATED := NULL;
                    END;

                    IF    (TRUNC (SYSDATE) <> TRUNC (WCINDTCREATED))
                       OR WCINDTCREATED IS NULL
                    THEN
                        NOK := 0;
                    END IF;
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                END;
            ----Actor delinquency should be checked at the same date of confirmation
            -- LB 2014-01-08 check RBMS Rating for used car agency and used car dealership before activating the role
            --modified 08/20/2014-LB
            ELSIF SCONTROLE = ('CEOGD')
            THEN
                DECLARE
                    NGRADE      ACTTCOVALEUR.TVACODE%TYPE;
                    NPHASE      AROAGE.PHACODE%TYPE;
                    NCEOACTID   ACTEUR.ACTID%TYPE;
                    NDATE       DATE;
                    SUGECODE    UTILISATEUR.UGECODE%TYPE
                                    := F_GETCURRENTUGECODE;
                BEGIN
                    NOK := 1;

                    BEGIN
                        SELECT NVL (MAX (ARO.PHACODE), 'XXX'), ACTR.ACTID
                          INTO NPHASE, NCEOACTID
                          FROM ACTRELATION ACTR, AROAGE ARO
                         WHERE     ACTR.TRECODE = 'REPRE'
                               AND ARO.ACTID = ACTR.ACTIDRELATION
                               --AND ARO.ROLCODE IN ('AGBRCH','USECARA','USCAAGB','AGCOMP','SUBAGBR')
                               AND ARO.ROLCODE IN ('USECARD', 'USECARA')
                               AND ARO.AAGDTFIN IS NULL
                               AND ACTR.ACTIDRELATION = NACTID;

                        BEGIN
                            SELECT ACV.TVACODE, ACV.ATVDTDEB
                              INTO NGRADE, NDATE
                              FROM ACTTCOVALEUR ACV, TCOTATION TCO
                             WHERE     TCO.TCOCODE = 'SLCHGR'
                                   AND TCO.UGECODE = SUGECODE
                                   AND ACV.TCOID = TCO.TCOID
                                   AND ACV.ATVDTFIN IS NULL
                                   AND ACV.ACTID = NCEOACTID;

                            IF (    (   (NGRADE = 'R2')
                                     OR (TRUNC (NDATE) < TRUNC (SYSDATE)))
                                AND NPHASE IN ('XXX', 'ES', 'INI'))
                            THEN
                                NOK := 0;
                            END IF;
                        EXCEPTION
                            WHEN NO_DATA_FOUND
                            THEN
                                NOK := 0;
                        --WHEN OTHERS THEN
                        -- nOk := 1;
                        END;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NOK := 1;
                    END;
                END;
            --  END LB 2014-01-08 check RBMS Rating for used car agency and used car dealership before activating the role
            -- LB 2014-08-20 check RBMS Rating forAG and sales person before activating the role
            ELSIF SCONTROLE = ('SALGD')
            THEN
                DECLARE
                    NGRADE     ACTTCOVALEUR.TVACODE%TYPE;
                    NPHASE     AROAGE.PHACODE%TYPE;
                    NDATE      DATE;
                    SUGECODE   UTILISATEUR.UGECODE%TYPE
                                   := F_GETCURRENTUGECODE;
                BEGIN
                    NOK := 1;

                    BEGIN
                        SELECT NVL (MAX (ARO.PHACODE), 'XXX')
                          INTO NPHASE
                          FROM AROAGE ARO
                         WHERE     ARO.ROLCODE IN ('AG', 'SALAGBR')
                               AND ARO.AAGDTFIN IS NULL
                               AND ARO.ACTID = NACTID;

                        BEGIN
                            SELECT ACV.TVACODE, ACV.ATVDTDEB
                              INTO NGRADE, NDATE
                              FROM ACTTCOVALEUR ACV, TCOTATION TCO
                             WHERE     TCO.TCOCODE = 'SLCHGR'
                                   AND TCO.UGECODE = SUGECODE
                                   AND ACV.TCOID = TCO.TCOID
                                   AND ACV.ATVDTFIN IS NULL
                                   AND ACV.ACTID = NACTID;

                            IF (    (   (NGRADE = 'R2')
                                     OR (TRUNC (NDATE) < TRUNC (SYSDATE)))
                                AND NPHASE IN ('XXX', 'ES', 'INI'))
                            THEN
                                NOK := 0;
                            END IF;
                        EXCEPTION
                            WHEN NO_DATA_FOUND
                            THEN
                                NOK := 0;
                        --WHEN OTHERS THEN
                        -- nOk := 1;
                        END;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            NOK := 1;
                    END;
                END;
            --  END LB 2014-08-20 check RBMS Rating forAG and sales person before activating the role
            --ELSIF sControle = ('CODQA') THEN
            --                                  DECLARE
            --                                                wcindtcreated DATE ;
            --                                                nactidt                    NUMBER ;
            --                                                wproid        NUMBER ;
            --                                                wcinid        NUMBER;
            --                                                nbdelq        NUMBER;
            --
            --                                  BEGIN
            --                                                nOk := 1 ;
            --                                                --GET PROID
            --                                                BEGIN
            --                                                  SELECT proid INTO wproid FROM process WHERE procode = 'RLH_DTRLH0';
            --                                                EXCEPTION
            --                                                WHEN OTHERS THEN
            --                                                  wproid := NULL;
            --                                                END;
            --                                                --- Get ACTID
            --                                                BEGIN
            --               SELECT  ACTID, DEVCODE
            --               INTO    nActIdGestion, sDevCodeDos
            --               FROM   DOSSIER
            --               WHERE  DOSID = nDosId;
            --
            --               SELECT MAX(ACTID)
            --               INTO    nactid
            --               FROM   DOSACTEUR DAC
            --               WHERE DOSID = nDosId
            --               AND     ROLCODE IN ('EMPRUNT', 'CLIENT', 'LOCAT')
            --               AND     (DAC.DACDTFIN IS NULL OR DAC.DACDTFIN > SYSDATE);
            --                                    EXCEPTION
            --                                                WHEN OTHERS THEN
            --                                                  nActIdClient := NULL;
            --                                                END;
            --Jerry Control
            -- ACH 2013-12-18
            ELSIF SCONTROLE = ('TRT')
            THEN
                DECLARE
                    NACTGUARANTEE   COLOBJECT.ACTID%TYPE;
                BEGIN
                    BEGIN
                        SELECT COLOB.ACTID
                          INTO NACTGUARANTEE
                          FROM COLLATERAL  COLL,
                               COLPHASE    COLP,
                               COLOBJECT   COLOB
                         WHERE     COLL.COLID = COLP.COLID
                               AND COLOB.COLID = COLP.COLID
                               AND COLP.PHACODE = 'ACTIVE'
                               AND COLP.CPHDTEND IS NULL
                               AND COLL.TCTCODE IN ('DAM', 'MRE')
                               AND COLL.ACTIDOWNER = NACTID;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            NOK := 1;
                    END;

                    IF NACTGUARANTEE IS NOT NULL
                    THEN
                        SELECT COUNT (1)
                          INTO NCOUNT
                          FROM AROAGE
                         WHERE     ACTID = NACTGUARANTEE
                               AND ROLCODE IN ('USECARA', 'USECARD')
                               AND PHACODE = 'ES'
                               AND AAGDTFIN IS NULL;

                        IF (NCOUNT >= 1)
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    END IF;
                END;
            -- ACH 2013-12-18
            --EVACT_SUBUNITE --Due date condition
            ELSIF SCONTROLE = ('BASUBDD')
            THEN
                BEGIN
                    SELECT DECODE (
                               (  F_GETRCTDUEDATE (NACTID, DAU1.AUNORDRE)
                                - F_GETRCTDUEDATE (NACTID, DAU2.AUNORDRE)),
                               0, 0,
                               1)
                      INTO NOK
                      FROM DOSACTUNITE DAU1, DOSACTUNITE DAU2
                     WHERE     DAU1.ACTID = DAU2.ACTID
                           AND DAU1.DOSID =
                               (SELECT MAX (DAU.DOSID)
                                  FROM DOSACTUNITE DAU, CREVT CRE
                                 WHERE     CRE.CREID = DAU.CREID
                                       AND CRE.TMFFONCTION = 'EVACT_SUBUNITE'
                                       AND DAU.DOSID = DAU1.DOSID
                                       AND DAU.CREID =
                                           (SELECT MAX (CREID)
                                              FROM CREVT
                                             WHERE ACTID = NACTID))
                           AND DAU2.ACTID = NACTID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --EVACT_SUBUNITE --payment method
            ELSIF SCONTROLE = ('BASUBMP')
            THEN
                BEGIN
                    SELECT DECODE (AUN1.TMPCODE, AUN2.TMPCODE, 1, 0)
                      INTO NOK
                      FROM DOSACTUNITE DAU1, ACTUNITE AUN2, ACTUNITE AUN1
                     WHERE     AUN1.ACTID = AUN2.ACTID
                           AND DAU1.ACTID = AUN1.ACTID
                           AND DAU1.CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_SUBUNITE'
                                       AND ACTID = NACTID)
                           AND AUN1.AUNORDRE = DAU1.AUNORDRE
                           AND AUN2.AUNORDRE =
                               (SELECT AUNORDRE
                                  FROM DOSACTUNITE
                                 WHERE     DOSID = DAU1.DOSID
                                       AND ACTID = NACTID
                                       AND DAUORDER = DAU1.DAUORDER - 1)
                           AND AUN1.ACTID = NACTID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --EVACT_SUBUNITE --Bank account
            ELSIF SCONTROLE = ('BASUBRIB')
            THEN
                BEGIN
                    SELECT DECODE (AUN1.RIBID, AUN2.RIBID, 1, 0)
                      INTO NOK
                      FROM DOSACTUNITE DAU1, ACTUNITE AUN2, ACTUNITE AUN1
                     WHERE     AUN1.ACTID = AUN2.ACTID
                           AND DAU1.ACTID = AUN1.ACTID
                           AND DAU1.CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_SUBUNITE'
                                       AND ACTID = NACTID)
                           AND AUN1.AUNORDRE = DAU1.AUNORDRE
                           AND AUN2.AUNORDRE =
                               (SELECT AUNORDRE
                                  FROM DOSACTUNITE
                                 WHERE     DOSID = DAU1.DOSID
                                       AND ACTID = NACTID
                                       AND DAUORDER = DAU1.DAUORDER - 1)
                           AND AUN1.ACTID = NACTID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --EVACT_SUBUNITE --address
            ELSIF SCONTROLE = ('BASUBADR')
            THEN
                BEGIN
                    SELECT DECODE (AUN1.AADORDRE, AUN2.AADORDRE, 1, 0)
                      INTO NOK
                      FROM DOSACTUNITE DAU1, ACTUNITE AUN2, ACTUNITE AUN1
                     WHERE     AUN1.ACTID = AUN2.ACTID
                           AND DAU1.ACTID = AUN1.ACTID
                           AND DAU1.CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_SUBUNITE'
                                       AND ACTID = NACTID)
                           AND AUN1.AUNORDRE = DAU1.AUNORDRE
                           AND AUN2.AUNORDRE =
                               (SELECT AUNORDRE
                                  FROM DOSACTUNITE
                                 WHERE     DOSID = DAU1.DOSID
                                       AND ACTID = NACTID
                                       AND DAUORDER = DAU1.DAUORDER - 1)
                           AND AUN1.ACTID = NACTID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            --EVACT_SUBUNITE --Sending method
            ELSIF SCONTROLE = ('BASUBSD')
            THEN
                BEGIN
                    SELECT DECODE (AUN1.AUNNOTICEMETHODSEND,
                                   AUN2.AUNNOTICEMETHODSEND, 1,
                                   0)
                      INTO NOK
                      FROM DOSACTUNITE DAU1, ACTUNITE AUN2, ACTUNITE AUN1
                     WHERE     AUN1.ACTID = AUN2.ACTID
                           AND DAU1.ACTID = AUN1.ACTID
                           AND DAU1.CREID =
                               (SELECT MAX (CREID)
                                  FROM CREVT
                                 WHERE     TMFFONCTION = 'EVACT_SUBUNITE'
                                       AND ACTID = NACTID)
                           AND AUN1.AUNORDRE = DAU1.AUNORDRE
                           AND AUN2.AUNORDRE =
                               (SELECT AUNORDRE
                                  FROM DOSACTUNITE
                                 WHERE     DOSID = DAU1.DOSID
                                       AND ACTID = NACTID
                                       AND DAUORDER = DAU1.DAUORDER - 1)
                           AND AUN1.ACTID = NACTID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            ----- MAH - AIL : Debut controle sur adresse facturation et si?ge pour client
            ELSIF SCONTROLE = 'ACTAD'
            THEN
                BEGIN
                    DECLARE
                        nCount    NUMBER := 0;
                        nCount1   NUMBER := 0;
                        nCount2   NUMBER := 0;
                        NSTATUT   ACTPHASE.JALCODE%TYPE;
                        NPHASE    ACTPHASE.PHACODE%TYPE;
                    BEGIN
                        BEGIN
                            --v??rifier si l'acteur a le r??le client
                            SELECT P.JALCODE
                              INTO NSTATUT
                              FROM ACTEUR A, ACTPHASE P
                             WHERE     A.ACTID = NACTID
                                   AND A.ACTID = P.ACTID
                                   AND APHDTFIN IS NULL;

                            SELECT P.PHACODE
                              INTO NPHASE
                              FROM ACTEUR A, ACTPHASE P
                             WHERE     A.ACTID = NACTID
                                   AND A.ACTID = P.ACTID
                                   AND APHDTFIN IS NULL;

                            SELECT COUNT (*)
                              INTO nCount
                              FROM actrole
                             WHERE actid = nActId AND rolcode = 'CLIENT';

                            IF nCount = 0
                            THEN
                                nok := 1;
                            ELSE
                                SELECT COUNT (*)
                                  INTO nCount1
                                  FROM actadresse
                                 WHERE     actid = nActId
                                       AND NVL (aadflagfacturation, 0) = 1;

                                SELECT COUNT (*)
                                  INTO nCount2
                                  FROM actadresse
                                 WHERE     actid = nActId
                                       AND NVL (aadflagsiege, 0) = 1;

                                IF nCount1 * nCount2 = 0
                                THEN
                                    IF    NSTATUT = 'PROSPC'
                                       OR NPHASE = 'ACTIVE'
                                    THEN
                                        nok := 0;
                                    ELSE
                                        NOK := 1;
                                    END IF;
                                ELSE
                                    nok := 1;
                                END IF;
                            END IF;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                nok := 0;
                        END;
                    END;
                END;
            ----- MAH - AIL : Fin controle sur adresse facturation et si?ge pour client
            ---- MAH - AIL : Debut controle R?f?rence t?l?com obligatoire pour un CLIENT de phase INI et de jalon OUVERT
            ELSIF SCONTROLE = ('RFTEL')
            THEN
                DECLARE
                    NSTATUT        ACTPHASE.JALCODE%TYPE;
                    NCLIENT        NUMBER;
                    NREFTELCOUNT   NUMBER;
                BEGIN
                    SELECT COUNT (*)
                      INTO NCLIENT
                      FROM ACTROLE
                     WHERE ACTID = NACTID AND ROLCODE = 'CLIENT';

                    SELECT COUNT (*)
                      INTO NREFTELCOUNT
                      FROM ACTTELECOM
                     WHERE ATETYPE IN ('MOB', 'TEL') AND ACTID = NACTID;

                    SELECT P.JALCODE
                      INTO NSTATUT
                      FROM ACTEUR A, ACTPHASE P
                     WHERE     A.ACTID = NACTID
                           AND A.ACTID = P.ACTID
                           AND P.APHDTFIN IS NULL;

                    -- D??clencher l'erreur s'il s'agit d'un CLIENT et qu'il n'a pas de r??f??rence t??l??com MOB ou TEL
                    IF     NCLIENT = 1
                       AND NSTATUT = 'OUVERT'
                       AND NREFTELCOUNT = 0
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle R?f?rence t?l?com obligatoire pour un CLIENT de phase INI et de jalon OUVERT
            ---- MAH - AIL : Debut controle Format du Matricule Fiscal PP si renseigne
            ELSIF SCONTROLE = ('FRMMF')
            THEN
                DECLARE
                    NACTTYPE          ACTEUR.ACTTYPE%TYPE := NULL;
                    NMTRFISCPP        CCHVALUE.CVASTRINGVALUE%TYPE := NULL;
                    NLENGTHMTRFISC    NUMBER := NULL;
                    NCHAINEVERIFCLE   VARCHAR2 (26)
                                          := 'ABCDEFGHJKLMNPQRSTVWXYZ';
                    NTOTALVERIFCLE    NUMBER := 0;
                    NINDXVERIFCLE     NUMBER;
                    NCLE              VARCHAR2 (1);
                    nCount            NUMBER := NULL;
                BEGIN
                    SELECT ACTTYPE
                      INTO NACTTYPE
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    SELECT COUNT (*)
                      INTO nCount
                      FROM CCHVALUE
                     WHERE CCHSID = 'TFDCCHSID63224' AND ACTID = NACTID;

                    IF nCount <> 0
                    THEN
                        SELECT CVASTRINGVALUE, LENGTH (CVASTRINGVALUE)
                          INTO NMTRFISCPP, NLENGTHMTRFISC
                          FROM CCHVALUE
                         WHERE CCHSID = 'TFDCCHSID63224' AND ACTID = NACTID;

                        IF NACTTYPE = 'PART' AND NMTRFISCPP IS NOT NULL
                        THEN
                            IF    NLENGTHMTRFISC != 13
                               OR NLENGTHMTRFISC != LENGTH (NMTRFISCPP)
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NMTRFISCPP, 1, 7),
                                              '0123456789',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NMTRFISCPP, 9, 1),
                                              'ABDFMNP',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NMTRFISCPP, 10, 1),
                                              'MCPNE',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NMTRFISCPP, 11, 3),
                                              '0123456789',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSE
                                FOR ITERATOR IN 1 .. 7
                                LOOP
                                    NINDXVERIFCLE := 8 - ITERATOR;
                                    NTOTALVERIFCLE :=
                                          NTOTALVERIFCLE
                                        +   SUBSTR (NMTRFISCPP, ITERATOR, 1)
                                          * NINDXVERIFCLE;
                                END LOOP;

                                NINDXVERIFCLE := MOD (NTOTALVERIFCLE, 23) + 1;
                                NCLE := SUBSTR (NMTRFISCPP, 8, 1);

                                IF NCLE <>
                                   SUBSTR (NCHAINEVERIFCLE, NINDXVERIFCLE, 1)
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle Format du Matricule Fiscal PP si renseigne
            ---- MAH - AIL : Debut controle Format du num?ro d'identifiant
            ELSIF SCONTROLE = ('FRMID')
            THEN
                DECLARE
                    NACTTYPE          ACTEUR.ACTTYPE%TYPE;
                    NIDTYPE           CCHVALUE.CVASTRINGVALUE%TYPE;
                    NACTSIRET         ACTEUR.ACTSIRET%TYPE;
                    NASLENGTH         NUMBER;
                    NCHAINEVERIFCLE   VARCHAR2 (26)
                                          := 'ABCDEFGHJKLMNPQRSTVWXYZ';
                    NTOTALVERIFCLE    NUMBER := 0;
                    NINDXVERIFCLE     NUMBER;
                    NCJUCODE          ACTEUR.CJUCODE%TYPE := NULL; --AIL_UAT_026
                    NCLE              VARCHAR2 (1);
                BEGIN
                    SELECT ACTSIRET, LENGTH (ACTSIRET), CJUCODE
                      INTO NACTSIRET, NASLENGTH, NCJUCODE
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    SELECT ACTTYPE
                      INTO NACTTYPE
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    IF NACTTYPE = 'PART'
                    THEN
                        SELECT CVASTRINGVALUE
                          INTO NIDTYPE
                          FROM CCHVALUE
                         WHERE CCHSID = 'CMBCCHSID63211' AND ACTID = NACTID;

                        IF NIDTYPE = 'CIN'
                        THEN
                            IF NASLENGTH != 8
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (NACTSIRET,
                                                     '0123456789',
                                                     ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            END IF;
                        END IF;
                    ELSE -------> Algorithme verification Matricule Fiscal Tunisien
                        IF NCJUCODE NOT IN ('9999', '7000')
                        THEN                                     --AIL_UAT_026
                            IF    NASLENGTH != 13
                               OR NASLENGTH != LENGTH (TRIM (NACTSIRET))
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NACTSIRET, 1, 7),
                                              '0123456789',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NACTSIRET, 9, 1),
                                              'ABDFMNP',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NACTSIRET, 10, 1),
                                              'MCPNE',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSIF LENGTH (
                                      TRIM (
                                          TRANSLATE (
                                              SUBSTR (NACTSIRET, 11, 3),
                                              '0123456789',
                                              ' ')))
                                      IS NOT NULL
                            THEN
                                NOK := 0;
                            ELSE
                                FOR ITERATOR IN 1 .. 7
                                LOOP
                                    NINDXVERIFCLE := 8 - ITERATOR;
                                    NTOTALVERIFCLE :=
                                          NTOTALVERIFCLE
                                        +   SUBSTR (NACTSIRET, ITERATOR, 1)
                                          * NINDXVERIFCLE;
                                END LOOP;

                                NINDXVERIFCLE := MOD (NTOTALVERIFCLE, 23) + 1;
                                NCLE := SUBSTR (NACTSIRET, 8, 1);

                                IF NCLE <>
                                   SUBSTR (NCHAINEVERIFCLE, NINDXVERIFCLE, 1)
                                THEN
                                    NOK := 0;
                                END IF;
                            END IF;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle Format du num?ro d'identifiant
            ---- MAH - AIL : Debut controle Num?ro d'identifiant obligatoire
            ELSIF SCONTROLE = ('IDOBL')
            THEN
                DECLARE
                    NACTSIRET   ACTEUR.ACTSIRET%TYPE := NULL;
                    NCOUNT      NUMBER := NULL;
                BEGIN
                    SELECT COUNT (*)
                      INTO NCOUNT
                      FROM ACTROLE
                     WHERE     ACTID = NACTID
                           AND ROLCODE IN ('AVOCT',
                                           'CLIENT',
                                           'FOURN',
                                           'GARANT',
                                           'EMPRUNT',
                                           'GERANT',
                                           'NOTAIRE',
                                           'REP',
                                           'SDG',
                                           'EXP',
                                           'CONS',
                                           'ASSUR',
                                           'LOCA');

                    IF NCOUNT <> 0
                    THEN
                        SELECT ACTSIRET
                          INTO NACTSIRET
                          FROM ACTEUR
                         WHERE ACTID = NACTID;

                        IF NACTSIRET IS NULL
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;

                ---- MAH - AIL : Fin controle Num?ro d'identifiant obligatoire
                ---- MAH - AIL : Debut controle Num?ro d'identifiant unique
                IF sControle = 'AGEN'
                THEN
                    DECLARE
                        l_grocode   UTILISATEUR.GROCODE%TYPE;
                    BEGIN
                        SELECT COUNT (*)
                          INTO l_count
                          FROM ACTROLE
                         WHERE     ACTID = nActId
                               AND rolcode IN ('AGENCE', 'PARTEN', 'CLIENT');

                        IF (l_count > 0)
                        THEN
                            SELECT grocode
                              INTO l_grocode
                              FROM CREVT c, utilisateur u
                             WHERE     c.CREID = nCreId
                                   AND c.uticodecreat = u.uticode;

                            IF (l_grocode = 'GRPORFI')
                            THEN
                                nok := 1;
                            ELSE
                                nok := 0;
                            END IF;
                        END IF;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            nok := 0;                       -- Erreur donc KO.
                    END;
                /*ELSIF scontrole IN ('SCIN') THEN -- CONTROLE MIGRE, MTR 28102013 SCIN= SAISI CIN CORRECTE SANS CARACTERE SPECIAUX OU ESPACE
                DECLARE
                   nCIN   varchar2(100);
                   nTEST1 number;
                   nTEST2 number;
                   nTest3 number;
                BEGIN
                select max(cch.cvastringvalue) into nCIN
                from cchvalue cch
                where cch.cchsid='TFDCCHSID1676'
                and substr(cvapkeyvalue,instr(cvapkeyvalue,'-')+1,instr(cvapkeyvalue,'|')-7)=nActid
                and ltrim(cvastringvalue) is not null;

                IF nCIN IS NULL THEN
                 nOk:=1;
                ELSE
                SELECT instr(nCIN,' ',1) into nTEST1 FROM DUAL;

                  IF nTEST1 >0 THEN
                  nOk:=0;
                  ELSE
                    SELECT instr(nCIN,'.',1) into nTEST2 FROM DUAL;
                    IF nTEST2 >0 THEN
                    nOk:=0;
                    ELSE
                      nTest3:=isNumeric(nCIN);
                      IF nTest3=0 THEN
                      nOk:=0;
                      ELSE
                      nOk:=1;
                      END IF; --TEST SUR ESPACE
                    END IF; --TEST SUR LE POINT
                   END IF;--TEST SI ISNUMERIC
                END IF; --TEST SI CIN RENSEIGNE, SI NON RENSEIGNE, CONTROLE NE SE DECLENCHE PAS
                    EXCEPTION
                         WHEN OTHERS
                         THEN
                            nok := 0;                       -- Erreur donc KO.
                      END;*/


                END IF;
            ELSIF SCONTROLE = ('IDUNQ')
            THEN
                DECLARE
                    NACTTYPE    ACTEUR.ACTTYPE%TYPE := NULL;
                    NIDTYPE     CCHVALUE.CVASTRINGVALUE%TYPE := NULL;
                    NACTSIRET   ACTEUR.ACTSIRET%TYPE := NULL;
                    NCOUNT      NUMBER := NULL;
                BEGIN
                    SELECT ACTTYPE
                      INTO NACTTYPE
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    SELECT ACTSIRET
                      INTO NACTSIRET
                      FROM ACTEUR
                     WHERE ACTID = NACTID;

                    IF NACTTYPE = 'PART'
                    THEN
                        SELECT CVASTRINGVALUE
                          INTO NIDTYPE
                          FROM CCHVALUE
                         WHERE CCHSID = 'CMBCCHSID63211' AND ACTID = NACTID;

                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM ACTEUR A, CCHVALUE C
                         WHERE     A.ACTID = C.ACTID
                               AND A.ACTSIRET = NACTSIRET
                               AND C.CVASTRINGVALUE = NIDTYPE;

                        IF NCOUNT > 1
                        THEN
                            NOK := 0; --D??clenchement du contr?le : Il y a plusieurs acteurs avec le m??me couple (id, typeId)
                        ELSE
                            NOK := 1;
                        END IF;
                    ELSE
                        SELECT COUNT (*)
                          INTO NCOUNT
                          FROM ACTEUR
                         WHERE ACTTYPE = 'PM' AND ACTSIRET = NACTSIRET;

                        IF NCOUNT > 1
                        THEN
                            NOK := 0; --D??clenchement du contr?le : Il y a plusieurs acteurs avec le m??me id
                        ELSE
                            NOK := 1;
                        END IF;
                    END IF;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 0;
                END;
            ---- MAH - AIL : Fin controle Num?ro d'identifiant unique

            END IF;

            ----------- Debut des controles OCACTEUR CDMLF ajoutes :
            -- STG 05112010
            -- EBM controle 4.0
            IF sControle = ('SGM11')
            THEN
                --            nOK := 1;
                BEGIN /* Recuperation de la categorie juridique pour les clients et les actionnaires */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    /* Recherche de la valeur de la carte d'identite */
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sIdentityNum
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='IDENTITYCARDNU';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'IDENTITYCARDNU',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sIdentityNum
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sIdentityNum := NULL;
                    END;

                    IF sIdentityNum IS NOT NULL
                    THEN
                        /* Test de la valeur de la date de delivrance */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityDel
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='IDENTITYCALIVERYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'IDENTITYCALIVERYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityDel
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityDel := NULL;
                        END;

                        /* Test de la date d'expiration */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityExp
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='TFDCCHVAL65';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'TFDCCHVAL65',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityExp
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityExp := NULL;
                        END;

                        /* Test du pays de delivrance */
                        BEGIN
                            /*SELECT CVASTRINGVALUE
                            INTO   sIdentityPays
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='IDENTITYCAERYCOUNTRY';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'IDENTITYCAERYCOUNTRY',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO sIdentityPays
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sIdentityPays := NULL;
                        END;

                        IF    (sIdentityPays IS NULL)
                           OR (dIdentityExp IS NULL)
                           OR (dIdentityDel IS NULL)
                           OR (TO_DATE (dIdentityDel) >
                               TO_DATE (dIdentityExp))
                        THEN
                            nOk := 0;
                        --                      RETURN (nOk);
                        END IF;
                    END IF;                 -- Fin Test sur numero d'indeitite
                END IF;                        -- Fin Test Categorie Juridique
            END IF;                                      -- Fin Controle SGM11

            -- STG 07112010
            IF sControle = ('SGM12')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    /* Recherche de la valeur du passeport */
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sIdentityNum
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='PASSPORTNUPORTNUMBER';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'PASSPORTNUPORTNUMBER',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sIdentityNum
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sIdentityNum := NULL;
                    END;

                    IF sIdentityNum IS NOT NULL
                    THEN
                        /* Test de la valeur de la date de delivrance */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityDel
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='PASSPORTDELIVERYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'PASSPORTDELIVERYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityDel
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityDel := NULL;
                        END;

                        /* Test de la date d'expiration */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityExp
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='PASSPORTVALIDITYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'PASSPORTVALIDITYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityExp
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityExp := NULL;
                        END;

                        /* Test du pays de delivrance */
                        BEGIN
                            /*SELECT CVASTRINGVALUE
                            INTO   sIdentityPays
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='PASSPORTDEERYCOUNTRY';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'PASSPORTDEERYCOUNTRY',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO sIdentityPays
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sIdentityPays := NULL;
                        END;

                        IF    (sIdentityPays IS NULL)
                           OR (dIdentityExp IS NULL)
                           OR (dIdentityDel IS NULL)
                           OR (TO_DATE (dIdentityDel) >
                               TO_DATE (dIdentityExp))
                        THEN
                            nOk := 0;
                        --                      RETURN (nOk);
                        END IF;
                    END IF;                -- Fin Test sur numero de passeport
                END IF;                        -- Fin Test Categorie Juridique
            END IF;                                      -- Fin Controle SGM12


            -- STG 07112010
            IF sControle = ('SGM13')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    /* Recherche de la valeur de la carte diplomatique */
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sIdentityNum
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='DIPLOMATICCARDNUMBER';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'DIPLOMATICCARDNUMBER',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sIdentityNum
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sIdentityNum := NULL;
                    END;

                    IF sIdentityNum IS NOT NULL
                    THEN
                        /* Test de la valeur de la date de delivrance */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityDel
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='DIPLOMATICLIVERYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'DIPLOMATICLIVERYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityDel
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityDel := NULL;
                        END;

                        /* Test de la date d'expiration */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityExp
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='DIPLOMATICLIDITYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'DIPLOMATICLIDITYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityExp
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityExp := NULL;
                        END;

                        /* Test du pays de delivrance */
                        BEGIN
                            /*SELECT CVASTRINGVALUE
                            INTO   sIdentityPays
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='DIPLOMATICERYCOUNTRY';*/


                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'DIPLOMATICERYCOUNTRY',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO sIdentityPays
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sIdentityPays := NULL;
                        END;

                        IF    (sIdentityPays IS NULL)
                           OR (dIdentityExp IS NULL)
                           OR (dIdentityDel IS NULL)
                           OR (TO_DATE (dIdentityDel) >
                               TO_DATE (dIdentityExp))
                        THEN
                            nOk := 0;
                        --                      RETURN (nOk);
                        END IF;
                    END IF;       -- Fin Test sur numero de carte diplomatique
                END IF;                        -- Fin Test Categorie Juridique
            END IF;                                      -- Fin Controle SGM13

            -- STG 07112010
            IF sControle = ('SGM14')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    /* Recherche de la valeur du numero de carte de sejour */
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sIdentityNum
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESIDENTSPRMITNUMBER';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'RESIDENTSPRMITNUMBER',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sIdentityNum
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sIdentityNum := NULL;
                    END;

                    IF sIdentityNum IS NOT NULL
                    THEN
                        /* Test de la valeur de la date de delivrance */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityDel
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESIDENTSPLIVERYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'RESIDENTSPLIVERYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityDel
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityDel := NULL;
                        END;

                        /* Test de la date d'expiration */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dIdentityExp
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESIDENTSPLIDITYDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'RESIDENTSPLIDITYDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dIdentityExp
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dIdentityExp := NULL;
                        END;

                        /* Test du pays de delivrance */
                        BEGIN
                            /*SELECT CVASTRINGVALUE
                            INTO   sIdentityPays
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESIDENTSPERYCOUNTRY';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'RESIDENTSPERYCOUNTRY',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO sIdentityPays
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sIdentityPays := NULL;
                        END;

                        IF    (sIdentityPays IS NULL)
                           OR (dIdentityExp IS NULL)
                           OR (dIdentityDel IS NULL)
                           OR (TO_DATE (dIdentityDel) >
                               TO_DATE (dIdentityExp))
                        THEN
                            nOk := 0;
                        --                      RETURN (nOk);
                        END IF;
                    END IF;          -- Fin Test sur numero de carte de sejour
                END IF;                        -- Fin Test Categorie Juridique
            END IF;                                      -- Fin Controle SGM14

            -- STG 07112010
            IF sControle = ('SGM15')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sClientNature
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='CLIENTNATUIENTNATURE';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'CLIENTNATUIENTNATURE',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sClientNature
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sClientNature := NULL;
                    END;

                    IF (sClientNature = '2')
                    THEN
                        /* Test sur la patente */
                        BEGIN
                            /*SELECT CVASTRINGVALUE
                            INTO   sPatente
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESIDENTSPERYCOUNTRY';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'RESIDENTSPERYCOUNTRY',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO sPatente
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sPatente := NULL;
                        END;

                        /* Test sur l'identifiant fiscal et registre du commerce*/
                        BEGIN
                            SELECT ACTSIRET, ACTNUMRCM
                              INTO sIdentifiantFiscal, sRegComm
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActId;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                sIdentifiantFiscal := NULL;
                                sRegComm := NULL;
                        END;

                        IF     (sPatente IS NULL)
                           AND (sIdentifiantFiscal IS NULL)
                           AND (sRegComm IS NULL)
                        THEN
                            nOk := 0;
                        --                        RETURN (nOk);
                        END IF;
                    END IF;                           -- Fin client nature = 2
                END IF;                                      -- fin Legal type
            END IF;                                      -- Fin Controle SGM15

            -- STG 07112010
            IF sControle = ('SGM16')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('P', 'EI')
                THEN
                    BEGIN
                        /* SELECT CVASTRINGVALUE
                         INTO   sClientNature
                         FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                         WHERE  CCHVALUE.ACTID = nActId
                         AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                         AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='CLIENTNATUIENTNATURE';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'CLIENTNATUIENTNATURE',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sClientNature
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sClientNature := NULL;
                    END;

                    IF (sClientNature = '2')
                    THEN
                        /* Test sur la date de creation  de l'entreprise */
                        BEGIN
                            /*SELECT CVADTVALUE
                            INTO   dCreationComp
                            FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                            WHERE  CCHVALUE.ACTID = nActId
                            AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                            AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='COMPANYCREEATIONDATE';*/

                            /* STG 07/12/2010 */
                            SELECT f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'COMPANYCREEATIONDATE',
                                       nActid,
                                       ACTEUR.LANCODE)
                              INTO dCreationComp
                              FROM ACTEUR
                             WHERE ACTEUR.ACTID = nActid;
                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                dCreationComp := NULL;
                        END;

                        IF (dCreationComp IS NULL)
                        THEN
                            nOk := 0;
                        --                      RETURN (nOk);
                        END IF;
                    END IF;                           -- Fin client nature = 2
                END IF;                                      -- fin Legal type
            END IF;                                      -- Fin Controle SGM16

            -- STG 07112010
            IF sControle = ('SGM17')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND ACTEUR.CJUCODE IN ('01',
                                                  '02',
                                                  '03',
                                                  '10',
                                                  '13',
                                                  '15',
                                                  '27',
                                                  '28')
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType = 'E'
                THEN
                    /* Test sur registre de commerce */
                    BEGIN
                        SELECT ACTNUMRCM
                          INTO sRegComm
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActId;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sRegComm := NULL;
                    END;

                    IF (sRegComm IS NULL)
                    THEN
                        nOk := 0;
                    --                   RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM17

            -- STG 07112010
            IF sControle = ('SGM18')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType = 'E'
                THEN
                    /* Test sur la taille de l'entreprise*/
                    BEGIN
                        /*  SELECT CVASTRINGVALUE
                          INTO   sTailleEntreprise
                          FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                          WHERE  CCHVALUE.ACTID = nActId
                          AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                          AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='COMPANYTYPOMPANYTYPE'
                          AND    CCHVALUE.CVAID = (SELECT MAX(CVAID) FROM CCHVALUE*/

                        /* STG 06/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'COMPANYTYPOMPANYTYPE',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sTailleEntreprise
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sTailleEntreprise := NULL;
                    END;

                    IF    (sTailleEntreprise IS NULL)
                       OR (sTailleEntreprise = 'AUCUN')
                    THEN
                        nOk := 0;
                    --                   RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM18

            IF sControle = ('SGM19')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType = 'E'
                THEN
                    BEGIN
                        SELECT ADRVILLE, ADRMSACODE
                          INTO sVille, sLocaliteCode
                          FROM ACTADRESSE, ADRESSE
                         WHERE     ACTADRESSE.ADRID = ADRESSE.ADRID
                               AND ACTADRESSE.ACTID = nActId
                               AND ROWNUM = 1;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sVille := NULL;
                            sLocaliteCode := NULL;
                    END;

                    IF (sVille IS NULL) OR (sLocaliteCode IS NULL)
                    THEN
                        nOk := 0;
                    --                  RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM19

            IF sControle = ('SGM20')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('E', 'EI')
                THEN
                    /* Recuperation duree de l'exercice fiscal */
                    BEGIN
                        /*SELECT NVL(CVANUMERICVALUE,0)
                        INTO   nDureeFisc
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='FISCALYEARARDURATION';*/

                        /* STG 07/12/2010 */
                        SELECT NVL (
                                   f_PlCROGetCustomCharacteristic (
                                       'ACTEUR',
                                       'FISCALYEARARDURATION',
                                       nActid,
                                       ACTEUR.LANCODE),
                                   0)
                          INTO nDureeFisc
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            nDureeFisc := 0;
                    END;

                    IF (nDureeFisc = 0) OR (nDureeFisc > 12)
                    THEN
                        nOk := 0;
                    --                   RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM20

            IF sControle = ('SGM21')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('E', 'EI')
                THEN
                    /* Recuperation Type de resultat */
                    BEGIN
                        /*SELECT CVASTRINGVALUE
                        INTO   sTypeResult
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='RESULTTYPERESULTTYPE';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'RESULTTYPERESULTTYPE',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO sTypeResult
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            sTypeResult := 0;
                    END;

                    IF (sTypeResult IS NULL) OR (sTypeResult = 'AUCUN')
                    THEN
                        nOk := 0;
                    --                   RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM21

            IF sControle = ('SGM22')
            THEN
                --            nOK := 1;
                BEGIN             /* Recuperation de la categorie juridique */
                    SELECT CJUTYPE
                      INTO sLegalType
                      FROM ACTEUR, CATJURIDIQUE, ACTROLE
                     WHERE     ACTEUR.ACTID = nActId
                           AND ACTEUR.CJUCODE = CATJURIDIQUE.CJUCODE
                           AND CATJURIDIQUE.PAYCODE =
                               ACTEUR.PAYCODECATJURIDIQUE
                           AND ACTROLE.ACTID = ACTEUR.ACTID
                           AND ACTROLE.ROLCODE IN ('CLIENT', 'ACTION');
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        sLegalType := NULL;
                END;

                IF sLegalType IN ('E', 'EI')
                THEN
                    /* Date de debut d'exercice */
                    BEGIN
                        /*SELECT CVADTVALUE
                        INTO   dDebExercice
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='DATEOFTHEFEFIRSTTERM';*/

                        /* STG 07/12/2010*/
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'DATEOFTHEFEFIRSTTERM',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO dDebExercice
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            dDebExercice := NULL;
                    END;

                    /* Test sur la date de creation  de l'entreprise */
                    BEGIN
                        /*SELECT CVADTVALUE
                        INTO   dCreationComp
                        FROM   CCHVALUE,CUSTOMCHARACTERISTIC
                        WHERE  CCHVALUE.ACTID = nActId
                        AND    CCHVALUE.CCHSID =  CUSTOMCHARACTERISTIC.CCHSID
                        AND    CUSTOMCHARACTERISTIC.CCHVALUECODE='COMPANYCREEATIONDATE';*/

                        /* STG 07/12/2010 */
                        SELECT f_PlCROGetCustomCharacteristic (
                                   'ACTEUR',
                                   'COMPANYCREEATIONDATE',
                                   nActid,
                                   ACTEUR.LANCODE)
                          INTO dCreationComp
                          FROM ACTEUR
                         WHERE ACTEUR.ACTID = nActid;
                    EXCEPTION
                        WHEN OTHERS
                        THEN
                            dCreationComp := NULL;
                    END;

                    IF (dDebExercice < dCreationComp)
                    THEN
                        nOk := 0;
                    --                   RETURN (nOk);
                    END IF;
                END IF;                                      -- End Legal Type
            END IF;                                     -- End Control U_SGM22

             -----------------HME SOGELEASE --------
          IF sControle = ('SOG1')
            THEN
                --            nOK := 1;
                BEGIN 
                     SELECT ACTTYPE
                      INTO Sacttype
                      FROM ACTEUR, ACTROLE
                     WHERE     ACTEUR.ACTID = nactid
                     AND ACTROLE.ACTID = ACTEUR.ACTID
                     AND ACTROLE.ROLCODE IN ('CLIENT');

                        EXCEPTION
                            WHEN OTHERS
                            THEN
                                Sacttype := NULL;
                        END;

                IF Sacttype IN ('E')
                THEN
                    /* Recherche les customchars */
                     BEGIN
                             SELECT COUNT (*)
                               INTO l_count
                               FROM cchvalue cch
                              WHERE     cch.cchsid = 'TFDCCHVAL2000'
                                    AND cch.cvastringvalue IN
                                           (SELECT cvastringvalue
                                              FROM cchvalue
                                             WHERE     SUBSTR (
                                                          cvapkeyvalue,
                                                          INSTR (cvapkeyvalue, '-') + 1,
                                                          INSTR (cvapkeyvalue, '|') - 7) =
                                                          nActid
                                                   AND cchsid = 'TFDCCHVAL2000'
                                                   AND LTRIM (cvastringvalue) IS NOT NULL);



                             IF l_count > 1
                             THEN
                                nok := 0;
                             ELSE
                                nok := 1;
                             END IF;
                   END;
                 END IF;                 -- Fin Test sur unicite patente

            END IF;  
            ----------------------------------------------------------------------
            IF sControle = ('SOG2')

     THEN
                --            nOK := 1;
                BEGIN 
                   SELECT ACTTYPE
                      INTO Sacttype
                      FROM ACTEUR, ACTROLE
                     WHERE     ACTEUR.ACTID = nactid
                     AND ACTROLE.ACTID = ACTEUR.ACTID
                     AND ACTROLE.ROLCODE IN ('CLIENT');

                EXCEPTION
                    WHEN OTHERS
                    THEN
                        Sacttype := NULL;
                END;


                IF Sacttype IN ('E','P')
                then
                BEGIN

                SELECT COUNT (*)
                           INTO l_count
                           FROM cchvalue cch
                          WHERE     cch.cchsid = 'TFDCCHSID1570'
                                AND cch.cvastringvalue IN
                                       (SELECT cvastringvalue
                                          FROM cchvalue
                                         WHERE     actid=
                                                      nActid
                                               AND cchsid = 'TFDCCHSID1570'
                                               AND LTRIM (cvastringvalue) IS NOT NULL); 


                             IF l_count > 1
                             THEN
                                nok := 0;
                             ELSE
                                nok := 1;
                             END IF;
            END;
                    END IF;                 -- Fin Test sur unicite Identifiant Fiscal

            END IF;  

            ----------------------------------------
             IF sControle = ('SOG3')
            THEN
                --            nOK := 1;
                BEGIN 
                    SELECT ACTTYPE
                      INTO Sacttype
                      FROM ACTEUR, ACTROLE
                     WHERE     ACTEUR.ACTID = nactid
                     AND ACTROLE.ACTID = ACTEUR.ACTID
                     AND ACTROLE.ROLCODE IN ('CLIENT');

                EXCEPTION
                    WHEN OTHERS
                    THEN
                        Sacttype := NULL;
                END;

                IF Sacttype IN ('E')
                THEN
                    /* Recherche les customchars */
                     BEGIN
                             SELECT COUNT (*)
                               INTO l_count
                               FROM cchvalue cch
                              WHERE     cch.cchsid = 'TFDCCHSID681'
                                    AND cch.cvastringvalue IN
                                           (SELECT cvastringvalue
                                              FROM cchvalue
                                             WHERE     SUBSTR (
                                                          cvapkeyvalue,
                                                          INSTR (cvapkeyvalue, '-') + 1,
                                                          INSTR (cvapkeyvalue, '|') - 7) =
                                                          nActid
                                                   AND cchsid = 'TFDCCHSID681'
                                                   AND LTRIM (cvastringvalue) IS NOT NULL);



                             IF l_count > 1
                             THEN
                                nok := 0;
                             ELSE
                                nok := 1;
                             END IF;
            END;
                    END IF;                 -- Fin Test sur unicite Identifiant Fiscal

            END IF;  
            -------------------------------Pour Personne physique
            -----------------HME SOGELEASE --------
       IF sControle = ('SOG4')
            THEN
                --            nOK := 1;
                BEGIN 
                    SELECT ACTTYPE
                      INTO Sacttype
                      FROM ACTEUR, ACTROLE
                     WHERE     ACTEUR.ACTID = nactid
                     AND ACTROLE.ACTID = ACTEUR.ACTID
                     AND ACTROLE.ROLCODE IN ('CLIENT');

                EXCEPTION
                    WHEN OTHERS
                    THEN
                        Sacttype := NULL;
                END;

                IF Sacttype IN ('P')
                THEN
                    BEGIN
                     SELECT CVASTRINGVALUE into stypeid from cchvalue where actid=nactid
					 and cchsid='CMBCCHSID196';

					    IF (stypeid= 'I') THEN 

							       SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'TFDCCHSID64'
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE     SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid
														   AND cchsid = 'TFDCCHSID64'
														   AND LTRIM (cvastringvalue) IS NOT NULL);



									 IF l_count > 1
									 THEN
										nok := 0;
									 ELSE
										nok := 1;
									 END IF;


					END IF;

                    IF (stypeid= 'D') THEN 

							       SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'TFDCCHSID188'
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE     SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid
														   AND cchsid = 'TFDCCHSID188'
														   AND LTRIM (cvastringvalue) IS NOT NULL);



									 IF l_count > 1
									 THEN
										nok := 0;
									 ELSE
										nok := 1;
									 END IF;


					END IF;
                     IF (stypeid= 'P') THEN 

							       SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'TFDCCHSID182'
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE     SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid
														   AND cchsid = 'TFDCCHSID182'
														   AND LTRIM (cvastringvalue) IS NOT NULL);



									 IF l_count > 1
									 THEN
										nok := 0;
									 ELSE
										nok := 1;
									 END IF;


					END IF;
                     IF (stypeid= 'C') THEN 

							       SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'TFDCCHSID186'
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE     SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid
														   AND cchsid = 'TFDCCHSID186'
														   AND LTRIM (cvastringvalue) IS NOT NULL);



									 IF l_count > 1
									 THEN
										nok := 0;
									 ELSE
										nok := 1;
									 END IF;


					END IF;
					END;	
                END IF;                 -- Fin Test sur unicite PP

            END IF;  
            ------HME CONTROLE Sur type empremteur &nature d'activite
            IF sControle = ('SOG5')
             THEN
             select CVASTRINGVALUE into nsgclt from cchvalue where actid=nactid and cchsid='CMBCCHSID82';

              If (nsgclt='10201') THEN

                        SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'CMBCCHSID300'
                                      AND ACTID=nactid
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE    /* SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid */
                                  actid=nactid
														   AND cchsid = 'CMBCCHSID300'
														   AND LTRIM (cvastringvalue) IS NOT NULL);

                                         IF   l_count > 0 THEN
                                         nok:=1;
                                         ELSE
                                           nok := 0;
                                        END if;
                        END IF;
                     END IF;   
               ------------                          
                   IF sControle = ('SOG6')
             THEN
             select CVASTRINGVALUE into nsgclt from cchvalue where actid=nactid and cchsid='CMBCCHSID82';

              If (nsgclt='10201') THEN

                        SELECT COUNT (*)
									   INTO l_count
									   FROM cchvalue cch
									  WHERE     cch.cchsid = 'CMBCCHSID301'
                                      AND ACTID=nactid
											AND cch.cvastringvalue IN
												   (SELECT cvastringvalue
													  FROM cchvalue
													 WHERE   /*  SUBSTR (
																  cvapkeyvalue,
																  INSTR (cvapkeyvalue, '-') + 1,
																  INSTR (cvapkeyvalue, '|') - 7) =
																  nActid */
                                  actid =nactid
														   AND cchsid = 'CMBCCHSID301'
														   AND LTRIM (cvastringvalue) IS NOT NULL);

                                         IF   l_count > 0 THEN
                                         nok:=1;
                                         ELSE
                                           nok := 0;
                                        END if;
                        END IF;
                     END IF; 

                     -----HME CONTROLE SUR DATE ADRESSE CLIENT 
                   IF sControle = ('SOG7')  
                   THEN
                   SELECT MAX(AADDTDEB) INTO nadract FROM ACTADRESSE WHERE ACTID=NACTID;

                      IF ( nadract> TO_DATE (SYSDATE,'DD/MM/YY')) THEN
                       nok := 0;
                      ELSE
                      nok := 1;
                      END IF;
                   END IF;   

                   ------HME CONTROLE SUR DES CHAMPS Alphanuméric
                   ------------chi ----controle sur existence de pls adresse de facturation
             IF sControle = ('FFACT')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM ACTADRESSE
                 WHERE  ACTID = NACTID
                       AND AADFLAGFACTURATION=1 ;


                IF NCOUNT > 1
                THEN
                    NOK := 0;
                END IF;
                END IF;

                   ----NUM RC
                   IF sControle =('SOG8')
                   THEN
                    select ACTNUMRCM INTO nACTNUMRCM from acteur where actid=NACTID;
                    IF (nACTNUMRCM IS NOT NULL) THEN
                       SELECT is_number(nACTNUMRCM) INTO NCOUNT FROM dual;

                        IF  (NCOUNT =0) THEN
                         nok :=0;
                        ELSE
                         nok :=1;
                        END IF;
                   END IF;
                   END IF;

                   ----HME CONTROLE sur num compte et cle RIB
                   IF sControle =('SOG9')

                   THEN
                         SELECT MAX (RIBID)
                          INTO W_RIB
                          FROM ACTRIB
                         WHERE ACTID = NACTID ;


                IF W_RIB IS NOT NULL
                THEN
                   select RIBCOMPTE INTO NRIBCOMPTE from RIB where RIBID=W_RIB;

                 IF NRIBCOMPTE IS NOT NULL
                 THEN
                   SELECT is_number(NRIBCOMPTE) INTO NCOUNT FROM dual;

                      IF  (NCOUNT =0) THEN
                         nok :=0;
                        ELSE
                         nok :=1;
                        END IF;
					 END IF;  	
                  END IF;
				END IF;

                 IF sControle =('SOG10')

                   THEN
                           SELECT MAX (RIBID)
                          INTO W_RIB
                          FROM ACTRIB
                         WHERE ACTID = NACTID ;


                IF W_RIB IS NOT NULL
                THEN
                  select RIBCLE INTO NRIBCLE from RIB where RIBID=W_RIB;

                 IF NRIBCLE IS NOT NULL
                 THEN
                   SELECT is_number(NRIBCLE) INTO NCOUNT FROM dual;

                   IF  (NCOUNT =0) THEN
                         nok :=0;
                        ELSE
                         nok :=1;
                        END IF;
					END IF;	
                  END IF;
				END IF;
            -------- Existance d'un client Amplitude avec le même numéro de client
            IF sControle =('SOG11')
                    THEN
                        DECLARE
                            ncount      NUMBER := 0;
                            codeCliAmp  ACTEUR.ACTIDSERVICING%TYPE;
                        BEGIN
                        SELECT actidservicing INTO codeCliAmp from acteur where actid=NACTID;
                            SELECT  COUNT (*)           
                                  INTO ncount
                                  FROM acteur
                                  WHERE actidservicing = codeCliAmp;
                            
                            IF (ncount>1) THEN
                               
                              NOK :=0;                                      
                              ELSE
                                NOK := 1;  --OK
                                  END IF;
                            
                            END; 
            END IF;

            ----------- Fin des controles OCACTEUR  ajoutes

            RETURN NOK;
        END;
    END OCACTEUR;

    -- LMI 2013-09-06
    FUNCTION OCCOLLATERAL (NCOLID      IN COLLATERAL.COLID%TYPE,
                           SCONTROLE   IN VARCHAR2,
                           NCREID      IN CREVT.CREID%TYPE)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NOK           NUMBER := 1;
            SCCH          NUMBER;
            NHYP          NUMBER;
            SCVAPKEY      CCHVALUE.CVAPKEYVALUE%TYPE;
            NACTIDOWNER   COLLATERAL.ACTIDOWNER%TYPE;
            NRIBID        COLLATERAL.RIBID%TYPE;
            NEXIST        NUMBER := 0;
            -- CV-07112013 CASNT-8187
            DTCOLDT       COLPHASE.CPHDTBEG%TYPE;
            DTCCH         DATE;
            DTRGDT        DATE;
            DTRELDT       DATE;
            DTRELEADT     DATE;
            W_EXIST       NUMBER := 0;
            NCOUNT        NUMBER := 0;
        BEGIN
            IF SCONTROLE = ('EVAM')
            THEN
                SCVAPKEY := 'Colid-' || NCOLID || '|%';

                SELECT NVL (MAX (CVANUMERICVALUE), 0)
                  INTO SCCH
                  FROM CCHVALUE CVA, CUSTOMCHARACTERISTIC CCH
                 WHERE     CCH.ENTCODE = CVA.ENTCODE
                       AND CVA.ENTCODE = 'COLLATERAL'
                       AND CCH.CCHSID = CVA.CCHSID
                       AND CVAPKEYVALUE LIKE SCVAPKEY
                       AND CCHVALUECODE = 'CC_EVAVAL';

                SELECT SUM (NVL (CRAMTRESIDUAL, 0))
                  INTO NHYP
                  FROM COLRANK CRA
                 WHERE COLID = NCOLID;

                IF (NHYP > SCCH)
                THEN
                    NOK := 0;
                END IF;
            ---- control  Blocking  event 'Register collateral' JCH 2013-12-20
            ELSIF SCONTROLE = ('EXREG')
            THEN
                BEGIN
                    SELECT MIN (
                               DECODE (
                                   GREATEST (
                                         (  NVL (CREMTDEPTMAX, 1)
                                          / NVL (CREMT, 1))
                                       * 100,
                                       95),
                                   95, 0,
                                   1))
                      INTO NOK
                      FROM COLREGISTRATION
                     WHERE COLID = NCOLID;
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;
            -- EMI 2014-02-05 Percentage of registered amount
            ELSIF SCONTROLE = ('ABREG')
            THEN
                BEGIN
                    SELECT (NVL (CREMTDEPTMAX, 1) / NVL (CREMT, 1)) * 100
                      INTO NHYP
                      FROM COLREGISTRATION
                     WHERE     COLID = NCOLID
                           AND CREORDER = (SELECT MAX (CREORDER)
                                             FROM COLREGISTRATION
                                            WHERE COLID = NCOLID);
                EXCEPTION
                    WHEN OTHERS
                    THEN
                        NOK := 1;
                END;

                IF (NHYP < 100 AND NHYP >= 95)
                THEN
                    NOK := 0;
                END IF;
            -- OKA 2013-12-26 CASNT-9709
            ELSIF SCONTROLE = ('REGNM')
            THEN
                SELECT COUNT (*)
                  INTO NCOUNT
                  FROM COLLATERAL C
                 WHERE C.COLID IN
                           (  SELECT C.COLID
                                FROM COLLATERAL C, COLRANK R
                               WHERE     C.COLID = R.COLID
                                     AND C.TCTCODE IN ('DAM', 'MRE')
                                     AND C.COLID = NCOLID
                                     AND R.CRAREGNUM IS NOT NULL
                            GROUP BY C.COLID, R.CRAREGNUM
                              HAVING COUNT (*) > 1);

                IF NCOUNT > 0
                THEN
                    NOK := 0;
                END IF;
            -- End         CASNT-9709
            -- ALA 2013-09-17 begin control unicity of deposit amount collateral
            ELSIF SCONTROLE = ('RIBCL')
            THEN
                BEGIN
                    SELECT ACTIDOWNER, RIBID
                      INTO NACTIDOWNER, NRIBID
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE = 'DAM';
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                END;

                IF NACTIDOWNER IS NOT NULL AND NRIBID IS NOT NULL
                THEN
                    BEGIN
                        SELECT 1
                          INTO NEXIST
                          FROM COLLATERAL COL, COLPHASE PHA
                         WHERE     COL.COLID = PHA.COLID
                               AND PHA.PHACODE = 'ACTIVE'
                               AND COL.ACTIDOWNER = NACTIDOWNER
                               AND COL.RIBID = NRIBID
                               AND COL.COLID != NCOLID;
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            NOK := 1;
                        WHEN TOO_MANY_ROWS
                        THEN
                            NOK := 0;
                    END;

                    IF NEXIST = 1
                    THEN
                        NOK := 0;
                    END IF;
                END IF;
            -- ALA 2013-09-17 end control unicity of deposit amount collateral
            -- CV-07112013 CASNT-8188
            -------setting date <= SYSDATE---------------
            ELSIF SCONTROLE = ('SETDT')
            THEN
                NEXIST := 0;

                BEGIN
                    SELECT 1
                      INTO NEXIST
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE IN ('DAM', 'MRE');

                    IF NEXIST IS NOT NULL
                    THEN
                        BEGIN
                            SELECT TO_DATE (
                                       SUBSTR (
                                           MAX (
                                                  LPAD (CRAORDRE, 2, 0)
                                               || '-'
                                               || TO_CHAR (CRADTSETTING,
                                                           'YYYY/MM/DD')),
                                           4,
                                           12),
                                       'YYYY/MM/DD')
                              INTO DTCCH
                              FROM COLRANK
                             WHERE COLID = NCOLID;
                        -- EXCEPTION
                        --WHEN NO_DATA_FOUND THEN nOk := 1;
                        END;

                        IF     DTCCH IS NOT NULL
                           AND TRUNC (DTCCH) > TRUNC (SYSDATE)
                        THEN
                            NOK := 0;
                        END IF;
                    END IF;
                END;
            -------- SETTING DATE >= REGISTRATION DATE ----------
            ELSIF SCONTROLE = ('SRD')
            THEN
                BEGIN
                    SELECT 1
                      INTO W_EXIST
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE IN ('DAM', 'MRE');

                    --EXCEPTION
                    --WHEN NO_DATA_FOUND THEN nOk := 1;
                    IF W_EXIST IS NOT NULL
                    THEN
                        BEGIN
                            SELECT TO_DATE (
                                       SUBSTR (
                                           MAX (
                                                  LPAD (CRAORDRE, 2, 0)
                                               || '-'
                                               || TO_CHAR (CRADTSETTING,
                                                           'YYYY/MM/DD')),
                                           4,
                                           12),
                                       'YYYY/MM/DD')
                              INTO DTCCH
                              FROM COLRANK
                             WHERE COLID = NCOLID;
                        END;

                        SELECT TO_DATE (
                                   SUBSTR (
                                       MAX (
                                              LPAD (CRAORDRE, 2, 0)
                                           || '-'
                                           || TO_CHAR (CRADTREGISTER,
                                                       'YYYY/MM/DD')),
                                       4,
                                       12),
                                   'YYYY/MM/DD')
                          INTO DTRGDT
                          FROM COLRANK
                         WHERE COLID = NCOLID;
                    END IF;

                    IF     DTCCH IS NOT NULL
                       AND DTRGDT IS NOT NULL
                       AND TRUNC (DTCCH) < TRUNC (DTRGDT)
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                END;
            ------------------RELEASE DATE <= SYSDATE-----------------
            ELSIF SCONTROLE = ('REL')
            THEN
                BEGIN
                    SELECT 1
                      INTO W_EXIST
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE IN ('DAM', 'MRE');

                    --EXCEPTION
                    --WHEN NO_DATA_FOUND THEN nOk := 1;
                    IF W_EXIST IS NOT NULL
                    THEN
                        BEGIN
                            SELECT TO_DATE (
                                       SUBSTR (
                                           MAX (
                                                  LPAD (CRAORDRE, 2, 0)
                                               || '-'
                                               || TO_CHAR (CRADTRELEASE,
                                                           'YYYY/MM/DD')),
                                           4,
                                           12),
                                       'YYYY/MM/DD')
                              INTO DTRELEADT
                              FROM COLRANK
                             WHERE COLID = NCOLID;
                        END;

                        IF     DTRELEADT IS NOT NULL
                           AND TRUNC (DTRELEADT) > TRUNC (SYSDATE)
                        THEN
                            NOK := 0;
                        ELSE
                            NOK := 1;
                        END IF;
                    END IF;
                END;
            --------------------SETTING DATE <= RELEASE DATE--------------
            ELSIF SCONTROLE = ('RELS')
            THEN
                BEGIN
                    SELECT 1
                      INTO W_EXIST
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE IN ('DAM', 'MRE');

                    IF W_EXIST IS NOT NULL
                    THEN
                        BEGIN
                            SELECT TO_DATE (
                                       SUBSTR (
                                           MAX (
                                                  LPAD (CRAORDRE, 2, 0)
                                               || '-'
                                               || TO_CHAR (CRADTSETTING,
                                                           'YYYY/MM/DD')),
                                           4,
                                           12),
                                       'YYYY/MM/DD')
                              INTO DTCCH
                              FROM COLRANK
                             WHERE COLID = NCOLID;

                            SELECT TO_DATE (
                                       SUBSTR (
                                           MAX (
                                                  LPAD (CRAORDRE, 2, 0)
                                               || '-'
                                               || TO_CHAR (CRADTRELEASE,
                                                           'YYYY/MM/DD')),
                                           4,
                                           12),
                                       'YYYY/MM/DD')
                              INTO DTRELEADT
                              FROM COLRANK
                             WHERE COLID = NCOLID;
                        END;
                    END IF;

                    IF     DTCCH IS NOT NULL
                       AND DTRELEADT IS NOT NULL
                       AND TRUNC (DTCCH) > TRUNC (DTRELEADT)
                    THEN
                        NOK := 0;
                    ELSE
                        NOK := 1;
                    END IF;
                END;
            -- End CV-07112013 CASNT-8188
            -- LMI 2013-10-11 Check Evidence document and Roles for owner of a collateral
            ELSIF SCONTROLE = ('EVIDE')
            THEN
                BEGIN
                    SELECT ACTIDOWNER
                      INTO NACTIDOWNER
                      FROM COLLATERAL
                     WHERE COLID = NCOLID AND TCTCODE IN ('DAM', 'MRE');
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        NOK := 1;
                END;

                IF NACTIDOWNER IS NOT NULL
                THEN
                    BEGIN
                        SELECT 1
                          INTO NEXIST
                          FROM ACTROLE
                         WHERE     ACTID = NACTIDOWNER
                               AND ROLCODE IN ('USECARA', 'AGBRCH');
                    EXCEPTION
                        WHEN NO_DATA_FOUND
                        THEN
                            NOK := 1;
                        WHEN TOO_MANY_ROWS
                        THEN
                            NEXIST := 1;
                    END;

                    IF NEXIST = 1
                    THEN
                        BEGIN
                            SELECT 1
                              INTO NOK
                              FROM DOCUMENTMANAGEMENT
                             WHERE COLID = NCOLID AND DMATYPE = 'EVID';
                        EXCEPTION
                            WHEN NO_DATA_FOUND
                            THEN
                                NOK := 0;
                            WHEN TOO_MANY_ROWS
                            THEN
                                NOK := 1;
                        END;
                    END IF;
                END IF;
            -- LMI 2013-10-11 Check Evidence document and Roles for owner of a collateral
            END IF;

            RETURN NOK;
        END;
    END OCCOLLATERAL;

    -- LMI 2013-09-06
    PROCEDURE P_CONTROLEUSER (
        SDESTINATION   IN     LANTPCTACCONTROLE.TPCDEST%TYPE,
        NIDENTIFIANT   IN     NUMBER,
        SCONTROLE      IN     VARCHAR2,
        NCREID         IN     CREVT.CREID%TYPE,
        SUTICODE       IN     UTILISATEUR.UTICODE%TYPE,
        SUGECODE       IN     UTILISATEUR.UGECODE%TYPE,
        NOK            IN OUT NUMBER)
    AS
    BEGIN
        -- sDestination : Type d'occurrence controlee avec les valeurs possibles =
        --
        --    . ACTEUR  : Controles sur les Acteurs
        --    . BIEN    : Controles sur les Biens
        --    . CHAN    : Controles sur les Chantiers
        --    . COMDE   : Controles sur les Commandes
        --    . DEPENSE : Controles sur les Depenses
        --    . DOSSIER : Controles sur les Dossiers
        --    . REGLE   : Controles sur les Encaissements/decaissements et Operations diverses de gestion
        --    . SITU    : Controles sur les Situations
        --    . TRANCHE : Controles sur les Tranches et rubriques d'immobilisations
        -- nIdentifiant : Numero interne de l'occurrence traitee en fonction de sDestination =
        --
        --    . ACTEUR  : ACTID
        --    . BIEN    : BIMID
        --    . CHAN    : CHAID
        --    . COMDE   : COMID
        --    . DEPENSE : DEPID
        --    . DOSSIER : DOSID
        --    . REGLE   : REGID
        --    . SITU    : DEPID
        --    . TRANCHE : ITRID
        -- sControle : Code du controle a faire
        -- nOk : si une erreur est detectee mettre le nOk a 0 sinon controle OK
        BEGIN
            IF (SDESTINATION = 'DOSSIER')
            THEN
                NOK := OCDOSSIER (NIDENTIFIANT, SCONTROLE, NCREID);
            ELSIF (SDESTINATION = 'TRANCHE')
            THEN
                NOK := OCTRANCHE (NIDENTIFIANT, SCONTROLE, NCREID);
            ELSIF (SDESTINATION = 'DEPENSE')
            THEN
                NOK :=
                    OCDEPENSE (NIDENTIFIANT,
                               SCONTROLE,
                               SUTICODE,
                               NCREID);
            ELSIF (SDESTINATION = 'REGLE')
            THEN
                NOK :=
                    OCREGLEMENT (NIDENTIFIANT,
                                 SCONTROLE,
                                 SUTICODE,
                                 SUGECODE,
                                 NCREID);
            ELSIF (SDESTINATION = 'FACTURE')
            THEN
                NOK := OCFACTURE (NIDENTIFIANT, SCONTROLE, NCREID);
            ELSIF (SDESTINATION = 'FACIR')
            THEN
                NOK := OCFACTURE (NIDENTIFIANT, SCONTROLE, NCREID);
            ELSIF (SDESTINATION = 'ACTEUR')
            THEN
                NOK := OCACTEUR (NIDENTIFIANT, SCONTROLE, NCREID);
            -- LMI 2013-09-06
            ELSIF (SDESTINATION = 'COLLATE')
            THEN
                NOK := OCCOLLATERAL (NIDENTIFIANT, SCONTROLE, NCREID);
            ELSIF (SDESTINATION = 'AVDOSS')
            THEN
                NOK := OCDOSSIERFRONT (NIDENTIFIANT, SCONTROLE, NCREID);
            -- LMI 2013-09-06
            ELSE
                NOK := 1;
            END IF;
        EXCEPTION
            -- Si une exception n'a pas ete attrapee, on considere
            -- qu'il s'agit d'une erreur dans le controle
            WHEN OTHERS
            THEN
                NOK := 0;
        END;
    END P_CONTROLEUSER;

    PROCEDURE P_USRINDRESI (
        NDOSID         IN     DOSSIER.DOSID%TYPE,
        NCREID         IN     CREVT.CREID%TYPE,
        DTDATE         IN     DATE,
        SMETHODE       IN     DOSSIER.DOSTYPEPENALITERESIL%TYPE,
        NPERCENTE      IN     DOSSIER.DOSPCTPENALITERESIL%TYPE,
        NWRITERESULT   IN     INTEGER,
        NNBLOYNECH     IN OUT NUMBER,
        NMTLOYNECH     IN OUT NUMBER,
        NRESULT        IN OUT NUMBER)
    AS
    BEGIN
        NNBLOYNECH := 0;
        NMTLOYNECH := 0;
        NRESULT := 0;
    END P_USRINDRESI;

    FUNCTION OALERTEACTEUR (NIDENTIFIANT   IN     NUMBER,
                            NCTLID         IN OUT NUMBER,
                            SUTICODE       IN     UTILISATEUR.UTICODE%TYPE,
                            STUGECODE      IN     UTILISATEUR.UGECODE%TYPE,
                            NALERTCOUNT    IN OUT NUMBER)
        RETURN NUMBER
    IS
    BEGIN
        DECLARE
            NRETOUR            PLS_INTEGER;
            PREMIERPASSAGE     BOOLEAN;
            LOK                PLS_INTEGER;
            NMSG               NUMBER;
            NB_ELEMENT         BINARY_INTEGER;
            ATYPE              PA_FUNGENCONTROLE.TBL_VARCHAR2;
            AORDRE             PA_FUNGENCONTROLE.TBL_NUMBER;
            ADEC               PA_FUNGENCONTROLE.TBL_NUMBER;
            AMSG               PA_FUNGENCONTROLE.TBL_VARCHAR2;
            NCOUNT             PLS_INTEGER;
            SRVVCODE           AAGRVE.RVVCODE%TYPE;
            NERREURBLOQUANTE   NUMBER;
            NWARNING           NUMBER;
            NDEROGATION        NUMBER;
            SUSER              UTILISATEUR.UTICODE%TYPE := SUTICODE;
            SUGECODE           UTILISATEUR.UGECODE%TYPE := STUGECODE;

            CURSOR CPARA
            IS
                SELECT TPCCODE
                  FROM TPARAGRAPHECTL
                 WHERE TPCDEST = 'ACTEUR' AND TPCCODE = 'ALERT';

            CURSOR CCTL (
                S1   IN TPARAGRAPHECTL.TPCCODE%TYPE)
            IS
                  SELECT TTCCODE, 1 PTCFLAGUSER
                    FROM TPCTACCONTROLE TPC
                   WHERE     (TPC.UGECODE = SUGECODE OR TPC.UGECODE IS NULL)
                         AND TPCCODE = S1
                         AND TPCDEST = 'ACTEUR'
                ORDER BY TPCCODE;
        BEGIN
            -- Par defaut, initialisation  pas d alerte trouvee
            NRETOUR := 0;
            PREMIERPASSAGE := TRUE;
            NERREURBLOQUANTE := 0;
            NWARNING := 0;

            FOR CPARA_REC IN CPARA
            LOOP
                IF PREMIERPASSAGE
                THEN
                    SELECT SEQ_RCTID.NEXTVAL INTO NCTLID FROM DUAL;

                    --AkshiK INSERT RAPPORTCTL
                    INSERT INTO RAPPORTCTL (RCTDESTINATION,
                                            RCTDESTID,
                                            RCTDTCREAT,
                                            UTICODE,
                                            RCTEVTID,
                                            RCTCTLTYPE,
                                            TACCODE)
                         VALUES ('ACTEUR',
                                 NIDENTIFIANT,
                                 SYSDATE,
                                 SUSER,
                                 NCTLID,
                                 'P',
                                 'GLOBAL');

                    PREMIERPASSAGE := FALSE;
                END IF;

                FOR CCTL_REC IN CCTL (CPARA_REC.TPCCODE)
                LOOP
                    LOK := 1;
                    NB_ELEMENT := 0;

                    -- Initialisation du N  de message
                    SELECT MSGID
                      INTO NMSG
                      FROM TPCTACCONTROLE
                     WHERE     TPCCODE = CPARA_REC.TPCCODE
                           AND TTCCODE = CCTL_REC.TTCCODE
                           AND TPCDEST = 'ACTEUR';

                    IF CCTL_REC.TTCCODE = 'WATCH'
                    THEN
                        PA_COMMON.S_TPATEXTE ('BALE2BTG',
                                              'WATCHLIST',
                                              SRVVCODE);

                        IF SRVVCODE IS NOT NULL
                        THEN
                            SELECT COUNT (*)
                              INTO NCOUNT
                              FROM AAGRVE
                             WHERE     ACTID = NIDENTIFIANT
                                   AND RVACODE = 'RISQUE'
                                   AND AVEDTFIN IS NULL
                                   AND RVVCODE = SRVVCODE;

                            IF NCOUNT > 0
                            THEN
                                LOK := 0;
                            END IF;
                        END IF;
                    END IF;

                    -- Existence d une alerte
                    IF LOK = 0
                    THEN
                        NALERTCOUNT := NALERTCOUNT + 1;
                        PA_FUNGENCONTROLE.OCECRITCOMPTERENDU (
                            NCTLID,
                            'P',
                            CPARA_REC.TPCCODE,
                            CCTL_REC.TTCCODE,
                            NMSG,
                            CCTL_REC.PTCFLAGUSER,
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

                    -- On a trouve au moins un controle a passer sur l occurrence
                    NRETOUR := 1;
                END LOOP;
            END LOOP;

            RETURN NRETOUR;
        END;
    END OALERTEACTEUR;

    --ALA 200207 Gestion des alertes
    PROCEDURE P_ALERTE (NIDENTIFIANT   IN     NUMBER,
                        SDESTINATION   IN     LANTPCTACCONTROLE.TPCDEST%TYPE,
                        NCTLID         IN OUT NUMBER,
                        SUTICODE       IN     UTILISATEUR.UTICODE%TYPE,
                        STUGECODE      IN     UTILISATEUR.UGECODE%TYPE,
                        NALERTECOUNT   IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            NRETOUR        PLS_INTEGER;
            ID_NOT_FOUND   EXCEPTION;
            ERR_PARM1      EXCEPTION;
            ERR_PARM2      EXCEPTION;
            ERR_PARM3      EXCEPTION;
            ERR_PARM4      EXCEPTION;
            ERR_PARM5      EXCEPTION;
            ACTEUR_REC     ACTEUR%ROWTYPE;
        BEGIN
            IF NIDENTIFIANT IS NULL
            THEN
                RAISE ERR_PARM1;
            ELSIF SDESTINATION IS NULL
            THEN
                RAISE ERR_PARM2;
            ELSIF SDESTINATION NOT IN ('ACTEUR',
                                       'DOSSIER',
                                       'TRANCHE',
                                       'COMDE',
                                       'BIEN',
                                       'DEPENSE',
                                       'REGLE',
                                       'SITU',
                                       'CHAN',
                                       'FACTURE',
                                       'BUDGET',
                                       'MANDAT',
                                       'ASSUR',
                                       'SINIST',
                                       'CONSULT',
                                       'CONEXP',
                                       'FDGSTE')
            THEN
                RAISE ERR_PARM3;
            END IF;

            NALERTECOUNT := 0;

            --Supprimer les dernieres alertes
            --PA_ORFICONTROLE.oPonctuelSup(sDestination, nIdentifiant);
            IF (SDESTINATION = 'ACTEUR')
            THEN
                BEGIN
                    NRETOUR :=
                        OALERTEACTEUR (NIDENTIFIANT,
                                       NCTLID,
                                       SUTICODE,
                                       STUGECODE,
                                       NALERTECOUNT);
                EXCEPTION
                    WHEN NO_DATA_FOUND
                    THEN
                        RAISE ID_NOT_FOUND;
                END;
            END IF;
        EXCEPTION
            WHEN ID_NOT_FOUND
            THEN
                NRETOUR := ID_NOTFOUND;
            WHEN ERR_PARM1
            THEN
                NRETOUR := ERR_PARAM1;
            WHEN ERR_PARM2
            THEN
                NRETOUR := ERR_PARAM2;
            WHEN ERR_PARM3
            THEN
                NRETOUR := ERR_PARAM3;
            WHEN ERR_PARM4
            THEN
                NRETOUR := ERR_PARAM4;
            WHEN ERR_PARM5
            THEN
                NRETOUR := ERR_PARAM5;
            WHEN OTHERS
            THEN
                NRETOUR := -1 * SQLCODE;
        END;
    END P_ALERTE;
/*ELSIF scontrole IN ('DTDEB') -- BMI 25032011 controle sur date previsionnelle de mis en service
   THEN
      -- nOk:=0 ==> Le retour du CTRL est KO
      -- nOk:=1 ==> Le retour du CTRL est OK.

 DECLARE
 --nOk            NUMBER := 1;
 l_DTDeb   VARCHAR2(2) := '00';
 l_dat VARCHAR2(2);
 l_percp VARCHAR(1);
 nActv            NUMBER;
 CURSOR c_quant_av IS
 SELECT tupcode FROM lktuptactpg lk, dossier dos WHERE lk.TUSNOM = 'QUANTIEME_AVANC'
 and lk.tpgcode= dos.tpgcode and lk.taccode=dos.taccode
 and dosid =nDosId;

 CURSOR c_quant_ech IS
 SELECT tupcode FROM lktuptactpg lk, dossier dos WHERE lk.TUSNOM = 'QUANTIEME_ECHU'
 and lk.tpgcode= dos.tpgcode and lk.taccode=dos.taccode
 and dosid =nDosId;

BEGIN

  PA_COMMON.S_TPALOGIQUE ( 'DOSSIER' , 'CONTRL_QUANT' ,nActv);

  IF nActv = 1 THEN

  -- Correction Brahim le 27/04/2011 pour ne prendre en compte que la premiere rubrique financiere du dossier

    SELECT  df.drfperception, to_char (dr.drudtdeb, 'DD') into l_percp,l_DTDeb --20120410
    FROM dosrubflux df,dosrubrique dr
    WHERE df.dosid =ndosid
    AND df.dosid =dr.dosid
    AND df.druordre=dr.druordre
   -- AND df.drfordre= 1 -- BMI 20120313
    AND df.drfordre=(SELECT MIN(DRFORDRE) FROM DOSRUBFLUX df1 WHERE dosid=ndosid AND rubid=17 and df1.druordre=dr.druordre) -- MNG 20120629
    AND dr.rubid= 17
    AND dr.druordre=(SELECT max(druordre) FROM dosrubrique WHERE dosid=ndosid AND rubid=17);

    /*SELECT to_char (dossier.DOSDTEFFET, 'DD') INTO l_DTDeb
    FROM dossier
    WHERE dosid =ndosid;*/
                         /*
ALPHA
   IF (l_percp = 'A') THEN
     OPEN c_quant_av;
     LOOP
     FETCH c_quant_av
     INTO l_dat;

     EXIT WHEN c_quant_av%NOTFOUND;
       IF (l_DTDeb = l_dat)
       THEN
         nok := 1;            -- dossier commencant par quantieme ==> OK
         EXIT;
       ELSE
         nok := 0;            --  KO
       END IF;

     END LOOP;
     CLOSE c_quant_av;
   else
     OPEN c_quant_ech;
     LOOP
     FETCH c_quant_ech
     INTO l_dat;

     EXIT WHEN c_quant_ech%NOTFOUND;
       IF (l_DTDeb = l_dat)
       THEN
         nok := 1;            -- dossier commencant par quantieme ==> OK
         EXIT;
       ELSE
         nok := 0;            --  KO
       END IF;

     END LOOP;
     CLOSE c_quant_ech;
   end if;

 END IF;

EXCEPTION
      WHEN OTHERS
      THEN
       nok := 0;
END;*/


END PA_USERCONTROLE;