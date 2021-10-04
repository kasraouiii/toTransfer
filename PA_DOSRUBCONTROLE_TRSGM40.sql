create or replace PACKAGE       "PA_DOSRUBCONTROLE" AS

   PROCEDURE pdosrub(
      nIdentifiant     IN     NUMBER,
      nCtlId           IN     NUMBER,
      sPara            IN     VARCHAR2,
      sControle        IN     VARCHAR2,
      nUserOption      IN     NUMBER,
      sTypeControle    IN     VARCHAR2,
      contrat_rec      IN     DOSSIER%ROWTYPE,
      crevt_rec        IN     CREVT%ROWTYPE,
      nErreurBloquante IN OUT NUMBER,
      nWarning         IN OUT NUMBER,
      nDerogation      IN OUT NUMBER  );

	FUNCTION IsRubFinPre(
      sDruClasse DOSRUBRIQUE.DRUCLASSE%TYPE,
      sDruType   DOSRUBRIQUE.DRUTYPE%TYPE ) RETURN BOOLEAN;

END PA_DOSRUBCONTROLE;
 
/
create or replace PACKAGE BODY       "PA_DOSRUBCONTROLE" AS

   nb_element BINARY_INTEGER;
   --nOrdre BINARY_INTEGER; -- Pour plus tard
   aOrdre pa_fungencontrole.TBL_NUMBER;
   aMsg   pa_fungencontrole.TBL_VARCHAR2;
   aType  pa_fungencontrole.TBL_VARCHAR2;
   aDec   pa_fungencontrole.TBL_NUMBER;

   -- DA-24012000 : Fonction non utilisee
   --FUNCTION IsRubAdosse(
   --   nDosId    IN DOSSIER.DOSID%TYPE,
   --   nDruOrdre IN DOSRUBRIQUE.DRUORDRE%TYPE ) RETURN NUMBER;

   -- DA-24012000 : Fonction non utilisee
   --FUNCTION SommeRubAdosse(
   --   nDosId    IN DOSSIER.DOSID%TYPE,
   --   nDruOrdre IN DOSRUBRIQUE.DRUORDRE%TYPE,
   --   nCase     IN NUMBER ) RETURN NUMBER;

     -- MP 14/06/01
 /*	FUNCTION ocDossRubGen(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;
   */
   FUNCTION ocDossRubFlux(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

   FUNCTION ocDossRubCvt(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubPool(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubFinPre(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubEch(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubAcc(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION OkFormule(
      nRub   IN DOSRUBRIQUE.DRUORDRE%TYPE,
      nFtvId IN FORMULETXVAR.FTVID%TYPE ) RETURN NUMBER;

	FUNCTION ocDossRubVar(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubSaison(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubFiscal(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubCpteCourant(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	FUNCTION ocDossRubrecadrage(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;
	FUNCTION ocDossRubProv(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;
   -- CV-15012010 CFS39572
	FUNCTION ocDossRubPassive(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER;

	/*
	// Detail du controle d'un contrat
	*/
	PROCEDURE pdosrub(
      nIdentifiant     IN     NUMBER,
      nCtlId           IN     NUMBER,
      sPara            IN     VARCHAR2,
      sControle        IN     VARCHAR2,
      nUserOption      IN     NUMBER,
      sTypeControle    IN     VARCHAR2,
      contrat_rec      IN     DOSSIER%ROWTYPE,
      crevt_rec        IN     CREVT%ROWTYPE,
      nErreurBloquante IN OUT NUMBER,
      nWarning         IN OUT NUMBER,
      nDerogation      IN OUT NUMBER ) IS
   BEGIN
      DECLARE
         lOK NUMBER;
         nMsg NUMBER;
         nAny NUMBER;
         nCount   NUMBER;
         nOrdre    NUMBER := 0;
         nItrId    LKDOSRUBITRRUB.ITRID%TYPE;
         nDosFlagCommCa NUMBER;
         nDosFlagLoyerCa   DOSSIERLOCATIF.DOSFLAGLOYERCA%TYPE;
         nSumComposant  NUMBER;
         sItrDevCode    DOSSIER.DEVCODE%TYPE;
         nTauxConv      NUMBER;
         nMtConv        NUMBER;
         err_parm1     EXCEPTION;
			nCdfAvecPartDirect  NUMBER;
         nMaxDfiOrdre        DOSFINANCE.DFIORDRE%TYPE;
         nDfiMtVr         DOSFINANCE.DFIMTVR%TYPE;
         nDfiMtTotal      DOSFINANCE.DFIMTTOTAL%TYPE;
         nDfiPcTvr        DOSFINANCE.DFIPCTVR%TYPE;
         nDruMtResiduel   DOSRUBRIQUE.DRUMTRESIDUEL%TYPE;
         nDec             NUMBER:=0;
         nEcart           NUMBER:=0;
         nDruTauxFixeLoy  DOSRUBRIQUE.DRUTAUXFIXE%TYPE;
         nDruTauxFixeGpc  DOSRUBRIQUE.DRUTAUXFIXE%TYPE;
         dtMaxDrfDtFin    DOSRUBFLUX.DRFDTFIN%TYPE;
         bTrouve          BOOLEAN;
         bExist           BOOLEAN;
         nQp 					  NUMBER;
			nBase 				  NUMBER;

         CURSOR c_DosItr  IS
            SELECT   LDR.ITRID,
                     ITR.ITRNUM,
                     ITR.ITRNOM,
                     ITR.ITRSECTGESTION
            FROM     LKDOSRUBITRRUB LDR,
                     IMMOTRANCHE ITR
            WHERE    DOSID = contrat_rec.DOSID
              AND    LDR.ITRID = ITR.ITRID
            GROUP BY LDR.ITRID,
                     ITR.ITRNUM,
                     ITR.ITRNOM,
                     ITR.ITRSECTGESTION   ;
         CURSOR  c_LkDosRubItrRub IS
            SELECT   IRUORDRE, DIRMT,DRUORDRE
            FROM     LKDOSRUBITRRUB
            WHERE    DOSID = contrat_rec.DOSID
            AND      ITRID = nItrId;
         CURSOR c_DosRubDtfin IS
            SELECT DRUDTFIN, DRUORDRE
            FROM   DOSRUBRIQUE
            WHERE  DOSID     = contrat_rec.DOSID
            AND    DRUTYPE   = 'F';


         CURSOR c_DosRubEmpDtfin IS
            SELECT DRUDTFIN, DRUORDRE
            FROM   DOSRUBRIQUE
            WHERE  DOSID     = contrat_rec.DOSID
            AND    DRUTYPE   = 'R'
            AND    DRUCLASSE = 'F';
      BEGIN
         lOk        := 1;
         nb_element := 0;

         SELECT MSGID
         INTO   nMsg
         FROM   TPCTACCONTROLE
         WHERE  TPCCODE = sPara
         AND    TTCCODE = sControle
         AND    TPCDEST = 'DOSSIER'
         AND    TACCODE = contrat_rec.TACCODE;

         IF SUBSTR( sControle, 1, 2 ) = 'U_' THEN
            PA_USERCONTROLE.P_CONTROLEUSER( 'DOSSIER', nIdentifiant, SUBSTR( sControle, 3 ), crevt_rec.CREID, lOk );
         ELSIF SUBSTR( sControle, 1, 3 ) IN ( 'R01', 'R02', 'R07', 'R08' ) OR sControle IN ( 'R0534', 'R0532', 'R0533', 'R1231', 'R0437' )  THEN
           IF sControle IN ( 'R0836', 'R0838') THEN
                FOR c_DosItr_rec IN c_DosItr LOOP
                  BEGIN
                     nItrId := c_DosItr_rec.ITRID;
                     IF sControle = 'R0836' THEN
                        SELECT COUNT(*)
                        INTO   nCount
                        FROM   LKITRBIM
                        WHERE  ITRID = c_DosItr_rec.ITRID ;
                           IF nCount < 1 THEN
                              RAISE err_parm1;
                        END IF;
                  /* ELSIF sControle = 'R0837' THEN
                        FOR c_LkDosRubItrRub_rec IN c_LkDosRubItrRub LOOP
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   ITRRUBCOMPOSANT
                           WHERE  ITRID    = nItrId
                           AND    IRUORDRE = c_LkDosRubItrRub_rec.IRUORDRE;
                           IF ( nAny > 0 ) THEN
                              SELECT SUM(IRC.IRCQUANTITE * IRC.IRCPU) , ITR.DEVCODE
                              INTO   nSumComposant , sItrDevCode
                              FROM   IMMOTRANCHE ITR, ITRRUBCOMPOSANT IRC
                              WHERE  ITR.ITRID = IRC.ITRID
                              AND    IRC.ITRID = nItrId
                              AND    IRC.IRUORDRE = c_LkDosRubItrRub_rec.IRUORDRE
                              GROUP BY ITR.DEVCODE;
                              PA_COMMON.F_CONVERSIONDEVISE(
                                 sItrDevCode,
                                 contrat_rec.DEVCODE,
                                 nSumComposant,
                                 TRUNC(SYSDATE),
                                 nTauxConv,
                                 nMtConv );
                              IF ( nMtConv = c_LkDosRubItrRub_rec.DIRMT ) THEN
                                 nMtConv := NULL;
                              END IF;
                           ELSE
                              nMtConv := 0;
                           END IF;
                           IF ( nMtConv IS NOT NULL ) THEN
                               lOk                := 0;
                               nb_element   	     := nb_element + 1;
                               nOrdre             := nb_element;
                            	 aOrdre(nb_element) := nOrdre;
                               aMsg(nb_element)   := c_LkDosRubItrRub_rec.DRUORDRE;
                               aType(nb_element)  := 'N';
                               aDec(nb_element)   := NULL;
                               nb_element   	     := nb_element + 1;
                               aOrdre(nb_element) := nOrdre;
                               aMsg(nb_element)   := c_LkDosRubItrRub_rec.IRUORDRE;
                               aType(nb_element)  := 'N';
                               aDec(nb_element)   := NULL;
                               nb_element   	     := nb_element + 1;
                               aOrdre(nb_element) := nOrdre;
                               aMsg(nb_element)   := nMtConv;
                               aType(nb_element)  := 'N';
                               aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
                           END IF;
                        END LOOP;*/
                     ELSIF sControle  = 'R0838' THEN
                        IF c_DosItr_rec.ITRSECTGESTION != contrat_rec.DOSSECTGESTION THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  EXCEPTION
                     WHEN err_parm1 THEN
                              lOk := 0;
                              nb_element   	   := nb_element + 1;
                              nOrdre             := nb_element;
                            	aOrdre(nb_element) := nOrdre;
                            	aMsg(nb_element)   := c_DosItr_rec.ITRNUM || ' / ' || c_DosItr_rec.ITRNOM;
                              aType(nb_element)  := 'C';
                              aDec(nb_element)   := NULL;
                  END;
               END LOOP;
            ELSIF sControle IN ( 'R0136', 'R0137', 'R0156' ) AND contrat_rec.TACCODE != 'EMPRUNT' THEN
               IF ( sControle = 'R0136' ) AND ( contrat_rec.DOSDTDEB IS NOT NULL ) THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID     = contrat_rec.DOSID
                    AND  DRUCLASSE = 'F'
                    AND  DRUTYPE   = 'F'
                    AND  DRUDTDEB  = contrat_rec.DOSDTDEB;
                  IF ( nAny = 0 ) THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBRIQUE
                     WHERE  DOSID     = contrat_rec.DOSID
                       AND  DRUCLASSE = 'F';
                     IF ( nAny != 0 ) THEN
                        lOk := 0;
                     END IF;
                  END IF;
               -- CV-11042003 FSA-11197 pour l'evt 'EVD_MEL', dossier doir passer d'abord par R0136
               ELSIF sControle = 'R0156' AND contrat_rec.DOSDTDEB IS NULL AND contrat_rec.DOSDTEFFET IS NOT NULL THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID     = contrat_rec.DOSID
                    AND  DRUCLASSE = 'F'
                    AND  DRUTYPE   = 'F'
                    AND  DRUDTDEB  = contrat_rec.DOSDTEFFET;
                  IF ( nAny = 0 ) THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBRIQUE
                     WHERE  DOSID     = contrat_rec.DOSID
                       AND  DRUCLASSE = 'F';
                     IF ( nAny != 0 ) THEN
                        lOk := 0;
                     END IF;
                  END IF;

            --FSA 13878
               ELSIF sControle = 'R0137' AND contrat_rec.DOSDTFIN IS NOT NULL AND contrat_rec.TACCODE != 'LOCFIN' THEN
                   bTrouve := FALSE ;
                   bExist  := FALSE ;
                  FOR c_DosRubDtfin_rec IN c_DosRubDtfin LOOP
                     bExist := TRUE;
                     if c_DosRubDtfin_rec.DRUDTFIN IS NOT NULL THEN
                        if contrat_rec.DOSDTFIN = c_DosRubDtfin_rec.DRUDTFIN THEN
                           bTrouve := TRUE;
                           EXIT;
                        END if;
                     ELSE
                        dtMaxDrfDtFin := NULL;
                        BEGIN
                           SELECT MAX(DRFDTFIN)
                           INTO dtMaxDrfDtFin
                           FROM DOSRUBFLUX
                           WHERE DOSID = contrat_rec.DOSID
                           AND DRUORDRE = c_DosRubDtfin_rec.DRUORDRE;
                        EXCEPTION
                           WHEN OTHERS THEN
                              dtMaxDrfDtFin := NULL;
                        END;
                        if dtMaxDrfDtFin IS NOT NULL THEN
                           if dtMaxDrfDtFin = contrat_rec.DOSDTFIN THEN
                              bTrouve := TRUE;
                              EXIT;
                           END if;
                        END if;
                     END if;
                     if bTrouve THEN
                        EXIT;
                     END if;
                   END LOOP;
                   if NOT bTrouve AND bExist THEN
                     lOk := 0;
                   END IF;
               END if;
            ELSIF sControle IN ( 'R0138', 'R0139' ) AND contrat_rec.TACCODE = 'EMPRUNT' THEN
               IF sControle = 'R0138' AND contrat_rec.DOSDTDEB IS NOT NULL THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID     = contrat_rec.DOSID
                  AND    DRUTYPE   = 'R'
                  AND    DRUDTDEB  = contrat_rec.DOSDTDEB
                  AND    DRUCLASSE = 'F';
                  IF nAny = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R0139' AND contrat_rec.DOSDTFIN IS NOT NULL THEN
                  bTrouve := FALSE ;
                  bExist  := FALSE ;
                  FOR c_DosRubEmpDtfin_rec IN c_DosRubEmpDtfin LOOP
                     bExist := TRUE;
                     if c_DosRubEmpDtfin_rec.DRUDTFIN IS NOT NULL THEN
                        if contrat_rec.DOSDTFIN = c_DosRubEmpDtfin_rec.DRUDTFIN THEN
                           bTrouve := TRUE;
                           EXIT;
                        END if;
                     ELSE
                        dtMaxDrfDtFin := NULL;
                        BEGIN
                           SELECT MAX(DRFDTFIN)
                           INTO dtMaxDrfDtFin
                           FROM DOSRUBFLUX
                           WHERE DOSID = contrat_rec.DOSID
                           AND DRUORDRE = c_DosRubEmpDtfin_rec.DRUORDRE;
                        EXCEPTION
                           WHEN OTHERS THEN
                              dtMaxDrfDtFin := NULL;
                        END;
                        if dtMaxDrfDtFin IS NOT NULL THEN
                           if dtMaxDrfDtFin = contrat_rec.DOSDTFIN THEN
                              bTrouve := TRUE;
                              EXIT;
                           END if;
                        END if;
                     END if;
                     if bTrouve THEN
                        EXIT;
                     END if;
                   END LOOP;
                   if NOT bTrouve AND bExist THEN
                     lOk := 0;
                   END IF;
               END if;
            ELSIF sControle IN ( 'R0133', 'R0134', 'R0135', 'R0144', 'R0233', 'R0234', 'R0532', 'R0533', 'R0730', 'R0735', 'R1231' ) THEN
               SELECT COUNT(*)
               INTO   nAny
               FROM   DOSSIERIMMOBILIER
               WHERE  DOSID = contrat_rec.DOSID;
               IF nAny = 1 THEN
    				   lOk := ocDossRubCvt( contrat_rec, sControle );
               END IF;
            ELSIF sControle IN ( 'R0148', 'R0149' ) AND ( contrat_rec.TACCODE = 'LOCATIF' ) THEN
               SELECT NVL(DOSFLAGCOMMCA,0),NVL(DOSFLAGLOYERCA,0)
               INTO   nDosFlagCommCa , nDosFlagLoyerCa
               FROM   DOSSIERLOCATIF
               WHERE  DOSID = contrat_rec.DOSID;

               SELECT COUNT(1)
               INTO   nAny
               FROM   DOSRUBRIQUE
               WHERE  DOSID           = contrat_rec.DOSID
                 AND  DRUTYPEMONTAGE IN ( 'CASEUL', 'CAPLUS', 'CAMIXT' );

               IF ( sControle = 'R0148' ) AND ( nDosFlagCommCa = 1 ) AND nDosFlagLoyerCa = 1
                                          AND ( nAny = 0 ) THEN
                  lOk := 0;
               ELSIF ( sControle = 'R0149' ) AND ( nDosFlagCommCa = 0 ) AND nDosFlagLoyerCa = 0
                                             AND ( nAny != 0 ) THEN
                  lOk := 0;
               END IF;
            ELSIF sControle = 'R0240' THEN
                SELECT  COUNT(*)
                INTO    nCount
                FROM    DOSRUBRIQUE
                WHERE   DOSID = contrat_rec.DOSID AND DRUTYPE = 'F'
                AND     DRUCLASSE = 'F'
                AND     F_PlFinRUBRIQUE( contrat_rec.DOSID,DRUORDRE) = contrat_rec.DOSDTFIN;

                IF nCount >0 THEN
                    SELECT  MAX(DFIORDRE)
                    INTO    nMaxDfiOrdre
                    FROM    DOSFINANCE
                    WHERE   DOSID = contrat_rec.DOSID;
                    IF nMaxDfiOrdre IS NOT NULL THEN

                       SELECT  DFIMTVR,DFIMTTOTAL,DFIPCTVR
                       INTO    nDfiMtVr, nDfiMtTotal, nDfiPcTvr
                       FROM    DOSFINANCE
                       WHERE   DOSID = contrat_rec.DOSID
                       AND     DFIORDRE = nMaxDfiOrdre;
                   ELSE
                      nDfiMtVr:= NULL;
                      nDfiMtTotal:= 0;
                      nDfiPcTvr:=0;
                   END IF;
                    SELECT  NVL(SUM(DRUMTRESIDUEL),0)
                    INTO    nDruMtResiduel
                    FROM    DOSRUBRIQUE
                    WHERE   DOSID = contrat_rec.DOSID AND DRUTYPE = 'F'
                    AND     DRUCLASSE = 'F'
                    AND     F_PlFinRUBRIQUE( contrat_rec.DOSID,DRUORDRE) = contrat_rec.DOSDTFIN;

                    nDec :=  F_NbDecimalDansDevise( contrat_rec.DEVCODE );

						 -- Verification du pool (cdf partenaire en direct)
	               PA_UPDATEDOSSIER3.S_CDFAVECPARTDIRECT(
   							contrat_rec.DOSID,
   							contrat_rec.ACTID,
   							contrat_rec.DOSPOOL,
   							nQp,
   							nBase );
	               IF nQp != -1 AND nBase != -1 THEN
               		nCdfAvecPartDirect := 1;
            		ELSE
            			nCdfAvecPartDirect := 0;
            		END IF;

	               IF nCdfAvecPartDirect = 0 THEN
	                    IF nDfiMtVr IS NOT NULL THEN
                       IF nDruMtResiduel != nDfiMtVr THEN
                            SELECT NVL(abs(nDruMtResiduel - nDfiMtVr),0) INTO nEcart FROM DUAL;
                            lOk := 0;
                            nb_element   	   := nb_element + 1;
                            nOrdre             := nb_element;
                            aOrdre(nb_element) := nOrdre;
                            aMsg(nb_element)   := nEcart;
                            aType(nb_element)  := 'N';
                            aDec(nb_element)   := nDec;
                       END if;
                    ELSE
	               IF nDruMtResiduel != round(nDfiMtTotal * nDfiPctVr / 100) THEN
                            SELECT NVL(abs(nDruMtResiduel - ( round(nDfiMtTotal * nDfiPctVr / 100))),0)
                            INTO nEcart FROM DUAL;
                            lOk := 0;
                            nb_element   	   := nb_element + 1;
                            nOrdre             := nb_element;
                            aOrdre(nb_element) := nOrdre;
                            aMsg(nb_element)   := nEcart;
                            aType(nb_element)  := 'N';
                            aDec(nb_element)   := nDec;
                       END if;
                    END if;
                   END IF;
                END if;
            ELSIF sControle = 'R0241' THEN
                SELECT  MAX(DRUTAUXFIXE)
                INTO    nDruTauxFixeLoy
                FROM    DOSRUBRIQUE
                WHERE   DOSID   = contrat_rec.DOSID
                AND     DRUTYPE = 'F' AND DRUCLASSE = 'F';

                SELECT  MAX(DRU.DRUTAUXFIXE)
                INTO    nDruTauxFixeGpc
                FROM    DOSRUBRIQUE DRU, RUBRIQUE RUB
                WHERE   DRU.DOSID   = contrat_rec.DOSID
                AND     DRU.RUBID   = RUB.RUBID
                AND     RUB.RUBCODE = 'TAFGPC';

                IF nDruTauxFixeGpc > nDruTauxFixeLoy THEN
                   lOk                := 0;
                   nb_element   	     := nb_element + 1;
                   nOrdre             := nb_element;
                   aOrdre(nb_element) := nOrdre;
                   aMsg(nb_element)   := nDruTauxFixeGpc;
                   aType(nb_element)  := 'N';
                   aDec(nb_element)   := NULL;
                   nb_element 	     := nb_element + 1;
                   aOrdre(nb_element) := nOrdre;
                   aMsg(nb_element)   := nDruTauxFixeLoy;
                   aType(nb_element)  := 'N';
                   aDec(nb_element)   := NULL;
                END IF;
            ELSIF sControle = 'R0166' THEN
               IF ( contrat_rec.TACCODE IN ( 'CBI', 'CBM', 'CBMIXTE', 'LOCATIF', 'LOCFIN' ) ) THEN
                  SELECT COUNT(1)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID   = contrat_rec.DOSID
                    AND  DRUTYPE = 'F';
               ELSIF ( contrat_rec.TACCODE = 'EMPRUNT' ) THEN
                  SELECT COUNT(1)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID   = contrat_rec.DOSID
                    AND  DRUTYPE = 'R';
               ELSIF ( contrat_rec.TACCODE = 'PRET' ) THEN
                  SELECT COUNT(1)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID    = contrat_rec.DOSID
                    AND  DRUTYPE IN ( 'F', 'P' );
               ELSE
                  nAny := 1;
               END IF;
               IF ( nAny = 0 ) THEN
                  lOk := 0;
               END IF;
            ELSE
               lOk := PA_DOSRUBCONTROLE2.ocDossRubGen( contrat_rec, sControle, nb_element, aOrdre, aMsg, aType, aDec );
            END IF;
         ELSIF SUBSTR( sControle, 1, 3 ) = 'R03' THEN

      	   lOk := ocDossRubFinPre( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R04' THEN

      	   lOk := ocDossRubAcc( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R05' THEN

      	   lOk := ocDossRubVar( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R09' THEN

      	   lOk := ocDossRubSaison( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R10' THEN

      	   lOk := ocDossRubFiscal( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R11' THEN

      	   lOk := ocDossRubCpteCourant( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R06' THEN

      	   lOk := ocDossRubPool( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R12' THEN

      	   lOk := ocDossRubRecadrage( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R13' THEN

      	   lOk := ocDossRubFlux( contrat_rec, sControle );

         ELSIF SUBSTR( sControle, 1, 3 ) = 'R14' THEN

      	   lOk := ocDossRubEch( contrat_rec, sControle );
         ELSIF SUBSTR (sControle, 1,3) = 'R15' THEN
            lOk := ocDossRubProv( contrat_rec, sControle );
         -- CV-15012010 CFS39572
         ELSIF SUBSTR (sControle, 1,3) = 'R16' THEN
            lOk := ocDossRubPassive( contrat_rec, sControle );
		 END IF;

			-- Existence d'une erreur
         IF lOk = 0 THEN
            pa_fungencontrole.ocEcritCompteRendu(
               nCtlId,
               sTypeControle,
               sPara,
               sControle,
               nMsg,
               nUserOption,
               nErreurBloquante,
               nWarning,
               nDerogation,
               nb_element,
               aOrdre,
               aMsg,
               aType,
               aDec,
               'DOSSIER',
               contrat_rec.TACCODE );

            -- On vide les tableaux
            aOrdre.DELETE;
            aMsg.DELETE;
            aType.DELETE;
            aDec.DELETE;
            -- DBMS_SESSION.FREE_UNUSED_USER_MEMORY;
         END IF;
      END;
   END pdosrub;

	/*
	// Determine si la rubrique est une rubrique financiere de prefinancement
	*/
	FUNCTION IsRubFinPre(
      sDruClasse DOSRUBRIQUE.DRUCLASSE%TYPE,
      sDruType   DOSRUBRIQUE.DRUTYPE%TYPE ) RETURN BOOLEAN IS
	BEGIN
      DECLARE
         bRubFinPre BOOLEAN := FALSE;
      BEGIN
         IF ( sDruClasse IS NOT NULL ) AND ( sDruType IS NOT NULL ) THEN
            bRubFinPre := ( sDruClasse = 'F' ) AND ( sDruType = 'P' );
         END IF;
         RETURN bRubFinPre;
      END;
   END IsRubFinPre;

	/*
	// Controle existence d'un code formule
	*/
	FUNCTION OkFormule(
      nRub   IN DOSRUBRIQUE.DRUORDRE%TYPE,
      nFtvId IN FORMULETXVAR.FTVID%TYPE ) RETURN NUMBER IS
	BEGIN
      DECLARE
         lOk    NUMBER := 1;
         nAny   NUMBER;
         nOrdre NUMBER := 0;
      BEGIN
         SELECT COUNT(*)
         INTO   nAny
         FROM   FORMULETXVAR
         WHERE  FTVID = nFtvId;
         IF nAny = 0 THEN
            lOk                := 0;
            nb_element         := nb_element + 1;
            nOrdre             := nb_element;
            aOrdre(nb_element) := nOrdre;
            aMsg(nb_element)   := nRub;
            aType(nb_element)  := 'N';
            aDec(nb_element)   := NULL;
				nb_element   	    := nb_element + 1;
				aOrdre(nb_element) := nOrdre;
				aMsg(nb_element)   := nFtvId;
				aType(nb_element)  := 'N';
				aDec(nb_element)   := NULL;
         END IF;
         RETURN lOk;
      END;
   END OkFormule;

	/*
	// Controle pools rubriques R06
	*/
   FUNCTION ocDossRubPool(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
      DECLARE
         lOk       NUMBER := 1;
         nOrdre    NUMBER := 0;
         nAny      NUMBER;
         sAny      VARCHAR2(7);
         cPoolActeur POOACTEUR%ROWTYPE;
         err_p1    EXCEPTION;
         err_p2    EXCEPTION;
         err_p3    EXCEPTION;
         err_p4    EXCEPTION;
         nBase     NUMBER;
         nSumQp    NUMBER;
         nCount    NUMBER;
         nPool     DOSRUBRIQUE.POOID%TYPE;
         nRubrique DOSRUBRIQUE.DRUORDRE%TYPE;
         sActLibCourt ACTEUR.ACTLIBCOURT%TYPE;
         sActCode     ACTEUR.ACTLIBCOURT%TYPE;
         sDosPool     DOSSIER.DOSPOOL%TYPE;
         CURSOR    cDosRubPool IS SELECT POOID, DRUDTDEB, DRUCLASSE, DRUTYPEMONTAGE, DRUTYPE,
                                         DRUORDRE
                                  FROM   DOSRUBRIQUE
                                  WHERE  DOSID = contrat_rec.DOSID
                                  AND    POOID IS NOT NULL
                                  AND    (contrat_rec.TACCODE = 'PRET' OR DRUTYPE != 'P');

         CURSOR    cDosRubSpec IS SELECT ACTID, DPAQP
                                  FROM   LKDOSRUBPOOACT
                                  WHERE  DOSID    = contrat_rec.DOSID
                                  AND    POOID    = nPool
                                  AND    DRUORDRE = nRubrique
                                  AND	  DPADTFIN IS NULL;

         CURSOR    cDosRubFacture IS   SELECT DACORDRE, DRUORDRE
                                       FROM   DOSRUBRIQUE
                                       WHERE  DOSID = contrat_rec.DOSID
                                       AND    POOID IS NOT NULL;
         CURSOR    cDosRubPoolPref IS SELECT POOID,
                                         DRUORDRE
                                  FROM   DOSRUBRIQUE
                                  WHERE  DOSID = contrat_rec.DOSID
                                  AND    POOID IS NOT NULL
                                  AND   NOT  (contrat_rec.TACCODE = 'PRET' OR DRUTYPE != 'P');


      BEGIN
         FOR cDosRubFacture_rec IN cDosRubFacture LOOP
            BEGIN
               IF sControle = 'R0671' THEN
                     BEGIN
                        SELECT   ACT.ACTCODE, ACT.ACTLIBCOURT, DAC.DACGRPFAC
                        INTO     sActCode, sActLibCourt, sAny
                        FROM     DOSACTEUR DAC, ACTEUR ACT
                        WHERE    DAC.DOSID    = contrat_rec.DOSID
                        AND      DAC.DACORDRE = cDosRubFacture_rec.DACORDRE
                        AND      DAC.ACTID    = ACT.ACTID;
                     EXCEPTION WHEN OTHERS THEN
                        sAny := NULL;
                     END;
                     IF sAny IS NOT NULL THEN
                        BEGIN
                           SELECT   DOSPOOL
                           INTO     sDosPool
                           FROM     DOSSIER
                           WHERE    DOSID    = contrat_rec.DOSID;
                        EXCEPTION WHEN OTHERS THEN
                           sDosPool := NULL;
                        END;
                        IF sDosPool IS NULL OR sDosPool != 'DIR' THEN
                           RAISE err_p4;
                        END IF;
                     END IF;
               END IF;
            EXCEPTION
               WHEN err_p4 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubFacture_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element 	       := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := sActCode;
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
                  nb_element 	       := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := sActLibCourt;
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
               WHEN NO_DATA_FOUND THEN
                  lOk                := 0;
            END;
         END LOOP;

         FOR cDosRubPoolPref_rec IN cDosRubPoolPref LOOP
            nRubrique :=  cDosRubPoolPref_rec.DRUORDRE ;
            nPool     :=  cDosRubPoolPref_rec.POOID;

            FOR  cDosRubSpec_rec  IN cDosRubSpec LOOP
               BEGIN
                  IF sControle = 'R0602' AND cDosRubSpec_rec.DPAQP IS NULL THEN
                     RAISE err_p3;
                  END IF;
               EXCEPTION
                  WHEN err_p3 THEN
                     SELECT ACTLIBCOURT
                     INTO   sActLibCourt
                     FROM   ACTEUR
                     WHERE  ACTID = cDosRubSpec_rec.ACTID;
                     lOk                := 0;
                     nb_element   	  := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubPoolPref_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                     nb_element         := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := sActLibCourt;
                     aType(nb_element)  := 'C';
                     aDec(nb_element)   := NULL;
               END;
            END LOOP;
         END LOOP;

         FOR cDosRubPool_rec IN cDosRubPool LOOP
            BEGIN
               IF sControle = 'R0670' THEN
                  IF contrat_rec.TACCODE = 'LOCATIF' THEN
                  	SELECT COUNT(*)
                  	INTO   nAny
                  	FROM   POOL
                  	WHERE  POOID            = cDosRubPool_rec.POOID
                  	AND    TRUNC(POODTDEB) <= TRUNC(cDosRubPool_rec.DRUDTDEB)
                  	AND    POOTYPE          = 'FINANCE';
                  	IF nAny = 0 THEN
                  	   RAISE err_p1;
                  	END IF;
               	ELSE
                     if NOT (contrat_rec.TACCODE =  'PRET'   AND cDosRubPool_rec.DRUTYPE = 'P') THEN
                  	  SELECT COUNT(*)
                       INTO  nCount
                       FROM DOSPOOL
                       WHERE DOSID = contrat_rec.DOSID;
                       if nCount <= 1 THEN
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   POOL
                           WHERE  POOID            = cDosRubPool_rec.POOID
                           AND    TRUNC(POODTDEB) <= TRUNC(cDosRubPool_rec.DRUDTDEB)
                           AND    POOTYPE          = 'FINANCE';
                           IF nAny = 0 THEN
                              RAISE err_p1;
                           END IF;
                       END if;
                     END if;
						END IF;

               --ELSIF sControle = 'R0630' AND cDosRubPool_rec.DRUCLASSE      IS NOT NULL
               --                          AND cDosRubPool_rec.DRUCLASSE      =  'F'
               --                          AND cDosRubPool_rec.DRUTYPEMONTAGE IS NOT NULL
               --                          AND cDosRubPool_rec.DRUTYPE        IS NOT NULL
               --                          AND cDosRubPool_rec.DRUTYPE        =  'V' THEN
               --   RAISE err_p1;
               --ELSIF sControle = 'R0601' THEN
               --   SELECT POOBASE
               --   INTO   nBase
               --   FROM   POOL
               --   WHERE  DOSID = contrat_rec.DOSID
               --   AND    POOID = cDosRubPool_rec.POOID
               --   AND    POOTYPE = 'FINANCE';

               --   SELECT NVL(SUM(DPAQP),0)
               --   INTO   nSumQp
               --   FROM   LKDOSRUBPOOACT
                --  WHERE   DOSID    = contrat_rec.DOSID AND
               --           POOID    = cDosRubPool_rec.POOID AND
               --           DRUORDRE = cDosRubPool_rec.DRUORDRE;
               --   IF nSumQp != nBase THEN
               --      RAISE err_p2;
               --   END IF;

               ELSE

                  IF sControle = 'R0631' THEN
                     IF contrat_rec.DOSIDREFINANCE IS NULL THEN
                        SELECT COUNT(*)
                        INTO   nCount
                        FROM   LKDOSRUBPOOACT
                        WHERE  DOSID = contrat_rec.DOSID AND
                               DRUORDRE = cDosRubPool_rec.DRUORDRE AND
                               POOID = cDosRubPool_rec.POOID
                               AND	  DPADTFIN IS NULL;
                        IF nCount = 0 THEN
                           RAISE err_p1;
                        END IF;
                     END IF;
                  ELSIF contrat_rec.DOSPOOL != 'NON' THEN

                     nRubrique := cDosRubPool_rec.DRUORDRE;
                     nPool     := cDosRubPool_rec.POOID;

                     FOR cDosRubSpec_rec IN cDosRubSpec LOOP
                         BEGIN
                            SELECT *
                            INTO   cPoolActeur
                            FROM   POOACTEUR
                            WHERE  POOID = nPool
                              AND  ACTID = cDosRubSpec_rec.ACTID;
                            IF contrat_rec.DOSPOOL = 'CDF' THEN
                               IF sControle = 'R0633' AND (contrat_rec.TACCODE = 'EMPRUNT' OR ( cDosRubPool_rec.DRUTYPE !='R' and cDosRubPool_rec.DRUCLASSE !='A'))  THEN
                                  IF NVL(cPoolActeur.PACFLAGCDF,0) = 0 AND NVL(cPoolActeur.PACFLAGVISIBLEEXPLOIT,0) = 1 AND cDosRubSpec_rec.DPAQP != 0 THEN
                                     RAISE err_p3;
                                  END IF;
                               ELSIF sControle = 'R0635' THEN
                                  IF cDosRubSpec_rec.ACTID = contrat_rec.ACTID AND NVL(cPoolActeur.PACFLAGCDF,0) = 0 THEN
                                     RAISE err_p3;
                                  END IF;
                               END IF;
                            ELSE
                               --IF sControle = 'R0632' THEN
                               --   IF NVL(cPoolActeur.PACFLAGCDF,0) = 0 AND cDosRubSpec_rec.DPAQP != 0 THEN
                               --      RAISE err_p3;
                               --   END IF;
                               --ELSIF sControle = 'R0634' THEN
                               --   IF NVL(cPoolActeur.PACFLAGCDF,0) = 1 AND cDosRubSpec_rec.DPAQP = 0 THEN
                               --      RAISE err_p3;
                               --   END IF;
                               --END IF;
                               NULL;
                            END IF;
                            IF sControle = 'R0602' AND cDosRubSpec_rec.DPAQP IS NULL THEN
                               RAISE err_p3;
                            END IF;
                         EXCEPTION
                            WHEN err_p3 THEN
                               SELECT ACTLIBCOURT
                               INTO   sActLibCourt
                               FROM   ACTEUR
                               WHERE  ACTID = cDosRubSpec_rec.ACTID;
                               lOk                := 0;
                               nb_element   	  := nb_element + 1;
                               nOrdre             := nb_element;
                               aOrdre(nb_element) := nOrdre;
                               aMsg(nb_element)   := cDosRubPool_rec.DRUORDRE;
                               aType(nb_element)  := 'N';
                               aDec(nb_element)   := NULL;
                               nb_element         := nb_element + 1;
                               aOrdre(nb_element) := nOrdre;
                               aMsg(nb_element)   := sActLibCourt;
                               aType(nb_element)  := 'C';
                               aDec(nb_element)   := NULL;
                         END;
                     END LOOP;

                  END IF;

               END IF;
            EXCEPTION
               WHEN err_p1 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubPool_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_p2 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubPool_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element 	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nBase;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element 	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nSumQp;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_p4 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubPool_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element 	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := sActCode;
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
                  nb_element 	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := sActLibCourt;
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
               WHEN NO_DATA_FOUND THEN
                  lOk                := 0;
            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubPool;
/*
// Controle rubriques a taux variables R05
*/
FUNCTION ocDossRubVar(
   contrat_rec IN DOSSIER%ROWTYPE,
   sControle   IN VARCHAR2 ) RETURN NUMBER IS
BEGIN
   DECLARE
	   lOk       NUMBER := 1;
	   nOrdre    NUMBER := 0;
	   nAny      NUMBER;
      err_parm1 EXCEPTION;
      err_parm2 EXCEPTION;             -- CV-22092003 FSA-10681
      err_parm3 EXCEPTION;
      err_parm4 EXCEPTION;
      dtDate1   DATE;                  -- CV-22092003 FSA-10681
      dtDate2   DATE;                  -- CV-22092003 FSA-10681
      var_rec   DOSRUBRIQUETXVAR%ROWTYPE;
      sCode     VARCHAR2(200);
      nDrtOrdre  DOSRUBTXVTAUX.DRTORDRE%TYPE;
      nDruOrdre  DOSRUBRIQUE.DRUORDRE%TYPE;
      nDruOrdreFirst DOSRUBRIQUE.DRUORDRE%TYPE;
      CURSOR cDosRubVar IS SELECT DOSID, DRUORDRE, DRUDTDEB, DRUDTFIN, DRUTXNATURE, DRUCLASSE, DRUTYPE, DRUTYPEMONTAGE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    DRUTXNATURE IN ( 'VR', 'VRR', 'VR2', 'TFR' )
                           AND    DRUTYPEMONTAGE != 'CVT'
                           AND    DRUCLASSE != 'A';
      CURSOR cDosRubVarTxMarge IS  SELECT DRTMARGE, DRTORDRE, DRTTYPE
                                    FROM  DOSRUBTXVAR
                                    WHERE DOSID = contrat_rec.DOSID
                                    AND   DRUORDRE = nDruOrdre;
      CURSOR cDosRubTxMarge IS SELECT DTTMARGEMOYENNE,DTTMARGEFINALE
                                  FROM   DOSRUBTXVTAUX
                                  WHERE  DOSID = contrat_rec.DOSID
                                  AND    DRUORDRE = nDruOrdre
                                  AND    DRTORDRE = nDrtOrdre;


   BEGIN
      FOR cDosRubVar_rec IN cDosRubVar LOOP
         BEGIN
            SELECT *
            INTO   var_rec
            FROM   DOSRUBRIQUETXVAR
            WHERE  DOSID    = cDosRubVar_rec.DOSID
            AND    DRUORDRE = cDosRubVar_rec.DRUORDRE;
            dtDate1 := NULL;
            dtDate2 := NULL;
            IF sControle = 'R0501' AND var_rec.DTVFTVIDBASE IS NULL THEN
               RAISE err_parm1;
            ELSIF ( sControle IN ( 'R0502', 'R0503', 'R0504' ) ) AND NOT IsRubFinPre( cDosRubVar_rec.DRUCLASSE, cDosRubVar_rec.DRUTYPE ) THEN
               IF sControle = 'R0502' AND var_rec.DTVDTECH IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0503' AND var_rec.DTVPERIODE IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0504' AND var_rec.DTVMULTIPLE IS NULL THEN
                  RAISE err_parm1;
               END IF;
            ELSIF sControle = 'R0505' AND var_rec.DTVTYPEPLANCHER IS NOT NULL
                                      AND var_rec.DTVTYPEPLANCHER =  'VALEUR'
                                      AND var_rec.DTVTAUXPLANCHER IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0506' AND var_rec.DTVTYPEPLANCHER IS NOT NULL
                                      AND var_rec.DTVTYPEPLANCHER =  'FORMULE'
                                      AND var_rec.DTVFTVIDPLANCHER  IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0507' AND var_rec.DTVTYPEPLAFOND IS NOT NULL
                                      AND var_rec.DTVTYPEPLAFOND =  'VALEUR'
                                      AND var_rec.DTVTAUXPLAFOND IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0508' AND var_rec.DTVTYPEPLAFOND IS NOT NULL
                                      AND var_rec.DTVTYPEPLAFOND =  'FORMULE'
                                      AND var_rec.DTVFTVIDPLAFOND  IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0509' AND NVL( var_rec.DTVFLAGCONSOLID, 0 ) = 1
                                      AND var_rec.DTVFTVIDCONSOLID IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0510' AND cDosRubVar_rec.DRUTXNATURE = 'VRR'
                                      AND var_rec.DTVTYPEREGUL IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0511' AND cDosRubVar_rec.DRUTXNATURE =  'VRR'
                                      AND var_rec.DTVTYPEREGUL       IS NOT NULL
                                      AND var_rec.DTVTYPEREGUL       =  'FORMULE'
                                      AND var_rec.DTVFTVIDREGUL        IS NULL THEN
               RAISE err_parm1;
            ELSIF sControle = 'R0512' THEN
               nDruOrdre := cDosRubVar_rec.DRUORDRE;
               FOR cDosRubVarTxMarge_rec IN cDosRubVarTxMarge LOOP
                  if cDosRubVarTxMarge_rec.DRTMARGE IS NULL THEN
                     nDrtOrdre := cDosRubVarTxMarge_rec.DRTORDRE;
                     FOR cDosRubTxMarge_rec IN cDosRubTxMarge LOOP
                        if (cDosRubTxMarge_rec.DTTMARGEMOYENNE IS NULL  AND cDosRubTxMarge_rec.DTTMARGEFINALE IS NULL) THEN
                           RAISE err_parm1;
                        END if;
                     END LOOP;
                  END if;
               END LOOP;
            ELSIF sControle = 'R0535' AND var_rec.DTVTAUXPLANCHER IS NOT NULL
                                      AND var_rec.DTVTAUXPLAFOND  IS NOT NULL THEN
               IF var_rec.DTVTAUXPLAFOND <= var_rec.DTVTAUXPLANCHER THEN
                  RAISE err_parm1;
               END IF;
            -- CV-22092003 FSA-10681 MSG7405
            ELSIF sControle = 'R0536'
               AND var_rec.DTVDTDEBPLANCHER IS NOT NULL AND var_rec.DTVDTFINPLANCHER IS NOT NULL
               AND var_rec.DTVDTDEBPLANCHER > var_rec.DTVDTFINPLANCHER THEN
               dtDate1 := var_rec.DTVDTDEBPLANCHER;
               dtDate2 := var_rec.DTVDTFINPLANCHER;
               RAISE err_parm2;
            -- MSG6634
            ELSIF sControle = 'R0537'
               AND var_rec.DTVDTDEBPLAFOND IS NOT NULL AND var_rec.DTVDTFINPLAFOND IS NOT NULL
               AND var_rec.DTVDTDEBPLAFOND > var_rec.DTVDTFINPLAFOND THEN
               dtDate1 := var_rec.DTVDTDEBPLAFOND;
               dtDate2 := var_rec.DTVDTFINPLAFOND;
               RAISE err_parm2;
            -- MSG6633
            ELSIF sControle = 'R0538' AND var_rec.DTVDTECH IS NOT NULL
               AND var_rec.DTVDTDEBPLANCHER IS NOT NULL AND var_rec.DTVDTFINPLANCHER IS NOT NULL THEN
               dtDate2 := var_rec.DTVDTECH;
               IF ( var_rec.DTVDTDEBPLANCHER < var_rec.DTVDTECH ) THEN
                  dtDate1 := var_rec.DTVDTDEBPLANCHER;
                  RAISE err_parm2;
               ELSIF ( var_rec.DTVDTFINPLANCHER < var_rec.DTVDTECH ) THEN
                  dtDate1 := var_rec.DTVDTFINPLANCHER;
                  RAISE err_parm2;
               END IF;
            -- MSG7406
            ELSIF sControle = 'R0539' AND var_rec.DTVDTECH IS NOT NULL
               AND var_rec.DTVDTDEBPLAFOND IS NOT NULL AND var_rec.DTVDTFINPLAFOND IS NOT NULL
               AND (( var_rec.DTVDTDEBPLAFOND < var_rec.DTVDTECH )
               OR  ( var_rec.DTVDTFINPLAFOND < var_rec.DTVDTECH )) THEN
               IF ( var_rec.DTVDTDEBPLAFOND < var_rec.DTVDTECH ) THEN
                  dtDate1 := var_rec.DTVDTDEBPLAFOND;
               ELSE
                  dtDate1 := var_rec.DTVDTFINPLAFOND;
               END IF;
               dtDate2 := var_rec.DTVDTECH;
               RAISE err_parm2;
            ELSIF sControle IN ( 'R0540', 'R0541' ) THEN
               BEGIN
                  SELECT DRUDTFIN
                  INTO   dtDate2
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRubVar_rec.DRUORDRE;
               EXCEPTION
                  WHEN OTHERS THEN
                     dtDate2 := NULL;
               END;
               IF dtDate2 IS NULL THEN
                  BEGIN
                     SELECT MAX(DRFDTFIN)
                     INTO   dtDate2
                     FROM   DOSRUBFLUX
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRubVar_rec.DRUORDRE;
                  EXCEPTION
                     WHEN OTHERS THEN
                        dtDate2 := NULL;
                  END;
               END IF;
               IF dtDate2 IS NOT NULL THEN
                  -- MSG7407
                  IF sControle = 'R0540' AND var_rec.DTVDTDEBPLANCHER IS NOT NULL AND var_rec.DTVDTFINPLANCHER IS NOT NULL THEN
                     IF var_rec.DTVDTDEBPLANCHER > dtDate2 THEN
                        dtDate1 := var_rec.DTVDTDEBPLANCHER;
                        RAISE err_parm2;
                     ELSIF var_rec.DTVDTFINPLANCHER > dtDate2 THEN
                        dtDate1 := var_rec.DTVDTFINPLANCHER;
                        RAISE err_parm2;
                     END IF;
                  -- MSG7408
                  ELSIF sControle = 'R0541' AND var_rec.DTVDTDEBPLAFOND IS NOT NULL AND var_rec.DTVDTFINPLAFOND IS NOT NULL THEN
                     IF var_rec.DTVDTDEBPLAFOND > dtDate2 THEN
                        dtDate1 := var_rec.DTVDTDEBPLAFOND;
                        RAISE err_parm2;
                     ELSIF var_rec.DTVDTFINPLAFOND > dtDate2 THEN
                        dtDate1 := var_rec.DTVDTFINPLAFOND;
                        RAISE err_parm2;
                     END IF;
                  END IF;
               END IF;
            ELSIF sControle IN ( 'R0542', 'R0543' ) THEN
               BEGIN
                  SELECT DRUDTDEB
                  INTO   dtDate2
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRubVar_rec.DRUORDRE;
               EXCEPTION
                  WHEN OTHERS THEN
                     dtDate2 := NULL;
               END;
               IF dtDate2 IS NOT NULL THEN
                  -- MSG7409
                  IF sControle = 'R0542' AND var_rec.DTVDTDEBPLANCHER IS NOT NULL AND var_rec.DTVDTFINPLANCHER IS NOT NULL THEN
                     IF var_rec.DTVDTDEBPLANCHER < dtDate2 THEN
                        dtDate1 := var_rec.DTVDTDEBPLANCHER;
                        RAISE err_parm2;
                     ELSIF var_rec.DTVDTFINPLANCHER < dtDate2 THEN
                        dtDate1 := var_rec.DTVDTFINPLANCHER;
                        RAISE err_parm2;
                     END IF;
                  -- MSG7410
                  ELSIF sControle = 'R0543' AND var_rec.DTVDTDEBPLAFOND IS NOT NULL AND var_rec.DTVDTFINPLAFOND IS NOT NULL THEN
                     IF var_rec.DTVDTDEBPLAFOND < dtDate2 THEN
                        dtDate1 := var_rec.DTVDTDEBPLAFOND;
                        RAISE err_parm2;
                     ELSIF var_rec.DTVDTFINPLAFOND < dtDate2 THEN
                        dtDate1 := var_rec.DTVDTFINPLAFOND;
                        RAISE err_parm2;
                     END IF;
                  END IF;
               END IF;
            ELSIF sControle = 'R0544' THEN
            	IF var_rec.DTVDTECH IS NOT NULL THEN
            		nDruOrdreFirst := F_PlGetDruOrdrePrec(contrat_rec.DOSID,cDosRubVar_rec.DRUORDRE);

            		SELECT 	DRUDTDEB
            		INTO   	dtDate1
            		FROM 		DOSRUBRIQUE
            		WHERE 	DOSID = contrat_rec.DOSID
            		AND   	DRUORDRE = nDruOrdreFirst;

            		IF cDosRubVar_rec.DRUDTFIN IS NULL THEN
            			dtDate2 := F_PlFinRUBRIQUE(contrat_rec.DOSID, cDosRubVar_rec.DRUORDRE);
            		ELSE
            			dtDate2 := cDosRubVar_rec.DRUDTFIN;
            		END IF;

            		IF dtDate1 > var_rec.DTVDTECH OR var_rec.DTVDTECH > dtDate2 THEN
            			RAISE err_parm3;
            		END IF;
            	END IF;
            ELSIF sControle = 'R0545' THEN
            	IF cDosRubVar_rec.DRUTYPEMONTAGE NOT IN ('VRG','VRG2') AND
            		( var_rec.DTVPERIODTAFCALC IS NOT NULL OR var_rec.DTVPERIODTAFCALCMULTIPLE IS NOT NULL ) THEN
            		RAISE err_parm4;
            	END IF;
           	ELSIF sControle = 'R0546' THEN
            	IF var_rec.DTVPERIODTAFCALCMULTIPLE IS NOT NULL THEN
            		NULL;
            		-- CALCULE MULTIPLE
            	END IF;
            -- Fin CV-22092003 FSA-10681
            ELSIF sControle = 'R0570' THEN
               IF var_rec.DTVFTVIDBASE IS NOT NULL THEN
                  IF OkFormule( cDosRubVar_rec.DRUORDRE, var_rec.DTVFTVIDBASE ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
               IF var_rec.DTVFTVIDPLANCHER IS NOT NULL THEN
                  IF OkFormule( cDosRubVar_rec.DRUORDRE, var_rec.DTVFTVIDPLANCHER ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
               IF var_rec.DTVFTVIDPLAFOND IS NOT NULL THEN
                  IF OkFormule( cDosRubVar_rec.DRUORDRE, var_rec.DTVFTVIDPLAFOND ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
               IF var_rec.DTVFTVIDCONSOLID IS NOT NULL THEN
                  IF OkFormule( cDosRubVar_rec.DRUORDRE, var_rec.DTVFTVIDCONSOLID ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
               IF var_rec.DTVFTVIDREGUL IS NOT NULL THEN
                  IF OkFormule( cDosRubVar_rec.DRUORDRE, var_rec.DTVFTVIDREGUL ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
            END IF;
         EXCEPTION
            WHEN err_parm1 THEN
               lOk                := 0;
               nb_element         := nb_element + 1;
               nOrdre             := nb_element;
             	aOrdre(nb_element) := nOrdre;
             	aMsg(nb_element)   := cDosRubVar_rec.DRUORDRE;
               aType(nb_element)  := 'N';
             	aDec(nb_element)   := NULL;
            -- CV-22092003 FSA-10681
            WHEN err_parm2 THEN
               lOk                := 0;
               nb_element   	    := nb_element + 1;
               nOrdre             := nb_element;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := TO_CHAR( dtDate1,'YYYYMMDD' );
               aType(nb_element)  := 'D';
               aDec(nb_element)   := NULL;
               nb_element   	    := nb_element + 1;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := TO_CHAR( dtDate2,'YYYYMMDD' );
               aType(nb_element)  := 'D';
               aDec(nb_element)   := NULL;
               nb_element         := nb_element + 1;
             	aOrdre(nb_element) := nOrdre;
             	aMsg(nb_element)   := cDosRubVar_rec.DRUORDRE;
               aType(nb_element)  := 'N';
             	aDec(nb_element)   := NULL;
            -- Fin CV-22092003 FSA-10681
            WHEN err_parm3 THEN
             	lOk                := 0;
               nb_element   	    := nb_element + 1;
               nOrdre             := nb_element;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := TO_CHAR( var_rec.DTVDTECH,'YYYYMMDD' );
               aType(nb_element)  := 'D';
               aDec(nb_element)   := NULL;
               nb_element   	    := nb_element + 1;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := TO_CHAR( dtDate1,'YYYYMMDD' );
               aType(nb_element)  := 'D';
               aDec(nb_element)   := NULL;
               nb_element   	    := nb_element + 1;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := TO_CHAR( dtDate2,'YYYYMMDD' );
               aType(nb_element)  := 'D';
               aDec(nb_element)   := NULL;
               nb_element         := nb_element + 1;
             	aOrdre(nb_element) := nOrdre;
             	aMsg(nb_element)   := cDosRubVar_rec.DRUORDRE;
               aType(nb_element)  := 'N';
             	aDec(nb_element)   := NULL;
             WHEN err_parm4 THEN
             	nb_element   	    := nb_element + 1;
               nOrdre             := nb_element;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := 'LANTTRPARAM|CONTRATMONTAGE|' || cDosRubVar_rec.DRUTYPEMONTAGE ;
               aType(nb_element)  := 'S';
               aDec(nb_element)   := NULL;
               nb_element   	    := nb_element + 1;
               aOrdre(nb_element) := nOrdre;
               aMsg(nb_element)   := cDosRubVar_rec.DRUORDRE;
               aType(nb_element)  := 'N';
               aDec(nb_element)   := NULL;
            WHEN NO_DATA_FOUND THEN
               IF sControle = 'R0501' THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
             	   aOrdre(nb_element) := nOrdre;
             	   aMsg(nb_element)   := cDosRubVar_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
             	   aDec(nb_element)   := NULL;
               END IF;
         END;
      END LOOP;
      RETURN lOk;
   END;
END ocDossRubVar;

   -- DA-24012000 : Fonction non utilisee
	/*
	// La rubrique est-elle adossee ?
	*/
   /*
   FUNCTION IsRubAdosse(
      nDosId    IN DOSSIER.DOSID%TYPE,
      nDruOrdre IN DOSRUBRIQUE.DRUORDRE%TYPE ) RETURN NUMBER IS
   BEGIN
      DECLARE
         lOk  NUMBER := 1;
         nAny NUMBER;
      BEGIN
         SELECT COUNT(*)
         INTO   nAny
         FROM   LKDOSRUBITRRUB
         WHERE  DOSID    = nDosId
         AND    DRUORDRE = nDruOrdre;
         IF nAny != 0 THEN
            lOk := 0;
         ELSE
            SELECT COUNT(*)
            INTO   nAny
            FROM   LKITRRUBDOSRUB
            WHERE  DOSID    = nDosId
            AND    DRUORDRE = nDruOrdre;
            IF nAny != 0 THEN
               lOk := 0;
            ELSE
               SELECT COUNT(*)
               INTO   nAny
               FROM   L2DOSRUBRIQUE
               WHERE  DOSIDPRET    = nDosId
               AND    DRUORDREPRET = nDruOrdre;
               IF nAny != 0 THEN
                  lOk := 0;
               END IF;
            END IF;
         END IF;
         RETURN lOk;
      END;
   END IsRubAdosse;
   */

   -- DA-24012000 : Fonction non utilisee
   /*
	// Somme le montant adosse
	*/
   /*
   FUNCTION SommeRubAdosse(
      nDosId    IN DOSSIER.DOSID%TYPE,
      nDruOrdre IN DOSRUBRIQUE.DRUORDRE%TYPE,
      nCase     IN NUMBER ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   nSum      NUMBER;
      BEGIN
         IF nCase = 0 THEN
            -- La ligne suivante correspond a ce qui etait ecrit
            SELECT NVL(SUM(DIRMT),0)   -- donnera 0 meme si aucune ligne n'existe
            INTO   nSum
            -- La ligne suivante donnerait null si aucune ligne n'existe
            -- SELECT SUM(DIRMT)
            INTO   nSum
            FROM   LKDOSRUBITRRUB
            WHERE  DOSID    = nDosId
            AND    DRUORDRE = nDruOrdre;
         ELSIF nCase = 1 THEN
            SELECT NVL(SUM(IDRMT), 0)
            INTO   nSum
            FROM   LKITRRUBDOSRUB
            WHERE  DOSID    = nDosId
            AND    DRUORDRE = nDruOrdre;
         ELSIF nCase = 2 THEN
            SELECT NVL(SUM(DR2MT), 0)
            INTO   nSum
            FROM   L2DOSRUBRIQUE
            WHERE  DOSIDPRET    = nDosId
            AND    DRUORDREPRET = nDruOrdre;
         END IF;
         RETURN nSum;
      END;
   END SommeRubAdosse;
   */

   /*
   -- supprime par JPB, remplace par fonction supra
	FUNCTION SommeRubAdosse(
      nDosId    IN DOSSIER.DOSID%TYPE,
		nDruOrdre IN DOSRUBRIQUE.DRUORDRE%TYPE,
      nCase     IN NUMBER ) RETURN NUMBER IS
	BEGIN
	   DECLARE
	      nSum      NUMBER := 0;
         CURSOR cDosImmo IS SELECT DIRMT
                           FROM   LKDOSRUBITRRUB
                           WHERE  DOSID    = nDosId
                           AND    DRUORDRE = nDruOrdre;
         CURSOR cImmoDos IS SELECT IDRMT
                            FROM   LKITRRUBDOSRUB
                            WHERE  DOSID    = nDosId
                            AND    DRUORDRE = nDruOrdre;
         CURSOR cEmpPret IS SELECT DR2MT
                            FROM   L2DOSRUBRIQUE
                            WHERE  DOSIDPRET    = nDosId
                            AND    DRUORDREPRET = nDruOrdre;
      BEGIN
         IF nCase = 0 THEN
            FOR cDosImmo_rec IN cDosImmo LOOP
               nSum := nSum + NVL( cDosImmo_rec.DIRMT, 0 );
            END LOOP;
         ELSIF nCase = 1 THEN
            FOR cImmoDos_rec IN cImmoDos LOOP
               nSum := nSum + NVL( cImmoDos_rec.IDRMT, 0 );
            END LOOP;
         ELSIF nCase = 2 THEN
            FOR cEmpPret_rec IN cEmpPret LOOP
               nSum := nSum + NVL( cEmpPret_rec.DR2MT, 0 );
            END LOOP;
         END IF;
         RETURN nSum;
	   END;
   END SommeRubAdosse;
   */

	/*
	// Controles echeanciers des rubriques
	*/
	FUNCTION ocDossRubEch(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
         lOk        NUMBER := 1;
         nOrdre     NUMBER := 0;
         nAny       NUMBER;
         err_parm1  EXCEPTION;
         err_parm2  EXCEPTION;
         err_parm3  EXCEPTION;
         err_parm4  EXCEPTION;
         err_parm5  EXCEPTION;
         err_parm6  EXCEPTION;
         err_parm7  EXCEPTION;
         nDruOrdre  DOSRUBRIQUE.DRUORDRE%TYPE;
         nAmo       NUMBER;
         nEpsilon   NUMBER;
         nInt       NUMBER;
         dtDreDtFin DATE;
         dtControle DATE;
         bIntNega   BOOLEAN;
         bEchCalc   BOOLEAN;
         dtErrMsg1  DATE;
         dtErrMsg2  DATE;
         bDateEch   BOOLEAN;
         dtDate     DATE;
         nCreIdRegFin CREVT.CREID%TYPE;
         sTmfFonction CREVT.TMFFONCTION%TYPE;
			nOption		NUMBER;
		sTypeRub		DOSRUBRIQUE.DRUTYPE%TYPE;
         CURSOR cDosRub IS
            SELECT DRUORDRE,
                   DRUMTORIGINE,
                   DRUMTRESIDUEL,
                   DRUTYPEMONTAGE,
                   DRUDTFIN,
                   DRUDTDEB,
                   DRUTYPE ,
                   DRUTAUXCALC,
                   DRUTAUXFIXE,
                   DRUCLASSE,
                   RUBID				-- CV-19112004 FSA-17080 (SG) IAS
            FROM   DOSRUBRIQUE
            WHERE  DOSID = contrat_rec.DOSID AND DRUTYPE != 'P'
			  AND  DRUTYPE = NVL(sTypeRub,DRUTYPE);

         CURSOR cDosRubEch IS
            SELECT DREMTAMO, DREMTINTPERIODE, DREDTDEB, DREDTFIN
            FROM   DOSRUBECHEANCIER
            WHERE  DOSID    = contrat_rec.DOSID
            AND    DRUORDRE = nDruOrdre
            AND    DRETYPE != 'INV'
            ORDER BY DREORDRE;

         CURSOR cDre IS
            SELECT DREDTECH, DREDTDEB, DREDTFIN, DREMTBASE, DRENUM
            FROM   DOSRUBECHEANCIER
            WHERE  DOSID     = contrat_rec.DOSID
            AND    DRUORDRE  = nDruOrdre
            ORDER BY DREORDRE;
         nDruTxCalcMax  NUMBER;
         nMtMaxEch      DOSRUBECHEANCIER.DREMTBASE%TYPE;
         nDreMtBaseMax  DOSRUBECHEANCIER.DREMTBASE%TYPE;
		 nMultiCurr		NUMBER;
		 nOptionBILLCUR AGEOPTION.AOPLOGIQUE%TYPE;
      BEGIN
         PA_COMMON.S_TPANOMBRE('ECHEANCIER','TXRETMAX', nDruTxCalcMax );
         PA_COMMON.S_TPANOMBRE('ECHEANCIER','MTRETMAX', nDreMtBaseMax );
         nEpsilon   := pa_fungencontrole.GetEpsilonParam( 'MONTANT' ) * (-1);

		 SELECT COUNT(1)
		  INTO nMultiCurr
		  FROM DOSCURRENCY dcu
		 WHERE dcu.DOSID = contrat_rec.DOSID
		   AND dcu.DEVCODE != contrat_rec.DEVCODE;

	    BEGIN
			SELECT NVL(AOPLOGIQUE,0)
			  INTO nOptionBILLCUR
			  FROM AGEOPTION
			 WHERE ACTID = contrat_rec.ACTID
			   AND TOSCODE = 'BILLCUR';
		EXCEPTION
			WHEN OTHERS THEN
				nOptionBILLCUR := 0;
		END;

		IF nMultiCurr > 0 AND nOptionBILLCUR = 1 THEN
			sTypeRub := 'B';
		END IF;

         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               nDruOrdre := cDosRub_rec.DRUORDRE;
               IF sControle = 'R1439' THEN
                  bDateEch := TRUE;
                  dtDate   := NULL;
                  IF cDosRub_rec.DRUTYPE != 'D' THEN
                     FOR cDosRubEch_rec IN cDosRubEch LOOP
                        IF dtDate IS NOT NULL THEN
                           IF cDosRubEch_rec.DREDTDEB != dtDate+1 THEN
                             bDateEch := FALSE;
                           END IF;
                        END IF;
                        dtDate := cDosRubEch_rec.DREDTFIN;
                     END LOOP;
                  END IF;
                  IF bDateEch = FALSE THEN
                     RAISE err_parm2;
                  END IF;

               ELSIF ( sControle IN ( 'R1431', 'R1432' ) ) AND ( cDosRub_rec.DRUTYPE != 'D' ) THEN
                  nAmo      := 0;
                  bIntNega  := FALSE;
                  bEchCalc  := FALSE;
                  FOR cDosRubEch_rec IN cDosRubEch LOOP
                     bEchCalc := TRUE;
                     nInt := cDosRubEch_rec.DREMTINTPERIODE;
                     IF nInt < nEpsilon THEN
                        BEGIN
                           SELECT MAX(CREID)
                           INTO   nCreIdRegFin
                           FROM   DOSFINANCE
                           WHERE  DOSID        = contrat_rec.DOSID
                             AND  DFIDTFINANCE = cDosRubEch_rec.DREDTDEB;
                           IF ( nCreIdRegFin IS NOT NULL ) THEN
                              SELECT TMFFONCTION
                              INTO   sTmfFonction
                              FROM   CREVT
                              WHERE  CREID = nCreIdRegFin;
                           ELSE
                              sTmfFonction := NULL;
                           END IF;
                        EXCEPTION
                           WHEN OTHERS THEN
                              sTmfFonction := NULL;
                        END;
                        IF ( sTmfFonction IS NULL ) OR ( sTmfFonction NOT IN ( 'EVD_REGFIN', 'EVD_FACFOURN', 'EVD_AVOFOURN' ) ) THEN
                           bIntNega := TRUE;
                        END IF;
                     END IF;
                     nAmo := nAmo + cDosRubEch_rec.DREMTAMO;
                  END LOOP;
                  -- CV-19112004 FSA-17080 On ne doit pas tester les rubriques IAS
                  IF sControle = 'R1431' AND bEchCalc THEN
                  	SELECT COUNT(*)
                  	INTO   nAny
                  	FROM   RUBRIQUE A,
                  	       DOSRUBRIQUE B,
                  	       TUSPARAM
							WHERE  DOSID    = contrat_rec.DOSID
							AND    DRUORDRE = cDosRub_rec.DRUORDRE
							AND    A.RUBID  = B.RUBID
							AND    TUPCODE  = A.RUBCODE
                     AND    TUSNOM   = 'GROUPE';
                     IF nAny > 0 THEN
                     	bEchCalc := FALSE;
                     END IF;
                     -- ne pas faire le controle si option site DURATION + activite PRET
                     IF contrat_rec.TACCODE ='PRET' and bEchCalc THEN
                     	PA_COMMON.S_TPALOGIQUE ('DOSRUBRIQUE','RUBDURATIONFIN',nOption);
                     	IF nOption =1 THEN
                  			bEchCalc := FALSE;
                  		END IF;
                  	END IF;
                  END IF;
                  IF sControle = 'R1431' AND bEchCalc
                                         AND ABS( cDosRub_rec.DRUMTORIGINE - nAmo ) > ABS( nEpsilon ) THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBRIQUE
                     WHERE  DOSID        = contrat_rec.DOSID
                       AND  DRUORDREPREC = nDruOrdre;
                     IF nAny = 0 AND cDosRub_rec.DRUCLASSE != 'A' THEN --bd150909
                        RAISE err_parm1;
                     END IF;
                  END IF;
                  IF sControle = 'R1432' AND bIntNega AND cDosRub_rec.DRUTYPEMONTAGE != 'CVT' AND cDosRub_rec.DRUCLASSE != 'A' THEN --bd150909
                     RAISE err_parm2;
                  END IF;
               ELSIF ( cDosRub_rec.DRUTYPE = 'D' ) THEN
                  IF ( sControle IN ( 'R1401', 'R1402', 'R1403', 'R1404', 'R1433', 'R1434', 'R1435', 'R1436' ) ) THEN
                     dtDreDtFin := NULL;
                     FOR cDre_rec IN  cDre LOOP
                        BEGIN
                           IF sControle = 'R1401' AND cDre_rec.DREDTDEB IS NULL THEN
                              RAISE err_parm4;
                           ELSIF sControle = 'R1402' AND cDre_rec.DREDTFIN IS NULL THEN
                              RAISE err_parm4;
                           ELSIF sControle = 'R1403' AND NVL(cDre_rec.DREMTBASE, 0 ) = 0 THEN
                              RAISE err_parm4;
                           ELSIF sControle = 'R1404' AND cDre_rec.DRENUM IS NULL THEN
                              RAISE err_parm4;
                           -- Coherence des donnees --
                           ELSIF sControle = 'R1433' AND cDre_rec.DREDTDEB IS NOT NULL
                                                     AND cDre_rec.DREDTFIN IS NOT NULL THEN
                              IF cDre_rec.DREDTFIN < cDre_rec.DREDTDEB THEN
         	                     dtErrMsg1 := cDre_rec.DREDTFIN;
                                 dtErrMsg2 := cDre_rec.DREDTDEB;
                                 RAISE err_parm5;
                              END IF;
                           ELSIF sControle = 'R1434' AND cDre_rec.DREDTDEB IS NOT NULL
                                                     AND cDre_rec.DREDTFIN IS NOT NULL THEN
                              IF cDre_rec.DREDTECH < cDre_rec.DREDTDEB THEN
         	                     dtErrMsg1 := cDre_rec.DREDTECH;
                                 dtErrMsg2 := cDre_rec.DREDTDEB;
                                 RAISE err_parm5;
                              END IF;
                           ELSIF sControle = 'R1435' AND cDre_rec.DREDTDEB IS NOT NULL
                                                     AND cDre_rec.DREDTFIN IS NOT NULL THEN
                              IF cDre_rec.DREDTECH > cDre_rec.DREDTFIN THEN
         	                     dtErrMsg1 := cDre_rec.DREDTECH;
                                 dtErrMsg2 := cDre_rec.DREDTFIN;
                                 RAISE err_parm5;
                              END IF;
                           ELSIF sControle = 'R1436' AND cDre_rec.DREDTDEB IS NOT NULL
                                                     AND dtDreDtFin            IS NOT NULL THEN
                              IF dtDreDtFin > cDre_rec.DREDTDEB THEN
         	                     dtErrMsg1 := dtDreDtFin;
                                 dtErrMsg2 := cDre_rec.DREDTDEB;
                                 RAISE err_parm5;
                              END IF;
                           END IF;
                        EXCEPTION
                           WHEN err_parm4 THEN
                              lOk          	    := 0;
	                           nb_element   	    := nb_element + 1;
                              nOrdre		       := nb_element;
                              aOrdre(nb_element) := nOrdre;
			                     aMsg(nb_element)   := nDruOrdre;
                              aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
                           WHEN err_parm5 THEN
                              lOk          	    := 0;
                              nb_element   	    := nb_element + 1;
                              nOrdre             := nb_element;
         		               aOrdre(nb_element) := nOrdre;
         	                  aMsg(nb_element)   := TO_CHAR( dtErrMsg1, 'YYYYMMDD' );
                              aType(nb_element)  := 'D';
                              aDec(nb_element)   := NULL;
	                           nb_element   	    := nb_element + 1;
         		               aOrdre(nb_element) := nOrdre;
         	                  aMsg(nb_element)   := TO_CHAR( dtErrMsg2, 'YYYYMMDD' );
                              aType(nb_element)  := 'D';
                              aDec(nb_element)   := NULL;
			                     nb_element   	    := nb_element + 1;
         		               aOrdre(nb_element) := nOrdre;
         		               aMsg(nb_element)   := nDruOrdre;
			                     aType(nb_element)  := 'N';
         		               aDec(nb_element)   := NULL;
                        END;
                        -- Recup de la date fin du precedent
                        dtDreDtFin := cDre_rec.DREDTFIN;
                     END LOOP;
                  ELSIF sControle = 'R1437' AND cDosRub_rec.DRUDTDEB IS NOT NULL THEN
                     BEGIN
                        SELECT MIN( DREDTDEB )
                        INTO   dtControle
                        FROM   DOSRUBECHEANCIER
                        WHERE  DOSID     = contrat_rec.DOSID
                        AND    DRUORDRE  = nDruOrdre;
                     EXCEPTION
                        WHEN OTHERS THEN
                           dtControle := NULL;
                     END;
                     IF dtControle IS NOT NULL THEN
                        IF dtControle != cDosRub_rec.DRUDTDEB THEN
         	               dtErrMsg1 := dtControle;
                           dtErrMsg2 := cDosRub_rec.DRUDTDEB;
                           RAISE err_parm5;
                        END IF;
                     END IF;
                  ELSIF sControle = 'R1438' AND cDosRub_rec.DRUDTFIN  IS NOT NULL THEN
                     BEGIN
                        SELECT MAX( DREDTFIN )
                        INTO   dtControle
                        FROM   DOSRUBECHEANCIER
                        WHERE  DOSID     = contrat_rec.DOSID
                        AND    DRUORDRE  = nDruOrdre;
                     EXCEPTION
                        WHEN OTHERS THEN
                           dtControle := NULL;
                     END;
                     IF dtControle IS NOT NULL THEN
                        IF dtControle != cDosRub_rec.DRUDTFIN THEN
         	               dtErrMsg1 := dtControle;
                           dtErrMsg2 := cDosRub_rec.DRUDTFIN;
                           RAISE err_parm5;
                        END IF;
                     END IF;
                  END IF;
               ELSIF   sControle = 'R1440' AND cDosRub_rec.DRUTYPE = 'R' AND cDosRub_rec.DRUCLASSE = 'A'
                AND SUBSTR(cDosRub_rec.DRUTYPEMONTAGE,1,4) = 'ASSI' AND nDruTxCalcMax IS NOT NULL  THEN
                   DBMS_OUTPUT.PUT_LINE('nDruTxCalcMax' || nDruTxCalcMax  );
                   SELECT MAX(DRFTXASSIETTE)
                   INTO   nMtMaxEch
                   FROM   DOSRUBFLUX
                   WHERE  DOSID =contrat_rec.DOSID
                   AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                  IF NVL(nMtMaxEch,0) > nDruTxCalcMax THEN
                      DBMS_OUTPUT.PUT_LINE('erre6' );
                     RAISE err_parm6;
                  END if;
               ELSIF   sControle = 'R1441' AND cDosRub_rec.DRUTYPE = 'R' THEN
                  BEGIN
                     SELECT MAX(DREMTBASE)
                     INTO nMtMaxEch
                     FROM DOSRUBECHEANCIER
                     WHERE DOSID =contrat_rec.DOSID
                     AND   DRUORDRE = cDosRub_rec.DRUORDRE;
                    DBMS_OUTPUT.PUT_LINE('nMtMaxEch' || nMtMaxEch  );
                  EXCEPTION
                     WHEN OTHERS THEN
                        nMtMaxEch := NULL;
                  END ;
                  DBMS_OUTPUT.PUT_LINE('nDreMtBaseMax' || nDreMtBaseMax  );
                  if nMtMaxEch IS NOT NULL AND nDreMtBaseMax IS NOT NULL  THEN
                     if nMtMaxEch  > nDreMtBaseMax THEN
                        RAISE err_parm7;
                     END if;
                  END if;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
         		   nOrdre		       := nb_element;
         			aOrdre(nb_element) := nOrdre;
			         aMsg(nb_element)   := nAmo;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
	      	      nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         	      aMsg(nb_element)   := cDosRub_rec.DRUMTORIGINE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
			         nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         		   aMsg(nb_element)   := nDruOrdre;
			         aType(nb_element)  := 'N';
         		   aDec(nb_element)   := NULL;
               WHEN err_parm2 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
         		   nOrdre		       := nb_element;
         		   aOrdre(nb_element) := nOrdre;
         		   aMsg(nb_element)   := nDruOrdre;
			         aType(nb_element)  := 'N';
         		   aDec(nb_element)   := NULL;
               WHEN err_parm5 THEN
                  lOk          	    := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
         		   aOrdre(nb_element) := nOrdre;
         	      aMsg(nb_element)   := TO_CHAR( dtErrMsg1, 'YYYYMMDD' );
                  aType(nb_element)  := 'D';
                  aDec(nb_element)   := NULL;
	               nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         	      aMsg(nb_element)   := TO_CHAR( dtErrMsg2, 'YYYYMMDD' );
                  aType(nb_element)  := 'D';
                  aDec(nb_element)   := NULL;
			         nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         		   aMsg(nb_element)   := nDruOrdre;
			         aType(nb_element)  := 'N';
         		   aDec(nb_element)   := NULL;
               WHEN err_parm6 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
         		   nOrdre		       := nb_element;
         			aOrdre(nb_element) := nOrdre;
			         aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
	      	      nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         	      aMsg(nb_element)   := nDruTxCalcMax;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parm7 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
         		   nOrdre		       := nb_element;
         			aOrdre(nb_element) := nOrdre;
			         aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
	      	      nb_element   	    := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
         	      aMsg(nb_element)   := nDreMtBaseMax;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubEch;

	/*
	// Controles flux des rubriques
	*/
	FUNCTION ocDossRubFlux(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk       NUMBER := 1;
		   nOrdre    NUMBER := 0;
		   nAny      NUMBER;
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;
         err_parm3 EXCEPTION;
         err_parm4 EXCEPTION;
         err_parm5 EXCEPTION;
         err_parm6 EXCEPTION;
         err_parm7 EXCEPTION;
         nRubrique DOSRUBRIQUE.DRUORDRE%TYPE;
         nFlux     DOSRUBFLUX.DRFORDRE%TYPE;
         dDeb      DATE;
         dFin      DATE;
         dDate1    DATE;
         dDate2    DATE;
         sCode     VARCHAR2(200);
         nSum      NUMBER;
         nAmo      NUMBER;
         nSum1     NUMBER;
         nSum2     NUMBER;
         nEpsilon  NUMBER;
         sTableNom TTRPARAM.TTRNOM%TYPE;
         sDrfPerception DOSRUBFLUX.DRFPERCEPTION%TYPE;
         nDruOrdre DOSRUBRIQUE.DRUORDRE%TYPE;
         nDrfOrdre DOSRUBFLUX.DRFORDRE%TYPE;
         sBimNum BIENIMMOBILIER.BIMNUM%TYPE;

         sPhaCode       DOSPHASE.PHACODE%TYPE;
         sDruTypeConsCa DOSRUBRIQUE.DRUTYPEMONTAGE%TYPE;
         dtDrbDtFin     DOSRENOUVBAIL.DRBDTFIN%TYPE;
         sNbaCode       DOSRENOUVBAIL.NBACODE%TYPE;
         nCount         NUMBER;
         dtDreDtFin     DOSRUBECHEANCIER.DREDTFIN%TYPE;
         nEpsiPal       NUMBER;

	 nCountPeriode    NUMBER;
	 nCountFiltre     NUMBER;

         nDacOrdre      DOSACTEUR.DACORDRE%TYPE;
         bErreur        BOOLEAN;
         nNbPeriodeFlux NUMBER;  -- CV-27042007 CFS21971 Report CFS21971 BATICAL

         CURSOR cDosRub IS
            SELECT DRUORDRE, DRUMONTAGE, DRUMTORIGINE, DRUTYPE, DRUCLASSE, DRUTYPEMONTAGE,
                   DRUDTDEB, DRUMTRESIDUEL, DRUDTFIN, DRUORDREFACTURATION, RUBID,
                   DRUORDREMAITRE, DRUSENS, DACORDRE, DRUFLAGMANUEL, DRUMODCALINT, DRUINTSIMPLEPERIODE
            FROM   DOSRUBRIQUE
            WHERE  DOSID    = contrat_rec.DOSID
              AND  DRUTYPE != 'P';

         CURSOR cDosFlux IS
            SELECT DRFPERIODE, DRFDTDEB, DRFMULTIPLE, DRFPERCEPTION, DRFMT,
                   DRFNBSAISON, DRFFRANCHISE, DRFPROGRESSION, DRFORDRE, DRFNBPERIODE,
                   DRFDTFIN, DRFTXASSIETTE
            FROM   DOSRUBFLUX
            WHERE  DOSID    = contrat_rec.DOSID
              AND  DRUORDRE = nRubrique
            ORDER BY DRFORDRE;

         CURSOR cLkDrfDbi IS
            SELECT DBIORDRE, BIMID, DBIMT
            FROM   LKDRFDBI
            WHERE  DOSID    = contrat_rec.DOSID
              AND  DRUORDRE = nDruOrdre
              AND  DRFORDRE = nDrfOrdre;

         CURSOR cDau IS
            SELECT DAU.DAUDTDEB, DAU.DAUDTFIN, AUN.AUNPERIODE, AUN.AUNMULTIPLE
            FROM   DOSACTUNITE DAU,
                   ACTUNITE AUN
            WHERE  DAU.DOSID    = contrat_rec.DOSID
              AND  DAU.DACORDRE = nDacOrdre
              AND  AUN.ACTID    = DAU.ACTID
              AND  AUN.AUNORDRE = DAU.AUNORDRE
            ORDER BY DAU.DAUDTDEB;
      BEGIN
         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               nRubrique := cDosRub_rec.DRUORDRE;
               nDacOrdre := cDosRub_rec.DACORDRE;
               dDeb      := NULL;
               nAmo      := 0;
               dFin      := NULL;
               IF sControle = 'R1370' THEN
                  IF cDosRub_rec.DRUMONTAGE IS NOT NULL AND cDosRub_rec.DRUMONTAGE = 'FISCAL' THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBAMOFISCAL
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = nRubrique;
                     IF nAny = 0 THEN
                        RAISE err_parm1;
                     END IF;
                  ELSIF ( cDosRub_rec.DRUTYPE != 'D' ) THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBFLUX
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = nRubrique;
                     IF nAny = 0 THEN
                        RAISE err_parm1;
                     END IF;
                  END IF;
               ELSIF sControle = 'R1341' THEN
                  IF cDosRub_rec.DRUMONTAGE = 'FISCAL' THEN
                     SELECT NVL(SUM(DAFMT),0)
                     INTO   nAmo
                     FROM   DOSRUBAMOFISCAL
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = nRubrique;
                     nEpsilon   := pa_fungencontrole.GetEpsilonParam( 'MONTANT' );
                     IF NVL(ABS(round(cDosRub_rec.DRUMTORIGINE, 2) - round(nAmo,2)), 0) > ABS( nEpsilon) THEN
                        nSum := nAmo;
                        RAISE err_parm7;
                     END IF;
                  END IF;
               ELSIF ( sControle = 'R1346' ) THEN
                  IF ( contrat_rec.TACCODE = 'LOCATIF' ) AND ( cDosRub_rec.DRUTYPEMONTAGE = 'CASEUL' ) THEN
                     SELECT PHACODE
                     INTO   sPhaCode
                     FROM   DOSPHASE
                     WHERE  DOSID   = contrat_rec.DOSID
                       AND  PHADEST = 'DOSSIER'
                       AND  DPHDTFIN IS NULL;
                     IF ( sPhaCode = 'INI' ) THEN
                        SELECT COUNT(1)
                        INTO   nAny
                        FROM   DOSRUBFLUX
                        WHERE  DOSID    = contrat_rec.DOSID
                          AND  DRUORDRE = nRubrique;

                        IF ( nAny > 1 ) THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  END IF;
               ELSE
                  FOR cDosFlux_rec IN cDosFlux LOOP
                     BEGIN
                        IF dDeb IS NULL THEN
                           dDeb := cDosFlux_rec.DRFDTDEB;
                        ELSIF sControle = 'R1332' AND cDosFlux_rec.DRFDTDEB != dFin + 1 THEN
                           IF cDosFlux_rec.DRFDTDEB != dFin THEN
                              RAISE err_parm1;
                           END IF;
                        END IF;
                        IF sControle = 'R1343' AND cDosRub_rec.DRUTYPE = 'V' AND cDosRub_rec.DRUORDREFACTURATION IS NOT NULL THEN
                           BEGIN
					   SELECT DRFPERCEPTION
      	                     INTO   sDrfPerception
            	               FROM   DOSRUBFLUX
                  	         WHERE  DOSID    = contrat_rec.DOSID
                        	   AND    DRUORDRE = cDosRub_rec.DRUORDREFACTURATION
					   AND    DRFORDRE = (SELECT MAX(DRFORDRE)
                                               FROM   DOSRUBFLUX
                                               WHERE  DOSID = contrat_rec.DOSID
                                               AND    DRUORDRE = cDosRub_rec.DRUORDREFACTURATION
                                               AND    cDosFlux_rec.DRFDTFIN BETWEEN DRFDTDEB AND DRFDTFIN );
			         EXCEPTION
					WHEN OTHERS THEN
						sDrfPerception := NULL;
				   END;
					/*
                           AND    DRFORDRE = ( SELECT MIN( DRFORDRE )
                                               FROM   DOSRUBFLUX
                                               WHERE  DOSID = contrat_rec.DOSID
                                               AND    DRUORDRE = cDosRub_rec.DRUORDREFACTURATION
                                               AND    DRFDTDEB = cDosFlux_rec.DRFDTDEB );*/
                           IF sDrfPerception IS NULL OR sDrfPerception != cDosFlux_rec.DRFPERCEPTION THEN
                              RAISE err_parm2;
                           END IF;
                        ELSIF sControle IN ('R1347', 'R1348') AND contrat_rec.TACCODE = 'LOCATIF' THEN
                           BEGIN
                              SELECT DRBDTFIN , NBACODE
                              INTO   dtDrbDtFin , sNbaCode
                              FROM   DOSRENOUVBAIL
                              WHERE  DOSID = contrat_rec.DOSID
                              AND    DRBORDRE = (SELECT MAX(DRBORDRE) FROM DOSRENOUVBAIL
                                                WHERE DOSID = contrat_rec.DOSID);
                           EXCEPTION
                              WHEN OTHERS THEN
                                 dtDrbDtFin := NULL;
                           END;
                           -- TD le 270701 --
                           BEGIN
                                 SELECT MAX(DREDTFIN)
                                 INTO   dtDreDtFin
                                 FROM   DOSRUBECHEANCIER
                                 WHERE  DOSID    = contrat_rec.DOSID
                                 AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                           EXCEPTION WHEN OTHERS THEN
                                 dtDreDtFin := NULL;
                           END;
                           -- Prendre en compte la date Max(dredtfin)
                           IF dtDreDtFin IS NOT NULL THEN
                              IF dtDreDtFin > dtDrbDtFin THEN
                                 dtDrbDtFin := dtDreDtFin;
                              END IF;
                           END IF;
                           IF cDosRub_rec.DRUDTDEB <= dtDrbDtFin THEN
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   NATUREBAIL
                              WHERE  NBACODE = sNbaCode
                              AND    NBADECRET = '1953'
                              AND    NBAFLAGPRECAIRE = 1;

                              IF cDosFlux_rec.DRFDTFIN > dtDrbDtFin  AND
                                 ( (sControle = 'R1347' AND nAny > 0 ) OR (sControle = 'R1348' AND nAny =0))   THEN
                                  RAISE err_parm2;
                              ELSE
                                 SELECT COUNT(*)
                                 INTO   nCount
                                 FROM   DOSRUBFLUPALIER
                                 WHERE  DOSID = contrat_rec.DOSID
                                 AND    DRUORDRE = cDosRub_rec.DRUORDRE
                                 AND    DRFORDRE = cDosFlux_rec.DRFORDRE
                                 AND    DFPDTFIN > dtDrbDtFin ;
                                 IF nCount > 0  AND
                                 ( (sControle = 'R1347' AND nAny > 0 ) OR (sControle = 'R1348' AND nAny =0))   THEN
                                    RAISE err_parm2;
                                 END IF;
                              END IF;
                           END IF ;
                        ELSIF sControle = 'R1349' AND contrat_rec.TACCODE = 'LOCATIF' THEN
                           IF cDosFlux_rec.DRFDTFIN < cDosFlux_rec.DRFDTDEB  OR cDosFlux_rec.DRFDTFIN < cDosRub_rec.DRUDTDEB OR
                              cDosFlux_rec.DRFDTFIN > cDosRub_rec.DRUDTFIN OR cDosFlux_rec.DRFDTDEB < cDosRub_rec.DRUDTDEB OR
                              cDosFlux_rec.DRFDTDEB > cDosRub_rec.DRUDTFIN THEN
                                 RAISE err_parm2;
                           END IF;
                        ELSIF sControle = 'R1301' AND ( ( ( cDosRub_rec.DRUMONTAGE  IS NOT NULL ) AND ( cDosRub_rec.DRUMONTAGE  != 'SAISON' ) )
                                               OR    IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) )
                                               AND cDosFlux_rec.DRFPERIODE IS NULL
                                               AND  F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'DEPGAR' ) <> 1 THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1302' AND ( ( ( cDosRub_rec.DRUMONTAGE  IS NOT NULL ) AND ( cDosRub_rec.DRUMONTAGE  != 'SAISON' ) )
                                                  OR    IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) )
                                                  AND cDosFlux_rec.DRFMULTIPLE IS NULL
                                                  AND  F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'DEPGAR' ) <> 1 THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1303' AND cDosRub_rec.DRUMONTAGE     IS NOT NULL
                                                  AND cDosRub_rec.DRUMONTAGE     != 'SAISON'
                                                  AND cDosFlux_rec.DRFPERCEPTION IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1304' AND NOT IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                                  AND cDosFlux_rec.DRFNBPERIODE IS NULL
                                                  AND cDosFlux_rec.DRFNBSAISON  IS NULL
                                                  AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'DEPGAR' ) <> 1 THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1305' AND cDosRub_rec.DRUMONTAGE IS NOT NULL
                                                  AND cDosRub_rec.DRUCLASSE = 'F'
                                                  AND cDosRub_rec.DRUMONTAGE IN ( 'LOYFIX', 'AMOFIX' )
                                                  AND cDosFlux_rec.DRFMT IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1307' AND cDosRub_rec.DRUCLASSE = 'A'
                                                  AND cDosRub_rec.DRUTYPEMONTAGE IN ( 'ASSIETT', 'ASSIEIN' )
                                                  AND cDosFlux_rec.DRFTXASSIETTE IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1306' AND NOT IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                                  AND cDosRub_rec.DRUTYPEMONTAGE  NOT IN ( 'ASSIETT', 'ASSIEIN' )
                                                  AND cDosFlux_rec.DRFPROGRESSION IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1308' AND NVL(cDosFlux_rec.DRFMT,0) = 0 THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1309' AND cDosRub_rec.DRUCLASSE   = 'F'
                                                  AND cDosRub_rec.DRUMONTAGE IS NOT NULL
                                                  AND cDosRub_rec.DRUMONTAGE IN ( 'LOYFIX', 'AMOFIX' )
                                                  AND cDosFlux_rec.DRFMT      < 0 THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1335' AND cDosFlux_rec.DRFFRANCHISE IS NOT NULL
                                                  AND cDosFlux_rec.DRFNBPERIODE IS NOT NULL
                                                  AND cDosFlux_rec.DRFFRANCHISE > cDosFlux_rec.DRFNBPERIODE THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1373' AND cDosFlux_rec.DRFPROGRESSION IS NOT NULL
                                                  AND cDosRub_rec.DRUMONTAGE      IS NOT NULL
                                                  AND cDosRub_rec.DRUCLASSE       IS NOT NULL
                                                  AND cDosRub_rec.DRUTYPEMONTAGE  IS NOT NULL
                                                  AND cDosRub_rec.DRUTYPEMONTAGE  NOT IN ( 'ASSIETT', 'ASSIEIN' ) THEN
                           IF cDosRub_rec.DRUCLASSE = 'A' THEN
                              IF ( contrat_rec.TACCODE = 'LOCATIF' ) THEN
                                 sTableNom := 'LSPROGACC';
                              ELSE
                                 sTableNom := 'PROGACC';
                              END IF;
                           ELSE
                              IF cDosRub_rec.DRUMONTAGE = 'LOYFIX' THEN
                                 sTableNom := 'PROGLOYFIX';
                              ELSIF cDosRub_rec.DRUMONTAGE = 'SAISON' THEN
                                 sTableNom := 'PROGSAISON';
                              ELSE
                                 sTableNom := 'PROGAMOFIX';
                              END IF;
                           END IF;
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   TTRPARAM
                           WHERE  TTRNOM  = sTableNom
                           AND    TTPCODE = cDosFlux_rec.DRFPROGRESSION;
                           IF nAny = 0 THEN
                              sCode := cDosFlux_rec.DRFPROGRESSION;
                              RAISE err_parm4;
                           END IF;
                        ELSIF sControle = 'R1371' AND cDosFlux_rec.DRFPERIODE IS NOT NULL THEN
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   TTRPARAM
                           WHERE  TTRNOM  = 'PERIODE'
                           AND    TTPCODE = cDosFlux_rec.DRFPERIODE;
                           IF nAny = 0 THEN
                              sCode := cDosFlux_rec.DRFPERIODE;
                              RAISE err_parm4;
                           END IF;
                        ELSIF sControle = 'R1372' AND cDosFlux_rec.DRFPERCEPTION IS NOT NULL THEN
                           IF ( contrat_rec.TACCODE = 'LOCATIF' ) THEN
                              sTableNom := 'LSPERCEPT';
                           ELSE
                              IF IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) THEN
                                 sTableNom := 'PERCEPTIONPRE';
                              ELSE
                                 sTableNom := 'PERCEPTION';
                              END IF;
                           END IF;
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   TTRPARAM
                           WHERE  TTRNOM  = sTableNom
                           AND    TTPCODE = cDosFlux_rec.DRFPERCEPTION;
                           IF nAny = 0 THEN
                              sCode := cDosFlux_rec.DRFPERCEPTION;
                              RAISE err_parm4;
                           END IF;
                        ELSIF ( sControle = 'R1336' AND SUBSTR( cDosFlux_rec.DRFPROGRESSION, 1 ) = 'P'
                                                    AND cDosFlux_rec.DRFNBPERIODE IS NOT NULL ) OR
                              ( sControle = 'R1338' AND SUBSTR( cDosFlux_rec.DRFPROGRESSION, 1 ) = 'P'
                                                    AND cDosRub_rec.DRUMONTAGE IS NOT NULL
                                                    AND cDosRub_rec.DRUMONTAGE =  'AMOFIX'
                                                    AND NVL(cDosRub_rec.DRUFLAGMANUEL,0) = 0
                                                    AND cDosFlux_rec.DRFMT     IS NOT NULL ) THEN
                           nFlux := cDosFlux_rec.DRFORDRE;
                           /*
                           nSum  := 0;
                           optimisation JPB
                           FOR cDosPal_rec IN cDosPal LOOP
                              IF sControle = 'R1338' THEN
                                 nSum := nSum + ( NVL( cDosPal_rec.DFPMT, 0 ) * NVL( cDosPal_rec.DFPNBPERIODE, 0 ) );
                              ELSE
                                 nSum := nSum + NVL( cDosPal_rec.DFPNBPERIODE, 0 );
                              END IF;
                           END LOOP;
                           */
                           SELECT NVL(SUM(NVL(DFPMT,0)* NVL(DFPNBPERIODE,0)),0), NVL(SUM(DFPNBPERIODE),0)
                           INTO   nSum1, nSum2
                           FROM   DOSRUBFLUPALIER
                           WHERE  DOSID    = contrat_rec.DOSID
                           AND    DRUORDRE = nRubrique
                           AND    DRFORDRE = nFlux;
                           -- CV-27042007 CFS22064 Report CFS21971 BATICAL : repartis lisse
                           IF cDosRub_rec.DRUMODCALINT = 'IS' AND cDosRub_rec.DRUINTSIMPLEPERIODE != cDosFlux_rec.DRFPERIODE THEN
                              SELECT NVL(SUM(DFPNBPERIODE),0)
                              INTO   nSum2
                              FROM   DOSRUBFLUPALIER
                              WHERE  DOSID    = contrat_rec.DOSID
                              AND    DRUORDRE = nRubrique
                              AND    DRFORDRE = nFlux;
                              nNbPeriodeFlux := cDosFlux_rec.DRFNBPERIODE * TO_NUMBER(cDosFlux_rec.DRFPERIODE);
                              nSum2          := nSum2 * TO_NUMBER(cDosRub_rec.DRUINTSIMPLEPERIODE);
                           ELSE
                              nNbPeriodeFlux := cDosFlux_rec.DRFNBPERIODE;
                           END IF;
                           nEpsiPal := pa_fungencontrole.GetEpsilonParam( 'MONTANT' );

                           IF sControle = 'R1338' AND abs(nSum1 - cDosFlux_rec.DRFMT ) > nEpsiPal THEN
                              nSum:= nSum1;
                              RAISE err_parm5;
                           -- CV-27042007 CFS21971 BATICAL
                           -- ELSIF sControle = 'R1336' AND nSum2 != cDosFlux_rec.DRFNBPERIODE THEN
                           ELSIF sControle = 'R1336' AND nSum2 != nNbPeriodeFlux THEN
                              nSum:= nSum2;
                              RAISE err_parm2;
                           END IF;
                        ELSIF sControle = 'R1342' AND IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                                  AND cDosFlux_rec.DRFMULTIPLE IS NOT NULL
                                                  AND cDosFlux_rec.DRFMULTIPLE < 0 THEN
                           RAISE err_parm2;
                        ELSIF ( ( sControle = 'R1374' ) AND ( contrat_rec.TACCODE = 'LOCATIF' )
													    and f_PlDossierIsModele(contrat_rec.DOSID) = 0
                                                        -- FSA 14976 Ne pas passer le controle sur les rubriques generees par
                                                        -- l'evenement de constatation du CA (REGUL et FUTUR)
                                                        AND ( cDosRub_rec.DRUTYPEMONTAGE NOT IN ('REGUL', 'FUTUR') )
                                                        AND ( cDosRub_rec.DRUSENS != '-' )
                                                        AND ( F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ACCLOY' ) = 1 ) ) THEN
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   LKDRFDBI
                              WHERE  DOSID    = contrat_rec.DOSID
                                AND  DRUORDRE = cDosRub_rec.DRUORDRE
                                AND  DRFORDRE = cDosFlux_rec.DRFORDRE;
                              IF nAny = 0 THEN
                                 RAISE ERR_PARM2;
                              END IF;
                        ELSIF ( ( sControle = 'R1376' ) AND ( contrat_rec.TACCODE = 'LOCATIF' )
                                                        AND ( cDosRub_rec.DRUTYPEMONTAGE != 'REGUL' )
                                                        AND ( cDosRub_rec.DRUSENS = '-' )
                                                        AND ( F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ACCLOY' ) = 1 ) ) THEN
                           IF ( nAny != 0 ) THEN
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   LKDRFDBI
                              WHERE  DOSID    = contrat_rec.DOSID
                                AND  DRUORDRE = cDosRub_rec.DRUORDRE
                                AND  DRFORDRE = cDosFlux_rec.DRFORDRE;
                              IF nAny = 0 THEN
                                 RAISE ERR_PARM2;
                              END IF;
                           END IF;                        ELSIF  ( sControle = 'R1377' ) AND ( contrat_rec.TACCODE = 'LOCATIF' ) THEN
                        	nDruOrdre := cDosRub_rec.DRUORDRE;
                           nDrfOrdre := cDosFlux_rec.DRFORDRE;
                        	FOR cLkDrfDbi_rec IN cLkDrfDbi LOOP
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   DOSBIM
                              WHERE  DOSID    = contrat_rec.DOSID
                                AND  DBIORDRE =cLkDrfDbi_rec.DBIORDRE
                                AND  DBIDTDEBOCCUP <= cDosFlux_rec.DRFDTDEB
                                AND	 NVL(DBIDTFINOCCUP,TO_DATE(31129998,'DDMMYYYY')) >= NVL(cDosFlux_rec.DRFDTFIN,TO_DATE(31129998,'DDMMYYYY'));
                              IF nAny = 0 THEN
                             	    dbms_output.put_line('R1377 ' );
                              	  SELECT BIMNUM INTO sCode
                              	  FROM   BIENIMMOBILIER
                              	  WHERE  BIMID = (SELECT BIMID FROM DOSBIM
                              	  						WHERE DOSID = contrat_rec.DOSID
                              	  						AND   DBIORDRE =cLkDrfDbi_rec.DBIORDRE);
                                   lOk                := 0;
			                           nb_element   	    := nb_element + 1;
         			                  nOrdre             := nb_element;
                  			         aOrdre(nb_element) := nOrdre;
                           			aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
			                           aType(nb_element)  := 'N';
         			        	         aDec(nb_element)   := NULL;
                  			         nb_element   	    := nb_element + 1;
                           			aOrdre(nb_element) := nOrdre;
			                           aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
         			                  aType(nb_element)  := 'N';
                 				         aDec(nb_element)   := NULL;
                           			nb_element   	    := nb_element + 1;
			                           aOrdre(nb_element) := nOrdre;
         			                  aMsg(nb_element)   := F_StdTrimAll(sCode);
                           			aType(nb_element)  := 'C';
			                 	         aDec(nb_element)   := NULL;
                            	END IF;
                        	END LOOP;
                        ELSIF ( ( sControle = 'R1375' ) AND ( contrat_rec.TACCODE = 'LOCATIF' ) ) THEN
                           SELECT   COUNT(*)
                           INTO     nAny
                           FROM     RUBRIQUE
                           WHERE    RUBID = cDosRub_rec.RUBID
                           AND      ( RUBPROVISION IS NOT NULL
                                      OR
                                      RUBID IN (SELECT RUBID FROM RUBACCES
                                                   WHERE RACACCES = 'ACCLOY'));
                           IF nAny > 0 THEN
                           nDruOrdre := cDosRub_rec.DRUORDRE;
                           nDrfOrdre := cDosFlux_rec.DRFORDRE;
                           FOR cLkDrfDbi_rec IN cLkDrfDbi LOOP
                              IF ( cDosRub_rec.DRUTYPEMONTAGE IN ( 'CASEUL', 'CAPLUS', 'CAMIXT' ) ) THEN
                                 SELECT COUNT(*)
                                 INTO   nAny
                                 FROM   LKDRFDBI DBI,
                                        DOSRUBRIQUE DRU,
                                        DOSRUBFLUX DRF
                                 WHERE  DBI.DOSID             = contrat_rec.DOSID
                                   AND  DBI.DRUORDRE         != cDosRub_rec.DRUORDRE
                                   AND  DBI.DBIORDRE          = cLkDrfDbi_rec.DBIORDRE
                                   AND  DBI.BIMID             = cLkDrfDbi_rec.BIMID
                                   AND  DBI.DOSID             = DRU.DOSID
                                   AND  DBI.DRUORDRE          = DRU.DRUORDRE
                                   AND  DRU.RUBID             = cDosRub_rec.RUBID
                                   AND  ( DRU.DRUORDREMAITRE IS NULL OR DRU.DRUORDREMAITRE != cDosRub_rec.DRUORDRE )
                                   AND  DBI.DOSID = DRF.DOSID
                                   AND  DBI.DRUORDRE = DRF.DRUORDRE
                                   AND  DBI.DRFORDRE = DRF.DRFORDRE
                                   AND  DRF.DRFDTDEB  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN
                                   AND  DRF.DRFDTFIN  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN;
                              ELSIF ( cDosRub_rec.DRUTYPEMONTAGE = 'FUTUR' ) THEN
                                 SELECT COUNT(*)
                                 INTO   nAny
                                 FROM   LKDRFDBI DBI,
                                        DOSRUBRIQUE DRU,
                                        DOSRUBFLUX DRF
                                 WHERE  DBI.DOSID           = contrat_rec.DOSID
                                   AND  DBI.DRUORDRE       != cDosRub_rec.DRUORDRE
                                   AND  DBI.DBIORDRE        = cLkDrfDbi_rec.DBIORDRE
                                   AND  DBI.BIMID           = cLkDrfDbi_rec.BIMID
                                   AND  DBI.DOSID           = DRU.DOSID
                                   AND  DBI.DRUORDRE        = DRU.DRUORDRE
                                   AND  DRU.RUBID           = cDosRub_rec.RUBID
                                   AND  DBI.DRUORDRE       != cDosRub_rec.DRUORDREMAITRE
                                   AND  DBI.DOSID = DRF.DOSID
                                   AND  DBI.DRUORDRE = DRF.DRUORDRE
                                   AND  DBI.DRFORDRE = DRF.DRFORDRE
                                   AND  DRF.DRFDTDEB  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN
                                   AND  DRF.DRFDTFIN  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN;
                              ELSE
                                 SELECT COUNT(*)
                                 INTO   nAny
                                 FROM   LKDRFDBI DBI,
                                        DOSRUBRIQUE DRU,
                                        DOSRUBFLUX DRF
                                 WHERE  DBI.DOSID     = contrat_rec.DOSID
                                   AND  DBI.DRUORDRE != cDosRub_rec.DRUORDRE
                                   AND  DBI.DBIORDRE  = cLkDrfDbi_rec.DBIORDRE
                                   AND  DBI.BIMID     = cLkDrfDbi_rec.BIMID
                                   AND  DBI.DOSID     = DRU.DOSID
                                   AND  DBI.DRUORDRE  = DRU.DRUORDRE
                                   AND  DRU.RUBID     = cDosRub_rec.RUBID
                                   AND  DBI.DOSID     = DRF.DOSID
                                   AND  DBI.DRUORDRE  = DRF.DRUORDRE
                                   AND  DBI.DRFORDRE  = DRF.DRFORDRE
                                   AND  DRF.DRFDTDEB  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN
                                   AND  DRF.DRFDTFIN  BETWEEN cDosFlux_rec.DRFDTDEB AND cDosFlux_rec.DRFDTFIN;
                              END IF;
                              IF ( nAny != 0 ) THEN
                                 BEGIN
                                    SELECT BIMNUM
                                    INTO   sBimNum
                                    FROM   BIENIMMOBILIER
                                    WHERE  BIMID = cLkDrfDbi_rec.BIMID;
                                 EXCEPTION
                                    WHEN OTHERS THEN
                                       sBimNum := '';
                                 END;
                                 lOk                := 0;
                                 nb_element   	    := nb_element + 1;
                                 nOrdre             := nb_element;
                                 aOrdre(nb_element) := nOrdre;
                                 aMsg(nb_element)   := F_StdTrimAll(sBimNum);
                                 aType(nb_element)  := 'C';
                       	         aDec(nb_element)   := NULL;
                                 nb_element   	    := nb_element + 1;
                                 aOrdre(nb_element) := nOrdre;
                                 aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
                                 aType(nb_element)  := 'N';
                       	         aDec(nb_element)   := NULL;
                                 nb_element   	    := nb_element + 1;
                                 aOrdre(nb_element) := nOrdre;
                                 aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                                 aType(nb_element)  := 'N';
                       	         aDec(nb_element)   := NULL;
                              END IF;
                           END LOOP;
                           END IF;
                        ELSIF ((sControle = 'R1344') AND (contrat_rec.TACCODE = 'LOCATIF')) THEN
                           SELECT SUM(DBIMT)
                           INTO   nAny
                           FROM   LKDRFDBI
                           WHERE  DOSID = contrat_rec.DOSID
                                  AND DRUORDRE = cDosRub_rec.DRUORDRE
                                  AND DRFORDRE = cDosFlux_rec.DRFORDRE;
                           IF ( nAny != cDosFlux_rec.DRFMT
                                        --     AND cDosRub_rec.DRUSENS != '-'
                                 AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ACCLOY' ) = 1) THEN
                              RAISE ERR_PARM2;
                           END IF;
                        ELSIF ( sControle = 'R1345' ) AND ( contrat_rec.TACCODE          = 'LOCATIF' )
                                                      AND ( cDosRub_rec.DRUTYPEMONTAGE   = 'CASEUL' )
                                                      AND ( cDosFlux_rec.DRFPROGRESSION != 'C' ) THEN
                           RAISE err_parm6;
                         --ALA 082007 Periodicite definie dans les filtres des la rubrique
                        ELSIF sControle = 'R1351' AND cDosFlux_rec.DRFPERIODE  IS NOT NULL THEN
                            SELECT COUNT(*)
                            INTO   nCount
                            FROM   TTRPARAM
                            WHERE  TTRNOM = 'IMPUTANAFILTRE'
                            AND    TTPCODE IN ('001', '030', '090','180', '360')
                            AND    TTPCODE = cDosFlux_rec.DRFPERIODE;
                           IF nCount > 0 THEN
                              SELECT COUNT(*)
                              INTO   nCountFiltre
                              FROM   RUBACCES
                              WHERE  RUBID = cDosRub_rec.RUBID
                              AND    RACACCES IN ('001', '030', '090','180', '360');

			      SELECT COUNT(*)
                              INTO   nCountPeriode
                              FROM   RUBACCES
                              WHERE  RUBID = cDosRub_rec.RUBID
                              AND    RACACCES = cDosFlux_rec.DRFPERIODE;

                              IF nCountFiltre > 0 AND nCountPeriode = 0 THEN
                                 RAISE err_parm2;
                              END IF;
                           END IF;
                        ELSIF ( sControle = 'R1350' ) AND ( cDosFlux_rec.DRFNBPERIODE > 1 )
                                                      AND ( cDosFlux_rec.DRFPERIODE  IS NOT NULL )
                                                      AND ( cDosFlux_rec.DRFMULTIPLE IS NOT NULL )
                                                      AND ( cDosFlux_rec.DRFDTDEB    IS NOT NULL )
                                                      AND ( cDosFlux_rec.DRFDTDEB > TO_DATE('01/01/2003','DD/MM/YYYY') ) THEN
                           bErreur := FALSE;
                           FOR cDau_rec IN cDau LOOP
                              IF ( cDosFlux_rec.DRFDTDEB >= cDau_rec.DAUDTDEB ) AND ( ( cDau_rec.DAUDTFIN IS NULL ) OR ( cDosFlux_rec.DRFDTDEB <= cDau_rec.DAUDTFIN ) ) THEN
                                 IF ( cDau_rec.AUNPERIODE IS NOT NULL ) AND ( cDau_rec.AUNMULTIPLE IS NOT NULL ) THEN
                                    IF ( cDau_rec.AUNPERIODE != cDosFlux_rec.DRFPERIODE ) OR ( cDau_rec.AUNMULTIPLE != cDosFlux_rec.DRFMULTIPLE ) THEN
                                       bErreur := TRUE;
                                    END IF;
                                 END IF;
                              END IF;
                           END LOOP;
                           IF ( bErreur ) THEN
                              RAISE err_parm2;
                           END IF;
                        END IF;
                        dFin := cDosFlux_rec.DRFDTFIN;
                        IF cDosRub_rec.DRUMONTAGE IS NOT NULL AND cDosRub_rec.DRUMONTAGE = 'AMOFIX' THEN
                           nAmo := nAmo + cDosFlux_rec.DRFMT;
                        END IF;
                     EXCEPTION
                        WHEN err_parm2 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                        WHEN err_parm4 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := F_StdTrimAll(sCode);
                           aType(nb_element)  := 'C';
                 	         aDec(nb_element)   := NULL;
                        WHEN err_parm3 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
               	         nb_element         := nb_element + 1;
			      	         aOrdre(nb_element) := nOrdre;
               	         aMsg(nb_element)   := TO_CHAR(dDate1,'YYYYMMDD');
               	         aType(nb_element)  := 'D';
               	         aDec(nb_element)   := NULL;
               	         nb_element   	    := nb_element + 1;
               	         aOrdre(nb_element) := nOrdre;
               	         aMsg(nb_element)   := TO_CHAR(dDate2,'YYYYMMDD');
               	         aType(nb_element)  := 'D';
               	         aDec(nb_element)   := NULL;
                        WHEN err_parm5 THEN
                 			   lOk          	    := 0;
	               		   nb_element   	    := nb_element + 1;
                     	   nOrdre		       := nb_element;
                           aOrdre(nb_element) := nOrdre;
			                  aMsg(nb_element)   := cDosFlux_rec.DRFMT;
               	         aType(nb_element)  := 'N';
                 			   aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
	                        nb_element   	    := nb_element + 1;
                     	   aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nSum;
                       	   aType(nb_element)  := 'N';
               			   aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
				   	         nb_element   	    := nb_element + 1;
               	         aOrdre(nb_element) := nOrdre;
               	         aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
			      	         aType(nb_element)  := 'N';
               	         aDec(nb_element)   := NULL;
				   	         nb_element   	    := nb_element + 1;
               	         aOrdre(nb_element) := nOrdre;
               	         aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
			      	         aType(nb_element)  := 'N';
               	         aDec(nb_element)   := NULL;
                        WHEN err_parm6 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := 'LANTTRPARAM|LSPROGACC|C';
                           aType(nb_element)  := 'S';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosFlux_rec.DRFORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                 	         aDec(nb_element)   := NULL;
                     END;
                  END LOOP;
                  IF sControle = 'R1330' AND dDeb != cDosRub_rec.DRUDTDEB AND F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'DOWNP')  = 0 THEN
                     SELECT COUNT(1)
      		   INTO   nCount
      		   FROM   TUSPARAM TTR, RUBRIQUE
      		   WHERE  TTR.TUSNOM = 'GROUPE' AND RUBRIQUE.RUBID = cDosRub_rec.RUBID AND TTR.TUPCODE = RUBRIQUE.RUBCODE;
                     IF NOT ( nCount > 0  AND cDosRub_rec.DRUCLASSE = 'F' AND cDosRub_rec.DRUTYPE = 'B') THEN
                        dDate1 := dDeb;
                        dDate2 := cDosRub_rec.DRUDTDEB;
                        RAISE err_parm2;
                     END IF;
                  ELSIF sControle = 'R1331' AND dFin != cDosRub_rec.DRUDTFIN AND cDosRub_rec.DRUDTFIN IS NOT NULL THEN
                     NULL;
                     --dDate1 := dFin;
                     --dDate2 := cDosRub_rec.DRUDTFIN;
                     --RAISE err_parm2;
                  ELSIF sControle = 'R1339' AND cDosRub_rec.DRUMONTAGE IS NOT NULL
                                            AND cDosRub_rec.DRUMONTAGE = 'AMOFIX'
                                            AND cDosRub_rec.DRUMTORIGINE-NVL(cDosRub_rec.DRUMTRESIDUEL,0) != nAmo THEN
                     --RAISE err_parm5;
                     NULL;
                  END IF;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                	aOrdre(nb_element) := nOrdre;
                	aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                	aDec(nb_element)   := NULL;
               WHEN err_parm2 THEN
                  lOk          	    := 0;
					   nb_element   	    := nb_element + 1;
					   nOrdre		       := nb_element;
					   aOrdre(nb_element) := nOrdre;
					   aMsg(nb_element)   := TO_CHAR(dDate1,'YYYYMMDD');
					   aType(nb_element)  := 'D';
					   aDec(nb_element)   := NULL;
					   nb_element   	    := nb_element + 1;
					   aOrdre(nb_element) := nOrdre;
					   aMsg(nb_element)   := TO_CHAR(dDate2,'YYYYMMDD');
					   aType(nb_element)  := 'D';
					   aDec(nb_element)   := NULL;
					   nb_element   	    := nb_element + 1;
					   aOrdre(nb_element) := nOrdre;
					   aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
					   aType(nb_element)  := 'N';
					   aDec(nb_element)   := NULL;
               WHEN err_parm5 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
                  nOrdre		       := nb_element;
                  aOrdre(nb_element) := nOrdre;
			         aMsg(nb_element)   := nAmo;
                  aType(nb_element)  := 'N';
                	aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
	               nb_element   	    := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUMTORIGINE-NVL(cDosRub_rec.DRUMTRESIDUEL,0);
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
				      nb_element   	    := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
			         aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parm7 THEN
                  lOk          	    := 0;
	               nb_element   	    := nb_element + 1;
                  nOrdre		       := nb_element;
                  aOrdre(nb_element) := nOrdre;
			         aMsg(nb_element)   := nSum;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
	               nb_element   	    := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUMTORIGINE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
				      nb_element   	    := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
			         aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;

            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubFlux;

	/*
	// Controle rubriques CVT R01
	*/
   FUNCTION ocDossRubCvt(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk       NUMBER := 1;
		   lFlagOk   NUMBER;
		   nOrdre    NUMBER := 0;
		   nAny      NUMBER;
         nActId    NUMBER;
         nLuActId  NUMBER;
         sTaxe     DOSRUBRIQUE.TAXCODE%TYPE;
         sLuTaxe   DOSRUBRIQUE.TAXCODE%TYPE;
         nLoyOrd   DOSRUBRIQUE.DRUORDREFACTURATION%TYPE;
         dDeb      DOSRUBRIQUE.DRUDTDEB%TYPE;
         dFin      DOSRUBRIQUE.DRUDTFIN%TYPE;
         sPeriode  DOSRUBRIQUETXVAR.DTVPERIODE%TYPE;
         nMult     DOSRUBRIQUETXVAR.DTVMULTIPLE%TYPE;
         sRPeriode DOSRUBRIQUETXVAR.DTVPERIODE%TYPE;
         nRMult    DOSRUBRIQUETXVAR.DTVMULTIPLE%TYPE;
         loy_rec   DOSRUBRIQUE%ROWTYPE;
         nSum      NUMBER;
         nPosIni   NUMBER;
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;
         nNbIndLoy NUMBER;
         nNbIndApp NUMBER;
         bTrouve1   BOOLEAN;
         bTrouve2   BOOLEAN;
         bTrouve    BOOLEAN;
         dtNewFin   DATE;
         CURSOR cDosRubCvt IS SELECT DRUMODCALINT, DRUDTROMPU, DRUDTDEB, DRUORDREFACTURATION,
                                     DRUROMPUREPART, DRUROMPUTAUX, DACORDRE, DRUDTFIN, TAXCODE,
                                     DRUORDRE
                              FROM   DOSRUBRIQUE
                              WHERE  DOSID          = contrat_rec.DOSID
                              AND    DRUTYPEMONTAGE = 'CLASSIC'
                              AND    DRUCLASSE      = 'F'
                              AND    DRUTYPE        = 'V';

         CURSOR cDosRubCvt2 IS SELECT DRUMODCALINT, DRUDTROMPU, DRUDTDEB, DRUORDREFACTURATION,
                                     DRUROMPUREPART, DRUROMPUTAUX, DACORDRE, DRUDTFIN, TAXCODE,
                                     DRUORDRE
                              FROM   DOSRUBRIQUE
                              WHERE  DOSID          = contrat_rec.DOSID
                              AND    DRUTYPEMONTAGE = 'CLASSIC'
                              AND    DRUCLASSE      = 'F'
                              AND    DRUTYPE        = 'V';

         CURSOR cDosRubLoy IS SELECT DRUORDRE, DRUDTDEB, DRUMTORIGINE, DRUMODCALINT,
                                     DRUINTSIMPLEPERIODE, DRUINTSIMPLEMULTIPLE,
                                     DRUCVTMODEINDEX, DRUTXNATURE, DRUCCFLAGAFFICHINT
                              FROM   DOSRUBRIQUE
                              WHERE  DOSID          = contrat_rec.DOSID
                              AND    DRUTYPEMONTAGE = 'CVT'
                              AND    DRUCLASSE      = 'F'
                              AND    DRUTYPE        = 'F';

         CURSOR cDosRubApp IS SELECT DRUMODCALINT, DRUINTSIMPLEPERIODE, DRUINTSIMPLEMULTIPLE,
                                     DRUORDRE, DRUTXNATURE
                              FROM   DOSRUBRIQUE
                              WHERE  DOSID               = contrat_rec.DOSID
                              AND    DRUTYPEMONTAGE      = 'CLASSIC'
                              AND    DRUCLASSE           = 'F'
                              AND    DRUORDREFACTURATION = nLoyOrd
                              AND    DRUTYPE             = 'V';
      BEGIN
         IF sControle = 'R0135' THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               nLoyOrd   := cDosRubLoy_rec.DRUORDRE;
               dDeb      := cDosRubLoy_rec.DRUDTDEB;
               IF nLoyOrd IS NOT NULL AND dDeb IS NOT NULL THEN
                  /*
                  Optimisation JPB
                  nSum := 0;
                  FOR cDosRubVer_rec IN cDosRubVer LOOP
                          nSum := nSum + cDosRubVer_rec.DRUMTORIGINE;
                  END LOOP;
                  SELECT DIMMTCC
                  INTO nPosIni
                  FROM DOSSIERIMMOBILIER
                  WHERE DOSID = contrat_rec.DOSID;
                  IF nPosIni IS NULL THEN
                          nPosIni := 0;
                  END IF;
                  */
                  /*
                  SELECT NVL(SUM(DRUMTORIGINE), 0), NVL(SUM(DIMMTCC), 0)
                  INTO   nSum, nPosIni
                  FROM   DOSRUBRIQUE DRU, DOSSIERIMMOBILIER DIM
                  WHERE  DIM.DOSID               = contrat_rec.DOSID
                  AND    DRU.DOSID               = contrat_rec.DOSID
                  AND    DRU.DRUTYPEMONTAGE      = 'CLASSIC'
                  AND    DRU.DRUCLASSE           = 'F'
                  AND    DRU.DRUORDREFACTURATION = nLoyOrd
                  AND    DRU.DRUDTDEB            = dDeb
                  AND    DRU.DRUTYPE             = 'V';

                  IF nSum != cDosRubLoy_rec.DRUMTORIGINE + nPosIni THEN
                     lOk                := 0;
                     nb_element         := nb_element + 1;
		     nOrdre	        := nb_element;
		     aOrdre(nb_element) := nOrdre;
		     aMsg(nb_element)   := cDosRubLoy_rec.DRUMTORIGINE + nPosIni;
        	     aType(nb_element)  := 'N';
                     aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
	             nb_element         := nb_element + 1;
		     aOrdre(nb_element) := nOrdre;
		     aMsg(nb_element)   := nSum;
                     aType(nb_element)  := 'N';
        	     aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
		     nb_element         := nb_element + 1;
        	     aOrdre(nb_element) := nOrdre;
	             aMsg(nb_element)   := nLoyOrd;
		     aType(nb_element)  := 'N';
		     aDec(nb_element)   := NULL;
                  END IF;
                  */
                  NULL;
               END IF;
            END LOOP;
         ELSIF sControle = 'R0233' THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               IF cDosRubLoy_rec.DRUMODCALINT IS NOT NULL AND cDosRubLoy_rec.DRUMODCALINT         =  'IS'
                                                          AND cDosRubLoy_rec.DRUINTSIMPLEPERIODE  IS NOT NULL
                                                          AND cDosRubLoy_rec.DRUINTSIMPLEMULTIPLE IS NOT NULL THEN
                  nLoyOrd   := cDosRubLoy_rec.DRUORDRE;
                  FOR cDosRubApp_rec IN cDosRubApp LOOP
                     IF cDosRubLoy_rec.DRUMODCALINT != cDosRubApp_rec.DRUMODCALINT OR cDosRubLoy_rec.DRUINTSIMPLEPERIODE  != cDosRubApp_rec.DRUINTSIMPLEPERIODE
                                                                                   OR cDosRubLoy_rec.DRUINTSIMPLEMULTIPLE != cDosRubApp_rec.DRUINTSIMPLEMULTIPLE THEN
                        lOk                := 0;
                        nb_element         := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                	aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	   := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cDosRubApp_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
               	        aDec(nb_element)   := NULL;
                     END IF;
                  END LOOP;
               END IF;
            END LOOP;
         ELSIF sControle = 'R0533' THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               nLoyOrd   := cDosRubLoy_rec.DRUORDRE;
               sRPeriode := NULL;
               nRMult    := NULL;
               IF NVL(cDosRubLoy_rec.DRUCCFLAGAFFICHINT,0) = 1 THEN
               	FOR cDosRubApp_rec IN cDosRubApp LOOP
               	   BEGIN
               	      SELECT DTVPERIODE, DTVMULTIPLE
               	      INTO   sPeriode, nMult
               	      FROM   DOSRUBRIQUETXVAR
               	      WHERE  DOSID    = contrat_rec.DOSID
               	      AND    DRUORDRE = cDosRubApp_rec.DRUORDRE;

               	      IF sRPeriode IS NULL AND nRMult IS NULL THEN
               	         sRPeriode := sPeriode;
               	         nRMult    := nMult;
               	      END IF;
               	      IF sRPeriode != sPeriode OR nRMult != nMult THEN
               	         lOk                := 0;
               	         nb_element   	   := nb_element + 1;
               	         nOrdre             := nb_element;
               		        aOrdre(nb_element) := nOrdre;
               	         aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
               	         aType(nb_element)  := 'N';
               	         aDec(nb_element)   := NULL;
               	         EXIT;
               	      END IF;
               	   EXCEPTION
               	      WHEN NO_DATA_FOUND THEN
               	         nLoyOrd   := cDosRubLoy_rec.DRUORDRE;
               	   END;
               	END LOOP;
               END IF;
            END LOOP;
         ELSIF sControle = 'R0730' AND NVL(contrat_rec.DOSFLAGINDEXABLE,0) = 1 THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               BEGIN
                  IF ( cDosRubLoy_rec.DRUCVTMODEINDEX IS NOT NULL ) THEN
                     IF ( cDosRubLoy_rec.DRUCVTMODEINDEX = 'VERS' ) THEN
                        SELECT COUNT(*)
                        INTO   nAny
                        FROM   DOSRUBMODEINDEX
                        WHERE  DOSID    = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRubLoy_rec.DRUORDRE;
                        IF ( nAny != 0 ) THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  END IF;
               EXCEPTION
                  WHEN err_parm1 THEN
                     lOk                := 0;
                     nb_element   	    := nb_element + 1;
                     nOrdre             := nb_element;
               	   aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
               END;
            END LOOP;
         ELSIF sControle = 'R0735' AND NVL(contrat_rec.DOSFLAGINDEXABLE,0) = 1 THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               IF cDosRubLoy_rec.DRUCVTMODEINDEX IS NOT NULL THEN
                  nLoyOrd := cDosRubLoy_rec.DRUORDRE;
                  SELECT COUNT(*)
                  INTO   nNbIndLoy
                  FROM   DOSRUBMODEINDEX
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = nLoyOrd;
                  FOR cDosRubApp_rec IN cDosRubApp LOOP
                     BEGIN
                        SELECT COUNT(*)
                        INTO   nNbIndApp
                        FROM   DOSRUBMODEINDEX
                        WHERE  DOSID    = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRubApp_rec.DRUORDRE;
                        IF ( cDosRubLoy_rec.DRUCVTMODEINDEX = 'LOYER' ) THEN
                           IF ( nNbIndLoy = 0 ) AND ( nNbIndApp > 0 ) THEN
                              RAISE err_parm2;
                           END IF;
                        ELSIF ( cDosRubLoy_rec.DRUCVTMODEINDEX = 'VERS' ) THEN
                           IF ( nNbIndLoy > 0 ) AND ( nNbIndApp = 0 ) THEN
                              RAISE err_parm2;
                           END IF;
                        ELSIF ( cDosRubLoy_rec.DRUCVTMODEINDEX = 'LOYVERS' ) THEN
                           IF ( nNbIndLoy = 0 ) OR ( nNbIndApp = 0 ) THEN
                              RAISE err_parm2;
                           END IF;
                        END IF;
                     EXCEPTION
                        WHEN err_parm2 THEN
                           lOk                := 0;
                           nb_element   	   := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                	         aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                     END;
                  END LOOP;
               END IF;
            END LOOP;
         ELSIF sControle = 'R0532' THEN
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               SELECT COUNT(*)
               INTO   nAny
               FROM   DOSRUBRIQUETXVAR
               WHERE  DRUORDRE = cDosRubLoy_rec.DRUORDRE
               AND    DOSID    = contrat_rec.DOSID;
               IF nAny != 0 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               END IF;
            END LOOP;
         ELSIF sControle = 'R0234' THEN
            /*
            FOR cDosRubLoy_rec IN cDosRubLoy LOOP
               IF cDosRubLoy_rec.DRUTXNATURE IS NOT NULL THEN
                  nLoyOrd   := cDosRubLoy_rec.DRUORDRE;
                  FOR cDosRubApp_rec IN cDosRubApp LOOP
                     IF cDosRubLoy_rec.DRUTXNATURE != cDosRubApp_rec.DRUTXNATURE THEN
                        lOk                := 0;
                        nb_element   	   := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                	aMsg(nb_element)   := cDosRubLoy_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	   := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cDosRubApp_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
                	aDec(nb_element)   := NULL;
                     END IF;
                  END LOOP;
               END IF;
            END LOOP;
            */
            NULL;
         ELSE
            FOR cDosRubCvt_rec IN cDosRubCvt LOOP
               lFlagOk := 1;
               nActId := NULL ;
               IF sControle = 'R1231' THEN
                  IF cDosRubCvt_rec.DRUMODCALINT IS NOT NULL THEN
                     IF SUBSTR( cDosRubCvt_rec.DRUMODCALINT, 1, 2 ) = 'IS' THEN
                        IF cDosRubCvt_rec.DRUDTROMPU IS NOT NULL AND cDosRubCvt_rec.DRUDTDEB IS NOT NULL
                                                                 AND cDosRubCvt_rec.DRUDTROMPU != cDosRubCvt_rec.DRUDTDEB THEN
                           BEGIN
                              SELECT *
                              INTO   loy_rec
                              FROM   DOSRUBRIQUE
                              WHERE  DOSID     = contrat_rec.DOSID
                              AND    DRUCLASSE = 'F'
                              AND    DRUTYPE   = 'F'
                              AND    DRUORDRE  = cDosRubCvt_rec.DRUORDREFACTURATION;
                              IF loy_rec.DRUROMPUREPART != cDosRubCvt_rec.DRUROMPUREPART OR loy_rec.DRUROMPUTAUX != cDosRubCvt_rec.DRUROMPUTAUX THEN
                                 lFlagOk := 2;
                              END IF;
                           EXCEPTION
                              WHEN NO_DATA_FOUND THEN
                                 lFlagOk := 2;
                           END;
                        END IF;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0133' THEN
                  IF nActId IS NULL THEN
                     nActId := cDosRubCvt_rec.DACORDRE;
                  END IF;
                  IF nActId != cDosRubCvt_rec.DACORDRE THEN
                     lFlagOk := 0;
                  ELSIF cDosRubCvt_rec.DRUORDREFACTURATION IS NOT NULL THEN
                     BEGIN
                        SELECT DACORDRE
                        INTO   nLuActId
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID     = contrat_rec.DOSID
                        AND    DRUCLASSE = 'F'
                        AND    DRUTYPE   = 'F'
                        AND    DRUORDRE  = cDosRubCvt_rec.DRUORDREFACTURATION;
                        IF nLuActId != nActId THEN
                           lFlagOk := 0;
                        END IF;
                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           lFlagOk := 0;
                     END;
                  END IF;
               ELSIF sControle = 'R0144' THEN
                  IF cDosRubCvt_rec.DRUORDREFACTURATION IS NOT NULL AND cDosRubCvt_rec.DRUDTDEB IS NOT NULL THEN
                     BEGIN
                        bTrouve1 := false;
                        bTrouve2 := false;
                        SELECT DRUDTDEB, DRUDTFIN
                        INTO   dDeb, dFin
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID     = contrat_rec.DOSID
                        AND    DRUCLASSE = 'F'
                        AND    DRUTYPE   = 'F'
                        AND    DRUORDRE  = cDosRubCvt_rec.DRUORDREFACTURATION;

                        IF dFin IS NULL THEN
                           SELECT DRFDTFIN
                           INTO   dFin
                           FROM  DOSRUBFLUX
                           WHERE DOSID  = contrat_rec.DOSID
                           AND DRUORDRE = cDosRubCvt_rec.DRUORDREFACTURATION
                           AND DRFORDRE = (  SELECT MAX(DRFORDRE)
                                             FROM DOSRUBFLUX
                                             WHERE DOSID = contrat_rec.DOSID
                                             AND DRUORDRE = cDosRubCvt_rec.DRUORDREFACTURATION );
                        END if;

                        IF dDeb IS NOT NULL THEN
                             IF cDosRubCvt_rec.DRUDTDEB = dDeb THEN
                                bTrouve1 := TRUE;
	 		                    ELSE
                                bTrouve1 := FALSE;
                             END IF;
                        END IF;

                        FOR cDosRubCvt2_rec IN cDosRubCvt2 LOOP
                           IF cDosRubCvt2_rec.DRUORDREFACTURATION IS NOT NULL AND cDosRubCvt2_rec.DRUDTDEB IS NOT NULL THEN

                              IF dFin IS NOT NULL THEN
                                 IF cDosRubCvt2_rec.DRUDTFIN IS NULL THEN
                                    SELECT DRFDTFIN
                                    INTO   dtNewFin
                                    FROM  DOSRUBFLUX
                                    WHERE DOSID  = contrat_rec.DOSID
                                    AND DRUORDRE = cDosRubCvt2_rec.DRUORDRE
                                    AND DRFORDRE = (  SELECT MAX(DRFORDRE)
                                                      FROM DOSRUBFLUX
                                                      WHERE DOSID = contrat_rec.DOSID
                                                      AND DRUORDRE = cDosRubCvt2_rec.DRUORDRE );
                                 ELSE
                                    dtNewFin := cDosRubCvt2_rec.DRUDTFIN;
                                 END if;
                                 IF dtNewFin = dFin THEN
                                    bTrouve2 := TRUE;
                                 END IF;
                              END IF;

                              IF not bTrouve1 THEN
                                 IF dDeb = cDosRubCvt2_rec.DRUDTDEB THEN
                                    bTrouve1 := TRUE;
                                 END if;
                              END IF;

                              bTrouve := TRUE;
                           END IF;
                        END LOOP;
                        IF bTrouve = FALSE THEN
                           bTrouve1 := TRUE;
                           bTrouve2 := TRUE;
                        END IF;

                        IF bTrouve1 = FALSE OR bTrouve2 = FALSE THEN
                           lFlagOk := 2;
                        END IF;

                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           lFlagOk := 1;
                     END;
                  END IF;
               ELSIF sControle = 'R0134' THEN
                  sTaxe := cDosRubCvt_rec.TAXCODE;
                  IF cDosRubCvt_rec.DRUORDREFACTURATION IS NOT NULL THEN
                     BEGIN
                        SELECT TAXCODE
                        INTO   sLuTaxe
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID     = contrat_rec.DOSID
                        AND    DRUCLASSE = 'F'
                        AND    DRUTYPE   = 'F'
                        AND    DRUORDRE  = cDosRubCvt_rec.DRUORDREFACTURATION;
                        IF sLuTaxe != sTaxe THEN
                           lFlagOk := 0;
                        END IF;
                     EXCEPTION
                        WHEN NO_DATA_FOUND THEN
                           lFlagOk := 0;
                     END;
                  END IF;
               END IF;
               IF lFlagOk = 0 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubCvt_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               ELSIF lFlagOk = 2 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubCvt_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubCvt_rec.DRUORDREFACTURATION;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               END IF;
            END LOOP;
         END IF;
         RETURN lOk;
      END;
   END ocDossRubCvt;

	/*
	// Controle rubriques financieres de prefinancement R03
	*/
	FUNCTION ocDossRubFinPre(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk            NUMBER := 1;
		   nOrdre         NUMBER := 0;
		   nAny           NUMBER;
         err_parm1      EXCEPTION;
         err_parm2      EXCEPTION;
         err_parm3      EXCEPTION;
         err_parm4      EXCEPTION;
         sCode          VARCHAR2(200);
         dtPhaseEnCours DOSPHASE.DPHDTEFFET%TYPE;
         dtDate1        DATE;
         dtDate2        DATE;
         bContinu       BOOLEAN := TRUE;
         nDruOrdre      DOSRUBRIQUE.DRUORDRE%TYPE;
         nCount         NUMBER;
         sDruBaseCalcMaitre  DOSRUBRIQUE.DRUBASECALC%TYPE;
         sDruBaseCalcliee    DOSRUBRIQUE.DRUBASECALC%TYPE;


         CURSOR cDosRubFinPre IS SELECT DRUMODCALINT, DRUTXNATURE, DRUTXTYPE, DRUTAUXFIXE,DRUORDREMAITRE, DACORDRE,
                                        DRUORDRE, DRUBASECALC, DRUDUREEFORFAIT, DRUDTDEB,
                                        F_PlFinRUBRIQUE( DOSID,DRUORDRE) DTFIN,
                                        DRUFLAGCAPITALISE, DRUPLAFONDCAPITALISE, DRUTAUXPERIODEAPPLICATION, RUBID
                                 FROM   DOSRUBRIQUE
                                 WHERE  DOSID     = contrat_rec.DOSID
                                 AND    DRUCLASSE = 'F'
                                 AND    DRUTYPE   = 'P';
         CURSOR cDosRubFinPreLiee IS SELECT  DRUORDRE, DRUORDREMAITRE, DACORDRE , DRUBASECALC
                                 FROM   DOSRUBRIQUE
                                 WHERE  DOSID     = contrat_rec.DOSID
                                 AND    DRUCLASSE = 'F'
                                 AND    DRUTYPE   = 'P'
                                 AND    DRUORDREMAITRE IS NOT NULL;

         CURSOR cDosRubDir    IS SELECT DIR.ITRID, DIR.IRUORDRE, DIR.DRUORDRE, ITR.ITRNUM
                                 FROM   LKDOSRUBITRRUB DIR, IMMOTRANCHE ITR
                                 WHERE  DOSID     = contrat_rec.DOSID
                                 AND    DRUORDRE  = nDruOrdre
                                 AND    ITR.ITRID = DIR.ITRID;

         --bd310301
         nTaux NUMBER;
         nAny1 NUMBER;
         nAny2 NUMBER;
         nAny3 NUMBER;
         nAny4 NUMBER;
         nDruOrdre1 DOSRUBRIQUE.DRUORDRE%TYPE;
         nDruOrdreMaitre   NUMBER;
         nDruOrdreLiee   NUMBER;

         cDosRubriqueTxVar1 DOSRUBRIQUETXVAR%ROWTYPE;
         cDosRubriqueTxVar2 DOSRUBRIQUETXVAR%ROWTYPE;
         CURSOR cPrefTaux    IS SELECT  DRUORDRE, DRUTXNATURE, DRUTAUXFIXE
                                FROM    DOSRUBRIQUE
                                WHERE   DOSID = contrat_rec.DOSID
                                AND     DRUTYPE = 'P'
                                AND     F_ISRUBIDONFILTRE( RUBID, 'PMTAUX' ) = 1;
         CURSOR cFormuleBase IS SELECT DISTINCT DOSRUBRIQUETXVAR.DTVFTVIDBASE, DOSRUBRIQUETXVAR.DRUORDRE
                                FROM   DOSRUBRIQUETXVAR
                                WHERE  DOSRUBRIQUETXVAR.DOSID = contrat_rec.DOSID AND
                                       DOSRUBRIQUETXVAR.DTVFTVIDBASE IS NOT NULL AND
                                       EXISTS ( SELECT 1
                                                FROM   DOSRUBRIQUE
                                                WHERE  DOSRUBRIQUE.DOSID = contrat_rec.DOSID AND
                                                       DOSRUBRIQUE.DRUORDRE = DOSRUBRIQUETXVAR.DRUORDRE AND
                                                       DOSRUBRIQUE.DRUTYPE = 'P' AND
                                                       F_ISRUBIDONFILTRE( DOSRUBRIQUE.RUBID, 'PMTAUX' ) = 1 );
         CURSOR cFormulePlancher IS SELECT DISTINCT DOSRUBRIQUETXVAR.DTVFTVIDPLANCHER, DOSRUBRIQUETXVAR.DRUORDRE
                                FROM   DOSRUBRIQUETXVAR
                                WHERE  DOSRUBRIQUETXVAR.DOSID = contrat_rec.DOSID AND
                                       DOSRUBRIQUETXVAR.DTVFTVIDPLANCHER IS NOT NULL AND
                                       EXISTS ( SELECT 1
                                                FROM   DOSRUBRIQUE
                                                WHERE  DOSRUBRIQUE.DOSID = contrat_rec.DOSID AND
                                                       DOSRUBRIQUE.DRUORDRE = DOSRUBRIQUETXVAR.DRUORDRE AND
                                                       DOSRUBRIQUE.DRUTYPE = 'P' AND
                                                       F_ISRUBIDONFILTRE( DOSRUBRIQUE.RUBID, 'PMTAUX' ) = 1 );
         CURSOR cFormulePlafond IS SELECT DISTINCT DOSRUBRIQUETXVAR.DTVFTVIDPLAFOND, DOSRUBRIQUETXVAR.DRUORDRE
                                FROM   DOSRUBRIQUETXVAR
                                WHERE  DOSRUBRIQUETXVAR.DOSID = contrat_rec.DOSID AND
                                       DOSRUBRIQUETXVAR.DTVFTVIDPLAFOND IS NOT NULL AND
                                       EXISTS ( SELECT 1
                                                FROM   DOSRUBRIQUE
                                                WHERE  DOSRUBRIQUE.DOSID = contrat_rec.DOSID AND
                                                       DOSRUBRIQUE.DRUORDRE = DOSRUBRIQUETXVAR.DRUORDRE AND
                                                       DOSRUBRIQUE.DRUTYPE = 'P' AND
                                                       F_ISRUBIDONFILTRE( DOSRUBRIQUE.RUBID, 'PMTAUX' ) = 1 );
         cDosRubTxVar2 DOSRUBTXVAR%ROWTYPE;
         cDosRubTxVar1 DOSRUBTXVAR%ROWTYPE;
         nItrId NUMBER;
         nIruOrdre NUMBER;
         nDruOrdreFi NUMBER;
         CURSOR cRubFi IS SELECT DRUORDRE
                          FROM   DOSRUBRIQUE
                          WHERE  DOSID = contrat_rec.DOSID AND
                                 DRUCLASSE = 'F' AND
                                 DRUTYPE = 'F';
         CURSOR cRubFiAdos IS SELECT DISTINCT ITRID, IRUORDRE
                              FROM LKDOSRUBITRRUB
                              WHERE DOSID = contrat_rec.DOSID AND
                                    DRUORDRE = nDruOrdreFi;
         bSameAdos BOOLEAN;
      BEGIN
			IF sControle ='R0338' THEN
				nCount	:= 0;
				FOR cRec IN cDosRubFinPre LOOP
					IF cRec.DRUBASECALC IS NOT NULL AND SUBSTR(cRec.DRUBASECALC,1,5) ='DBLOC' THEN
						SELECT COUNT(*) INTO nCount
						FROM	 DOSRUBRIQUE WHERE DOSID = contrat_rec.DOSID
						AND	 DRUTYPE ='P'
						AND	 DRUBASECALC IS NOT NULL AND SUBSTR(DRUBASECALC,1,7) ='FDRUBSF'
						AND 	( (DRUDTDEB BETWEEN cRec.DRUDTDEB AND NVL(cRec.DTFIN,TO_DATE('31122998','DDMMYYYY')))
								 or ( NVL(F_PlFinRUBRIQUE( DOSID,DRUORDRE),TO_DATE('31122998','DDMMYYYY')) BETWEEN cRec.DRUDTDEB AND NVL(cRec.DTFIN,TO_DATE('31122998','DDMMYYYY'))) );
					ELSIF cRec.DRUBASECALC IS NOT NULL AND SUBSTR(cRec.DRUBASECALC,1,7) ='FDRUBSF' THEN
						SELECT COUNT(*) INTO nCount
						FROM	 DOSRUBRIQUE WHERE DOSID = contrat_rec.DOSID
						AND	 DRUTYPE ='P'
						AND	 DRUBASECALC IS NOT NULL AND  SUBSTR(DRUBASECALC,1,5) ='DBLOC'
						AND 	( (DRUDTDEB BETWEEN cRec.DRUDTDEB AND NVL(cRec.DTFIN,TO_DATE('31122998','DDMMYYYY')))
								 or (NVL(F_PlFinRUBRIQUE( DOSID,DRUORDRE),TO_DATE('31122998','DDMMYYYY')) BETWEEN cRec.DRUDTDEB AND NVL(cRec.DTFIN,TO_DATE('31122998','DDMMYYYY'))) );
					END IF;
					IF nCount > 0 THEN
						lOk := 0;
						EXIT;
					END IF;
				END LOOP;
         ELSIF sControle = 'R0335' THEN

            SELECT COUNT( DISTINCT DRUTXNATURE )
            INTO   nAny
            FROM   DOSRUBRIQUE
            WHERE  DOSID = contrat_rec.DOSID
            AND    DRUTYPE = 'P'
            AND    F_ISRUBIDONFILTRE( RUBID, 'PMTAUX' ) = 1;

            IF contrat_rec.TACCODE = 'PRET' THEN
               NULL;

            ELSIF nAny > 1 THEN
               lOk := 0;

            ELSIF nAny = 1 THEN
               nTaux := NULL;
               FOR cPrefTaux_r IN cPrefTaux LOOP

                  nDruOrdre := cPrefTaux_r.DRUORDRE;

                  IF cPrefTaux_r.DRUTXNATURE = 'TF' THEN
                     IF nTaux IS NULL THEN
                        nTaux := cPrefTaux_r.DRUTAUXFIXE;
                     ELSE
                        IF nTaux != cPrefTaux_r.DRUTAUXFIXE THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                     END IF;
                  ELSE
                     IF nTaux IS NULL THEN
                        SELECT *
                        INTO   cDosRubriqueTxVar1
                        FROM   DOSRUBRIQUETXVAR
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        SELECT COUNT(*)
                        INTO   nAny1
                        FROM   DOSRUBTXVAR
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        SELECT COUNT(*)
                        INTO   nAny3
                        FROM   DOSRUBTXVTAUX
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        nDruOrdre1 := cPrefTaux_r.DRUORDRE;

                        nTaux := 1;

                     ELSE
                        SELECT *
                        INTO   cDosRubriqueTxVar2
                        FROM   DOSRUBRIQUETXVAR
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        IF cDosRubriqueTxVar1.DTVFTVIDBASE != cDosRubriqueTxVar2.DTVFTVIDBASE THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                        IF cDosRubriqueTxVar1.DTVFTVIDPLANCHER != cDosRubriqueTxVar2.DTVFTVIDPLANCHER THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                        IF cDosRubriqueTxVar1.DTVFTVIDPLAFOND != cDosRubriqueTxVar2.DTVFTVIDPLAFOND THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                        IF cDosRubriqueTxVar1.DTVFTVIDCONSOLID != cDosRubriqueTxVar2.DTVFTVIDCONSOLID THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                        IF cDosRubriqueTxVar1.DTVFTVIDREGUL != cDosRubriqueTxVar2.DTVFTVIDREGUL THEN
                           lOk := 0;
                           EXIT;
                        END IF;

                        SELECT COUNT(*)
                        INTO   nAny2
                        FROM   DOSRUBTXVAR
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        IF nAny1 != nAny2 THEN
                           lOk := 0;
                           EXIT;
                        END IF;

                        SELECT COUNT(*)
                        INTO   nAny4
                        FROM   DOSRUBTXVTAUX
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cPrefTaux_r.DRUORDRE;

                        IF nAny3 != nAny4 THEN
                           lOk := 0;
                           EXIT;
                        END IF;

                     END IF;

                  END IF;
               END LOOP;

               IF lOk = 1 THEN
                  nAny4 := 0;
                  FOR cFormuleBase_rec IN cFormuleBase LOOP
                      BEGIN
                         IF nAny4 = 0 THEN
                            SELECT *
                            INTO   cDosRubTxVar1
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormuleBase_rec.DRUORDRE AND
                                   DRTTYPE = 'BASE';
                            nAny4 := 1;
                         ELSE
                            SELECT *
                            INTO   cDosRubTxVar2
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormuleBase_rec.DRUORDRE AND
                                   DRTTYPE = 'BASE';

                           IF cDosRubTxVar1.DRTTYPEMOYENNE != cDosRubTxVar2.DRTTYPEMOYENNE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEDEBUT != cDosRubTxVar2.DRTTYPEDEBUT THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEFIN != cDosRubTxVar2.DRTTYPEFIN THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTMARGE != cDosRubTxVar2.DRTMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEMARGE != cDosRubTxVar2.DRTTYPEMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTDECOMPTE != cDosRubTxVar2.DRTDECOMPTE THEN
                              lOk := 0;
                              EXIT;
                           END IF;

                         END IF;
                      EXCEPTION
                         WHEN OTHERS THEN
                            lOk := 0;
                            EXIT;
                      END;
                  END LOOP;
               END IF;

               IF lOk = 1 THEN
                  nAny4 := 0;
                  FOR cFormulePlancher_rec IN cFormulePlancher LOOP
                      BEGIN
                         IF nAny4 = 0 THEN
                            SELECT *
                            INTO   cDosRubTxVar1
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormulePlancher_rec.DRUORDRE AND
                                   DRTTYPE = 'PLANCH';
                            nAny4 := 1;
                         ELSE
                            SELECT *
                            INTO   cDosRubTxVar2
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormulePlancher_rec.DRUORDRE AND
                                   DRTTYPE = 'PLANCH';

                           IF cDosRubTxVar1.DRTTYPEMOYENNE != cDosRubTxVar2.DRTTYPEMOYENNE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEDEBUT != cDosRubTxVar2.DRTTYPEDEBUT THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEFIN != cDosRubTxVar2.DRTTYPEFIN THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTMARGE != cDosRubTxVar2.DRTMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEMARGE != cDosRubTxVar2.DRTTYPEMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTDECOMPTE != cDosRubTxVar2.DRTDECOMPTE THEN
                              lOk := 0;
                              EXIT;
                           END IF;

                         END IF;
                      EXCEPTION
                         WHEN OTHERS THEN
                            lOk := 0;
                            EXIT;
                      END;
                  END LOOP;
               END IF;

               IF lOk = 1 THEN
                  nAny4 := 0;
                  FOR cFormulePlafond_rec IN cFormulePlafond LOOP
                      BEGIN
                         IF nAny4 = 0 THEN
                            SELECT *
                            INTO   cDosRubTxVar1
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormulePlafond_rec.DRUORDRE AND
                                   DRTTYPE = 'PLAFOND';
                            nAny4 := 1;
                         ELSE
                            SELECT *
                            INTO   cDosRubTxVar2
                            FROM   DOSRUBTXVAR
                            WHERE  DOSID    = contrat_rec.DOSID AND
                                   DRUORDRE = cFormulePlafond_rec.DRUORDRE AND
                                   DRTTYPE = 'PLAFOND';

                           IF cDosRubTxVar1.DRTTYPEMOYENNE != cDosRubTxVar2.DRTTYPEMOYENNE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEDEBUT != cDosRubTxVar2.DRTTYPEDEBUT THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEFIN != cDosRubTxVar2.DRTTYPEFIN THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTMARGE != cDosRubTxVar2.DRTMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTTYPEMARGE != cDosRubTxVar2.DRTTYPEMARGE THEN
                              lOk := 0;
                              EXIT;
                           END IF;
                           IF cDosRubTxVar1.DRTDECOMPTE != cDosRubTxVar2.DRTDECOMPTE THEN
                              lOk := 0;
                              EXIT;
                           END IF;

                         END IF;
                      EXCEPTION
                         WHEN OTHERS THEN
                            lOk := 0;
                            EXIT;
                      END;
                  END LOOP;
               END IF;

            END IF;

         ELSIF ( sControle = 'R0330' ) THEN
            BEGIN
               SELECT DPHDTEFFET
               INTO   dtPhaseEnCours
               FROM   DOSPHASE
               WHERE  DOSID   = contrat_rec.DOSID
               AND    PHADEST = 'DOSSIER'
               AND    PHACODE = 'EC';
            EXCEPTION
               WHEN OTHERS THEN
                  bContinu := FALSE;
            END;
         END IF;
         IF bContinu AND sControle != 'R0335' THEN
            FOR cDosRubFinPre_rec IN cDosRubFinPre LOOP
               BEGIN
                  IF sControle = 'R0301' AND cDosRubFinPre_rec.DRUBASECALC IS NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0302' AND cDosRubFinPre_rec.DRUTXNATURE IS NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0303' AND cDosRubFinPre_rec.DRUBASECALC     IS NOT NULL
                                            AND cDosRubFinPre_rec.DRUBASECALC     =  'DECAISTVA'
                                            AND cDosRubFinPre_rec.DRUDUREEFORFAIT IS NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0304' AND cDosRubFinPre_rec.DRUTXNATURE IS NOT NULL
                                            AND SUBSTR( cDosRubFinPre_rec.DRUTXNATURE, 1, 2 ) = 'TF'
                                            AND cDosRubFinPre_rec.DRUTAUXFIXE IS NULL
                                            AND F_ISRUBIDONFILTRE(cDosRubFinPre_rec.RUBID, 'AVPL') = 0 THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0305' AND cDosRubFinPre_rec.DRUTAUXPERIODEAPPLICATION IS NOT NULL THEN
                     SELECT   COUNT(*)
                     INTO     nCount
                     FROM     DOSRUBTAUXECHELLE
                     WHERE    DOSID    = contrat_rec.DOSID
                     AND      DRUORDRE = cDosRubFinPre_rec.DRUORDRE;

                     IF nCount = 0 THEN
                        RAISE err_parm1;
                     END if;
                  ELSIF sControle = 'R0306' AND cDosRubFinPre_rec.DRUTAUXPERIODEAPPLICATION IS NULL THEN
                     SELECT   COUNT(*)
                     INTO     nCount
                     FROM     DOSRUBTAUXECHELLE
                     WHERE    DOSID    = contrat_rec.DOSID
                     AND      DRUORDRE = cDosRubFinPre_rec.DRUORDRE;

                     IF nCount > 0 THEN
                        RAISE err_parm1;
                     END if;
                  ELSIF sControle = 'R0307' THEN
                     if cDosRubFinPre_rec.DRUBASECALC LIKE 'ENG%' THEN
                        BEGIN
                           SELECT MAX(DIR.ITRID)
                           INTO nItrId
                           FROM LKDOSRUBITRRUB DIR
                           WHERE DIR.DOSID = contrat_rec.DOSID
                           AND DIR.DRUORDRE = cDosRubFinPre_rec.DRUORDRE ;
                        EXCEPTION
                           WHEN OTHERS THEN
                              nItrId := NULL;
                        END;
                        if nItrId IS NOT NULL THEN
                           SELECT COUNT(*)
                           INTO nCount
                           FROM CHANTIER
                           WHERE ITRID = nItrId ;
                           if nCount = 0 THEN
                              RAISE err_parm3;
                           END if;
                        END if;
                     END if;
                  ELSIF sControle = 'R0330' AND cDosRubFinPre_rec.DRUDTDEB IS NOT NULL THEN
                     IF ( cDosRubFinPre_rec.DRUDTDEB < dtPhaseEnCours ) THEN
                        dtDate1 := cDosRubFinPre_rec.DRUDTDEB;
                        dtDate2 := dtPhaseEnCours;
                        RAISE err_parm2;
                     END IF;
                  ELSIF sControle = 'R0331' AND NVL(cDosRubFinPre_rec.DRUFLAGCAPITALISE,0) = 1 THEN
                     IF ( cDosRubFinPre_rec.DRUPLAFONDCAPITALISE IS NULL ) OR ( cDosRubFinPre_rec.DRUPLAFONDCAPITALISE <= 0 ) THEN
                        RAISE err_parm1;
                     END IF;
                  ELSIF sControle = 'R0332' AND cDosRubFinPre_rec.DRUTXNATURE IS NOT NULL
                                            AND SUBSTR( cDosRubFinPre_rec.DRUTXNATURE, 1, 2 ) = 'TF'
                                            AND cDosRubFinPre_rec.DRUTAUXFIXE IS NOT NULL
                                            AND ( ( cDosRubFinPre_rec.DRUTAUXFIXE <  0 )
                                            OR    ( cDosRubFinPre_rec.DRUTAUXFIXE >= 100 ) ) THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0333' AND cDosRubFinPre_rec.DRUBASECALC     IS NOT NULL
                                            AND cDosRubFinPre_rec.DRUBASECALC     =  'DECAISTVA'
                                            AND cDosRubFinPre_rec.DRUDUREEFORFAIT IS NOT NULL
                                            AND cDosRubFinPre_rec.DRUDUREEFORFAIT <= 0 THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0373' THEN
                     IF contrat_rec.TACCODE != 'PRET' THEN
                        SELECT COUNT(*)
                        INTO   nAny
                        FROM   LKDOSRUBITRRUB
                        WHERE  DOSID = contrat_rec.DOSID AND
                               DRUORDRE = cDosRubFinPre_rec.DRUORDRE;
                        IF nAny = 0 AND F_ISRUBIDONFILTRE(cDosRubFinPre_rec.RUBID, 'AVPL') = 0 THEN
                           lOk                := 0;
                           nb_element         := nb_element + 1;
                           nOrdre             := nb_element;
       	                   aOrdre(nb_element) := nOrdre;
                	   aMsg(nb_element)   := cDosRubFinPre_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                	   aDec(nb_element)   := NULL;
                        END IF;
                     END IF;
                  ELSIF sControle = 'R0334' THEN
                     nDruOrdre := cDosRubFinPre_rec.DRUORDRE;
                     FOR cDosRubDir_rec IN cDosRubDir LOOP
                        bSameAdos := FALSE;
                        nItrId    := cDosRubDir_rec.ITRID;
                        nIruOrdre := cDosRubDir_rec.IRUORDRE;
                        FOR cRubFi_rec IN cRubFi LOOP
                           nDruOrdreFi := cRubFi_rec.DRUORDRE;
                           FOR cRubFiAdos_rec IN cRubFiAdos LOOP
                              IF cRubFiAdos_rec.ITRID = nItrId AND cRubFiAdos_rec.IRUORDRE = nIruOrdre THEN
                                 bSameAdos := TRUE;
                                 EXIT;
                              END IF;
                           END LOOP;
                           IF bSameAdos THEN
                              EXIT;
                           END IF;
                        END LOOP;
                        IF NOT bSameAdos THEN
                           lOk                := 0;
                           nb_element         := nb_element + 1;
                           nOrdre             := nb_element;
                	   aOrdre(nb_element) := nOrdre;
                	   aMsg(nb_element)   := cDosRubDir_rec.IRUORDRE;
                           aType(nb_element)  := 'N';
                	   aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                	   aOrdre(nb_element) := nOrdre;
                	   aMsg(nb_element)   := F_StdTrimAll( cDosRubDir_rec.ITRNUM );
                           aType(nb_element)  := 'C';
                	   aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                	   aOrdre(nb_element) := nOrdre;
                	   aMsg(nb_element)   := nDruOrdre;
                           aType(nb_element)  := 'N';
                	   aDec(nb_element)   := NULL;
                           EXIT;
                        END IF;
                     END LOOP;

                  ELSIF ( sControle = 'R0336' ) THEN
                     IF  cDosRubFinPre_rec.DRUORDREMAITRE IS NULL THEN
                        FOR cDosRubFinPreLiee_rec  IN  cDosRubFinPreLiee LOOP
                           IF cDosRubFinPreLiee_rec.DRUORDREMAITRE = cDosRubFinPre_rec.DRUORDRE  THEN
                              IF cDosRubFinPreLiee_rec.DACORDRE != cDosRubFinPre_rec.DACORDRE THEN
                                 nDruOrdreMaitre := cDosRubFinPre_rec.DRUORDRE;
                                 nDruOrdreLiee :=  cDosRubFinPreLiee_rec.DRUORDRE;
                                 RAISE err_parm4;
                              END if;
                           END if;
                        END LOOP;
                     END if;
                  ELSIF ( sControle = 'R0337' ) THEN
                     FOR cDosRubFinPreLiee_rec  IN  cDosRubFinPreLiee LOOP
                        if  cDosRubFinPreLiee_rec.DRUORDREMAITRE =   cDosRubFinPre_rec.druordre THEN
                           BEGIN
                              SELECT DRUBASECALC
                              INTO sDruBaseCalcMaitre
                              FROM DOSRUBRIQUE
                              WHERE DOSID    =  contrat_rec.DOSID
                              AND   DRUORDRE =  cDosRubFinPre_rec.druordre;
                           EXCEPTION
                              WHEN OTHERS THEN
                                 sDruBaseCalcMaitre := NULL ;
                           END;
                           sDruBaseCalcLiee   :=  SUBSTR( cDosRubFinPreLiee_rec.DRUBASECALC,1,5)  ;
                           DBMS_OUTPUT.PUT_LINE('sDruBaseCalcLiee'  || sDruBaseCalcLiee);
                           sDruBaseCalcMaitre :=  SUBSTR( sDruBaseCalcMaitre,1,5)  ;
                           DBMS_OUTPUT.PUT_LINE('sDruBaseCalcMaitre'  || sDruBaseCalcMaitre);
                           if sDruBaseCalcLiee IS NOT NULL AND sDruBaseCalcLiee = 'FDRUB' AND sDruBaseCalcMaitre != sDruBaseCalcLiee THEN
                              lOk                := 0;
                              nb_element         := nb_element + 1;
                              nOrdre             := nb_element;
                	            aOrdre(nb_element) := nOrdre;
                	            aMsg(nb_element)   := cDosRubFinPreLiee_rec.DRUORDRE;
                              aType(nb_element)  := 'N';
                	            aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                	            aOrdre(nb_element) := nOrdre;
                	            aMsg(nb_element)   := cDosRubFinPreLiee_rec.DRUORDREMAITRE;
                                    aType(nb_element)  := 'N';
                	            aDec(nb_element)   := NULL;
                           END IF;
                        END if;

                     END LOOP;
                  ELSIF sControle = 'R0370' AND cDosRubFinPre_rec.DRUMODCALINT IS NOT NULL THEN
                     IF pa_fungencontrole.ocCodeTable( 'I',
                        cDosRubFinPre_rec.DRUMODCALINT,
                        'MODECALPRE',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R0371' AND cDosRubFinPre_rec.DRUTXNATURE IS NOT NULL THEN
                     IF pa_fungencontrole.ocCodeTable( 'I',
                        cDosRubFinPre_rec.DRUTXNATURE,
                        'MONTPRETAUX',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R0372' AND cDosRubFinPre_rec.DRUBASECALC IS NOT NULL THEN
                    IF contrat_rec.TACCODE = 'PRET' THEN
                       IF pa_fungencontrole.ocCodeTable( 'I',
                           cDosRubFinPre_rec.DRUBASECALC,
                           'ECHINTPRE',
                           nb_element,
                           aOrdre,
                           aMsg,
                           aType,
                           aDec ) = 0 THEN
                           lOk := 0;
                       END IF;
                    ELSE
                       IF pa_fungencontrole.ocCodeTable( 'I',
                           cDosRubFinPre_rec.DRUBASECALC,
                           'ASSIETTPRE',
                           nb_element,
                           aOrdre,
                           aMsg,
                           aType,
                           aDec ) = 0 THEN
                           lOk := 0;
                       END IF;
                    END IF;
                  END IF;
               EXCEPTION
                  WHEN err_parm1 THEN
                     lOk                := 0;
                     nb_element   	:= nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubFinPre_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                  WHEN err_parm2 THEN
                     lOk                := 0;
                     nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
		               aOrdre(nb_element) := nOrdre;
               	     aMsg(nb_element)   := TO_CHAR( dtDate1,'YYYYMMDD' );
               	     aType(nb_element)  := 'D';
               	     aDec(nb_element)   := NULL;
               	     nb_element   	    := nb_element + 1;
               	     aOrdre(nb_element) := nOrdre;
               	     aMsg(nb_element)   := 'LANPHASE|DOSSIER|EC';
               	     aType(nb_element)  := 'S';
               	     aDec(nb_element)   := NULL;
               	     nb_element   	    := nb_element + 1;
               	     aOrdre(nb_element) := nOrdre;
               	     aMsg(nb_element)   := TO_CHAR( dtDate2,'YYYYMMDD' );
               	     aType(nb_element)  := 'D';
               	     aDec(nb_element)   := NULL;
               	     nb_element         := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubFinPre_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                  WHEN err_parm3 THEN
                     lOk                := 0;
                     nb_element   	    := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubFinPre_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                  WHEN  err_parm4 THEN
                     lOk                := 0;
                     nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
		               aOrdre(nb_element) := nOrdre;
               	   aMsg(nb_element)   := nDruOrdreLiee;
               	   aType(nb_element)  := 'N';
               	   aDec(nb_element)   := NULL;
               	   nb_element   	    := nb_element + 1;
               	   aOrdre(nb_element) := nOrdre;
               	   aMsg(nb_element)   := nDruOrdreMaitre;
               	   aType(nb_element)  := 'N';
               	   aDec(nb_element)   := NULL;

               END;
            END LOOP;
         END IF;
         RETURN lOk;
      END;
   END ocDossRubFinPre;


	/*
	// Controle rubriques accessoires R04
	*/
   FUNCTION ocDossRubAcc(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk            NUMBER := 1;
		   nOrdre         NUMBER := 0;
		   nAny           NUMBER ;
         nOrdreAssiette L1DOSRUBRIQUE.DRUORDREASSIETTE%TYPE;
         err_parm1      EXCEPTION;
         err_parm2      EXCEPTION;
         err_parm3      EXCEPTION;
         sCode          VARCHAR2(200);
         sTaxType   TAXE.TAXTYPE%TYPE:=NULL;
         nTrouveTaxeRubLoyer  NUMBER:=NULL;
         err_Taxe      EXCEPTION;
         nAssur        NUMBER;
         sRolCode      ROLE.ROLCODE%TYPE;
         CURSOR cDosRubAcc IS SELECT DRUTYPEMONTAGE, DRUBASECALC, DRUORDRE, DRUACCMTMINI,
                                     DRUACCMTMAXI, DRUACCMTFRANCHISE, RUBID, POOID,
                                     DRUTAUXFIXE, DRUORDREMAITRE,DRU.TAXCODE, TAX.TAXTYPE,DRUTYPE,
                                     DACORDREREVERSEMENT, TAXCODEREVERSEMENT, DRUTXREVERSEMENT
                              FROM   DOSRUBRIQUE  DRU,
                                     TAXE TAX
                              WHERE  DOSID     = contrat_rec.DOSID
                              AND    DRU.TAXCODE = TAX.TAXCODE
                              AND    DRUCLASSE = 'A'
                              AND    DRUTYPE   IN ( 'F', 'R', 'M' );

         CURSOR cRubAssAcc IS SELECT DR1NOMBRE, DR1BASE, DRUORDRE, DRUORDREASSIETTE
                              FROM   L1DOSRUBRIQUE
                              WHERE  DOSID    = contrat_rec.DOSID
                              AND    DRUORDRE = nOrdreAssiette;
			nDruOrdre      NUMBER;
      	dtNaissance    DATE;
   	   dtDateDebut		DATE;
	   	dtDateFin		DATE;
   		nDureeAn    	NUMBER;
   		nDureeMois  	NUMBER;
			nDureeJour  	NUMBER;
			sActeur        VARCHAR2(100) ;
			nAgeMaxi       NUMBER;
			CURSOR cDosActAssure IS SELECT   DISTINCT ACT.ACTID, ACTCODE , ACTLIBCOURT, F_PlGetAgeMaxi(DRU.DOSID, DRU.DRUORDRE, 65) AGEMAXI, DRUORDRE, DRUDTDEB
                                 FROM 		DOSACTEUR DAC, ACTEUR ACT, DOSRUBRIQUE DRU
                        	      WHERE 	DAC.DOSID = contrat_rec.dosid
                        	      AND		DRU.DRUORDRE = nDruOrdre
                        	      AND   	ROLCODE = 'SCASS'
                                 AND      ACT.ACTID = DAC.ACTID
                                 AND      F_NATCATJ (CJUCODE) = 'P'
                                 AND      DRU.DOSID    = DAC.DOSID
                                 AND      DRU.DACORDREASSURE = DAC.DACORDRE
                                 AND      DRU.DRUCLASSE = 'A'
                                 AND      F_ISRUBIDONFILTRE( DRU.RUBID, 'ASSVIE') = 1
                        	      ORDER BY ACT.ACTID ;
      BEGIN
         FOR cDosRubAcc_rec IN cDosRubAcc LOOP
            BEGIN
               IF sControle IN ( 'R0401', 'R0402' ) AND cDosRubAcc_rec.DRUTYPEMONTAGE IS NOT NULL
                                                    AND cDosRubAcc_rec.DRUTYPEMONTAGE IN ('ASSIETT','ASSIEIN') THEN
                  IF sControle = 'R0401' AND cDosRubAcc_rec.DRUBASECALC IS NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R0402' THEN
                     nOrdreAssiette := cDosRubAcc_Rec.DRUORDRE;
                     FOR cRubAssAcc_rec IN cRubAssAcc LOOP
                        BEGIN
                           IF cRubAssAcc_rec.DR1NOMBRE IS NOT NULL THEN
                              IF cRubAssAcc_rec.DR1BASE IS NULL THEN
                                 RAISE err_parm2;
                              ELSIF cRubAssAcc_rec.DR1BASE < 1 THEN
                                 RAISE err_parm2;
                              END IF;
                           END IF;
                        EXCEPTION
                           WHEN err_parm2 THEN
                              lOk                := 0;
                              nb_element         := nb_element + 1;
                              nOrdre             := nb_element;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := cRubAssAcc_rec.DRUORDREASSIETTE;
                              aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
                              nb_element         := nb_element + 1;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := cRubAssAcc_rec.DRUORDRE;
                              aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
                        END;
                     END LOOP;
                  END IF;
               ELSIF sControle = 'R0436' AND contrat_rec.TACCODE IN ('CBI','CBMIXTE','LOCATIF')
                        AND cDosRubAcc_rec.TAXTYPE !='EXO'  THEN
                        DBMS_OUTPUT.PUT_LINE(' Type de taxe definie sur la rubrique '||cDosRubAcc_rec.TAXTYPE);
                  BEGIN
                     IF nTrouveTaxeRubLoyer IS NULL THEN
                        -- Permet de lancer la proc une seule fois
                        PA_DOSRUBCONTROLE2.GetTypeImposition (contrat_rec.DOSID,contrat_rec.TACCODE,
                                    sTaxType,nTrouveTaxeRubLoyer);
                        DBMS_OUTPUT.PUT_LINE(' Type de taxe sur rubrique de loyer ' || sTaxType|| ' trouve '|| TO_CHAR(nTrouveTaxeRubLoyer));
                     END IF;
                     IF sTaxType IS NOT NULL AND nTrouveTaxeRubLoyer=1 THEN
                        IF contrat_rec.TACCODE = 'LOCATIF' THEN
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   RUBACCES
                           WHERE  RUBID = cDosRubAcc_rec.RUBID
                           AND    RACACCES = 'ACCLOY';
                           IF cDosRubAcc_rec.TAXTYPE != sTaxType  AND nAny=0 THEN
                              DBMS_OUTPUT.PUT_LINE(' erreur sur type d''imposition');
                              IF cDosRubAcc_rec.TAXCODE != 'MIXTE' THEN
                                 RAISE err_Taxe;
                              ELSE
                                 SELECT COUNT(*)
                                 INTO   nAny
                                 FROM   DOSRUBTVATAXE
                                 WHERE  DOSID = contrat_rec.DOSID
                                 AND    DRUORDRE = cDosRubAcc_rec.DRUORDRE
                                 AND    TAXCODE NOT IN (SELECT TAXCODE FROM TAXE
                                                      WHERE TAXTYPE = sTaxType
                                                      OR    TAXTYPE = 'EXO');
                                 IF nAny > 0 THEN
                                    RAISE err_Taxe;
                                 END IF;
                              END IF;
                           END IF;
                        ELSE
                           IF cDosRubAcc_rec.TAXTYPE != sTaxType  AND cDosRubAcc_rec.DRUTYPE = 'F' THEN
                              IF cDosRubAcc_rec.TAXCODE != 'MIXTE' THEN
                                 RAISE err_Taxe;
                              ELSE
                                 SELECT COUNT(*)
                                 INTO   nAny
                                 FROM   DOSRUBTVATAXE
                                 WHERE  DOSID = contrat_rec.DOSID
                                 AND    DRUORDRE = cDosRubAcc_rec.DRUORDRE
                                 AND    TAXCODE NOT IN (SELECT TAXCODE FROM TAXE
                                                      WHERE TAXTYPE = sTaxType
                                                      OR    TAXTYPE = 'EXO');
                                 IF nAny > 0 THEN
                                    RAISE err_Taxe;
                                 END IF;
                              END IF;
                           END IF;
                        END IF;
                     END IF;
                  EXCEPTION
                     WHEN err_Taxe THEN
                        lOk                := 0;
                        nb_element   	     := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cDosRubAcc_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	     := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := 'LANTTRPARAM|TAXE|' ||sTaxType ;
                        aType(nb_element)  := 'S';
                        aDec(nb_element)   := NULL;
                  END;
               ELSIF (sControle = 'R0403') AND contrat_rec.DOSPOOL = 'CDF'  THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   RUBACCES
                  WHERE  RUBID = cDosRubAcc_rec.RUBID
                         AND RACACCES = 'ACCLOY';
                  IF (nAny != 0) AND (cDosRubAcc_rec.POOID IS NULL)
                     AND   F_ISRUBIDONFILTRE( cDosRubAcc_rec.RUBID, 'LSNPOOL' ) = 0  THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF ( sControle = 'R0404' ) AND ( contrat_rec.TACCODE = 'LOCATIF' )
                                             AND ( cDosRubAcc_rec.DRUTYPEMONTAGE IN ( 'CASEUL', 'CAPLUS', 'CAMIXT' ) ) THEN
				  SELECT COUNT(*) INTO nAny
				  FROM 	DOSRUBTAUXECHELLE DRT, DOSRUBTAUXECHVALEUR DVA
                  WHERE  DRT.DOSID = contrat_rec.DOSID
                  AND    DRT.DRUORDRE = cDosRubAcc_rec.DRUORDRE
				  AND	 DRT.DRTPERIODE ='CA'
				  AND	 DRT.DOSID = DVA.DOSID
				  AND	 DRT.DRUORDRE =DVA.DRUORDRE
				  AND	 DVA.DVAFIXEDRATE IS NULL;
				  IF nAny > 0 THEN
					RAISE err_parm1;
				END IF;
               ELSIF sControle = 'R0430' AND cDosRubAcc_rec.DRUACCMTMINI IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTMAXI IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTMINI >= cDosRubAcc_rec.DRUACCMTMAXI THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0431' AND cDosRubAcc_rec.DRUACCMTFRANCHISE IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTMAXI      IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTFRANCHISE >= cDosRubAcc_rec.DRUACCMTMAXI THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0432' AND cDosRubAcc_rec.DRUACCMTMINI      IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTFRANCHISE IS NOT NULL
                                         AND cDosRubAcc_rec.DRUACCMTMINI      >= cDosRubAcc_rec.DRUACCMTFRANCHISE THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0433' AND cDosRubAcc_rec.DRUTYPEMONTAGE    IS NOT NULL
                                         AND cDosRubAcc_rec.DRUTYPEMONTAGE    NOT IN ( 'ASSIETT', 'ASSIEIN' )
                                         AND ( cDosRubAcc_rec.DRUACCMTMINI    IS NOT NULL
                                         OR  cDosRubAcc_rec.DRUACCMTFRANCHISE IS NOT NULL
                                         OR  cDosRubAcc_rec.DRUACCMTMAXI      IS NOT NULL ) THEN
                  RAISE err_parm1;
               ELSIF ( sControle = 'R0434' ) AND ( contrat_rec.TACCODE = 'LOCATIF' )
                                             AND ( cDosRubAcc_rec.DRUTYPEMONTAGE IN ( 'CASEUL', 'CAPLUS', 'CAMIXT' ) ) THEN

				  SELECT COUNT(*) INTO nAny
				  FROM 	DOSRUBTAUXECHELLE DRT, DOSRUBTAUXECHVALEUR DVA
                  WHERE  DRT.DOSID = contrat_rec.DOSID
                  AND    DRT.DRUORDRE = cDosRubAcc_rec.DRUORDRE
				  AND	 DRT.DRTPERIODE ='CA'
				  AND	 DRT.DOSID = DVA.DOSID
				  AND	 DRT.DRUORDRE =DVA.DRUORDRE
				  AND	 DVA.DVAFIXEDRATE IS not NULL
				  and	 DVA.DVAFIXEDRATE <=0;
				  IF nAny > 0 THEN
					RAISE err_parm1;
				end if;

               ELSIF ( sControle = 'R0435' ) AND ( contrat_rec.TACCODE = 'LOCATIF' )
                                             AND cDosRubAcc_rec.DRUORDREMAITRE IS NULL
                                             AND cDosRubAcc_rec.DRUTYPEMONTAGE IS NOT NULL
                                             AND cDosRubAcc_rec.DRUTYPEMONTAGE IN ( 'ASSIETT','ASSIEIN' )
                                             AND F_ISRUBIDONFILTRE( cDosRubAcc_rec.RUBID, 'ACCLOY' ) = 1
                                             AND F_ISRUBIDONFILTRE( cDosRubAcc_rec.RUBID, 'NEGATIF' ) = 0 THEN
                  RAISE err_parm2;
               ELSIF sControle = 'R0470' AND cDosRubAcc_rec.DRUBASECALC IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable( 'I',
                     cDosRubAcc_rec.DRUBASECALC,
                     'ASSIETTACC',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R0471' THEN
                  SELECT COUNT(1)
                  INTO   nAny
                  FROM   RUBACCES
                  WHERE  RUBID = cDosRubAcc_rec.RUBID
                  AND    RACACCES IN ('REVERS', 'REVFACT');
                  IF nAny > 0 THEN
                     IF cDosRubAcc_rec.DACORDREREVERSEMENT IS NULL OR cDosRubAcc_rec.TAXCODEREVERSEMENT IS NULL OR
                        cDosRubAcc_rec.DRUTXREVERSEMENT IS NULL THEN
                        RAISE err_parm1;
                     ELSE
                        SELECT COUNT(1)
                        INTO   nAssur
                        FROM   RUBACCES
                        WHERE  RUBID = cDosRubAcc_rec.RUBID
                        AND    RACACCES = 'ASSUR';

                        IF nAssur > 0 THEN
                           SELECT ROLCODE
                           INTO   sRolCode
                           FROM   DOSACTEUR
                           WHERE  DACORDRE = cDosRubAcc_rec.DACORDREREVERSEMENT
                           AND    DOSID = contrat_rec.DOSID;

                           IF sRolCode != 'ASSUR' THEN
                              RAISE err_parm1;
                           END IF;
                        END IF;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0439' THEN
               	FOR cDosActAssure_rec IN cDosActAssure LOOP
		               BEGIN
                  	   SELECT APADTNAISS
                  	   INTO dtNaissance
                  	   FROM ACTEURPARTICULIER
                  	   WHERE ACTID = cDosActAssure_rec.ACTID ;
                  	EXCEPTION
                  	   WHEN OTHERS THEN
                  	      dtNaissance := TO_DATE(NULL) ;
                  	END ;
                  	DBMS_OUTPUT.PUT_LINE( 'cDosActAssure_rec.DRUDTDEB = ' || TO_CHAR(cDosActAssure_rec.DRUDTDEB,'dd/mm/yyyy'));
               		-- Calcul du nombre d'annee.
                  	IF cDosActAssure_rec.DRUDTDEB IS NOT NULL THEN

                  		IF cDosActAssure_rec.DRUDTDEB > dtNaissance THEN
                  			dtDateDebut  := dtNaissance;
                  			dtDateFin    := cDosActAssure_rec.DRUDTDEB;
                  		ELSE
                  			dtDateDebut  := cDosActAssure_rec.DRUDTDEB;
                  			dtDateFin    := dtNaissance;
                  		END IF;
                  		P_DateDureeYMD( dtDateDebut,
   												 dtDateFin,
   												 nDureeAn,
   												 nDureeMois,
													 nDureeJour);
								DBMS_OUTPUT.PUT_LINE( 'Date DureeAn = ' || TO_CHAR(nDureeAn));
								DBMS_OUTPUT.PUT_LINE( 'Date cDosActAssure_rec.AGEMAXI = ' || TO_CHAR(cDosActAssure_rec.AGEMAXI));
								IF nDureeAn	>= cDosActAssure_rec.AGEMAXI THEN
									nAgeMaxi := cDosActAssure_rec.AGEMAXI;
									sActeur := cDosActAssure_rec.ACTCODE || ' ' || CDOSACTASSURE_REC.ACTLIBCOURT ;
									RAISE err_parm3;
									--DBMS_OUTPUT.PUT_LINE ( 'Dans exption 2');
								END IF;
							END IF;
               	END LOOP;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubAcc_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parm2 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := 'LANTTRPARAM|TYPERUBACCESS|' || cDosRubAcc_rec.DRUTYPEMONTAGE;
                  aType(nb_element)  := 'S';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubAcc_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
					WHEN err_parm3 THEN
               	lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := F_StdTrimAll(sActeur);
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRubAcc_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               	nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nAgeMaxi;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nDureeAn;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
            END;
         END LOOP;
         RETURN lOk;
	   END;
   END ocDossRubAcc;

	/*
	// Controle "Saison" R09
	*/
	FUNCTION ocDossRubSaison(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk       NUMBER := 1;
		   nOrdre    NUMBER := 0;
		   nAny      NUMBER ;
         nRepart   NUMBER ;
         nRubOrdre DOSRUBRIQUE.DRUORDRE%TYPE;
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;
			nNbSaison		NUMBER:=0;
			nDrfNbPeriode	NUMBER:=0;
         CURSOR cDosRub IS SELECT DRUSAISONPERIODE, DRUSAISONMULTIPLE, DRUORDRE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID      = contrat_rec.DOSID
                           AND    DRUMONTAGE = 'SAISON';

         CURSOR cDosRubSaison IS SELECT DRSORDRE, DRSPONDERATION
                                 FROM   DOSRUBSAISON
                                 WHERE  DOSID    = contrat_rec.DOSID
                                 AND    DRUORDRE = nRubOrdre;
      BEGIN
         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               IF sControle = 'R0901' AND cDosRub_rec.DRUSAISONPERIODE IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0902' AND cDosRub_rec.DRUSAISONMULTIPLE IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0903' THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBSAISON
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                  IF nAny = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0904' THEN
                  nRepart   := 1;
                  nRubOrdre := cDosRub_Rec.DRUORDRE;
                  FOR cDosRubSaison_rec IN cDosRubSaison LOOP
                     BEGIN
                        IF cDosRubSaison_rec.DRSORDRE IS NULL OR cDosRubSaison_rec.DRSPONDERATION IS NULL THEN
                           RAISE err_parm2;
                        END IF;
                        nRepart := nRepart + 1;
                     EXCEPTION
                        WHEN err_parm2 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nRepart;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                     END;
                  END LOOP;
               ELSIF sControle = 'R0930' THEN
            		SELECT 	COUNT(1) INTO nNbSaison
            		FROM 		DOSRUBSAISON
            		WHERE  	DOSID    = contrat_rec.DOSID
                  AND    	DRUORDRE = cDosRub_rec.DRUORDRE;

                  SELECT 	NVL(SUM(DRFNBPERIODE),0)
                  INTO 		nDrfNbPeriode
                  FROM   	DOSRUBFLUX
                  WHERE  	DOSID    = contrat_rec.DOSID
                  AND    	DRUORDRE = cDosRub_rec.DRUORDRE;

                  IF nNbSaison > nDrfNbPeriode THEN
                  	lOk                := 0;
                     nb_element   	    := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := nNbSaison;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                     nb_element   	    := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := nDrfNbPeriode;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                     nb_element   	    := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                  END IF;
               ELSIF sControle = 'R0971' AND cDosRub_rec.DRUSAISONPERIODE IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable( 'I',
                     cDosRub_rec.DRUSAISONPERIODE,
                     'PERIODE',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                	aOrdre(nb_element) := nOrdre;
                	aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                	aDec(nb_element)   := NULL;
            END;
         END LOOP;

         RETURN lOk;
	   END;
   END ocDossRubSaison;

	/*
	// Controle "Fiscal" R10
	*/
	FUNCTION ocDossRubFiscal(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk       NUMBER := 1;
		   nOrdre    NUMBER := 0;
         lOneLoi   BOOLEAN;
         nDafOrdre DOSRUBAMOFISCAL.DAFORDRE%TYPE;
         nRubOrdre DOSRUBRIQUE.DRUORDRE%TYPE;
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;

         CURSOR cDosRub IS SELECT DRUORDRE, DRUCLASSE, DRUTYPE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID      = contrat_rec.DOSID
                           AND    DRUMONTAGE = 'FISCAL';

         CURSOR cDosRubFiscal IS SELECT DAFORDRE, DAFMT, DAFLOI, DAFCOEF, DAFDUREEAN,
                                        DAFDUREEMOIS, DAFDUREEJOUR
                                 FROM   DOSRUBAMOFISCAL
                                 WHERE  DOSID    = contrat_rec.DOSID
                                 AND    DRUORDRE = nRubOrdre;
      BEGIN
         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               IF sControle IN ( 'R1001', 'R1002', 'R1003', 'R1004', 'R1071' ) THEN
                  nRubOrdre := cDosRub_Rec.DRUORDRE;
                  FOR cDosRubFiscal_rec IN cDosRubFiscal LOOP
                     BEGIN
                        nDafOrdre := cDosRubFiscal_rec.DAFORDRE;
                        IF sControle = 'R1001' AND cDosRubFiscal_rec.DAFMT IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1002' AND cDosRubFiscal_rec.DAFLOI IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1003' AND cDosRubFiscal_rec.DAFLOI  IS NOT NULL
                                                  AND cDosRubFiscal_rec.DAFLOI  =  'DF'
                                                  AND CDosRubFiscal_rec.DAFCOEF IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1004' AND cDosRubFiscal_rec.DAFLOI       IS NOT NULL
                                                  AND cDosRubFiscal_rec.DAFLOI       NOT IN ( 'NA', 'IMM' )
                                                  AND CDosRubFiscal_rec.DAFDUREEAN   IS NULL
                                                  AND CDosRubFiscal_rec.DAFDUREEMOIS IS NULL
                                                  AND CDosRubFiscal_rec.DAFDUREEJOUR IS NULL THEN
                           RAISE err_parm2;
                        ELSIF sControle = 'R1071' AND cDosRubFiscal_rec.DAFLOI IS NOT NULL THEN
                  	      IF pa_fungencontrole.ocCodeTable( 'I',
                              cDosRubFiscal_rec.DAFLOI,
                              'TYPELOIAMOFIX',
                              nb_element,
                              aOrdre,
                              aMsg,
                              aType,
                              aDec ) = 0 THEN
                              lOk := 0;
                           END IF;
                        END IF;
                     EXCEPTION
                        WHEN err_parm2 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nDafOrdre;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                     END;
                  END LOOP;
               ELSIF sControle = 'R1030' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE  = 'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   != 'F' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1031' THEN
                  lOneLoi   := FALSE;
                  nRubOrdre := cDosRub_Rec.DRUORDRE;
                  FOR cDosRubFiscal_rec IN cDosRubFiscal LOOP
                     lOneLoi := TRUE;
                     EXIT;
                  END LOOP;
                  IF NOT lOneLoi THEN
                     RAISE err_parm1;
                  END IF;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubFiscal;

	/*
	// Controle "Compte courant" R11
	*/
   FUNCTION ocDossRubCpteCourant(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
	   DECLARE
		   lOk       NUMBER := 1;
         nIs       NUMBER;
         nOrdre    NUMBER := 0;
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;
         nCount NUMBER;
         CURSOR cDosRub IS SELECT DRUORDRE, DRUCCMETHODETEA, DRUCCTYPETVA, TAXCODE,
                                  DRUCCORIGINETAUX, DRUCCTAUX, DRUTYPE, DRUCVTMODEINDEX
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID          = contrat_rec.DOSID
                           AND    DRUCLASSE      = 'F'
                           AND    DRUTYPEMONTAGE = 'CVT';
      BEGIN
         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               IF sControle = 'R1101' AND cDosRub_rec.DRUCCMETHODETEA IS NULL
                                      AND cDosRub_rec.DRUCCORIGINETAUX IS NOT NULL
                                      AND cDosRub_rec.DRUCCORIGINETAUX != 'SAISI' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1131' AND cDosRub_rec.DRUCCORIGINETAUX IS NOT NULL
                                      AND cDosRub_rec.DRUCCORIGINETAUX = 'INITIAL' THEN
                  SELECT COUNT(*)
                  INTO   nCount
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID = contrat_rec.DOSID AND
                         DRUTYPE = 'V' AND
                         DRUORDREFACTURATION = cDosRub_rec.DRUORDRE AND
                         DRUTXNATURE IN ( 'VR', 'VRR' );
                  IF nCount = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R1102' AND cDosRub_rec.DRUCCTYPETVA IS NULL THEN
                  PA_SELECTDOSSIER2.S_TAXEISTVA( cDosRub_rec.TAXCODE, nIs );
                  IF nIs = 1 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R1103' AND cDosRub_rec.DRUCCORIGINETAUX IS NOT NULL
                                         AND cDosRub_rec.DRUCCORIGINETAUX =  'SAISI'
                                         AND cDosRub_rec.DRUCCTAUX        IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1130' AND cDosRub_rec.DRUTYPE IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE != 'F' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1171' AND cDosRub_rec.DRUCCMETHODETEA IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUCCMETHODETEA,
                     'CCMETHODETEA',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R1172' AND cDosRub_rec.DRUCVTMODEINDEX IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUCVTMODEINDEX,
                     'MODEINDCVT',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R1173' AND cDosRub_rec.DRUCCORIGINETAUX IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUCCORIGINETAUX,
                     'TAUXINTCVT',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubCpteCourant;

	/*
	// Controle "Recadrage" R12
	*/
	FUNCTION ocDossRubrecadrage(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
      DECLARE
		   lOk       NUMBER := 1;
		   nOrdre    NUMBER := 0;
         sTableNom VARCHAR2(15);
         err_parm1 EXCEPTION;
         err_parm2 EXCEPTION;

         CURSOR cDosRub IS SELECT DRUROMPUPERCEP, DRUDTROMPU, DRUROMPUNBJDECAL,
                                  DRUROMPUINTNUM, DRUROMPUINTDEN, DRUROMPUTAUX,
                                  DRUROMPUREPART, DRUMODCALINT, DRUDTDEB, DRUDTFIN,
                                  DRUFLAGROMPULOYCST, DRUCLASSE, DRUMONTAGE, DRUORDRE,
                                  DRUTYPE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID = contrat_rec.DOSID;
      BEGIN
         FOR cDosRub_rec IN cDosRub LOOP
            BEGIN
               IF sControle = 'R1232' AND cDosRub_rec.DRUDTROMPU IS NOT NULL
                                      AND cDosRub_rec.DRUDTDEB   IS NOT NULL
                                      AND contrat_rec.TACCODE != 'LOCATIF'
                                      AND cDosRub_rec.DRUDTROMPU <= cDosRub_rec.DRUDTDEB  THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1232' AND cDosRub_rec.DRUDTROMPU IS NOT NULL
                                      AND cDosRub_rec.DRUDTDEB   IS NOT NULL
                                      AND contrat_rec.TACCODE = 'LOCATIF'
                                      AND cDosRub_rec.DRUDTROMPU < cDosRub_rec.DRUDTDEB  THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R1233' AND cDosRub_rec.DRUDTROMPU IS NOT NULL
                                         AND cDosRub_rec.DRUDTFIN   IS NOT NULL
                                         AND cDosRub_rec.DRUDTROMPU > cDosRub_rec.DRUDTFIN THEN
                  RAISE err_parm1;
               ELSIF NOT IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) THEN
                  IF sControle = 'R1201' AND cDosRub_rec.DRUROMPUPERCEP IS NULL
                                         AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1202' AND cDosRub_rec.DRUROMPUPERCEP   IS NOT NULL
                                            AND cDosRub_rec.DRUROMPUPERCEP   IN ( 'D', 'F' )
                                            AND cDosRub_rec.DRUROMPUNBJDECAL IS NULL
                                            AND cDosRub_rec.DRUDTROMPU       IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1203' AND cDosRub_rec.DRUROMPUINTNUM IS NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1204' AND cDosRub_rec.DRUROMPUINTDEN IS NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1205' AND cDosRub_rec.DRUROMPUTAUX IS NULL
                                            AND cDosRub_rec.DRUDTROMPU   IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1206' AND cDosRub_rec.DRUROMPUREPART IS NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL
                                            AND cDosRub_rec.DRUMODCALINT   IS NOT NULL
                                            AND cDosRub_rec.DRUMODCALINT   =  'IS' THEN
                     RAISE err_parm1;
                  ELSIF sControle = 'R1271' AND cDosRub_rec.DRUROMPUPERCEP IS NOT NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     IF ( contrat_rec.TACCODE = 'LOCATIF' ) THEN
                        sTableNom := 'LSPERCEPT';
                     ELSE
                        sTableNom := 'PERCEPTION';
                     END IF;
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUROMPUPERCEP,
                        sTableNom,
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R1272' AND cDosRub_rec.DRUROMPUINTNUM IS NOT NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUROMPUINTNUM,
                        'ROMPUDECJOURS',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R1273' AND cDosRub_rec.DRUROMPUINTDEN IS NOT NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL THEN
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUROMPUINTDEN,
                        'ROMPUDECJOURS',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R1274' AND cDosRub_rec.DRUROMPUTAUX       IS NOT NULL
                                            AND cDosRub_rec.DRUFLAGROMPULOYCST IS NOT NULL
                                            AND cDosRub_rec.DRUDTROMPU         IS NOT NULL THEN
                     IF cDosRub_rec.DRUCLASSE = 'A' THEN
                        sTableNom := 'ECHACC';
                     ELSIF cDosRub_rec.DRUMONTAGE = 'AMOFIX' THEN
                        sTableNom := 'ECHRECAMOFIX';
                     ELSIF cDosRub_rec.DRUMONTAGE = 'FISCAL' THEN
                        sTableNom := 'ECHRECFISCAL';
                     ELSIF cDosRub_rec.DRUFLAGROMPULOYCST = 1 THEN
                        sTableNom := 'ECHCONS';
					 ELSIF cDosRub_rec.DRUFLAGROMPULOYCST = 2 THEN
						sTableNom := 'ECHDIFFERE';
                     ELSE
                        sTableNom := 'ECHRECAMOFIX';
                     END IF;
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUROMPUTAUX,
                        sTableNom,
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSIF sControle = 'R1275' AND cDosRub_rec.DRUROMPUREPART IS NOT NULL
                                            AND cDosRub_rec.DRUDTROMPU     IS NOT NULL
                                            AND cDosRub_rec.DRUMODCALINT   IS NOT NULL
                                            AND cDosRub_rec.DRUMODCALINT   =  'IS' THEN
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUROMPUREPART,
                        'ROMPUINTSIMPLE',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  END IF;
               END IF;
            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
            END;
         END LOOP;
         RETURN lOk;
      END;
   END ocDossRubRecadrage;

	FUNCTION ocDossRubProv(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2 ) RETURN NUMBER IS
   BEGIN
      DECLARE
		   lOk       NUMBER := 1;
		   nOrdre    NUMBER := 0;
         nCount    NUMBER :=0;
         nDruOrdre   DOSRUBRIQUE.DRUORDRE%TYPE;
         err_parm1   EXCEPTION;
         err_parm2   EXCEPTION;
         nBimId    BIENIMMOBILIER.BIMID%TYPE;
         CURSOR cDosRubProv IS
            SELECT DRUORDRE, DRUTYPEPROVISION, RUBID, DRUORDREMAITRE,DRUFLAGCUMUL
            FROM   DOSRUBRIQUE
            WHERE  DOSID = contrat_rec.DOSID
            AND    RUBID IN (SELECT RUBID FROM RUBRIQUE WHERE RUBPROVISION IS NOT NULL);
         CURSOR cFlux   IS
            SELECT DRFORDRE
            FROM   DOSRUBFLUX
            WHERE  DOSID = contrat_rec.DOSID
            AND    DRUORDRE = nDruOrdre;
         CURSOR cLkDrfDbi  IS
            SELECT LDD.BIMID,
                   BIM.BIMNUM,
                   DRF.DRFDTDEB,
                   DRF.DRFDTFIN
            FROM   LKDRFDBI LDD,
                   DOSRUBFLUX DRF,
                   BIENIMMOBILIER BIM
            WHERE  LDD.BIMID = BIM.BIMID
            AND    LDD.DOSID = DRF.DOSID
            AND    LDD.DRUORDRE = DRF.DRUORDRE
            AND    LDD.DRFORDRE = DRF.DRFORDRE
            AND    LDD.DOSID = contrat_rec.DOSID
            AND    LDD.DRUORDRE IN (SELECT DRUORDRE FROM DOSRUBRIQUE
                                    WHERE DOSID = contrat_rec.DOSID
                                    AND   RUBID IN (SELECT RUBID FROM RUBRIQUE
                                                      WHERE RUBPROVISION IS NOT NULL))
            ORDER BY LDD.BIMID,
                   BIM.BIMNUM,
                   DRF.DRFDTDEB,
                   DRF.DRFDTFIN;
      BEGIN
         IF sControle = 'R1501' THEN
            SELECT COUNT(*)
            INTO   nCount
            FROM   DOSRUBRIQUE
            WHERE  DOSID = contrat_rec.DOSID
            AND    RUBID IN (SELECT RUBID FROM RUBRIQUE
                                 WHERE  RUBPROVISION IS NOT NULL)
            AND    DRUTYPEPROVISION=  'LOYCHG'
            AND    DRUORDREMAITRE IS  NULL;
                  -- OR DRUORDREMAITRE IN (SELECT DRUORDRE FROM DOSRUBRIQUE
                    --                    WHERE DOSID = contrat_rec.DOSID
                      --                  AND F_ISRUBIDONFILTRE( RUBCODE, 'ACCLOY' ) != 1 ))

            IF nCount > 0 THEN
               lOk := 0;
            END IF;
         ELSIF sControle IN ('R1530', 'R1531', 'R1533') THEN
            FOR cDosRubProv_rec IN cDosRubProv LOOP
               BEGIN
                  IF sControle = 'R1530' THEN
                     IF cDosRubProv_rec.DRUTYPEPROVISION = 'LOYCHG' AND cDosRubProv_rec.DRUORDREMAITRE IS NOT NULL THEN
                        SELECT COUNT(*)
                        INTO   nCount
                        FROM   DOSRUBRIQUE
                        WHERE    DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRubProv_rec.DRUORDREMAITRE
                        AND   ( (F_ISRUBIDONFILTRE( RUBID, 'ACCLOY' ) = 1 AND DRUSENS = '-')
                               OR F_ISRUBIDONFILTRE( RUBID, 'ACCLOY' ) != 1  );
                        IF nCount > 0 THEN
                           RAISE err_parm1;
                        END IF;
                     END if;
                  ELSIF sControle = 'R1531' THEN
                     nDruOrdre := cDosRubProv_rec.DRUORDRE;
                     FOR cFlux_rec IN cFlux LOOP
                        BEGIN
                           SELECT COUNT (*)
                           INTO   nCount
                           FROM   LKDRFDBI
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    DRUORDRE = nDruOrdre
                           AND    DRFORDRE = cFlux_rec.DRFORDRE;
                           IF nCount < 1 THEN
                              RAISE err_parm2;
                           END IF;
                        EXCEPTION
                           WHEN err_parm2 THEN
                                 lOk:=0;
                                 nb_element   	     := nb_element + 1;
                                 nOrdre             := nb_element;
                                 aOrdre(nb_element) := nOrdre;
                                 aMsg(nb_element)   := nDruOrdre;
                                 aType(nb_element)  := 'N';
                                 aDec(nb_element)   := NULL;
               		            nb_element   	     := nb_element + 1;
		                           aOrdre(nb_element) := nOrdre;
               		            aMsg(nb_element)   := cFlux_rec.DRFORDRE;
		                           aType(nb_element)  := 'N';
               		            aDec(nb_element)   := NULL;
                        END;
                     END LOOP;
                  ELSIF sControle = 'R1533' THEN
                     IF cDosRubProv_rec.DRUTYPEPROVISION = 'LOYCHG' AND cDosRubProv_rec.DRUORDREMAITRE IS NOT NULL
                        AND NVL(cDosRubProv_rec.DRUFLAGCUMUL,0 ) = 0 THEN
                        SELECT COUNT(*)
                        INTO   nCount
                        FROM   DOSRUBRIQUE
                        WHERE    DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRubProv_rec.DRUORDREMAITRE
                        AND   F_ISRUBIDONFILTRE( RUBID, 'ACCLOY' ) = 1
                        AND   DRUSENS != '-';
                        IF nCount > 0 THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  END IF;
               EXCEPTION
                  WHEN err_parm1 THEN
                     lOk:=0;
                     nb_element   	     := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosRubProv_rec.DRUORDRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
               END;
            END LOOP;
         ELSIF sControle = 'R1532' THEN
            nBimId := NULL;
            FOR cLkDrfDbi_rec IN cLkDrfDbi LOOP
               IF nBimId IS NULL OR nBimId != cLkDrfDbi_rec.BIMID THEN
                  SELECT COUNT(*)
                  INTO   nCount
                  FROM   DOSRUBFLUX DRF,
                         LKDRFDBI LDD
                  WHERE  LDD.DOSID = contrat_rec.DOSID
                  AND    LDD.DOSID = DRF.DOSID
                  AND    LDD.DRUORDRE = DRF.DRUORDRE
                  AND  LDD.DRUORDRE IN (SELECT DRUORDRE FROM DOSRUBRIQUE
                                    WHERE DOSID = contrat_rec.DOSID
                                    AND   RUBID IN (SELECT RUBID FROM RUBRIQUE
                                                      WHERE RUBPROVISION IS NOT NULL))
                  AND    LDD.DRFORDRE = DRF.DRFORDRE
                  AND    LDD.BIMID = cLkDrfDbi_rec.BIMID
                  AND    ((cLkDrfDbi_rec.DRFDTFIN IS NOT NULL
                           AND DRF.DRFDTDEB BETWEEN cLkDrfDbi_rec.DRFDTDEB AND cLkDrfDbi_rec.DRFDTFIN)
         		 	OR 	 ((cLkDrfDbi_rec.DRFDTFIN IS NOT NULL
                           AND DRF.DRFDTFIN  IS NOT NULL
                            AND DRF.DRFDTFIN BETWEEN cLkDrfDbi_rec.DRFDTDEB AND cLkDrfDbi_rec.DRFDTFIN )
                            OR (cLkDrfDbi_rec.DRFDTFIN IS NULL
                           AND  DRFDTFIN IS NULL)));
                  IF nCount > 1 THEN
                     nBimId:= cLkDrfDbi_rec.BIMID;
                     lOk:= 0;
                     nb_element   	     := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := F_StdTrimAll(cLkDrfDbi_rec.BIMNUM);
                     aType(nb_element)  := 'C';
                     aDec(nb_element)   := NULL;
                  ELSE
                     nBimId:=NULL;
                  END IF;
               END IF;
            END LOOP;
         END IF;
         RETURN lOk;
      END;
   END ocDossRubProv;

   FUNCTION ocDossRubPassive(
   contrat_rec IN DOSSIER%ROWTYPE,
   sControle   IN VARCHAR2 ) RETURN NUMBER IS
BEGIN
   DECLARE
	   lOk         NUMBER := 1;
	   nOrdre      NUMBER := 0;
      nCount      NUMBER;
      nDruOrdreB  DOSRUBRIQUE.DRUORDRE%TYPE;
      nDruOrdreF  DOSRUBRIQUE.DRUORDRE%TYPE;
      nNombreB    NUMBER;
      nNombreF    NUMBER;
      nMinOrdre   DOSRUBFLUX.DRFORDRE%TYPE;
      dtDateEch   DATE;
      dtDateEch2  DATE;
      CURSOR C1    IS SELECT * FROM DOSRUBRIQUE WHERE DOSID = contrat_rec.DOSID AND DRUTYPE  = 'B';
      CURSOR C2    IS SELECT * FROM DOSRUBRIQUE WHERE DOSID = contrat_rec.DOSID AND DRUTYPE  = 'F' AND DRUORDREMAITRE = nDruOrdreB;
      CURSOR FLUXB IS SELECT * FROM DOSRUBFLUX  WHERE DOSID = contrat_rec.DOSID AND DRUORDRE = nDruOrdreB ORDER BY DRFORDRE;
   BEGIN
      FOR C1R IN C1 LOOP
         nDruOrdreB := C1R.DRUORDRE;
         FOR C2R IN C2 LOOP
            IF F_ISDOSMULTICURRENCY ( contrat_rec.ACTID, contrat_rec.DOSID, contrat_rec.TACCODE ) = 1 THEN
               -- MSG12292
               IF sControle = 'R1631' THEN
                  SELECT COUNT(*)
                  INTO   nCount
                  FROM   DOSRUBECHEANCIER DB
                  WHERE  DOSID = contrat_rec.DOSID
                  AND    DRUORDRE = nDruOrdreB
                  AND    (( DB.FACID IS NULL     AND EXISTS ( SELECT 1 FROM DOSRUBECHEANCIER DF  WHERE DF.DOSID  = DB.DOSID AND DRUORDRE = C2R.DRUORDRE AND DF.FACID  IS NOT NULL AND DF.DRENUM  = DB.DRENUM ))
                  OR      ( DB.FACID IS NOT NULL AND EXISTS ( SELECT 1 FROM DOSRUBECHEANCIER DF2 WHERE DF2.DOSID = DB.DOSID AND DRUORDRE = C2R.DRUORDRE AND DF2.FACID IS NULL     AND DF2.DRENUM = DB.DRENUM )));
                  IF nCount > 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R1632' THEN
                  SELECT COUNT(*)
                  INTO   nNombreB
                  FROM   DOSRUBECHEANCIER
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = nDruOrdreB;
                  SELECT COUNT(*)
                  INTO   nNombreF
                  FROM   DOSRUBECHEANCIER
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = C2R.DRUORDRE;
                  dbms_output.put_line( 'NB DOSRUBECHEANCIER : Rub ' || TO_CHAR(nDruOrdreB) || ' -> ' || TO_CHAR(nNombreB) || ' Rub.Red ' || TO_CHAR(C2R.DRUORDRE) || ' - > ' || TO_CHAR(nNombreF));
                  IF nNombreB != nNombreF THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R1633' THEN
                  SELECT COUNT(*)
                  INTO   nNombreB
                  FROM   DOSRUBFLUX
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = nDruOrdreB;
                  SELECT COUNT(*)
                  INTO   nNombreF
                  FROM   DOSRUBFLUX
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = C2R.DRUORDRE;
                  IF nNombreB != nNombreF THEN
                     lOk := 0;
                  ELSE
                     FOR FLUXB_Rec IN FLUXB LOOP
                        SELECT COUNT(*)
                        INTO   nCount
                        FROM   DOSRUBFLUX
                        WHERE  DOSID         = contrat_rec.DOSID
                        AND    DRUORDRE      = C2R.DRUORDRE
                        AND    DRFDTDEB      = FLUXB_Rec.DRFDTDEB
                        AND    DRFPERCEPTION = FLUXB_Rec.DRFPERCEPTION
                        AND    DRFNBPERIODE  = FLUXB_Rec.DRFNBPERIODE
                        AND    DRFMULTIPLE   = FLUXB_Rec.DRFMULTIPLE
                        AND    DRFPERIODE    = FLUXB_Rec.DRFPERIODE;
                        IF nCount = 0 THEN
                           lOk := 0;
                           EXIT;
                        END IF;
                     END LOOP;
                  END IF;
               END IF;
            END IF;
         END LOOP;
         -- Absence de rub fi MSG12295
         IF F_ISDOSMULTICURRENCY ( contrat_rec.ACTID, contrat_rec.DOSID, contrat_rec.TACCODE ) = 1 THEN
            IF sControle = 'R1601' AND C1R.DRUCLASSE = 'F' THEN
               SELECT COUNT(*)
               INTO   nCount
               FROM   DOSRUBRIQUE
               WHERE  DOSID          = contrat_rec.DOSID
               AND    DRUORDREMAITRE = nDruOrdreB
               AND    DRUCLASSE      = 'F'
               AND    DRUTYPE        = 'F';
               IF nCount = 0 THEN
                  lOk := 0;
               END IF;
            --- CV-25012010 CFS37948 MSG12341
            ELSIF sControle = 'R1636' THEN
               IF C1R.DRUTXNATURE != 'TF' THEN
                  BEGIN
                     SELECT DOSRUBRIQUETXVAR.DTVDTECH
                     INTO   dtDateEch
                     FROM   DOSRUBRIQUETXVAR
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = C1R.DRUORDRE;
                  EXCEPTION
                     WHEN OTHERS THEN
                        dtDateEch := NULL;
                  END;
                  IF dtDateEch IS NOT NULL THEN
                     BEGIN
                        SELECT DRUORDRE
                        INTO   nDruOrdreF
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID          = contrat_rec.DOSID
                        AND    DRUORDREMAITRE = C1R.DRUORDRE
                        AND    DRUTYPE        = 'F';
                        SELECT DOSRUBRIQUETXVAR.DTVDTECH
                        INTO   dtDateEch2
                        FROM   DOSRUBRIQUETXVAR
                        WHERE  DOSID    = contrat_rec.DOSID
                        AND    DRUORDRE = nDruOrdreF;
                     EXCEPTION
                        WHEN OTHERS THEN
                           dtDateEch2 := NULL;
                     END;
                     IF dtDateEch2 IS NOT NULL AND dtDateEch2 != dtDateEch THEN
                        lOk := 0;
                        nb_element   	    := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := C1R.DRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := TO_CHAR( dtDateEch2,'YYYYMMDD' );
                        aType(nb_element)  := 'D';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := nDruOrdreF;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := TO_CHAR( dtDateEch2,'YYYYMMDD' );
                        aType(nb_element)  := 'D';
                        aDec(nb_element)   := NULL;
                     END IF;
                  END IF;
               END IF;
            END IF;
         -- InCoherence dt tx var et ( dt debut rub ou dt ech ) MSG12296
         ELSIF sControle = 'R1634' THEN
            SELECT COUNT(*)
            INTO   nCount
            FROM   DOSRUBRIQUETXVAR
            WHERE  DOSID    = contrat_rec.DOSID
            AND    DRUORDRE = nDruOrdreB
            AND    DTVDTECH < C1R.DRUDTDEB ;
            IF nCount > 0 THEN
               lOk := 0;
            ELSE
               SELECT COUNT(*)
               INTO   nCount
               FROM   DOSRUBECHEANCIER
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB
               AND    DRETYPE  = 'LOYER'
               AND    EXISTS ( SELECT 1 FROM DOSRUBRIQUETXVAR
                               WHERE DOSID    = DOSRUBECHEANCIER.DOSID
                               AND   DRUORDRE = DOSRUBECHEANCIER.DRUORDRE
                               AND   TO_CHAR(DOSRUBRIQUETXVAR.DTVDTECH, 'DD') != TO_CHAR(DOSRUBECHEANCIER.DREDTDEB, 'DD'));
               IF nCount > 0 THEN
                  lOk := 0;
               END IF;
            END IF;
         ELSIF sControle = 'R1635' THEN
            BEGIN
               SELECT MIN(DRFORDRE)
               INTO   nMinOrdre
               FROM   DOSRUBFLUX
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB;
            EXCEPTION
               WHEN OTHERS THEN
                  nMinOrdre := NULL;
            END;
            IF nMinOrdre IS NOT NULL THEN
               SELECT COUNT(*)
               INTO   nCount
               FROM   DOSRUBFLUX
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB
               AND    DRFORDRE = nMinOrdre
               AND    DRFDTDEB = C1R.DRUDTDEB;
               IF nCount = 0 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := F_StdTrimAll('DOSRUBFLUX');
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
               	nb_element   	    := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
               	aMsg(nb_element)   := nDruOrdreB;
		            aType(nb_element)  := 'N';
               	aDec(nb_element)   := NULL;
               END IF;
            END IF;
            BEGIN
               SELECT MIN(DRENUM)
               INTO   nMinOrdre
               FROM   DOSRUBECHEANCIER
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB;
               EXCEPTION
                  WHEN OTHERS THEN
                     nMinOrdre := NULL;
            END;
            IF nMinOrdre IS NOT NULL THEN
               SELECT COUNT(*)
               INTO   nCount
               FROM   DOSRUBECHEANCIER
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB
               AND    DRENUM   = nMinOrdre
               AND    DREDTDEB = C1R.DRUDTDEB;
               IF nCount = 0 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := F_StdTrimAll('DOSRUBECHEANCIER');
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
               	nb_element   	    := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
               	aMsg(nb_element)   := nDruOrdreB;
		            aType(nb_element)  := 'N';
               	aDec(nb_element)   := NULL;
               END IF;
            END IF;
            IF C1R.DRUCLASSE = 'F' THEN
               SELECT COUNT(*)
               INTO   nCount
               FROM   DOSRUBASSIETTE
               WHERE  DOSID    = contrat_rec.DOSID
               AND    DRUORDRE = nDruOrdreB
               AND    DRADT    = C1R.DRUDTDEB;
               IF nCount = 0 THEN
                  lOk                := 0;
                  nb_element   	    := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := F_StdTrimAll('DOSRUBASSIETTE');
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
                  nb_element   	    := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nDruOrdreB;
		            aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               END IF;
            END IF;
         END IF;
      END LOOP;
      RETURN lOk;
   END;
END ocDossRubPassive;

END PA_DOSRUBCONTROLE;
 