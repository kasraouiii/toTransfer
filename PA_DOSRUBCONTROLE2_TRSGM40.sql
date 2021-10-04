create or replace PACKAGE       "PA_DOSRUBCONTROLE2" AS

   FUNCTION ocDossRubGen(
      contrat_rec IN     DOSSIER%ROWTYPE,
      sControle   IN     VARCHAR2,
   	nb_element  IN OUT BINARY_INTEGER,
		aOrdre      IN OUT pa_fungencontrole.TBL_NUMBER,
		aMsg        IN OUT pa_fungencontrole.TBL_VARCHAR2,
		aType       IN OUT pa_fungencontrole.TBL_VARCHAR2,
		aDec        IN OUT pa_fungencontrole.TBL_NUMBER ) RETURN NUMBER;

   PROCEDURE GetTypeImposition (
      nDosId   IN     DOSSIER.DOSID%TYPE,
      sTacCode IN     DOSSIER.TACCODE%TYPE,
      sTaxType IN OUT TAXE.TAXTYPE%TYPE,
      nTrouve  IN OUT NUMBER) ;


END PA_DOSRUBCONTROLE2;
 /
 create or replace PACKAGE BODY       "PA_DOSRUBCONTROLE2" AS

   -- Declaration des exceptions a trapper
   ERR_CHILD_FOUND      EXCEPTION;
   ERR_PARENT_NOT_FOUND EXCEPTION;

   -- Association des exceptions avec les erreurs ORACLE
   PRAGMA EXCEPTION_INIT( ERR_PARENT_NOT_FOUND, -2291 );
   PRAGMA EXCEPTION_INIT( ERR_CHILD_FOUND     , -2292 );

   -- Declaration des constantes de codes retours a renvoyer
   ERR_CHILD_FOUND_CONST 		CONSTANT INTEGER := 1045;
   ERR_PARENT_NOT_FOUND_CONST CONSTANT INTEGER := 1044;
   ERR_DUP_VAL_ON_INDEX_CONST CONSTANT INTEGER := 1046;
   ERR_NO_ROWS_CONST 		   CONSTANT INTEGER := 1;



	/*
	// Controle rubriques
	*/
   FUNCTION ocDossRubGen(
      contrat_rec IN DOSSIER%ROWTYPE,
      sControle   IN VARCHAR2,
		nb_element IN OUT BINARY_INTEGER,
		aOrdre IN OUT pa_fungencontrole.TBL_NUMBER,
		aMsg IN OUT pa_fungencontrole.TBL_VARCHAR2,
		aType IN OUT pa_fungencontrole.TBL_VARCHAR2,
		aDec IN OUT pa_fungencontrole.TBL_NUMBER ) RETURN NUMBER IS
   BEGIN
      DECLARE
         lOk         NUMBER := 1;
         nOrdre      NUMBER := 0;
         nAdosse     NUMBER;
         nMtRubO     NUMBER;
         nAny        NUMBER;
         err_parm1   	EXCEPTION;
         err_parm2   	EXCEPTION;
         err_parm3   	EXCEPTION;
         err_parm7   	EXCEPTION;
         err_parm8   	EXCEPTION;
         err_parm9   	EXCEPTION;
         err_parma   	EXCEPTION;
         err_parmb   	EXCEPTION;
         err_parmz   	EXCEPTION;
         err_parmy   	EXCEPTION;
         err_parmv   	EXCEPTION;
         err_parm10  	EXCEPTION;
         err_parm11  	EXCEPTION;
         err_parm12  	EXCEPTION;
         err_parmVR  	EXCEPTION;
         err_parmTiers 	EXCEPTION;	-- CV-04082004 FSA-15871 Controle sur Rub/Tiers
         dDate1      DATE;
         dDate2      DATE;
         sCode       VARCHAR2(200);
         sLibCourt   ACTEUR.ACTLIBCOURT%TYPE;
         sFiltre     RUBACCES.RACACCES%TYPE;
         sFiltre1    RUBACCES.RACACCES%TYPE;
         nActId      DOSSIER.ACTID%TYPE;
         nDruOrdre   DOSRUBRIQUE.DRUORDRE%TYPE;
         sDosNum     VARCHAR2(20);
         bErr        BOOLEAN;
         nItrId      ITRRUBRIQUE.ITRID%TYPE;
         nIruOrdre   ITRRUBRIQUE.IRUORDRE%TYPE;
         nBien       NUMBER;
         sDevCode    DOSSIER.DEVCODE%TYPE;
         sActDevCode ACTEUR.DEVCODE%TYPE;
         sPhaCode    DOSPHASE.PHACODE%TYPE;
         nTauxConv   NUMBER;
         nDosEmpr    NUMBER;
         nDruEmpr    NUMBER;
         nOkAdosse   NUMBER;
         nAssEmpr    NUMBER;
         nPooId      NUMBER;
         nBase       NUMBER;
         nQp               NUMBER;
         nTra              IMMOTRANCHE.ITRID%TYPE;
         err_index1        EXCEPTION;
         err_index2        EXCEPTION;
         err_index3        EXCEPTION;
         err_taux          EXCEPTION;
         dtLastIndex       DATE;
         dtFinFlux         DATE;
         --bd170301
         nIacOrdre         ITRACTEUR.IACORDRE%TYPE;
         dtDrbDtFin        DOSRENOUVBAIL.DRBDTFIN%TYPE;
         dtDreDtFin        DOSRUBECHEANCIER.DREDTFIN%TYPE;
         sNbaCode          DOSRENOUVBAIL.NBACODE%TYPE;
         nCont54           NUMBER:=0;
         sDrfPerceptionOld DOSRUBFLUX.DRFPERCEPTION%TYPE;
         sDrfPerception    DOSRUBFLUX.DRFPERCEPTION%TYPE;
         sTaxType          TAXE.TAXTYPE%TYPE;
         nDrfMt            DOSRUBFLUX.DRFMT%TYPE;
         nDrfNbPeriode     DOSRUBFLUX.DRFNBPERIODE%TYPE;
         sDruCp            DOSRUBRIQUE.DRUCP%TYPE;
         sTypeRubrique     DOSRUBRIQUE.DRUTYPE%TYPE;
         NDRUORDREMAITRE   DOSRUBRIQUE.DRUORDREMAITRE%TYPE;
         nBimId            DOSBIM.BIMID%TYPE;
         dtDruDtDeb        DATE;
         sTaxCode          TAXE.TAXCODE%TYPE;          -- CV-24052002 FSA-8759
         nCount      NUMBER;
         nMontant    NUMBER;

         nTegMiniOption    NUMBER;
         nTegTauxVal       TAUVALEUR.TVAVAL%TYPE;
         sTegTauxType      TAUX.TAUTYPE%TYPE;
         nTegUnitePeriode  TAUX.TAUFREQUNITE%TYPE;
         nTegMultiple      TAUX.TAUFREQMULTIPLE%TYPE;
         dtDruDtDebCBall DATE;
         dtDruDtFinCBall DATE;
         dtDacDtDeb        DOSACTEUR.DACDTDEB%TYPE;
         dtDacDtFin        DOSACTEUR.DACDTFIN%TYPE;
			nEpsilon   NUMBER;
         sDruType          DOSRUBRIQUE.DRUTYPE%TYPE;
         sDruClasse        DOSRUBRIQUE.DRUCLASSE%TYPE;

         nTotIgic          NUMBER;
         nTotIva           NUMBER;
         sJalCode          DOSPHASE.JALCODE%TYPE;
         sRolCode          ROLE.ROLCODE%TYPE;

         CURSOR cDosDruPool IS SELECT DISTINCT ACTID
                            FROM   LKDOSRUBPOOACT
                            WHERE  DOSID = contrat_rec.DOSID AND DRUORDRE = nDruOrdre AND DPADTFIN IS NULL;
         --bd170301
         CURSOR cDosLiee IS SELECT DRUORDRE, DRUORDREMAITRE, DRUTYPE
                            FROM   DOSRUBRIQUE
                            WHERE  DOSID = contrat_rec.DOSID
                            AND    DRUORDREMAITRE IS NOT NULL
                            AND    NOT EXISTS (SELECT 1 FROM DOSRUBRIQUE DMAITRE
                                                WHERE DMAITRE.DOSID = contrat_rec.DOSID
                                                AND   DMAITRE.DRUORDRE = DOSRUBRIQUE.DRUORDREMAITRE
                                                AND   SUBSTR(DMAITRE.DRUTYPEMONTAGE,1,2) = 'CA');

			CURSOR cLkDosRubPooAct IS
				SELECT 	LK.DRUORDRE, LK.POOID, LK.ACTID, LK.DPAQP, LK.DPAMARGEFINALE
				FROM		LKDOSRUBPOOACT LK, DOSRUBRIQUE DRU
         	WHERE    DRU.DOSID = contrat_rec.DOSID
         	AND      DRU.DRUORDRE = nDruOrdre
         	AND      F_ISRUBIDONFILTRE(DRU.RUBID, 'SUBV') = 1
         	AND      LK.DOSID = DRU.DOSID
         	AND      LK.DRUORDRE = DRU.DRUORDRE
         	AND		LK.DPADTFIN IS NULL;

			CURSOR cLkDosRubPooActOne IS
				SELECT 	LK.DRUORDRE, LK.POOID, LK.ACTID, LK.DPAQP, LK.DPAMARGEFINALE
				FROM		LKDOSRUBPOOACT LK, DOSRUBRIQUE DRU
         	WHERE    DRU.DOSID = contrat_rec.DOSID
         	AND      DRU.DRUORDRE = nDruOrdre
         	AND      F_ISRUBIDONFILTRE(DRU.RUBID, 'SUBV') = 1
         	AND      LK.DOSID = DRU.DOSID
         	AND      LK.DRUORDRE = DRU.DRUORDRE
         	AND      LK.DPADTFIN IS NULL;

         CURSOR cDosRub2 IS SELECT DRU.DRUORDRE, DRU.RUBID, DRU.DRUCLASSE, DRU.DRUTAUXCALC, DRU.DRUTAUXFIXE, DRU.DEVCODE
                           FROM   DOSRUBRIQUE DRU
                           WHERE  DRU.DOSID = contrat_rec.DOSID;

         CURSOR cEchLiee IS
            SELECT DREDTECH
            FROM   DOSRUBECHEANCIER
            WHERE  DOSID    = contrat_rec.DOSID
              AND  DRUORDRE = nDruOrdre
              AND  DRETYPE  = 'LOYER'
            ORDER BY 1;

         CURSOR cDosRub IS SELECT DRU.DRUDTDEB, DRU.DRUDTFIN, DRU.RUBID, DRU.TAXCODE, DRU.DRUTYPE, DRU.DACORDRE, DRU.DRUTYPEMONTAGE, DRU.DRUFLAGDEPOT,
                                  DRU.DRUCLASSE, DRU.DRUDECOMPTENUM, DRU.DRUDECOMPTEDEN, DRU.DRUMODCALINT,
                                  DRU.DMIORDRE, DRU.DRUDTBASEACTUAL, DRU.DRUDTMAJRUB, DRU.DRUDTMAJECH, DRU.DRUMONTAGE,
                                  DRU.DRUORDRE, DRU.DRUMTORIGINE, DRU.DRUTXTYPE,
                                  DRU.DRUTXNATURE, DRU.DRUINTSIMPLEPERIODE, DRU.DRUINTSIMPLEMULTIPLE, DRU.DRUTAUXFIXE,
                                  DRU.DRUORDREFACTURATION, DRU.DRUMTRESIDUEL, DRU.DRUORDREPREC, RUB.RUBPROVISION,
                                  DRU.DRUTYPEPROVISION, DRU.DRUTAUXCALC, DRU.DRUTAUXNOMINAL, DRU.POOID /*bd170301*/, DRU.DRUCP,
                                  DRU.DRUFLAGRENOUVAUTO, DRU.DRUORDREMAITRE, DRU.DRUBASECALC,
                                  DRU.DRUPERREFTAUX, DRU.DRUMULTREFTAUX, DRU.DRUFLAGCUMUL, RUB.RUBCODE,
                                  DRU.DACORDREREVERSEMENT, DRU.DRUTXREVERSEMENT,DRUORDREGRPFAC,
                                  DRU.DRUMETHODERECOND, DRU.RUBIDRECOND, DRU.DRUPERIODICITERECOND, DRU.DRUMULTIPLERECOND, DRU.DRUNBPERIODERECOND, DRU.DRUMTRECOND
                           FROM   DOSRUBRIQUE DRU,
                                  RUBRIQUE    RUB
                           WHERE  DRU.DOSID = contrat_rec.DOSID
                           AND    DRU.RUBID = RUB.RUBID;

         CURSOR cFinRub IS SELECT DRUORDRE, DRUTAUXCALC
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    DRUCLASSE = 'F'
                           AND    DRUTYPE = 'F'
                           AND    F_ISRUBIDONFILTRE( RUBID, 'SUBV' ) = 0
                           AND    F_ISRUBIDONFILTRE( RUBID, 'AVPR' ) = 0
                           AND    DRUTAUXCALC IS NOT NULL
                           AND    DRUDTDEB <= dtDruDtDeb;

         CURSOR cFinEch IS SELECT DRUCLASSE, DRUORDRE, DRUTYPE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID = contrat_rec.DOSID;
                           --AND    DRUCLASSE = 'F';

         CURSOR cAccLoyEch IS SELECT DRUORDRE
                           FROM   DOSRUBRIQUE
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    F_ISRUBIDONFILTRE( RUBID, 'ACCLOY' ) = 1;

         CURSOR cBien IS SELECT LK.ITRID, LK.IRUORDRE, LK.DIRMT, ITR.IRUMTORIGINE, IMM.DEVCODE,
                                ITR.ACTIDFRS, IMM.ITRNUM, ITR.RUBID, ITR.DEPID, ITR.DLIORDRE, NVL(DRU.DEVCODE, contrat_rec.DEVCODE) DEVCODERUB
                         FROM   LKDOSRUBITRRUB LK, DOSRUBRIQUE DRU, ITRRUBRIQUE ITR, IMMOTRANCHE IMM
                         WHERE  LK.DOSID     = contrat_rec.DOSID
                         AND    LK.DRUORDRE  = nDruOrdre
                         AND    DRU.DOSID    = LK.DOSID
                         AND    DRU.DRUORDRE = LK.DRUORDRE
                         AND    ITR.ITRID    = LK.ITRID
                         AND    ITR.IRUORDRE = LK.IRUORDRE
                         AND    IMM.ITRID    = LK.ITRID;

         CURSOR cBAll IS SELECT DRU.DRUDTDEB, LK.DOSID, LK.DRUORDRE, DRU.DRUORDREPREC, LK.ITRID, LK.IRUORDRE, LK.DIRMT, ITR.IRUMTORIGINE, IMM.DEVCODE,DRU.DRUSENS,
                                DRU.DRUCLASSE, DRU.DRUTYPE, NVL(DRU.DEVCODE, contrat_rec.DEVCODE) DEVCODERUB
                         FROM   LKDOSRUBITRRUB LK, ITRRUBRIQUE ITR, IMMOTRANCHE IMM, DOSRUBRIQUE DRU
                         WHERE  LK.ITRID     = nItrId
                         AND    LK.IRUORDRE  = nIruOrdre
                         AND    ITR.ITRID    = nItrId
                         AND    ITR.IRUORDRE = nIruOrdre
                         AND    IMM.ITRID    = nItrId
                         AND    DRU.DOSID    = LK.DOSID
                         AND    DRU.DRUORDRE = LK.DRUORDRE
                         --mp030602 ne sert seulement au controle 'R0833'
                         AND    DRU.DRUDTDEB < dtDruDtFinCBall
                         AND    F_PlFinRUBRIQUE(DRU.DOSID,DRU.DRUORDRE) > dtDruDtDebCBall;

         CURSOR cEmp IS SELECT  L2.DOSIDEMPR, L2.DRUORDREEMPR, L2.DR2MT, DOSEMP.DEVCODE DEVCODEEMP
                        FROM    L2DOSRUBRIQUE L2, DOSSIER DOSEMP
                        WHERE   L2.DOSIDPRET    = contrat_rec.DOSID
                        AND     L2.DRUORDREPRET = nDruOrdre
                        AND     DOSEMP.DOSID    = L2.DOSIDEMPR;

         CURSOR cEmp1 IS SELECT 1 PP, LK.ITRID ID, IDRMT MT, DEVCODE DEV
                         FROM   LKITRRUBDOSRUB LK, IMMOTRANCHE IMM
                         WHERE  LK.DOSID    = nDosEmpr
                         AND    LK.DRUORDRE = nDruEmpr
                         AND    IMM.ITRID   = LK.ITRID
                         UNION
                         SELECT 0 PP, DOSIDPRET ID, DR2MT MT, DEVCODE DEV
                         FROM   L2DOSRUBRIQUE L2, DOSSIER DOS
                         WHERE  L2.DOSIDEMPR    = nDosEmpr
                         AND    L2.DRUORDREEMPR = nDruEmpr
                         AND    DOS.DOSID       = L2.DOSIDPRET;

         CURSOR cLk1 IS  SELECT DIRMT, ITRID
                         FROM   LKDOSRUBITRRUB
                         WHERE  DOSID    = contrat_rec.DOSID
                         AND    DRUORDRE = nDruOrdre;

         CURSOR cDri IS  SELECT DRIORDRE, DMIORDRE, DRIDTDEBREV, DRIDTFINREV
                         FROM   DOSRUBMODEINDEX
                         WHERE  DOSID    = contrat_rec.DOSID
                         AND    DRUORDRE = nDruOrdre
                         ORDER BY DRIORDRE;
         -- CV-24052002 FSA-8759
         CURSOR cRubRgpe IS SELECT DRUORDRE, TAXCODE
                            FROM   DOSRUBRIQUE
                            WHERE  DRUORDREMAITRE      = nDruOrdre
                            AND    NVL(DRUFLAGCUMUL,0) = 1
                            AND    TAXCODE            != sTaxCode
                            AND    DOSID               = contrat_rec.DOSID ORDER BY 1;
         --HN 07/05/03 FSA 12125
         CURSOR cRubDosAssDetail IS SELECT DRUORDRE, DRADT, DRAMT, DRANBDAYCLIENT, DRANBDAYSUPPLIER
                                    FROM   DOSRUBASSIETTE
                                    WHERE  DOSID = contrat_rec.DOSID;

         CURSOR cRubFinBar IS SELECT DRUORDRE, DRU.BARID, DRUTXNATURE, DRUTAUXCALC, BEDTXFIXEMINI , BEDTXMARGEMINI, BEDPERREF
                     FROM   DOSRUBRIQUE DRU,
                            BAREVODUREE BED
                     WHERE  DRU.DOSID = contrat_rec.DOSID
                     AND    DRU.DRUTYPE = 'F'
                     AND    DRU.DRUCLASSE = 'F'
                     AND    F_ISRUBIDONFILTRE( DRU.RUBID, 'SUBV' ) = 0
                     AND    BED.BARID = DRU.BARID
                     AND    BED.BEVORDRE = DRU.BEVORDRE
                     AND    BED.BEDORDRE = DRU.BEDORDRE;

         CURSOR cDosRubTxVar IS SELECT DRTMARGE
                     FROM DOSRUBTXVAR
                     WHERE DOSID = contrat_rec.DOSID
                     AND   DRUORDRE = nDruOrdre
                     AND   DRTTYPE ='BASE';

         CURSOR cItr IS SELECT IRUORDRE, ITRID
                        FROM   LKDOSRUBITRRUB
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = nDruOrdre ;

         CURSOR cTaxCp IS
            SELECT DRUCP, NVL(SUM(DECODE(SUBSTR(DRU.TAXCODE,1,3),'IVA',1,0)),0) IVA, NVL(SUM(DECODE(SUBSTR(DRU.TAXCODE,1,4),'IGIC',1,0)),0) IGIC
            FROM   DOSRUBRIQUE DRU,
                   TAXE TAX
            WHERE  DRU.DOSID    = contrat_rec.DOSID
              AND  TAX.TAXCODE  = DRU.TAXCODE
              AND  TAX.TAXTYPE != 'EXO'
            GROUP BY DRUCP;

         nAnyIndex         NUMBER;
         nCountIndex       NUMBER;
         nPeriode          BAREVODUREE.BEDPERREF%TYPE;
         nTauxActuarielBar DOSRUBRIQUE.DRUTAUXCALC%TYPE;
         nDrtMarge         DOSRUBTXVAR.DRTMARGE%TYPE;
         nBedTxMargeMini   BAREVODUREE.BEDTXMARGEMINI%TYPE;
         nRetVal           NUMBER;
         nDruTxCalc        DOSRUBRIQUE.DRUTAUXCALC%TYPE;
         nBedTxFixeMini    BAREVODUREE.BEDTXFIXEMINI%TYPE;
         sUtiCodePhase     DOSPHASE.UTICODE%TYPE;   --bd191203
         sReprisePhaCode   DOSPHASE.PHACODE%TYPE;        --bd191203
         nOptionAvisMat    NUMBER;
         nIruFlagAvisMat   ITRRUBRIQUE.IRUFLAGAVISMAT%TYPE;
         sItrNum           IMMOTRANCHE.ITRNUM%TYPE;
         nSubv             NUMBER;
         nAopLogique       NUMBER;
         nAopNombre        NUMBER;
         sAopTexte         AGEOPTION.AOPTEXTE%TYPE;
         dtAopDate         AGEOPTION.AOPDATE%TYPE ;
         nMtFirstLoyer     NUMBER;
         nDfiMtTotal       NUMBER;
         nIva 		         NUMBER;
         nIgic             NUMBER;
         nFlagExploit      NUMBER;
         nFlagChantier     NUMBER;
			nCountDosIdTransfert NUMBER;
         -- CV-30072007 CFS22950+CFS22946+CFS23746
         nOptionVentilation   TOPPARAM.TPALOGIQUE%TYPE;
         sVentilation         TOPPARAM.TPATEXTE%TYPE;
         nDmvTxMarge          DOSMARGEVENTIL.DMVTXMARGE%TYPE;
         nDmvTxMargeOption    DOSMARGEVENTIL.DMVTXMARGE%TYPE;
      BEGIN
         --bd191203
         BEGIN
            SELECT UTICODE, PHACODE
            INTO   sUtiCodePhase, sReprisePhaCode
	      FROM   DOSPHASE
            WHERE  DOSID = contrat_rec.DOSID AND
                   DPHORDRE = ( SELECT MIN(DPHORDRE)
                                FROM   DOSPHASE A
                                WHERE  A.DOSID = DOSPHASE.DOSID AND A.PHACODE = 'ES' );
         EXCEPTION
            WHEN OTHERS THEN
               sUtiCodePhase := 'MOI';
               sReprisePhaCode      := 'MOI';
         END;
         IF sControle = 'R0831' THEN
         	PA_SELECTDOSSIER3.S_ISDOSSIERTRANSFERT(contrat_rec.DOSID,nCountDosIdTransfert);
         END IF;
         PA_COMMON.S_TPALOGIQUE('RUBRIQUE', 'AVISMAT', nOptionAvisMat );
         PA_COMMON.S_OPTIONSOCIETE(	contrat_rec.ACTID, 'TAUXMAXDEFISC', nAopLogique , nAopNombre ,	sAopTexte, dtAopDate);
         IF sControle = 'R0237' OR sControle = 'R0257' THEN
            IF contrat_rec.TACCODE = 'LOCATIF' THEN
               FOR cAccLoyEch_rec IN cAccLoyEch LOOP
                   SELECT COUNT(*)
                   INTO   nAny
                   FROM   DOSRUBECHEANCIER
                   WHERE  DOSID = contrat_rec.DOSID AND DRUORDRE = cAccLoyEch_rec.DRUORDRE AND FACID IS NULL;
                   IF nAny != 0 AND sControle = 'R0237' THEN
                      lOk                := 0;
		                nb_element         := nb_element + 1;
                      nOrdre	           := nb_element;
 	                   aOrdre(nb_element) := nOrdre;
		                aMsg(nb_element)   := cAccLoyEch_rec.DRUORDRE;
		                aType(nb_element)  := 'N';
		                aDec(nb_element)   := NULL;
                  END IF;
                   IF nAny > 1 AND sControle = 'R0257' THEN
                      lOk                := 0;
		                nb_element         := nb_element + 1;
                      nOrdre	           := nb_element;
 	                   aOrdre(nb_element) := nOrdre;
		                aMsg(nb_element)   := cAccLoyEch_rec.DRUORDRE;
		                aType(nb_element)  := 'N';
		                aDec(nb_element)   := NULL;
                  END IF;
               END LOOP;
            ELSE
               FOR cFinEch_rec IN cFinEch LOOP
                 IF ( sControle = 'R0237' AND cFinEch_rec.DRUCLASSE = 'F' AND cFinEch_rec.DRUTYPE != 'B' ) OR ( sControle = 'R0437' AND cFinEch_rec.DRUCLASSE = 'A' ) THEN
                   IF cFinEch_rec.DRUTYPE = 'R' THEN
                      SELECT COUNT(*)
                      INTO   nAny
                      FROM   DOSRUBECHEANCIER
                      WHERE  DOSID    = contrat_rec.DOSID
                        AND  DRUORDRE = cFinEch_rec.DRUORDRE
                        AND  DRETYPE  = 'LOYER'
                        AND  DEPID   IS NULL;
                   ELSE
                      SELECT COUNT(*)
                      INTO   nAny
                      FROM   DOSRUBECHEANCIER
                      WHERE  DOSID    = contrat_rec.DOSID
                        AND  DRUORDRE = cFinEch_rec.DRUORDRE
                        AND  DRETYPE  = 'LOYER'
                        AND  FACID   IS NULL;
                   END IF;
                   IF nAny != 0 THEN
                      lOk                := 0;
		                nb_element         := nb_element + 1;
		                nOrdre	           := nb_element;
 	                   aOrdre(nb_element) := nOrdre;
		                aMsg(nb_element)   := cFinEch_rec.DRUORDRE;
		                aType(nb_element)  := 'N';
		                aDec(nb_element)   := NULL;
                   END IF;
                 END IF;
                 --bde090909
                 IF ( sControle = 'R0257' AND cFinEch_rec.DRUCLASSE = 'F' AND cFinEch_rec.DRUTYPE != 'B' ) OR ( sControle = 'R0457' AND cFinEch_rec.DRUCLASSE = 'A' AND cFinEch_rec.DRUTYPE != 'B' ) THEN
                   IF cFinEch_rec.DRUTYPE = 'R' THEN
                      SELECT COUNT(*)
                      INTO   nAny
                      FROM   DOSRUBECHEANCIER
                      WHERE  DOSID    = contrat_rec.DOSID
                        AND  DRUORDRE = cFinEch_rec.DRUORDRE
                        AND  DRETYPE  = 'LOYER'
                        AND  DEPID   IS NULL;
                   ELSE
                      SELECT COUNT(*)
                      INTO   nAny
                      FROM   DOSRUBECHEANCIER
                      WHERE  DOSID    = contrat_rec.DOSID
                        AND  DRUORDRE = cFinEch_rec.DRUORDRE
                        AND  DRETYPE  = 'LOYER'
                        AND  FACID   IS NULL;
                   END IF;
                   IF nAny > 1 THEN
                      lOk                := 0;
		                nb_element         := nb_element + 1;
		                nOrdre	           := nb_element;
 	                   aOrdre(nb_element) := nOrdre;
		                aMsg(nb_element)   := cFinEch_rec.DRUORDRE;
		                aType(nb_element)  := 'N';
		                aDec(nb_element)   := NULL;
                   END IF;
                 END IF;
               END LOOP;
            END IF;
         ELSIF sControle IN ( 'R0190', 'R0191') THEN
                        lOk := 1;
         		FOR cDosRub_rec IN cDosRub LOOP
                            lOk := 1;
         				IF sControle = 'R0190' AND NVL(cDosRub_rec.DRUFLAGCUMUL,0) = 1
        						      AND cDosRub_rec.DRUORDREMAITRE IS NULL THEN
         					 lOk                := 0;
         				ELSIF sControle = 'R0191' AND cDosRub_rec.DRUORDREGRPFAC IS NOT NULL
  										AND NVL(cDosRub_rec.DRUFLAGCUMUL,0) = 1
 				                                                AND cDosRub_rec.DRUORDREMAITRE IS NOT NULL THEN
         					 lOk                := 0;
         				END IF;
                                        IF lOk = 0 THEN
         				nb_element   	    := nb_element + 1;
                nOrdre             := nb_element;
                aOrdre(nb_element) := nOrdre;
                aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                aType(nb_element)  := 'N';
                aDec(nb_element)   := NULL;
                                      END IF;
         		END LOOP;
         ELSIF sControle = 'R0244' THEN
            FOR cDosRub_rec IN cDosRub LOOP
               SELECT COUNT(*) INTO nCount FROM RUBACCES WHERE RUBID = cDosRub_rec.RUBID AND RACACCES = 'DEPGAR';
               IF nCount > 0 THEN
                  BEGIN
                  SELECT   DRUTAUXFIXE
                  INTO     nCount
                  FROM     DOSRUBRIQUE
                  WHERE    DOSID = contrat_rec.DOSID
                  AND      DRUTYPE = 'F'
                  AND      DRUORDRE != cDosRub_rec.DRUORDRE
                  AND      DRUCLASSE = 'F'
                  AND      RUBID IN (  SELECT   RUBID FROM RUBACCES
                                       WHERE    RUBID = DOSRUBRIQUE.RUBID
                                       AND      RACACCES != 'DEPGAR'
                                       AND      RACACCES = 'REDFIN');
                 EXCEPTION WHEN OTHERS THEN
                  nCount := NULL;
                 END;

                 IF nCount IS NOT NULL    AND cDosRub_rec.DRUTAUXFIXE IS NOT NULL
                                          AND nCount != cDosRub_rec.DRUTAUXFIXE THEN
                    lOk := 0;
                    EXIT;
                 END if;
               END if;
            END LOOP;
         ELSIF sControle = 'R0245' THEN
            FOR cDosRub_rec IN cDosRub LOOP
               IF cDosRub_rec.DRUTXREVERSEMENT IS NOT NULL THEN
                    IF cDosRub_rec.DACORDREREVERSEMENT IS NULL THEN
                       lOk                := 0;
                       nb_element   	    := nb_element + 1;
                       nOrdre             := nb_element;
                       aOrdre(nb_element) := nOrdre;
                       aMsg(nb_element)   := nDruOrdre;
                       aType(nb_element)  := 'N';
                       aDec(nb_element)   := NULL;
                    END if;
                END if;
            END LOOP;
         ELSIF sControle = 'R0178' THEN
            SELECT COUNT(*)
            INTO   nAny
            FROM   LKDOSRUBPOOACT
            WHERE  DOSID = contrat_rec.DOSID
            AND    DPADTFIN IS NULL;
            IF nAny >= 1 THEN
               FOR cDosRub_rec IN cDosRub LOOP
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   LKDOSRUBPOOACT
                  WHERE  DOSID = contrat_rec.DOSID AND
                         DRUORDRE = cDosRub_rec.DRUORDRE
                         AND DPADTFIN IS NULL;
                  IF nAny = 0 THEN
                     lOk := 0;
                     EXIT;
                  END IF;
               END LOOP;
            END IF;
		ELSIF sControle = 'R0196' THEN
			FOR cDosRub_rec IN cDosRub LOOP
				IF F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'SEPAREI') = 1 AND cDosRub_rec.DRUORDREGRPFAC IS NULL THEN
					    lOk          	   := 0;
	                    nb_element   	   := nb_element + 1;
		                aOrdre(nb_element)  := nb_element;
		                aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
        	            aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
				END IF;
			END LOOP;
        ELSIF sControle = 'R0155' AND contrat_rec.TACCODE IN ('CBI', 'CBMIXTE', 'LOCATIF') THEN
            GetTypeImposition (contrat_rec.DOSID,contrat_rec.TACCODE,
                              sTaxType,nAny);
            IF nAny > 1 THEN
               lOk:=0;
            END IF;
         ELSIF sControle = 'R0179' THEN
            SELECT COUNT(*)
            INTO   nAny
            FROM   DOSRUBRIQUE
            WHERE  DOSID = contrat_rec.DOSID
            AND    RUBID IN ( SELECT RUBID
                              FROM   RUBACCES
                              WHERE  RACACCES = 'DEPGAR' );
            IF nAny > 1 THEN
               lOk := 0;
            END IF;
         ELSIF sControle = 'R0180' THEN
            SELECT COUNT(*)
            INTO   nAny
            FROM   DOSACTEUR
            WHERE  DOSID = contrat_rec.DOSID
                   AND ( DACMTDG IS NOT NULL
                         OR DACNBMOISDG IS NOT NULL
                         OR DACQUOTEPARTDG IS NOT NULL );
            IF nAny != 0 THEN
               SELECT COUNT(*)
               INTO   nAny
               FROM   DOSRUBRIQUE
               WHERE  DOSID = contrat_rec.DOSID
                      AND RUBID IN ( SELECT RUBID
                                     FROM   RUBACCES
                                     WHERE  RACACCES = 'DEPGAR' );
               IF nAny = 0 THEN
                  lOk := 0;
               END IF;
            END IF;
         ELSIF sControle = 'R0772' THEN
            SELECT COUNT(*)
            INTO   nAny
            FROM   DOSMODEINDEX
            WHERE  DOSID = contrat_rec.DOSID
            AND    DMIORDRE NOT IN (SELECT DMIORDRE FROM DOSRUBMODEINDEX
                                    WHERE DOSID = contrat_rec.DOSID
                                    UNION
                                    SELECT DMIORDRE FROM DOSRUBRIQUE
                                     WHERE DOSID = contrat_rec.DOSID) ;
            IF nAny > 0 THEN
                  lOk:=0;
            END IF;
         ELSIF sControle = 'R0177' THEN
            FOR cDosLiee_rec IN cDosLiee LOOP
               nDruOrdre := cDosLiee_rec.DRUORDRE;
               IF cDosLiee_rec.DRUTYPE != 'B' THEN
               FOR cEchLiee_rec IN cEchLiee LOOP
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBECHEANCIER
                  WHERE  DOSID           = contrat_rec.DOSID
                    AND  DRUORDRE        = cDosLiee_rec.DRUORDREMAITRE
                    AND  DRETYPE         = 'LOYER'
                    AND  TRUNC(DREDTECH) = TRUNC(cEchLiee_rec.DREDTECH);
                  IF ( nAny = 0 ) THEN
                     lOk                := 0;
                     nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := TO_CHAR(cEchLiee_rec.DREDTECH,'YYYYMMDD');
                     aType(nb_element)  := 'D';
                     aDec(nb_element)   := NULL;
                     nb_element         := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := nDruOrdre;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                     nb_element         := nb_element + 1;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := cDosLiee_rec.DRUORDREMAITRE;
                     aType(nb_element)  := 'N';
                     aDec(nb_element)   := NULL;
                     EXIT;
                  END IF;
               END LOOP;
               END IF;
            END LOOP;
         -- LG 18/03/05 FSA18748 Controle ARVAL ESP
         ELSIF sControle = 'R0192' THEN
            FOR cTaxCp_rec IN cTaxCp LOOP
               IF ( cTaxCp_rec.IVA > 0 ) AND ( cTaxCp_rec.IGIC > 0 ) THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cTaxCp_rec.DRUCP;
                  aType(nb_element)  := 'C';
                  aDec(nb_element)   := NULL;
               END IF;
            END LOOP;
         ELSE
            FOR cDosRub_rec IN cDosRub LOOP
               BEGIN
               IF cDosRub_rec.DRUTYPE = 'M' AND SUBSTR(sControle,1,3) = 'R08' THEN
                  NULL;
               ELSIF sControle = 'R0187' AND cDosRub_rec.DRUORDREPREC IS NOT NULL
                    AND cDosRub_rec.DRUMODCALINT = 'IS' THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID = contrat_rec.DOSID
                  AND    DRUMODCALINT = 'IS'
                  AND    DRUORDRE = cDosRub_rec.DRUORDREPREC
                  AND    ( DRUINTSIMPLEPERIODE != cDosRub_rec.DRUINTSIMPLEPERIODE
                        OR DRUINTSIMPLEMULTIPLE != cDosRub_rec.DRUINTSIMPLEMULTIPLE);
                  If nAny > 0 THEN
                        lOk                := 0;
                        nb_element   	    := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cDosRub_rec.DRUORDREPREC;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                  END IF;
               -- CV-24052002 FSA-8759 ( 6186 ) Code taxe # de celui de la rubrique maitre
               ELSIF sControle = 'R0163' AND cDosRub_rec.DRUORDREMAITRE IS NULL THEN
                  nDruOrdre := cDosRub_rec.DRUORDRE;
                  sTaxCode  := cDosRub_rec.TAXCODE;
                  FOR cRubRgpe_rec IN cRubRgpe LOOP
                      lOk                := 0;
		                nb_element         := nb_element + 1;
      		          nOrdre             := nb_element;
   		             aOrdre(nb_element) := nOrdre;
                      aMsg(nb_element)   := 'LANTAXE|TAXCODE|' || cRubRgpe_rec.TAXCODE;
                      aType(nb_element)  := 'S';
	   	             aDec(nb_element)   := NULL;
		                nb_element         := nb_element + 1;
      	   	       aOrdre(nb_element) := nOrdre;
   		             aMsg(nb_element)   := cRubRgpe_rec.DRUORDRE;
	   	             aType(nb_element)  := 'N';
         		       aDec(nb_element)   := NULL;
   		             nb_element         := nb_element + 1;
	   	             aOrdre(nb_element) := nOrdre;
         		       aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
   		             aType(nb_element)  := 'N';
	   	             aDec(nb_element)   := NULL;
   		             nb_element         := nb_element + 1;
	   	             aOrdre(nb_element) := nOrdre;
                      aMsg(nb_element)   := 'LANTAXE|TAXCODE|' || sTaxCode;
                      aType(nb_element)  := 'S';
	   	             aDec(nb_element)   := NULL;
                  END LOOP;
                ELSE
                  nDruOrdre := cDosRub_rec.DRUORDRE;
               IF sControle = 'R0101' AND NOT PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                      AND cDosRub_rec.DRUDTDEB  IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0102' AND cDosRub_rec.RUBID IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0147' AND cDosRub_rec.TAXCODE = 'MIXTE' AND cDosRub_rec.DRUTYPEMONTAGE = 'CVT' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0185' AND cDosRub_rec.DRUCLASSE = 'F' AND cDosRub_rec.DRUTYPE = 'F' AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'SUBV' ) = 1 THEN
                  FOR cBienR IN cBien LOOP
                     IF F_ISRUBIDONFILTRE( cBienR.RUBID, 'SUBV' ) != 1 THEN
                        lOk          	   := 0;
	                    nb_element   	   := nb_element + 1;
		                aOrdre(nb_element)  := nb_element;
		                aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
        	            aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        EXIT;
                     END IF;
                  END LOOP;
               ELSIF sControle = 'R0186' AND contrat_rec.TACCODE = 'CBMIXTE' AND cDosRub_rec.DRUCLASSE = 'F' AND cDosRub_rec.DRUTYPE = 'F' AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'MEUBLE' ) = 1 THEN
                  FOR cBienR IN cBien LOOP
                     IF F_ISRUBIDONFILTRE( cBienR.RUBID, 'MEUBLE' ) != 1 THEN
                        lOk          	   := 0;
	                    nb_element   	   := nb_element + 1;
		                --nOrdre	           := nb_element;
		                aOrdre(nb_element)  := nb_element;
		                aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
        	            aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        EXIT;
                     END IF;
                  END LOOP;
               ELSIF sControle = 'R0188' AND contrat_rec.TACCODE = 'LOCATIF' AND
                     cDosRub_rec.DRUTYPEMONTAGE IN ( 'CASEUL', 'CAPLUS', 'CAMIXT' ) AND
                     cDosRub_rec.DRUORDREMAITRE IS NULL THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID      =  contrat_rec.DOSID
                  AND    DRUORDRE   != cDosRub_rec.DRUORDRE
                  AND    DRUORDREMAITRE IS NULL
                  AND    DRUDTDEB BETWEEN cDosRub_rec.DRUDTDEB AND F_PlFinRUBRIQUE(DOSID,cDosRub_rec.DRUORDRE);
                  IF nAny >  0  THEN
                     lOk          	    := 0;
                     nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
                     aOrdre(nb_element) := nOrdre;
                     aMsg(nb_element)   := TO_CHAR(cDosRub_rec.DRUDTDEB,'YYYYMMDD');
                     aType(nb_element)  := 'D';
                     aDec(nb_element)   := NULL;
                  END IF;
               ELSIF sControle IN ('R0152', 'R0153') AND contrat_rec.TACCODE = 'LOCATIF' THEN
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
                        sNbaCode := NULL;
                  END;
                  -- TD le 27/07/01 --
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

                  SELECT   COUNT(*)
                  INTO     nAny
                  FROM     DOSRUBECHEANCIER
                  WHERE    DOSID = contrat_rec.DOSID
                  AND      FACID IS NOT NULL
                  AND      DREDTFIN > dtDrbDtFin;
                  IF cDosRub_rec.DRUDTDEB > dtDrbDtFin AND nAny < 1 THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   NATUREBAIL
                     WHERE  NBACODE = sNbaCode
                     AND    NBADECRET = '1953'
                     AND    NBAFLAGPRECAIRE = 1;
                     IF (sControle = 'R0152' AND nAny > 0 ) OR (sControle = 'R0153' AND nAny =0 ) THEN
                        RAISE err_parm1;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0154' AND contrat_rec.TACCODE = 'LOCATIF'
                     AND nCont54 = 0 AND NVL(cDosRub_rec.DRUFLAGDEPOT,0) = 1 THEN
                  SELECT COUNT(*)
                  INTO   nCount
                  FROM   DOSRUBRIQUE
                  WHERE  DRUORDREMAITRE IS NOT NULL
                  AND    DOSID = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRub_rec.DRUORDRE
                  AND    DRUTYPEMONTAGE IN ( 'REGUL');
                  IF nCount = 0 THEN
                     IF sDrfPerceptionOld IS NULL THEN
                        BEGIN
                           SELECT DRFPERCEPTION
                           INTO   sDrfPerceptionOld
                           FROM   DOSRUBFLUX
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    DRUORDRE =  cDosRub_rec.DRUORDRE
                           AND    DRFORDRE = (SELECT MAX(DRFORDRE) FROM DOSRUBFLUX
                                              WHERE DOSID = contrat_rec.DOSID
                                              AND   DRUORDRE = cDosRub_rec.DRUORDRE);
                        EXCEPTION
                           WHEN OTHERS THEN
                              sDrfPerceptionOld := NULL;
                        END;
                     ELSE
                        BEGIN
                           SELECT DRFPERCEPTION
                           INTO   sDrfPerception
                           FROM   DOSRUBFLUX
                           WHERE  DOSID = contrat_rec.DOSID
                           AND    DRUORDRE =  cDosRub_rec.DRUORDRE
                           AND    DRFORDRE = (SELECT MAX(DRFORDRE) FROM DOSRUBFLUX
                                              WHERE DOSID = contrat_rec.DOSID
                                              AND   DRUORDRE = cDosRub_rec.DRUORDRE);
                        EXCEPTION
                           WHEN NO_DATA_FOUND THEN
                              sDrfPerception := sDrfPerceptionOld;
                           WHEN OTHERS THEN
                              sDrfPerception := NULL;
                        END;
                        IF sDrfPerception <> sDrfPerceptionOld THEN
                           nCont54 := 1;
                           lOk := 0;
                        END IF;
                     END IF;
                  END IF;
               --bd170301
               ELSIF sControle = 'R0151' AND cDosRub_rec.TAXCODE = 'MIXTE' THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBTVA
                  WHERE  DOSID = contrat_rec.DOSID AND DRUORDRE = nDruOrdre AND DRTDTFIN IS NULL;
                  IF nAny = 0 THEN
                     RAISE err_parm1;
                  END IF;
               --bd170301
               ELSIF sControle = 'R0150' AND contrat_rec.DOSPOOL = 'CDF' AND cDosRub_rec.POOID IS NOT NULL AND cDosRub_rec.DRUTYPE IN ( 'R', 'F', 'P' ) THEN
                  nAny := 1;
                  FOR cDosDruPool_rec IN cDosDruPool LOOP
                     IF cDosDruPool_rec.ACTID != contrat_rec.ACTID THEN
                        BEGIN
                           SELECT MAX(ITRID)
                           INTO   nItrId
                           FROM   POOL
                           WHERE  POOID = cDosRub_rec.POOID;
                           SELECT IACORDRE
                           INTO   nIacOrdre
                           FROM   ITRACTEUR
                           WHERE  ITRID = nItrId AND ACTID = cDosDruPool_rec.ACTID;
                           IF cDosRub_rec.DRUTYPE = 'R' THEN
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   ITRACTPAIEMENT
                              WHERE  ITRID = nItrId AND IACORDRE = nIacOrdre AND IAPTYPE = 'E' AND IAPDTDEB <= cDosRub_rec.DRUDTDEB;
                           ELSE
                              SELECT COUNT(*)
                              INTO   nAny
                              FROM   ITRACTPAIEMENT
                              WHERE  ITRID = nItrId AND IACORDRE = nIacOrdre AND IAPTYPE = 'D' AND IAPDTDEB <= cDosRub_rec.DRUDTDEB;
                           END IF;
                        EXCEPTION
                           WHEN OTHERS THEN
                              NULL;
                        END;
                        IF nAny = 0 THEN
                           SELECT ACTLIBCOURT
                           INTO   sCode
                           FROM   ACTEUR
                           WHERE  ACTID = cDosDruPool_rec.ACTID;
                           EXIT;
                        END IF;
                     END IF;
                  END LOOP;
                  IF nAny = 0 THEN
                     RAISE err_parm3;
                  END IF;
               --finbd170301

               ELSIF sControle = 'R0103' AND cDosRub_rec.TAXCODE IS NULL
                                         AND contrat_rec.TACCODE NOT IN (  'EMPRUNT' )
                                         AND cDosRub_rec.DRUTYPE != 'B' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0104' AND cDosRub_rec.DACORDRE IS NULL
                                         AND cDosRub_rec.DRUTYPE  != 'B' THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0105' AND NOT PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                         AND cDosRub_rec.DRUTYPEMONTAGE IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0106' AND cDosRub_rec.DRUCLASSE  != 'A'
                                         AND NOT PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                         AND cDosRub_rec.DRUMONTAGE IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0107' AND cDosRub_rec.DRUDECOMPTENUM IS NULL THEN
                  IF F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'AVPL') = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0108' AND cDosRub_rec.DRUDECOMPTEDEN IS NULL THEN
                  IF F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'AVPL') = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0109' AND cDosRub_rec.DRUTYPEPROVISION IS NULL THEN
                  IF cDosRub_rec.RUBPROVISION IS NOT NULL THEN
                     RAISE err_parm1;
                  END if;
               ELSIF sControle = 'R0110' AND cDosRub_rec.DRUCP IS NULL THEN
                     RAISE err_parm1;
               ELSIF sControle= 'R0112' AND nOptionAvisMat = 1 AND ( contrat_rec.TACCODE = 'CBM' OR contrat_rec.TACCODE = 'LOCFIN' ) THEN
                  nDruOrdre := cDosRub_rec.DRUORDRE ;
                  FOR cItr_rec IN cItr LOOP
                     BEGIN
                        SELECT NVL(IRUFLAGAVISMAT,0), ITRNUM, F_ISRUBIDONFILTRE( RUBID, 'SUBV' )
                        INTO   nIruFlagAvisMat , sItrNum, nSubv
                        FROM   ITRRUBRIQUE IRU , IMMOTRANCHE ITR
                        WHERE  IRU.ITRID = cItr_rec.ITRID
                        AND    IRU.IRUORDRE = cItr_rec.IRUORDRE
                        AND    ITR.ITRID   = IRU.ITRID  ;
                     END;
                     if nIruFlagAvisMat = 0 AND nSubv = 0 THEN
                        lOk                := 0;
                        nb_element   	    := nb_element + 1;
                        nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := cItr_rec.IRUORDRE;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := F_StdTrimAll( sItrNum );
                        aType(nb_element)  := 'C';
                        aDec(nb_element)   := NULL;
                        nb_element   	    := nb_element + 1;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := nDruOrdre;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                     END if;

                  END LOOP;
               ELSIF sControle = 'R0157' AND cDosRub_rec.DRUCP IS NOT NULL THEN
                  -- recherche si existence d'un jalon NOUVEAU CONTRAT
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   CREVT
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    TMFFONCTION = 'EVD_NOUCP'
                     AND    EXISTS (SELECT 1 FROM LKDRUCRE
                                 WHERE CREID = CREVT.CREID
                                 AND   DOSID = contrat_rec.DOSID
                                 AND   DRUORDRE = cDosRub_rec.DRUORDRE);
                    -- recuperation DRUCP sur rubrique precedente
                    IF nAny > 0 AND cDosRub_rec.DRUORDREPREC  IS NOT NULL THEN
                        SELECT DRUCP
                        INTO   sDruCp
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRub_rec.DRUORDREPREC;
                        IF sDruCp IS NOT NULL AND sDruCp = cDosRub_rec.DRUCP THEN
                              RAISE err_parm1;
                        END IF;
                    END IF;
               ELSIF sControle = 'R0201' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE = 'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'V', 'R', 'M' ) THEN
                  IF cDosRub_rec.DRUMODCALINT IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0181' AND contrat_rec.TACCODE IN ('LOCATIF','CBI','CBMIXTE') THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   TAXE
                  WHERE  TAXCODE = cDosRub_rec.TAXCODE
                  AND    TAXTYPE = 'TVA';
                  IF nAny > 0 OR cDosRub_rec.TAXCODE= 'MIXTE' THEN
                     dDate1:= cDosRub_rec.DRUDTDEB;
                     IF contrat_rec.TACCODE = 'LOCATIF' THEN
                        SELECT COUNT(*)
                        INTO   nAny
                        FROM   BIMOPTIONTVA
                        WHERE  BIMID = (SELECT BIMID FROM DOSSIERLOCATIF
                                         WHERE DOSID = contrat_rec.DOSID)
                        AND    TO_DATE('01'||LPAD(TO_CHAR(BOTMOISDEBUT),2,'00')||LPAD(TO_CHAR(BOTANNEEDEBUT),4,'0000'), 'DDMMYYYY') <= dDate1
                        AND    BOTMOISDEBUT  IS NOT NULL AND BOTANNEEDEBUT IS NOT NULL
                        AND   (( BOTMOISFIN  IS NOT NULL AND BOTANNEEFIN IS NOT NULL
                                 AND TO_DATE('01'||LPAD(TO_CHAR(BOTMOISFIN),2,'00')||LPAD(TO_CHAR(BOTANNEEFIN),4,'0000'), 'DDMMYYYY') >= dDate1)
                               OR ( BOTMOISFIN  IS  NULL AND BOTANNEEFIN IS NULL));
                        IF nAny < 1 THEN
                           RAISE err_parmb;
                        END IF;
                     ELSE
                        -- PRESENCE OU NON D'UN BIEN FINANCE
                        SELECT COUNT(ITRID)
                        INTO   nAny
                        FROM   LKDOSRUBITRRUB
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                        IF nAny != 0 THEN
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   BIMOPTIONTVA
                           WHERE  BIMID IN (SELECT BIMID FROM LKITRBIM
                                           WHERE ITRID IN (SELECT MAX(ITRID) FROM LKDOSRUBITRRUB
                                                            WHERE DOSID = contrat_rec.DOSID
                                                            AND DRUORDRE =cDosRub_rec.DRUORDRE ))
                           AND    TO_DATE('01'||LPAD(TO_CHAR(BOTMOISDEBUT),2,'00')||LPAD(TO_CHAR(BOTANNEEDEBUT),4,'0000'), 'DDMMYYYY') <= dDate1
                           AND    BOTMOISDEBUT  IS NOT NULL AND BOTANNEEDEBUT IS NOT NULL
                           AND   (( BOTMOISFIN  IS NOT NULL AND BOTANNEEFIN IS NOT NULL
                                 AND TO_DATE('01'||LPAD(TO_CHAR(BOTMOISFIN),2,'00')||LPAD(TO_CHAR(BOTANNEEFIN),4,'0000'), 'DDMMYYYY') >= dDate1)
                               OR ( BOTMOISFIN  IS  NULL AND BOTANNEEFIN IS NULL));
                           IF nAny < 1 THEN
                              RAISE err_parmb;
                           END IF;
                        END IF;
                     END IF;
                  END IF;
               --LG 25/03/2003 FSA 11595
               ELSIF sControle = 'R0182' AND cDosRub_rec.DRUORDREPREC IS NOT NULL THEN
                  BEGIN
                     SELECT DRUDTFIN
                     INTO   dDate1
                     FROM   DOSRUBRIQUE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDREPREC;
                  EXCEPTION
                     WHEN OTHERS THEN
                        dDate1 := NULL;
                  END;
                  IF dDate1 IS NULL THEN
                     BEGIN
                        SELECT MAX(DRFDTFIN)
                        INTO   dDate2
                        FROM   DOSRUBFLUX
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRub_rec.DRUORDREPREC;
                     EXCEPTION
                        WHEN OTHERS THEN
                           dDate2 := NULL;
                     END;
                  ELSE
                     dDate2 := NULL;
                  END IF;
                  IF ( cDosRub_rec.DRUDTDEB <= dDate1 ) OR ( cDosRub_rec.DRUDTDEB <= dDate2 ) THEN
                     RAISE err_parm1;
                  END IF;
               -- LG FSA 12034 28/04/03
               ELSIF sControle = 'R0165' THEN
                  IF cDosRub_rec.RUBCODE = 'LCOL' THEN
                     BEGIN
                        SELECT ACTID
                        INTO   nActId
                        FROM   DOSACTEUR
                        WHERE  DOSID    = contrat_rec.DOSID
                        AND    DACORDRE = cDosRub_rec.DACORDRE;
                     EXCEPTION
                        WHEN OTHERS THEN
                           nActId := NULL;
                     END;
                     IF nActId IS NOT NULL THEN
                        SELECT COUNT(*)
                        INTO   nAny
                        FROM   ACTROLE
                        WHERE  ACTID = nActId
                        AND    ROLCODE = 'CONDUCT';
                        IF nAny <= 0 THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  END IF;
               -- LG FSA 19002 NL 22/03/05
               ELSIF sControle = 'R0193' THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   RUBACCES
                  WHERE  RUBID = cDosRub_rec.RUBID
                  AND    RACACCES = 'INDEMUT';

                  IF nAny > 0 THEN
                     IF cDosRub_rec.DRUORDREPREC IS NOT NULL THEN
                        SELECT TAXCODE
                        INTO   sTaxCode
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRub_rec.DRUORDREPREC;
                     END IF;
                     IF sTaxCode != cDosRub_rec.TAXCODE THEN
                        RAISE err_parm12;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0194' AND	cDosRub_rec.DRUORDREMAITRE IS NOT NULL AND cDosRub_rec.DRUTYPE ='B' THEN
               	SELECT COUNT(*) INTO nAny
               	FROM	 DOSRUBRIQUE
               	WHERE  DOSID    = contrat_rec.DOSID
               	AND	 DRUORDRE = cDosRub_rec.DRUORDREMAITRE
               	AND	 DRUTYPE  = 'F';
               	IF nAny = 0 THEN
               		RAISE err_parm1;
               	END IF;
               -- CV-30072007 CFS22950+CFS22946+CFS23746 : MSG10159
               ELSIF sControle = 'R0195' AND (( cDosRub_rec.DRUTYPE = 'F' AND F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'INTFREE') = 0 ) OR ( cDosRub_rec.DRUTYPE = 'P' AND cDosRub_rec.DRUTXNATURE != 'TF' )) THEN
                  PA_COMMON.S_TPALOGIQUE( 'DOSRUBRIQUE', 'VENTILATION', nOptionVentilation );
                  IF nOptionVentilation = 1 THEN
                     PA_COMMON.S_TPATEXTE( 'DOSRUBRIQUE', 'VENTILATION', sVentilation );
                     -- On recherche ACTEUR
                     BEGIN
                        SELECT DAC.ACTID
                        INTO   nActId
                        FROM   DOSACTEUR DAC
                        WHERE  DAC.DOSID    = contrat_rec.DOSID
                        AND    DAC.DACORDRE = cDosRub_rec.DACORDRE;
                     EXCEPTION
                        WHEN OTHERS THEN
                        nActId := NULL;
                     END;
                     IF sVentilation IS NOT NULL THEN
                        sVentilation := ';' || sVentilation || ';';
                        IF nActId IS NOT NULL THEN
                           -- On cherche s'il existe la ventilation
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   DOSMARGEVENTIL
                           WHERE  DOSID              = contrat_rec.DOSID
                           AND    ACTID              = nActId
                           AND    DRUORDRE = cDosRub_rec.DRUORDRE
                           AND    NVL(DMVTXMARGE,0) != 0;
                           -- On recherche le Spred
                           BEGIN
                              SELECT NVL(DRTMARGE,0)
                              INTO   nDrtMarge
                              FROM   DOSRUBTXVAR
                              WHERE  DOSID    = contrat_rec.DOSID
                              AND    DRUORDRE = cDosRub_rec.DRUORDRE
                              AND    INSTR(';' || DRTTYPE || ';', sVentilation ) > 0;
                           EXCEPTION
                              WHEN OTHERS THEN
                                 nDrtMarge := 0;
                           END;
                           IF cDosRub_rec.DRUTYPE = 'F' THEN
                              -- Mt total de la ventilation
                              SELECT NVL(SUM (DMVTXMARGE),0)
                              INTO   nDmvTxMarge
                              FROM   DOSMARGEVENTIL
                              WHERE  DOSID     = contrat_rec.DOSID
                              AND    ACTID     = nActId
                              AND    DRUORDRE = cDosRub_rec.DRUORDRE ;
                              -- Mt total de la ventilation des tx parametres dans l'option
                              SELECT NVL(SUM (DMVTXMARGE),0)
                              INTO   nDmvTxMargeOption
                              FROM   DOSMARGEVENTIL
                              WHERE  DOSID     = contrat_rec.DOSID
                              AND    ACTID     = nActId
                              AND    DRUORDRE = cDosRub_rec.DRUORDRE
                              AND    INSTR(';' || TMVCODE || ';', sVentilation ) > 0 ;
                           ELSE
                              -- CV-01082007 CFS22950 le Spred est maintenant valorisee avec DRUTAUXFIXE
                              -- Mt total de la ventilation sans tx parametre
                              SELECT NVL(SUM (DMVTXMARGE),0)
                              INTO   nDmvTxMarge
                              FROM   DOSMARGEVENTIL
                              WHERE  DOSID     = contrat_rec.DOSID
                              AND    ACTID     = nActId
                              AND    DRUORDRE = cDosRub_rec.DRUORDRE
                              AND    INSTR(';' || TMVCODE || ';', sVentilation ) = 0 ;
                              nDmvTxMargeOption := 0;
                           END IF;
                           -- tx fixe pour 'F' et tx variable pour 'F' et 'P'
                           IF ( nAny > 0 ) AND (( NVL(cDosRub_rec.DRUTAUXFIXE,0) != nDmvTxMarge )
                                           OR  ( cDosRub_rec.DRUTXNATURE != 'TF' AND (NVL(nDmvTxMarge - nDmvTxMargeOption,0)) != nDrtMarge )) THEN
                              lOk                := 0;
                              nb_element   	    := nb_element + 1;
                              nOrdre             := nb_element;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                              aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
                           END IF;
                        END IF;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0235' AND cDosRub_rec.DRUTYPE = 'F' AND cDosRub_rec.DRUCLASSE = 'F' AND
                  cDosRub_rec.DRUTAUXFIXE IS NOT NULL AND cDosRub_rec.DRUTAUXNOMINAL IS NOT NULL THEN
                  PA_SELECTDOSSIER2.S_TEGMINIOPTION( contrat_rec.ACTID, nAny );
                  IF nAny = 1 THEN
                     BEGIN
                        SELECT AOPNOMBRE
                        INTO   nAny
                        FROM   AGEOPTION
                        WHERE  ACTID = contrat_rec.ACTID AND TOSCODE = 'DELTATEG';
                     EXCEPTION
                        WHEN OTHERS THEN
                           nAny := 0;
                     END;
                     IF abs( cDosRub_rec.DRUTAUXFIXE - cDosRub_rec.DRUTAUXNOMINAL ) > nAny THEN
                        RAISE err_taux;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0731' AND PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) THEN
                  IF cDosRub_rec.DMIORDRE IS NOT NULL THEN
                     RAISE err_parm1;
                  ELSE
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   DOSRUBMODEINDEX
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                     IF ( nAny > 0 ) THEN
                        RAISE err_parm1;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0701' AND cDosRub_rec.DMIORDRE        IS NOT NULL
                                         AND cDosRub_rec.DRUDTBASEACTUAL IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0770' AND cDosRub_rec.DMIORDRE IS NOT NULL THEN
                       SELECT COUNT(*)
                       INTO   nAny
                       FROM   DOSMODEINDEX
                       WHERE  DOSID    = contrat_rec.DOSID
                       AND    DMIORDRE = cDosRub_rec.DMIORDRE
                       AND    DMITYPE  = 'A';
                       IF nAny = 0 THEN
                          sCode := cDosRub_rec.DMIORDRE;
                          RAISE err_parm3;
                       END IF;
               -- TD 20/03/02
               ELSIF sControle = 'R0160' AND contrat_rec.TACCODE = 'PRET'  THEN
                     IF cDosRub_rec.DRUBASECALC IN ('DECAISTTC', 'DECAISHT', 'DECAISTVA') THEN
                        BEGIN
                           SELECT BIMID
                           INTO   nBimId
                           FROM   DOSBIM
                           WHERE  DOSID = contrat_rec.DOSID;
                        EXCEPTION WHEN OTHERS THEN
                           nBimId := NULL;
                        END;
                        IF nBimId IS NULL THEN
                           RAISE err_parm1;
                        END if;
                     END IF;
               ELSIF ( sControle IN ( 'R0702', 'R0703', 'R0736', 'R0737', 'R0738', 'R0739', 'R0771' ) ) THEN
                  dtLastIndex  := NULL;
                  nCountIndex  := 0;

                  SELECT COUNT(*)
                  INTO   nAnyIndex
                  FROM   DOSRUBMODEINDEX
                  WHERE  DOSID    = contrat_rec.DOSID AND
                         DRUORDRE = nDruOrdre;

                  FOR cDri_rec IN cDri LOOP
                     BEGIN
                        nCountIndex := nCountIndex + 1;
                        IF sControle = 'R0702' AND cDri_rec.DMIORDRE    IS NOT NULL
                                               AND cDri_rec.DRIDTDEBREV IS NULL THEN
                           RAISE err_index1;
                        ELSIF sControle = 'R0703' AND cDri_rec.DMIORDRE    IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV IS NULL
                                                  AND nCountIndex < nAnyIndex THEN
                           RAISE err_index1;
                        ELSIF sControle = 'R0736' AND cDri_rec.DMIORDRE    IS NOT NULL
                                                  AND cDri_rec.DRIDTDEBREV IS NOT NULL
                                                  AND cDosRub_rec.DRUDTDEB IS NOT NULL
                                                  AND cDosRub_rec.DRUORDREPREC IS NULL
                                                  AND cDri_rec.DRIDTDEBREV <  cDosRub_rec.DRUDTDEB THEN
                           dDate1 := cDri_rec.DRIDTDEBREV;
                           dDate2 := cDosRub_rec.DRUDTDEB;
                           RAISE err_index2;
                        ELSIF sControle = 'R0737' AND cDri_rec.DMIORDRE    IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV IS NOT NULL
                                                  AND cDosRub_rec.DRUDTFIN IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV >  cDosRub_rec.DRUDTFIN THEN
                           dDate1 := cDri_rec.DRIDTFINREV;
                           dDate2 := cDosRub_rec.DRUDTFIN;
                           RAISE err_index2;
                        ELSIF sControle = 'R0737' AND cDri_rec.DMIORDRE    IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV IS NOT NULL
                                                  AND cDosRub_rec.DRUDTFIN IS NULL THEN
                           SELECT MAX(DRFDTFIN)
                           INTO   dtFinFlux
                           FROM   DOSRUBFLUX
                           WHERE  DOSID = contrat_rec.DOSID AND
                                  DRUORDRE = nDruOrdre;
                           IF cDri_rec.DRIDTFINREV >  dtFinFlux THEN
                              dDate1 := cDri_rec.DRIDTFINREV;
                              dDate2 := dtFinFlux;
                              RAISE err_index2;
                           END IF;
                        ELSIF sControle = 'R0738' AND cDri_rec.DMIORDRE    IS NOT NULL
                                                  AND cDri_rec.DRIDTDEBREV IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV IS NOT NULL
                                                  AND cDri_rec.DRIDTFINREV < cDri_rec.DRIDTDEBREV THEN
                           dDate1 := cDri_rec.DRIDTDEBREV;
                           dDate2 := cDri_rec.DRIDTFINREV;
                           RAISE err_index2;
                        -- CV-24062004 FSA-15417 on peut avoir les memes date de revision
                        ELSIF sControle = 'R0739' THEN
                           IF ( dtLastIndex IS NOT NULL ) AND ( dtLastIndex > cDri_rec.DRIDTDEBREV ) AND cDosRub_rec.DRUORDREPREC IS NULL THEN
                              lOk                := 0;
                              nb_element   	    := nb_element + 1;
                              nOrdre             := nb_element;
                              aOrdre(nb_element) := nOrdre;
                              aMsg(nb_element)   := nDruOrdre;
                              aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
                              EXIT;
                           END IF;
                           dtLastIndex := cDri_rec.DRIDTFINREV;
                        ELSIF sControle = 'R0771' AND cDri_rec.DMIORDRE IS NOT NULL THEN
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   DOSMODEINDEX
                           WHERE  DOSID    = contrat_rec.DOSID
                           AND    DMIORDRE = cDri_rec.DMIORDRE
                           AND    DMITYPE  = 'I';
                           IF nAny = 0 THEN
                              sCode := cDri_rec.DMIORDRE;
                              RAISE err_index3;
                           END IF;
                        END IF;
                     EXCEPTION
                        WHEN err_index1 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nDruOrdre;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDri_rec.DRIORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                        WHEN err_index2 THEN
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
					            aMsg(nb_element)   := nDruOrdre;
					            aType(nb_element)  := 'N';
					            aDec(nb_element)   := NULL;
					            nb_element   	    := nb_element + 1;
					            aOrdre(nb_element) := nOrdre;
					            aMsg(nb_element)   := cDri_rec.DRIORDRE;
					            aType(nb_element)  := 'N';
					            aDec(nb_element)   := NULL;
                        WHEN err_index3 THEN
                           lOk                := 0;
                           nb_element   	    := nb_element + 1;
                           nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nDruOrdre;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
					            nb_element   	    := nb_element + 1;
					            aOrdre(nb_element) := nOrdre;
					            aMsg(nb_element)   := F_StdTrimAll( sCode );
					            aType(nb_element)  := 'C';
					            aDec(nb_element)   := NULL;
                           nb_element   	    := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDri_rec.DRIORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                     END;
                  END LOOP;
               ELSIF sControle = 'R0733' AND cDosRub_rec.DMIORDRE        IS NOT NULL
                                         AND cDosRub_rec.DRUDTBASEACTUAL IS NOT NULL
                                         AND cDosRub_rec.DRUDTBASEACTUAL < contrat_rec.DOSDTDEB THEN
                  dDate1 := cDosRub_rec.DRUDTBASEACTUAL;
                  dDate2 := contrat_rec.DOSDTDEB;
                  RAISE err_parm2;
               ELSIF sControle = 'R0174' AND cDosRub_rec.DRUDTMAJRUB IS NOT NULL
                                         AND cDosRub_rec.DRUDTMAJECH IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE != 'P'
                                         AND contrat_rec.TACCODE != 'LOCATIF'
                                         AND cDosRub_rec.DRUDTMAJECH < cDosRub_rec.DRUDTMAJRUB THEN
                  SELECT COUNT(1)
      				INTO   nCount
      				FROM	TUSPARAM TTR
      				WHERE	TTR.TUSNOM = 'GROUPE'
      				AND   TTR.TUPCODE = cDosRub_rec.RUBCODE;

                  IF NOT ( nCount > 0  AND cDosRub_rec.DRUCLASSE = 'F'
                  							AND cDosRub_rec.DRUTYPE = 'B') THEN
                  	dDate1 := cDosRub_rec.DRUDTMAJRUB;
                  	RAISE err_parmb;
                  END IF;

               ELSIF sControle = 'R0271' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUMODCALINT IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUMODCALINT,
                     'MODECALINT',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R0202' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUMODCALINT IS NOT NULL
                                         AND cDosRub_rec.DRUMODCALINT =  'IS' THEN
                  IF cDosRub_rec.DRUINTSIMPLEPERIODE IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0203' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUMODCALINT IS NOT NULL
                                         AND cDosRub_rec.DRUMODCALINT =  'IS' THEN
                  IF cDosRub_rec.DRUINTSIMPLEMULTIPLE IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0204' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'R', 'M' ) THEN
                  IF cDosRub_rec.DRUTXNATURE IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0205' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'V', 'R', 'M' ) THEN
                  IF cDosRub_rec.DRUTXTYPE IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               --bd020301
               ELSIF sControle = 'R0236' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   = 'F'
                                         AND cDosRub_rec.DRUTAUXCALC IS NOT NULL THEN
                  PA_SELECTDOSSIER2.S_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'SUBV', nAny );
                  IF nAny = 1 THEN
                     bErr := FALSE;
                     dtDruDtDeb := cDosRub_rec.DRUDTDEB;
                     nAny := pa_fungencontrole.GetEpsilonParam( 'TAUX' );
                     FOR cFinRub_r IN cFinRub LOOP
                        IF cDosRub_rec.DRUTAUXCALC > cFinRub_r.DRUTAUXCALC THEN
                           IF abs( cFinRub_r.DRUTAUXCALC - cDosRub_rec.DRUTAUXCALC ) > nAny THEN
                              lOk          	    := 0;
	                           nb_element   	    := nb_element + 1;
		                        nOrdre	          := nb_element;
		                        aOrdre(nb_element) := nOrdre;
		                        aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
        	                     aType(nb_element)  := 'N';
                              aDec(nb_element)   := NULL;
	                           nb_element   	    := nb_element + 1;
		                        aOrdre(nb_element) := nOrdre;
		                        aMsg(nb_element)   := cDosRub_rec.DRUTAUXCALC;
                              aType(nb_element)  := 'N';
        	                     aDec(nb_element)   := 6;
		                        nb_element   	    := nb_element + 1;
		                        aOrdre(nb_element) := nOrdre;
		                        aMsg(nb_element)   := cFinRub_r.DRUTAUXCALC;
		                        aType(nb_element)  := 'N';
		                        aDec(nb_element)   := 6;
		                        nb_element   	    := nb_element + 1;
		                        aOrdre(nb_element) := nOrdre;
		                        aMsg(nb_element)   := cFinRub_r.DRUORDRE;
		                        aType(nb_element)  := 'N';
		                        aDec(nb_element)   := NULL;
		                        nb_element         := nb_element + 1;
		                        aOrdre(nb_element) := nOrdre;
		                        aMsg(nb_element)   := nAny;
		                        aType(nb_element)  := 'N';
		                        aDec(nb_element)   := 6;
                              EXIT;
                           END IF;
                        END IF;
                     END LOOP;
                  END IF;
               -- CV-02062008 CFS28679 ajout 'R0209'
               -- MSG1364
               ELSIF sControle IN ( 'R0206', 'R0209' )
                  AND cDosRub_rec.DRUCLASSE   IS NOT NULL
                  AND cDosRub_rec.DRUCLASSE   IS NOT NULL
                  AND cDosRub_rec.DRUCLASSE   =  'F'
                  AND cDosRub_rec.DRUTYPE     IS NOT NULL
                  AND cDosRub_rec.DRUTYPE     IN ( 'F', 'V', 'R', 'M' )
                  AND cDosRub_rec.DRUTXNATURE IS NOT NULL
                  AND cDosRub_rec.DRUTAUXFIXE IS NULL
                  AND cDosRub_rec.DRUTYPEMONTAGE != 'CVT'THEN
                  IF ( sControle = 'R0206' AND SUBSTR( cDosRub_rec.DRUTXNATURE, 1, 2 ) = 'TF' )
                  OR ( sControle = 'R0209' AND SUBSTR( cDosRub_rec.DRUTXNATURE, 1, 2 ) != 'TF' ) THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0832' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE = 'A' THEN
                  IF cDosRub_rec.DRUTYPE != 'F' OR cDosRub_rec.DRUTYPE IS NULL THEN       --LG FSA 15381 10/06/04
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   LKDOSRUBITRRUB
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                     IF nAny != 0 THEN
                        RAISE err_parm1;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0801' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   =  'F'
                                         AND contrat_rec.DOSPOOL != 'REFI'
                                         AND cDosRub_rec.DRUMTORIGINE != NVL(cDosRub_rec.DRUMTRESIDUEL,0) --bd191203
                                         AND contrat_rec.TACCODE   NOT IN ( 'PRET', 'EMPRUNT' ) THEN
                  PA_SELECTDOSSIER2.S_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ADOSSE', nOkAdosse );
                  IF nOkAdosse = 0 THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   LKDOSRUBITRRUB
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                     IF nAny = 0 THEN
                        RAISE err_parm1;
                     ELSE
                        bErr := FALSE;
                        --nTra := NULL;
                        FOR cLk1R IN cLk1 LOOP
                           IF cLk1R.DIRMT IS NULL OR cLk1R.DIRMT = 0 THEN
                              bErr := TRUE;
                              EXIT;
                           --ELSE
                           --   IF nTra IS NULL THEN
                           --      nTra := cLk1R.ITRID;
                           --   ELSE
                           --      IF nTra != cLk1R.ITRID THEN
                           --         bErr := TRUE;
                           --         EXIT;
                           --      END IF;
                           --   END IF;
                           END IF;
                        END LOOP;
                        IF bErr THEN
                           RAISE err_parm1;
                        END IF;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0830' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      =  'F'
                                         AND cDosRub_rec.DRUORDREPREC IS NULL
                                         AND NOT ( sUtiCodePhase = 'REPRISE' AND sReprisePhaCode = 'ES' )
                                         AND contrat_rec.TACCODE      NOT IN ( 'PRET', 'EMPRUNT' ) THEN
                  PA_SELECTDOSSIER2.S_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ADOSSE', nOkAdosse );
                  IF nOkAdosse = 0 THEN
                     SELECT NVL(SUM(DIRMT),0)
                     INTO   nAdosse
                     FROM   LKDOSRUBITRRUB
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                     IF round(nAdosse , F_NbDecimalDansDevise( contrat_rec.DEVCODE ))!= round(cDosRub_rec.DRUMTORIGINE,F_NbDecimalDansDevise( contrat_rec.DEVCODE )) THEN
                        RAISE err_parma;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0834' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      =  'F'
                                         AND cDosRub_rec.DRUORDREPREC IS NULL
                                         AND contrat_rec.TACCODE      =  'PRET' THEN
                  SELECT SUM( DR2MT )
                  INTO   nAdosse
                  FROM   L2DOSRUBRIQUE
                  WHERE  DOSIDPRET    = contrat_rec.DOSID
                  AND    DRUORDREPRET = cDosRub_rec.DRUORDRE;
                  IF nAdosse > cDosRub_rec.DRUMTORIGINE THEN
                     RAISE err_parma;
                  END IF;

               ELSIF sControle = 'R0835' AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'F'
                                         AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE      =  'F'
                                         AND cDosRub_rec.DRUORDREPREC IS NULL
                                         AND contrat_rec.TACCODE      =  'PRET' THEN
                  bErr := FALSE;
                  FOR cEmpR IN cEmp LOOP
                     nDosEmpr := cEmpR.DOSIDEMPR;
                     nDruEmpr := cEmpR.DRUORDREEMPR;
                     nAdosse  := 0;
                     SELECT DRUMTORIGINE, DOS.DEVCODE, DOSNUM || ' ' || TO_CHAR(DOSAVENANT)
                     INTO   nAssEmpr, sDevCode, sDosNum
                     FROM   DOSRUBRIQUE DRU, DOSSIER DOS
                     WHERE  DRU.DOSID    = nDosEmpr
                     AND    DRU.DRUORDRE = nDruEmpr
                     AND    DOS.DOSID    = nDosEmpr;
                     FOR cEmp1R IN cEmp1 LOOP
                        IF cEmp1R.PP = 1 THEN
                           BEGIN
                              nItrId  := cEmp1R.ID;
                              SELECT POO.POOID, POO.POOBASE, PAC.PACQP
                              INTO   nPooId, nBase, nQp
                              FROM   POOL POO, POOACTEUR PAC
                              WHERE  POO.ITRID    = nItrId
                              AND    POO.POODTFIN IS NULL
                              AND    POO.POOTYPE = 'FINANCE'
                              AND    PAC.POOID    = POO.POOID
                              AND    PAC.ACTID    = contrat_rec.ACTID;
                              IF nQp IS NULL OR nQp = 0 THEN
                                 nBase := 1;
                                 nQp   := 1;
                              END IF;
                           EXCEPTION
                              WHEN OTHERS THEN
                                 nBase := 1;
                                 nQp   := 1;
                           END;
                        ELSE
                           nBase := 1;
                           nQp   := 1;
                        END IF;
                        IF sDevCode != cEmp1R.DEV THEN
                           PA_COMMON.F_CONVERSIONDEVISE(
                              cEmp1R.DEV,
                              sDevCode,
                              (cEmp1R.MT*nBase)/nQp,
                              TRUNC(SYSDATE),
                              nTauxConv,
                              nBien );
                        ELSE
                           nBien := (cEmp1R.MT*nBase)/nQp;
                        END IF;
                        nAdosse := nAdosse + nBien;
                     END LOOP;
                     IF nAssEmpr - nAdosse < 0.1 THEN
                        bErr := TRUE;
                        EXIT;
                     END IF;
                  END LOOP;
                  IF bErr THEN
                     RAISE err_parmy;
                  END IF;

               ELSIF sControle = 'R0831' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   =  'F'
                                         AND contrat_rec.DOSPOOL != 'REFI'
                                         AND contrat_rec.TACCODE   NOT IN ( 'PRET', 'EMPRUNT' )
                                         AND contrat_rec.DOSTYPEFINANCEMENT NOT IN ('TCONS','TTNL') THEN
                  IF nCountDosIdTransfert = 0 THEN
                  	PA_SELECTDOSSIER2.S_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ADOSSE', nOkAdosse );
                  	IF nOkAdosse = 0 THEN
                  	   bErr := FALSE;
                  	   FOR cBienR IN cBien LOOP
                  	      BEGIN
                  	         nAdosse := NVL(cBienR.DIRMT,0);
                  	         nItrId  := cBienR.ITRID;
                  	         SELECT SUM( DPAQP )
                  	         INTO   nBase
                  	         FROM   LKDOSRUBPOOACT
                  	         WHERE  DOSID = contrat_rec.DOSID AND
                  	                DRUORDRE = nDruOrdre
                  	                AND DPADTFIN IS NULL;
                  	         SELECT SUM( DPAQP )
                  	         INTO   nQp
                  	         FROM   LKDOSRUBPOOACT
                  	         WHERE  DOSID = contrat_rec.DOSID AND
                  	                DRUORDRE = nDruOrdre AND
                  	                ACTID = contrat_rec.ACTID
                  	                AND DPADTFIN IS NULL;
                  	         IF nQp IS NULL THEN
                  	            nQp   := 1;
                  	         END IF;
                  	         IF nBase IS NULL OR nBase = 0 THEN
                  	            nBase := 1;
                  	         END IF;
                  	         IF nQp IS NULL OR nQp = 0 THEN
                  	            nQp := -1;   --bd030301
                  	         END IF;
                  	      EXCEPTION
                  	         WHEN OTHERS THEN
                  	            nBase := 1;
                  	            nQp   := 1;
                  	      END;
                  	      IF nQp != -1  THEN
                  	         IF cBienR.DEVCODE != cBienR.DEVCODERUB THEN
                  	            PA_COMMON.F_CONVERSIONDEVISE(
                  	               cBienR.DEVCODE,
                  	               cBienR.DEVCODERUB,
                  	               ( cBienR.IRUMTORIGINE*nBase)/nQp,
                  	               -- TRUNC(SYSDATE),
                  	               cDosRub_rec.DRUDTDEB,
                  	               nTauxConv,
                  	               nMtRubO );
                  	         ELSE
                  	            nMtRubO := (cBienR.IRUMTORIGINE*nBase)/nQp;
                  	         END IF;

                  	         nAny := pa_fungencontrole.GetEpsilonParam( 'ADOSSE' );

                  	         IF ABS (nMtRubO - nAdosse ) > nAny THEN
                  	            bErr := TRUE;
                  	            EXIT;
                  	         END IF;
                  	      END IF;
                  	   END LOOP;
                  	   IF bErr THEN
                  	      RAISE err_parmz;
                  	   END IF;
                  	END IF;
                  END IF;
               ELSIF sControle = 'R0833' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   =  'F'
                                         AND contrat_rec.DOSPOOL != 'REFI'
                                         AND NOT ( sUtiCodePhase = 'REPRISE' AND sReprisePhaCode = 'ES' )  --bd191203
                                         AND contrat_rec.TACCODE   NOT IN ( 'PRET', 'EMPRUNT' ) THEN
                  PA_SELECTDOSSIER2.S_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ADOSSE', nOkAdosse );
                  IF nOkAdosse = 0 THEN
                     bErr := FALSE;
                     FOR cBienR IN cBien LOOP
                        BEGIN
                           nAdosse := 0;
                           nItrId  := cBienR.ITRID;
                           SELECT POO.POOID, POO.POOBASE, PAC.PACQP
                           INTO   nPooId, nBase, nQp
                           FROM   POOL POO, POOACTEUR PAC
                           WHERE  POO.ITRID    = nItrId
                           AND    POO.POOTYPE  = 'FINANCE'
                           AND    POO.POODTFIN IS NULL
                           AND    PAC.POOID    = POO.POOID
                           AND    PAC.ACTID    = contrat_rec.ACTID;
                           IF nQp IS NULL OR nQp = 0 THEN
                              nBase := 1;
                              nQp   := 1;
                           END IF;
                        EXCEPTION
                           WHEN OTHERS THEN
                              nBase := 1;
                              nQp   := 1;
                        END;
                        IF cBienR.DEVCODE != cBienR.DEVCODERUB THEN
                           PA_COMMON.F_CONVERSIONDEVISE(
                              cBienR.DEVCODE,
                              cBienR.DEVCODERUB,
                              (cBienR.IRUMTORIGINE*nBase)/nQp,
                              -- TRUNC(SYSDATE),
                              cDosRub_rec.DRUDTDEB,
                              nTauxConv,
                              nMtRubO );
                        ELSE
                           nMtRubO := (cBienR.IRUMTORIGINE*nBase)/nQp;
                        END IF;
                        nIruOrdre := cBienR.IRUORDRE;
                        dtDruDtDebCBall := cDosRub_rec.DRUDTDEB;
                        dtDruDtFinCBall := F_PlFinRUBRIQUE(contrat_rec.DOSID,cDosRub_rec.DRUORDRE);
                        FOR cBAllR IN cBAll LOOP
                           IF cBAllR.DRUORDREPREC IS NULL AND ( cBAllR.DRUTYPE = 'F' AND cBAllR.DRUCLASSE != 'A' ) THEN
                              SELECT DEVCODE, PHACODE
                              INTO   sDevCode, sPhaCode
                              FROM   DOSSIER DOS, DOSPHASE PHA
                              WHERE  DOS.DOSID    = cBAllR.DOSID
                              AND    PHA.DOSID    = cBAllR.DOSID
                              AND    PHA.DPHDTFIN IS NULL
                              AND    PHA.PHADEST  = 'DOSSIER';
                              IF sPhaCode != 'TER' THEN
                                 IF sDevCode != contrat_rec.DEVCODE THEN
                                    PA_COMMON.F_CONVERSIONDEVISE(
                                       sDevCode,
                                       contrat_rec.DEVCODE,
                                       cBAllR.DIRMT,
                                       -- TRUNC(SYSDATE),
                                       cDosRub_rec.DRUDTDEB,
                                       nTauxConv,
                                       nBien );
                                    IF cBallR.DRUSENS = '-' THEN
                                       nAdosse := nAdosse + NVL(nBien*(-1),0);
                                    ELSE
                                       nAdosse := nAdosse + NVL(nBien,0);
                                    END IF;
                                 ELSE
                                    IF cBallR.DRUSENS = '-' THEN
                                       nAdosse := nAdosse + NVL(cBAllR.DIRMT*(-1),0);
                                    ELSE
                                       nAdosse := nAdosse +  NVL(cBAllR.DIRMT,0);
                                    END IF;
                                 END IF;
                              END IF;
                           END IF;
                        END LOOP;
                        IF nMtRubO - nAdosse < -0.5 THEN
                           bErr := TRUE;
                           EXIT;
                        END IF;
                     END LOOP;
                     IF bErr THEN
                        RAISE err_parmz;
                     END IF;
                  END IF;
               --LG FSA 18748 Arval ESP 18/03/05
               ELSIF ( sControle = 'R0840' ) THEN
                  nTotIgic := 0;
                  nTotIva  := 0;
                  IF ( SUBSTR( cDosRub_rec.TAXCODE, 1, 3 ) = 'IVA' ) THEN
                     FOR cBienR IN cBien LOOP
                        SELECT COUNT(*)
                        INTO   nIgic
                        FROM   DEPLIGNE DEP,
                               TAXE TAX
                        WHERE  DEP.DEPID    = cBienR.DEPID
                          AND  DEP.DLIORDRE = cBienR.DLIORDRE
                          AND  SUBSTR( DEP.TAXCODE, 1, 4 ) = 'IGIC'
                          AND  TAX.TAXCODE  = DEP.TAXCODE
                          AND  TAX.TAXTYPE != 'EXO';
                        nTotIgic := nTotIgic + nIgic;
                     END LOOP;
                  ELSIF ( SUBSTR( cDosRub_rec.TAXCODE, 1, 4 ) = 'IGIC' ) THEN
                     FOR cBienR IN cBien LOOP
                        SELECT COUNT(*)
                        INTO   nIva
                        FROM   DEPLIGNE DEP,
                               TAXE TAX
                        WHERE  DEP.DEPID    = cBienR.DEPID
                          AND  DEP.DLIORDRE = cBienR.DLIORDRE
                          AND  SUBSTR( DEP.TAXCODE, 1, 3 ) = 'IVA'
                          AND  TAX.TAXCODE  = DEP.TAXCODE
                          AND  TAX.TAXTYPE != 'EXO';
                        nTotIva := nTotIva + nIva;
                     END LOOP;
                  END IF;
                  IF ( nTotIgic > 0 ) THEN
                     RAISE err_parm1;
                  ELSIF ( nTotIva > 0 ) THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0131' AND PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE ) THEN
                  IF cDosRub_rec.DRUORDREFACTURATION IS NOT NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0191' AND cDosRub_rec.DRUORDREGRPFAC IS NOT NULL  THEN
               	IF cDosRub_rec.DRUORDREGRPFAC > cDosRub_rec.DRUORDRE THEN
               		 RAISE err_parm1;
               	END IF;
               ELSIF sControle = 'R0272' AND cDosRub_rec.DRUCLASSE   IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE   =  'F'
                                         AND cDosRub_rec.DRUTYPE     IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE     IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUTXNATURE IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUTXNATURE,
                     'MONTAGETAUX',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               ELSIF sControle = 'R0273' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUTXTYPE IS NOT NULL THEN
                  IF SUBSTR( cDosRub_rec.DRUMONTAGE,1,3 ) = 'AMO' THEN
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUTXTYPE,
                        'TYPETAUA',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  ELSE
                     IF pa_fungencontrole.ocCodeTable(  'I',
                        cDosRub_rec.DRUTXTYPE,
                        'TYPETAUX',
                        nb_element,
                        aOrdre,
                        aMsg,
                        aType,
                        aDec ) = 0 THEN
                        lOk := 0;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0270' AND cDosRub_rec.DRUCLASSE           IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE           =  'F'
                                         AND cDosRub_rec.DRUTYPE             IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE             IN ( 'F', 'V', 'R', 'M' )
                                         AND cDosRub_rec.DRUINTSIMPLEPERIODE IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable(  'I',
                     cDosRub_rec.DRUINTSIMPLEPERIODE,
                     'PERIODE',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     lOk := 0;
                  END IF;
               --LG FSA 18961 23/03/05
               ELSIF sControle = 'R0274' THEN
                  IF cDosRub_rec.DRUCLASSE = 'F' AND cDosRub_rec.DRUTYPE  = 'F'
                     AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'NEGATIF' ) = 1 THEN
                        IF cDosRub_rec.DRUMTORIGINE < 0 THEN
                           RAISE err_parm1;
                        END IF;
                  END IF;
               ELSIF sControle = 'R0230' AND cDosRub_rec.DRUCLASSE      IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE      =  'F'
                                         AND cDosRub_rec.DRUTYPE        IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE        IN ( 'F', 'R' )
                                         AND cDosRub_rec.DRUTYPEMONTAGE IS NOT NULL
                                         AND cDosRub_rec.DRUMONTAGE     IS NOT NULL THEN
                  IF cDosRub_rec.DRUTYPEMONTAGE = 'CVT' AND cDosRub_rec.DRUMONTAGE = 'ECHFIX' THEN
                     RAISE err_parm7;
                  ELSIF cDosRub_rec.DRUTYPEMONTAGE IN ( 'AJUST', 'VRG', 'VRG+AJU' ) AND cDosRub_rec.DRUMONTAGE != 'LOYFIX' THEN
                     RAISE err_parm7;
                  END IF;
               ELSIF sControle = 'R0534' AND cDosRub_rec.DRUCLASSE      IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE      =  'F'
                                         AND cDosRub_rec.DRUTYPE        IS NOT NULL
                                         AND cDosRub_rec.DRUTYPEMONTAGE IS NOT NULL
                                         AND cDosRub_rec.DRUTYPEMONTAGE IN ( 'AJUST', 'VRG', 'VRG+AJU' )
                                         AND cDosRub_rec.DRUTXNATURE    IS NOT NULL
                                         AND cDosRub_rec.DRUTXNATURE    NOT IN ( 'VR', 'VRR' ) THEN
                  RAISE err_parm7;
               ELSIF sControle = 'R0231' AND cDosRub_rec.DRUCLASSE      IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE      =  'F'
                                         AND cDosRub_rec.DRUTYPE        IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE        =  'V'
                                         AND cDosRub_rec.DRUTYPEMONTAGE IS NOT NULL
                                         AND cDosRub_rec.DRUMONTAGE     IS NOT NULL THEN
                  IF cDosRub_rec.DRUMONTAGE NOT IN ( 'LOYFIX', 'SAISON', 'AMOFIX' ) THEN
                     RAISE err_parm8;
                  END IF;
               ELSIF sControle = 'R0232' AND cDosRub_rec.DRUCLASSE      IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE      =  'F'
                                         AND cDosRub_rec.DRUTYPE        IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE        =  'V'
                                         AND cDosRub_rec.DRUTYPEMONTAGE IS NOT NULL THEN
                  IF cDosRub_rec.DRUTYPEMONTAGE != 'CLASSIC' THEN
                     RAISE err_parm9;
                  END IF;
               ELSIF sControle = 'R0208' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'V', 'R', 'M' ) THEN
                  IF cDosRub_rec.DRUMTRESIDUEL IS NULL THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0207' AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'V', 'R', 'M' ) THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBASSIETTE
                  WHERE  DOSID = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRub_rec.DRUORDRE;
                  IF nAny = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0130' AND cDosRub_rec.DRUDTDEB IS NOT NULL
                                         AND cDosRub_rec.DRUDTFIN IS NOT NULL
                                         AND cDosRub_rec.DRUDTDEB > cDosRub_rec.DRUDTFIN THEN
                  dDate1 := cDosRub_rec.DRUDTDEB;
                  dDate2 := cDosRub_rec.DRUDTFIN;
                  RAISE err_parm2;
               ELSIF sControle = 'R0170' AND cDosRub_rec.RUBID   IS NOT NULL AND
                                             cDosRub_rec.DRUCLASSE IS NOT NULL AND
                                             cDosRub_rec.DRUTYPE   IS NOT NULL THEN
                  sFiltre  := '';
                  sFiltre1 := '';
                  IF cDosRub_rec.DRUCLASSE = 'A' THEN
                     IF cDosRub_rec.DRUTYPE = 'R' THEN
                        sFiltre := 'RETACC';
                        --IF contrat_rec.TACCODE IN ( 'CBI', 'CBMIXTE' ) THEN
                        --      sFiltre1 := 'DEPIMMO';
                        --ELSIF contrat_rec.TACCODE = 'CBM' THEN
                        --      sFiltre1 := 'DEPMOD';
                        --END IF;
                     ELSIF cDosRub_rec.DRUTYPE = 'F' AND  F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'DEPGAR' ) <> 1 THEN
                        sFiltre := 'REDACC';
                     END IF;
                  ELSIF cDosRub_rec.DRUCLASSE = 'F' THEN
                     IF cDosRub_rec.DRUTYPE = 'V' THEN
                        sFiltre := 'APPFOND';
                     ELSIF cDosRub_rec.DRUTYPE = 'R' THEN
                        sFiltre := 'RETFIN';
                     ELSIF cDosRub_rec.DRUTYPE = 'P' THEN
                        IF contrat_rec.TACCODE = 'CC' THEN
                           sFiltre := 'ECHINT';
                        ELSIF contrat_rec.TACCODE = 'PRET' THEN
                           IF  cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,9) = 'DECAISTTC' THEN
                              sFiltre := 'DECTTC';
                           ELSIF cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,8) = 'DECAISHT' THEN
                              sFiltre := 'DECHT';
                           ELSIF cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,9) = 'DECAISTVA' THEN
                              sFiltre := 'DECTVA';
                           -- LG 18/10/04 FSA 16807
                           ELSIF cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,3) = 'ENG' THEN
                              sFiltre := 'ENGAG';
                           ELSIF cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,6) = 'NDBLOC' THEN
                              sFiltre := 'ENGAG';
                           ELSIF cDosRub_rec.DRUBASECALC IS NOT NULL AND SUBSTR( cDosRub_rec.DRUBASECALC,1,6) = 'UNUSED' THEN
                              sFiltre := 'ENGAG';
                           ELSE
                              sFiltre := 'ECHINT';
                           END IF;
                        ELSE
                           sFiltre := 'PREFIN';
                        END IF;
                     ELSIF cDosRub_rec.DRUTYPE = 'M' THEN
                        sFiltre := 'MORA';
                     ELSIF cDosRub_rec.DRUTYPE = 'F' THEN
                        sFiltre := 'REDFIN';
                     END IF;
                  END IF;
                  IF sFiltre IS NOT NULL THEN
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   RUBACCES
                     WHERE  RUBID  = cDosRub_rec.RUBID
                     AND    RACACCES = sFiltre;
                     IF nAny = 0 AND sFiltre1 IS NOT NULL THEN
                        SELECT COUNT(*)
                        INTO   nAny
                        FROM   RUBACCES
                        WHERE  RUBID  = cDosRub_rec.RUBID
                        AND    RACACCES = sFiltre1;
                     END IF;
                  ELSE
                     SELECT COUNT(*)
                     INTO   nAny
                     FROM   RUBRIQUE
                     WHERE  RUBID = cDosRub_rec.RUBID;
                  END IF;
                  IF nAny = 0  THEN
                     sCode := cDosRub_rec.RUBCODE;
                     RAISE err_parm3;
                  END IF;
               ELSIF sControle = 'R0140' AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'R' )
                                         AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE  =  'F'
                                         AND cDosRub_rec.DRUDTDEB  IS NOT NULL
										 AND F_ISRUBIDONFILTRE(cDosRub_rec.RUBID, 'DOWNP')  = 0
                                         AND contrat_rec.DOSDTDEB  IS NOT NULL THEN
                  IF cDosRub_rec.DRUDTDEB < contrat_rec.DOSDTDEB THEN
                     dDate1 := cDosRub_rec.DRUDTDEB;
                     dDate2 := contrat_rec.DOSDTDEB;
                     RAISE err_parm2;
                  END IF;
               ELSIF sControle = 'R0141' AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE   IN ( 'F', 'R' )
                                         AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'F'
                                         AND F_PlFinRUBRIQUE( contrat_rec.DOSID,cDosRub_rec.DRUORDRE) IS NOT NULL
                                         AND contrat_rec.DOSDTFIN  IS NOT NULL THEN
                  BEGIN
                     SELECT MAX(DPHORDRE)
                     INTO   nAny
                     FROM   DOSPHASE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    PHACODE = 'TER'
                     AND    JALCODE IS NOT NULL
                     AND    SUBSTR(JALCODE,1,3) != ('SUS')
                     AND    JALCODE != 'FSUS';

                     SELECT JALCODE
                     INTO   sJalCode
                     FROM   DOSPHASE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    DPHORDRE = nAny;
                  EXCEPTION
                     WHEN OTHERS THEN
                        sJalCode := 'ES';
                  END;
                  IF ( NVL(sJalCode,'ES') != 'EARB')
                        AND (F_PlFinRUBRIQUE( contrat_rec.DOSID,cDosRub_rec.DRUORDRE) > contrat_rec.DOSDTFIN) THEN
                        dDate1 := cDosRub_rec.DRUDTFIN;
                        dDate2 := contrat_rec.DOSDTFIN;
                        RAISE err_parm2;
                  END IF;
               ELSIF sControle = 'R0143' AND cDosRub_rec.DRUTYPE   IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE =  'A'
                                         AND F_PlFinRUBRIQUE( contrat_rec.DOSID,cDosRub_rec.DRUORDRE) IS NOT NULL THEN
                  IF contrat_rec.TACCODE != 'LOCATIF' THEN
                   BEGIN
                     SELECT MAX(DPHORDRE)
                     INTO   nAny
                     FROM   DOSPHASE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    JALCODE IS NOT NULL
                     AND    PHACODE = 'TER'
                     AND    SUBSTR(JALCODE,1,3) != ('SUS')
                     AND    JALCODE != 'FSUS';

                     SELECT JALCODE
                     INTO   sJalCode
                     FROM   DOSPHASE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    DPHORDRE = nAny;
                  EXCEPTION
                     WHEN OTHERS THEN
                        sJalCode := 'ES';
                  END;
                     IF ( NVL(sJalCode,'ES') != 'EARB')
                        AND ( contrat_rec.DOSDTFIN  IS NOT NULL AND F_PlFinRUBRIQUE( contrat_rec.DOSID,cDosRub_rec.DRUORDRE) > contrat_rec.DOSDTFIN) THEN
                        dDate1 := cDosRub_rec.DRUDTFIN;
                        dDate2 := contrat_rec.DOSDTFIN;
                        RAISE err_parm2;
                     END IF;
                  ELSE
                     BEGIN
                        SELECT DRBDTFIN
                        INTO   dDate2
                        FROM   DOSRENOUVBAIL
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRBORDRE = (SELECT MAX(DRBORDRE) FROM DOSRENOUVBAIL
                                             WHERE DOSID = contrat_rec.DOSID);
                     EXCEPTION
                        WHEN OTHERS THEN
                           dDate2:= NULL;
                     END;
                     -- TD le 27/07/01 --
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
                          dDate2 := dtDreDtFin;
                        END IF;
                     END IF;

                     IF ( dDate2 IS NOT NULL AND cDosRub_rec.DRUDTFIN > dDate2 ) THEN
                        dDate1 := cDosRub_rec.DRUDTFIN;
                        RAISE err_parm2;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0142' AND cDosRub_rec.DRUTYPE      IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    IS NOT NULL
                                         AND cDosRub_rec.DRUDTDEB     IS NOT NULL
                                         AND cDosRub_rec.DRUCLASSE    =  'A' THEN
                  IF contrat_rec.TACCODE != 'LOCATIF' THEN
                     IF contrat_rec.DOSDTPRODUCT IS NOT NULL AND cDosRub_rec.DRUDTDEB < contrat_rec.DOSDTPRODUCT THEN
                        dDate1 := cDosRub_rec.DRUDTDEB;
                        dDate2 := contrat_rec.DOSDTPRODUCT;
                        RAISE err_parm2;
                     END IF;
                  ELSE
                     BEGIN
                        SELECT DRBDTEFFET
                        INTO   dDate2
                        FROM   DOSRENOUVBAIL
                        WHERE  DOSID = contrat_rec.DOSID
                        AND    DRBORDRE = (SELECT MIN(DRBORDRE) FROM DOSRENOUVBAIL
                                             WHERE DOSID = contrat_rec.DOSID);
                     EXCEPTION
                        WHEN OTHERS THEN
                           dDate2:= NULL;
                     END;

                     -- RM FSA 14976 Controle vrai seulement si la rubrique n'est pas une rubrique de regul du passe
                     SELECT   COUNT(1)
                     INTO     nAny
                     FROM     DOSRUBRIQUE
                     WHERE    DOSID = contrat_rec.DOSID
                              AND DRUORDRE = cDosRub_rec.DRUORDRE
                              AND DRUTYPEMONTAGE = 'REGUL';

                     IF (dDate2 IS NOT NULL) AND (cDosRub_rec.DRUDTDEB < dDate2) AND (nAny = 0) THEN
                        dDate1 := cDosRub_rec.DRUDTDEB;
                        RAISE err_parm2;
                     END IF;
                  END IF;
               ELSIF sControle = 'R0145' AND PA_DOSRUBCONTROLE.IsRubFinPre( cDosRub_rec.DRUCLASSE, cDosRub_rec.DRUTYPE )
                                         AND cDosRub_rec.DRUDTDEB     IS NOT NULL
                                         AND contrat_rec.DOSDTPRODUCT IS NOT NULL THEN
                  IF cDosRub_rec.DRUDTDEB < contrat_rec.DOSDTPRODUCT THEN
                     dDate1 := cDosRub_rec.DRUDTDEB;
                     dDate2 := contrat_rec.DOSDTPRODUCT;
                     RAISE err_parm2;
                  END IF;
               ELSIF sControle = 'R0146' AND cDosRub_rec.DACORDRE IS NOT NULL AND cDosRub_rec.DRUTYPE != 'R' THEN
                  SELECT ACT.DEVCODE, ACT.ACTLIBCOURT
                  INTO   sActDevCode, sLibCourt
                  FROM   DOSACTEUR DAC, ACTEUR ACT
                  WHERE  DAC.DOSID = contrat_rec.DOSID AND
                         DAC.DACORDRE = cDosRub_rec.DACORDRE AND
                         ACT.ACTID = DAC.ACTID;
                  IF sActDevCode != contrat_rec.DEVCODE THEN
                     RAISE err_parmv;
                  END IF;
               ELSIF sControle = 'R0171' AND cDosRub_rec.TAXCODE IS NOT NULL THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   TAXE
                  WHERE  TAXCODE = cDosRub_rec.TAXCODE;
                  IF nAny = 0  THEN
                     sCode := cDosRub_rec.TAXCODE;
                     RAISE err_parm3;
                  END IF;
               ELSIF sControle = 'R0132' AND cDosRub_rec.DRUTYPEMONTAGE      IS NOT NULL
                                         AND cDosRub_rec.DRUTYPEMONTAGE      =  'CLASSIC'
                                         AND cDosRub_rec.DRUTYPE             =  'V'
                                         AND cDosRub_rec.DRUORDREFACTURATION IS NULL THEN
                  RAISE err_parm1;
               ELSIF sControle = 'R0172' AND cDosRub_rec.DRUTYPE  IS NOT NULL
                                         AND cDosRub_rec.DACORDRE IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE  IN ( 'F', 'P', 'V', 'M' )
                                         AND cDosRub_rec.DRUCLASSE = 'F'
                                         AND cDosRub_rec.DRUTYPE != 'P'
                                         AND contrat_rec.DOSPOOL != 'PART' THEN
                  BEGIN
                     /*
                     Optimisation JPB
                     SELECT ACTID
                     INTO   nActId
                     FROM   DOSACTEUR
                     WHERE  DOSID = contrat_rec.DOSID AND DACORDRE = cDosRub_rec.DACORDRE;

                     SELECT ACTLIBCOURT
                     INTO sLibCourt
                     FROM ACTEUR
                     WHERE ACTID = nActId;
                     */

                     if contrat_rec.DOSPOOL NOT IN ('NON', 'CDF') AND cDosRub_rec.POOID IS NOT NULL THEN
                        BEGIN
                           SELECT NVL( PACFLAGVISIBLEEXPLOIT,0), NVL(PACFLAGVISIBLECHANTIER,0)
                           INTO   nFlagExploit, nFlagChantier
                           FROM POOACTEUR
                           WHERE ACTID =  contrat_rec.ACTID
                           AND POOID =  cDosRub_rec.POOID  ;
                        EXCEPTION
                           WHEN OTHERS THEN
                              nFlagExploit  := NULL;
                              nFlagChantier := NULL;
                        END ;
                        if nFlagExploit = 1 OR   nFlagChantier = 1 THEN
                            SELECT DAC.ACTID , ACTLIBCOURT
                            INTO   nActId, sLibCourt
                            FROM   DOSACTEUR DAC, ACTEUR ACT
                            WHERE  DAC.DOSID    = contrat_rec.DOSID
                            AND    DAC.DACORDRE = cDosRub_rec.DACORDRE
                            AND    ACT.ACTID    = DAC.ACTID;
                            SELECT COUNT(*)
                            INTO   nAny
                            FROM   DOSACTEUR
                            WHERE  ACTID = nActId
                            AND    F_PLROLEEXTERNE(ROLCODE) = 'CLIENT';
                            IF nAny < 1 THEN
                               sCode := sLibCourt;
                               RAISE err_parm3;
                            END IF;
                        END if;
                     ELSE
                        SELECT DAC.ACTID , ACTLIBCOURT
                        INTO   nActId, sLibCourt
                        FROM   DOSACTEUR DAC, ACTEUR ACT
                        WHERE  DAC.DOSID    = contrat_rec.DOSID
                        AND    DAC.DACORDRE = cDosRub_rec.DACORDRE
                        AND    ACT.ACTID    = DAC.ACTID;

                        --IF contrat_rec.DOSPOOL IS NOT NULL AND contrat_rec.DOSPOOL = 'PART' THEN
                        -- SELECT COUNT(*)
                        -- INTO   nAny
                        -- FROM   DOSACTEUR
                        -- WHERE  ACTID = nActId
                        -- AND    ROLCODE = 'PARTEN';
                        -- IF nAny < 1 THEN
                        --    sCode := sLibCourt;
                        --    RAISE err_parm3;
                        -- END IF;
                        --ELSE
                           SELECT COUNT(*)
                           INTO   nAny
                           FROM   DOSACTEUR
                           WHERE  ACTID = nActId
                           AND    F_PLROLEEXTERNE(ROLCODE) = 'CLIENT';
                           IF nAny < 1 THEN
                              sCode := sLibCourt;
                              RAISE err_parm3;
                           END IF;
                        --END IF;
                     END if;
                  EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        sLibCourt := NULL;
                  END;
               ELSIF sControle = 'R0183' AND cDosRub_rec.DRUTYPE  IS NOT NULL
                                         AND cDosRub_rec.DACORDRE IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE  IN ( 'F', 'V', 'M' ) THEN
                  BEGIN
                     IF ( contrat_rec.DOSPOOL = 'PART' AND cDosRub_rec.DRUCLASSE = 'A') OR contrat_rec.DOSPOOL != 'PART' THEN

                     	SELECT DAC.ACTID , ACTLIBCOURT, DAC.ROLCODE
                     	INTO   nActId, sLibCourt, sRolCode
                     	FROM   DOSACTEUR DAC, ACTEUR ACT
                     	WHERE  DAC.DOSID    = contrat_rec.DOSID
                     	AND    DAC.DACORDRE = cDosRub_rec.DACORDRE
                     	AND    ACT.ACTID    = DAC.ACTID;

                     	IF F_PLROLEEXTERNE(sRolCode) != 'CLIENT' THEN
                     		sCode := sLibCourt;
                     	   RAISE err_parm3;
                     	END IF;
                     END IF;
                  EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        sLibCourt := NULL;
                  END;
               ELSIF sControle = 'R0173' AND cDosRub_rec.DRUTYPE  IS NOT NULL
                                         AND cDosRub_rec.DACORDRE IS NOT NULL
                                         AND cDosRub_rec.DRUTYPE  =  'R' THEN
                  BEGIN
                     /*
                     Optimisation JPB
                     SELECT ACTID
                     INTO   nActId
                     FROM   DOSACTEUR
                     WHERE  DOSID = contrat_rec.DOSID AND DACORDRE = cDosRub_rec.DACORDRE;

                     SELECT ACTLIBCOURT
                     INTO sLibCourt
                     FROM ACTEUR
                     WHERE ACTID = nActId;
                     */
                     SELECT DAC.ACTID, ACT.ACTLIBCOURT
                     INTO   nActId, sLibCourt
                     FROM   DOSACTEUR DAC, ACTEUR ACT
                     WHERE  DAC.DOSID    = contrat_rec.DOSID
                     AND    DAC.DACORDRE = cDosRub_rec.DACORDRE
                     AND    ACT.ACTID    = DAC.ACTID;
                     /*
                     Optimisation JPB
                     -- Le code suivant ne sert a rien,
                     -- si SELECT ACTID into nActID n'a rien ramene, on sera passe directement a l'exception No_DATA_FOUND
                     -- si SELECT ACTID into nActID a ramene un numero,
                     --         c'est qu'il en existe au moins un, surtout que le critere de selection est plus restrictif
                     SELECT COUNT(*)
                     INTO nAny
                     FROM DOSACTEUR
                     WHERE ACTID = nActId;
                     IF nAny < 1 THEN
                             sCode := sLibCourt;
                             RAISE err_parm3;
                     END IF;
                     */
                  EXCEPTION
                     WHEN NO_DATA_FOUND THEN
                        sLibCourt := NULL;
                  END;
               ELSIF sControle = 'R0175' AND cDosRub_rec.DRUDECOMPTENUM IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable( 'I',
                     cDosRub_rec.DRUDECOMPTENUM,
                     'DECOMPTEJOURS',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0176' AND cDosRub_rec.DRUDECOMPTEDEN IS NOT NULL THEN
                  IF pa_fungencontrole.ocCodeTable( 'I',
                     cDosRub_rec.DRUDECOMPTEDEN,
                     'BASEANNUELLE',
                     nb_element,
                     aOrdre,
                     aMsg,
                     aType,
                     aDec ) = 0 THEN
                     RAISE err_parm1;
                  END IF;
               ELSIF sControle = 'R0158' AND cDosRub_rec.DRUTYPE = 'V' AND cDosRub_rec.DRUORDREMAITRE IS NOT NULL THEN

                  SELECT DRUTYPE
                  INTO   sTypeRubrique
                  FROM   DOSRUBRIQUE
                  WHERE  DOSID    = contrat_rec.DOSID
                  AND    DRUORDRE = cDosRub_rec.DRUORDREMAITRE;

                  IF sTypeRubrique != 'V' THEN
                     RAISE err_parm11;
                  END if;
               ELSIF sControle = 'R0159' AND cDosRub_rec.DRUTYPE = 'V' AND cDosRub_rec.DRUORDREMAITRE IS NOT NULL THEN
                  BEGIN
                     SELECT DRUORDREMAITRE
                     INTO   nDruOrdreMaitre
                     FROM   DOSRUBRIQUE
                     WHERE  DOSID    = contrat_rec.DOSID
                     AND    DRUORDRE = cDosRub_rec.DRUORDREMAITRE
                     AND    DRUTYPE  = 'V';
                  EXCEPTION WHEN OTHERS THEN
                     nDruOrdreMaitre := NULL;
                  END;

                  IF nDruOrdreMaitre IS NOT NULL THEN
                     RAISE err_parm11;
                  END if;

               ELSIF sControle = 'R0238' AND cDosRub_rec.DRUCLASSE         = 'F'
                                         AND cDosRub_rec.DRUTYPE           = 'F'
                                         AND cDosRub_rec.DRUFLAGRENOUVAUTO =  1 THEN
                  BEGIN
                     SELECT DRFMT, DRFNBPERIODE
                     INTO   nDrfMt, nDrfNbPeriode
                     FROM   DOSRUBFLUX
                     WHERE  DOSID    = contrat_rec.DOSID
                       AND  DRUORDRE = cDosRub_rec.DRUORDRE
                       AND  DRFORDRE = ( SELECT MAX(DRFORDRE)
                                         FROM   DOSRUBFLUX
                                         WHERE  DOSID    = contrat_rec.DOSID
                                           AND  DRUORDRE = cDosRub_rec.DRUORDRE );
                  EXCEPTION
                     WHEN OTHERS THEN
                        nDrfMt := NULL;
                  END;
                  IF ( nDrfMt IS NOT NULL ) THEN
                     -- Pour l'AMOFIX le montant du flux represente le montant total a amortir sur
                     -- la duree don on le divise par le nombre de periode pour le comparer avec la VR
                     IF ( cDosRub_rec.DRUMONTAGE = 'AMOFIX' ) AND ( NVL( nDrfNbPeriode, 0 ) != 0 ) THEN
                        nDrfMt := Round( nDrfMt / nDrfNbPeriode, F_NbDecimalDansDevise( contrat_rec.DEVCODE ) );
                     END IF;
                     IF ( nDrfMt >= cDosRub_rec.DRUMTRESIDUEL ) THEN
                        RAISE err_parm10;
                     END IF;
                  END IF;
               --HN 07/05/03
               ELSIF sControle IN ( 'R0242','R0248', 'R0250') THEN
                  nCount :=0 ;
                  FOR cRubDosAssDetailR IN cRubDosAssDetail LOOP

										IF sControle ='R0242' and cDosRub_rec.DRUTYPE != 'P' THEN -- CFS 24927
                     	SELECT COUNT(DRD.DRDORDRE),SUM (DRD.DRDMT)
                     	INTO  nCount, nMontant
                     	FROM  DOSRUBASSDETAIL DRD
                     	WHERE DRD.DOSID = contrat_rec.DOSID
                     	AND   DRD.DRUORDRE = cRubDosAssDetailR.DRUORDRE
                     	AND   DRD.DRADT = cRubDosAssDetailR.DRADT;
                     	if nCount !=0 THEN
                     	   if nMontant != cRubDosAssDetailR.DRAMT THEN
                     	      lOk                := 0;
                     	      nb_element         := nb_element + 1;
		               	      nOrdre             := nb_element;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := cRubDosAssDetailR.DRUORDRE;
                     	      aType(nb_element)  := 'N';
                     	      aDec(nb_element)   := NULL;
                     	      nb_element         := nb_element + 1;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := TO_CHAR(cRubDosAssDetailR.DRADT, 'YYYYMMDD' );
                     	      aType(nb_element)  := 'D';
                     	      aDec(nb_element)   := NULL;
                     	   END if;
                     	END if;
                   	ELSIF sControle ='R0248' THEN
                   		IF cDosRub_rec.DRUDTDEB >= NVL(contrat_rec.DOSDTDEB,contrat_rec.DOSDTEFFET)
                   			AND  cRubDosAssDetailR.DRADT < NVL(contrat_rec.DOSDTDEB,contrat_rec.DOSDTEFFET) THEN
                     	      lOk                := 0;
                     	      nb_element         := nb_element + 1;
		               	      nOrdre             := nb_element;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := cRubDosAssDetailR.DRUORDRE;
                     	      aType(nb_element)  := 'N';
                     	      aDec(nb_element)   := NULL;
                     	      nb_element         := nb_element + 1;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := TO_CHAR(cRubDosAssDetailR.DRADT, 'YYYYMMDD' );
                     	      aType(nb_element)  := 'D';
                     	      aDec(nb_element)   := NULL;
                   		END IF;
                  	ELSIF sControle ='R0250' THEN
                  		IF cDosRub_rec.DRUCLASSE = 'F' THEN
                  			IF cRubDosAssDetailR.DRANBDAYCLIENT > cRubDosAssDetailR.DRANBDAYSUPPLIER THEN
                  				 lOk                := 0;
                     	      nb_element         := nb_element + 1;
		               	        nOrdre             := nb_element;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := cRubDosAssDetailR.DRANBDAYCLIENT;
                     	      aType(nb_element)  := 'N';
                     	      aDec(nb_element)   := NULL;
                     	      nb_element         := nb_element + 1;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := cRubDosAssDetailR.DRANBDAYSUPPLIER;
                     	      aType(nb_element)  := 'N';
                     	      aDec(nb_element)   := NULL;
                     	      nb_element         := nb_element + 1;
                     	      aOrdre(nb_element) := nOrdre;
                     	      aMsg(nb_element)   := cRubDosAssDetailR.DRUORDRE;
                     	      aType(nb_element)  := 'N';
                     	      aDec(nb_element)   := NULL;
                  			END IF;
                  		END IF;
                  	END IF;
                   END LOOP;

               --HN 12/08/03 VR <= assiette brute financiere  MSG: 7312
               ELSIF sControle = 'R0243' AND cDosRub_rec.DRUCLASSE = 'F' THEN
                  if cDosRub_rec.DRUMTORIGINE < cDosRub_rec.DRUMTRESIDUEL THEN
                     RAISE err_parmVR;
                  END if;
               -- Montant du premier loyer superieur au seuil defiscalisation
               ELSIF   sControle = 'R0246' THEN
                  -- Montant de l'investissement
                  BEGIN
                     SELECT DFIMTTOTAL
                     INTO   nDfiMtTotal
                     FROM   DOSFINANCE
                     WHERE  DOSID = contrat_rec.DOSID
                     AND    DFIORDRE = (SELECT MAX(DFIORDRE) FROM DOSFINANCE WHERE DOSID = contrat_rec.DOSID);
                  EXCEPTION
                     WHEN OTHERS THEN
                        nDfiMtTotal  := 0 ;
                  END ;
                  -- Montant du premier loyer
                  BEGIN
                     SELECT f_PlMtFirstLoyer(contrat_rec.DOSID ,contrat_rec.ACTID,contrat_rec.DOSDTDEB,contrat_rec.DEVCODE)
                     INTO nMtFirstLoyer
                     FROM DUAL ;
                  EXCEPTION
                     WHEN OTHERS THEN
                        nMtFirstLoyer  := 0 ;
                  END ;
                  if nAopNombre IS NOT NULL THEN
                     if  ( nMtFirstLoyer > ( nDfiMtTotal * (nAopNombre/100)) ) THEN
                        lOk := 0 ;
                        nb_element         := nb_element + 1;
		                  nOrdre             := nb_element;
                        aOrdre(nb_element) := nOrdre;
                        aMsg(nb_element)   := nAopNombre;
                        aType(nb_element)  := 'N';
                        aDec(nb_element)   := NULL;
                     END if;
                  END if;
					ELSIF sControle = 'R0247' THEN
						SELECT 	MIN(DRUORDRE)
         			INTO     nDruOrdre
         			FROM 		DOSRUBRIQUE
         			WHERE 	DOSID = contrat_rec.DOSID
         			AND 		F_ISRUBIDONFILTRE(RUBID, 'SUBV') = 1
         			AND      DRUORDRE IN (	SELECT 	DRUORDRE
         											FROM 		LKDOSRUBPOOACT
         											WHERE 	DOSID = contrat_rec.DOSID
         											AND 		DPADTFIN IS NULL );

         			FOR cLkDosRubPooActOne_rec IN cLkDosRubPooActOne LOOP

         				FOR cDosRub2_rec IN cDosRub2 LOOP
         					IF F_ISRUBIDONFILTRE( cDosRub2_rec.RUBID, 'SUBV' ) = 1 THEN
         						SELECT 	COUNT(*)
         						INTO     nCount
         						FROM 		LKDOSRUBPOOACT
         						WHERE 	DOSID = contrat_rec.DOSID
         						AND 		DRUORDRE = cDosRub2_rec.DruOrdre
         						AND 		DPADTFIN IS NULL;

         						IF nCount > 0 THEN

         							nDruOrdre := cDosRub2_rec.DruOrdre;

         							FOR cLkDosRubPooAct_rec IN cLkDosRubPooAct LOOP
	         							IF cLkDosRubPooAct_rec.DRUORDRE != nDruOrdre AND cLkDosRubPooAct_rec.POOID = cLkDosRubPooActOne_rec.POOID
		         							AND cLkDosRubPooAct_rec.ACTID = cLkDosRubPooActOne_rec.ACTID THEN
		   		      					IF cLkDosRubPooAct_rec.DPAQP != cLkDosRubPooActOne_rec.DPAQP
   				      							OR NVL(cLkDosRubPooAct_rec.DPAMARGEFINALE,0) != NVL(cLkDosRubPooActOne_rec.DPAMARGEFINALE,0) THEN
   	   			   							lOk := 0;
   	      										EXIT;
		   	      						END IF;
      		   						END IF;
         							END LOOP;
         						ELSE
         							lOk := 0;
   		      					EXIT;
   	      					END IF;
	         					IF lOk = 0 THEN
         							EXIT;
         						END IF;
         					END IF;
         				END LOOP;
         				IF lOk = 0 THEN
         					EXIT;
         				END IF;
         			END LOOP;
					ELSIF sControle = 'R0249' THEN
         	   	FOR cDosRub2_rec IN cDosRub2 LOOP
         	   		IF cDosRub2_rec.DRUCLASSE = 'F'
         	   						AND cDosRub2_rec.DRUTAUXFIXE IS NOT NULL
         	   						AND cDosRub2_rec.DRUTAUXCALC IS NOT NULL THEN
         	   			nEpsilon   := pa_fungencontrole.GetEpsilonParam( 'TAUXCALC' );

         	   			SELECT 	COUNT(1)
         	   			INTO 	nCount
         	   			FROM 	DOSRUBASSIETTE
         	   			WHERE   DOSID = contrat_rec.DOSID
         	   			AND     DRUORDRE = cDosRub2_rec.DRUORDRE
         	   			AND     DRATYPEVERSEMENT = 'BONIF';

         	   			IF nCount = 0 THEN
         	   			   SELECT  COUNT(1)
         	   			   INTO    nCount
         	   			   FROM    DOSRUBASSDETAIL
         	   			   WHERE   DOSID = contrat_rec.DOSID
         	   			   AND     DRUORDRE = cDosRub2_rec.DRUORDRE
         	   			   AND     DRDTYPEVERSEMENT = 'BONIF';
         	   			END IF;

         	   			IF nCount = 0 THEN

         	   			IF NVL(ABS(cDosRub2_rec.DRUTAUXFIXE - cDosRub2_rec.DRUTAUXCALC), 0) > ABS( nEpsilon) THEN
                        	lOk                := 0;
                           nb_element         := nb_element + 1;
		                     nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub2_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub2_rec.DRUTAUXFIXE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub2_rec.DRUTAUXCALC;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := nEpsilon;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                     	END IF;
         	   		END IF;
							END IF;
         	   	END LOOP;
               --LG 09/04/02
               ELSIF sControle = 'R0161' THEN
                  FOR cBien_rec IN cBien LOOP
                     BEGIN
                        IF ( cBien_rec.ACTIDFRS IS NULL ) THEN
                           lOk                := 0;
                           nb_element         := nb_element + 1;
		                     nOrdre             := nb_element;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cBien_rec.IRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := F_StdTrimAll( cBien_rec.ITRNUM );
                           aType(nb_element)  := 'C';
                           aDec(nb_element)   := NULL;
                           nb_element         := nb_element + 1;
                           aOrdre(nb_element) := nOrdre;
                           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                           aType(nb_element)  := 'N';
                           aDec(nb_element)   := NULL;
                        END IF;
                     END;
                  END LOOP;

               --LG 09/04/02
               ELSIF sControle = 'R0162' AND cDosRub_rec.DRUCP IS NOT NULL THEN
                  SELECT COUNT(*)
                  INTO   nAny
                  FROM   DOSRUBRIQUE DRU,
                         DOSPHASE DPH
                  WHERE  DRU.DOSID    != contrat_rec.DOSID
                    AND  DRU.DRUCP     = cDosRub_rec.DRUCP
                    AND  DPH.DOSID     = DRU.DOSID
                    AND  DPH.PHACODE  != 'TER'
                    AND  DPH.DPHDTFIN IS NULL;
                  IF ( nAny != 0 ) THEN
                     lOk                := 0;
		               nb_element         := nb_element + 1;
                     nOrdre             := nb_element;
 	                  aOrdre(nb_element) := nOrdre;
		               aMsg(nb_element)   := F_StdTrimAll( cDosRub_rec.DRUCP );
		               aType(nb_element)  := 'C';
		               aDec(nb_element)   := NULL;
		               nb_element         := nb_element + 1;
 	                  aOrdre(nb_element) := nOrdre;
		               aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
		               aType(nb_element)  := 'N';
		               aDec(nb_element)   := NULL;
                  END IF;
               ELSIF ( sControle = 'R0239' ) AND ( cDosRub_rec.DRUCLASSE   = 'F' )
                                             AND ( cDosRub_rec.DRUTYPE     = 'F' )
                                             AND ( cDosRub_rec.DRUTXNATURE = 'TF' ) THEN
                  IF ( nTegMiniOption IS NULL ) THEN
                     PA_SELECTDOSSIER2.S_TEGMINIOPTION( contrat_rec.ACTID, nTegMiniOption );
                     IF ( nTegMiniOption = 1 ) THEN
                        PA_SELECTDOSSIER2.S_TEGMINIRUBRIQUE(
                           contrat_rec.ACTID,
                           TRUNC(SYSDATE),
                           nTegTauxVal,
                           sTegTauxType,
                           nTegUnitePeriode,
                           nTegMultiple );
                     END IF;
                  END IF;
                  IF ( nTegMiniOption = 1 ) THEN
                     IF ( cDosRub_rec.DRUTXTYPE != sTegTauxType ) OR ( cDosRub_rec.DRUPERREFTAUX  != LPAD(nTegUnitePeriode,3,'0') )
                                                                  OR ( cDosRub_rec.DRUMULTREFTAUX != nTegMultiple ) THEN
                        RAISE err_parm1;
                     END IF;
                  ELSE
                     EXIT;
                  END IF;
               ELSIF ( sControle = 'R0164' ) AND ( cDosRub_rec.DRUORDREMAITRE IS NOT NULL )
                                             AND ( NVL( cDosRub_rec.DRUFLAGCUMUL, 0 ) = 0 )
                                             AND ( F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'REGROUP' ) = 1 ) THEN
                  RAISE err_parm1;
               ELSIF ( sControle = 'R0167' ) AND ( cDosRub_rec.DACORDRE IS NOT NULL ) THEN
                  SELECT DAC.DACDTDEB, DAC.DACDTFIN, ACT.ACTLIBCOURT
                  INTO   dtDacDtDeb, dtDacDtFin, sLibCourt
                  FROM   DOSACTEUR DAC,
                         ACTEUR ACT
                  WHERE  DAC.DOSID    = contrat_rec.DOSID
                    AND  DAC.DACORDRE = cDosRub_rec.DACORDRE
                    AND  ACT.ACTID    = DAC.ACTID;

                  IF ( cDosRub_rec.DRUDTFIN IS NOT NULL ) THEN
                     dtFinFlux := cDosRub_rec.DRUDTFIN;
                  ELSE
                     SELECT MAX(DRFDTFIN)
                     INTO   dtFinFlux
                     FROM   DOSRUBFLUX
                     WHERE  DOSID    = contrat_rec.DOSID
                       AND  DRUORDRE = nDruOrdre;
                  END IF;
                  -- On verifie si l'acteur n'a pas remplace un autre acteur si c'est
                  -- le cas on ne peut pas verifier par rapport a sa date debut
                  SELECT COUNT(1)
                  INTO   nCount
                  FROM   DOSACTEUR
                  WHERE  DOSID            = contrat_rec.DOSID
                    AND  DACORDREREMPLACE = cDosRub_rec.DACORDRE;
                  IF ( nCount = 0 ) THEN
                     IF ( dtDacDtDeb > cDosRub_rec.DRUDTDEB ) THEN
                        sCode := sLibCourt;
                        RAISE err_parm3;
                     END IF;
                  END IF;
                  IF ( dtDacDtFin IS NOT NULL ) AND ( dtFinFlux  IS NOT NULL )
                                                AND ( dtDacDtFin  < dtFinFlux ) THEN
                     sCode := sLibCourt;
                     RAISE err_parm3;
                  END IF;
               -- CV-03082004 FSA-15608 MSG8149 : Reconduction
               ELSIF ( sControle = 'R0168' ) AND contrat_rec.TACCODE IN ( 'CBM', 'LOCFIN' ) AND ( cDosRub_rec.DRUTYPE = 'F' ) AND
               	cDosRub_rec.DRUFLAGRENOUVAUTO = 1 THEN
               	SELECT	COUNT(*)
               	INTO     nCount
						FROM 		TOPPARAM
						WHERE 	TOPTABLE = 'PROLONGATION'
						AND      TPAPARAM = 'MODIFICATION';
						IF nCount > 0 THEN
							IF cDosRub_rec.DRUMETHODERECOND IS NULL AND cDosRub_rec.RUBIDRECOND IS NULL OR cDosRub_rec.DRUPERIODICITERECOND IS NULL OR cDosRub_rec.DRUMULTIPLERECOND IS NULL OR
								cDosRub_rec.DRUMTRECOND IS NULL OR cDosRub_rec.DRUNBPERIODERECOND IS NULL THEN
							   IF cDosRub_rec.DRUMETHODERECOND != 'EXTVR' THEN
                    			RAISE err_parm1;
                    		ELSIF cDosRub_rec.DRUMETHODERECOND IS NULL OR cDosRub_rec.RUBIDRECOND IS NULL OR cDosRub_rec.DRUPERIODICITERECOND IS NULL OR cDosRub_rec.DRUMULTIPLERECOND IS NULL THEN
                    			RAISE err_parm1;
							   END IF;
							END IF;
						END IF;
					-- CV-04082004 FSA-15871 MSG8150 : S'il existe Rub.Red avec filtre ASSUR, acteur classe role ASSUR obligatoire
					ELSIF ( sControle = 'R0169' ) AND cDosRub_rec.DRUTYPE = 'F' AND F_ISRUBIDONFILTRE( cDosRub_rec.RUBID, 'ASSUR' ) = 1 THEN
						SELECT COUNT(*)
						INTO   nCount
						FROM   DOSACTEUR
						WHERE  DOSID                    = contrat_rec.DOSID
						AND    F_PLROLEEXTERNE(ROLCODE) = 'ASSUR'
						AND    ( DACDTFIN IS NULL
						OR     DACDTFIN                >= contrat_rec.DOSDTFIN );
						IF nCount = 0 THEN
               		RAISE err_parmTiers;
						END IF;
               ELSIF ( sControle = 'R0189' ) AND cDosRub_rec.DRUORDREPREC IS NOT NULL THEN
                     BEGIN
                        SELECT DRUTYPE, DRUCLASSE
                        INTO   sDruType, sDruClasse
                        FROM   DOSRUBRIQUE
                        WHERE  DOSID    = contrat_rec.DOSID
                        AND    DRUORDRE = cDosRub_rec.DRUORDREPREC;
                     EXCEPTION WHEN OTHERS THEN
                        sDruType   := NULL;
                        sDruClasse := NULL;
                     END;

                     IF cDosRub_rec.DRUTYPE != sDruType OR cDosRub_rec.DRUCLASSE != sDruClasse THEN
                        lOk := 0;
                        nb_element   	    := nb_element + 1;
      		            nOrdre	          := nb_element;
      		            aOrdre(nb_element) := nOrdre;
      		            aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
      		            aType(nb_element)  := 'N';
      		            aDec(nb_element)   := 0;
      		            nb_element   	     := nb_element + 1;
      		            aOrdre(nb_element) := nOrdre;
      		           aMsg(nb_element)   := cDosRub_rec.DRUORDREPREC;
      		           aType(nb_element)  := 'N';
      		           aDec(nb_element)   := 0;
                    END if;
               END IF;
            END IF;

            EXCEPTION
               WHEN err_parm1 THEN
                  lOk                := 0;
                  IF ( sControle IN ( 'R0175', 'R0176' ) ) THEN
                     nOrdre          := nb_element;
                     nb_element      := nb_element + 1;
                  ELSE
                     nb_element      := nb_element + 1;
                     nOrdre          := nb_element;
                  END IF;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parm2 THEN
                  lOk          	     := 0;
		            nb_element   	     := nb_element + 1;
		            nOrdre	     := nb_element;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := TO_CHAR(dDate1,'YYYYMMDD');
		            aType(nb_element)  := 'D';
		            aDec(nb_element)   := NULL;
		            nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		           aMsg(nb_element)   := TO_CHAR(dDate2,'YYYYMMDD');
		           aType(nb_element)  := 'D';
		           aDec(nb_element)   := NULL;
		           nb_element   	     := nb_element + 1;
		           aOrdre(nb_element) := nOrdre;
		           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
		           aType(nb_element)  := 'N';
		           aDec(nb_element)   := NULL;
               WHEN err_parmv THEN
                  lOk          	     := 0;
		            nb_element   	     := nb_element + 1;
		            nOrdre             := nb_element;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := contrat_rec.DEVCODE;
		            aType(nb_element)  := 'C';
		            aDec(nb_element)   := NULL;
      	         nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := sLibCourt;
		            aType(nb_element)  := 'C';
		            aDec(nb_element)   := NULL;
      	         nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := sActDevCode;
		            aType(nb_element)  := 'C';
		            aDec(nb_element)   := NULL;
		            nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
     	            aType(nb_element)  := 'N';
		            aDec(nb_element)   := NULL;
               WHEN err_parm3 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
		            nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := F_StdTrimAll(sCode);
		            aType(nb_element)  := 'C';
		            aDec(nb_element)   := NULL;
               WHEN err_parm7 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := 'LANTTRPARAM|CONTRATMONTAGE|' || cDosRub_rec.DRUTYPEMONTAGE;
                  aType(nb_element)  := 'S';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := 'LANTTRPARAM|MODEAMOFIN|' || cDosRub_rec.DRUMONTAGE;
                  aType(nb_element)  := 'S';
                  aDec(nb_element)   := NULL;
               WHEN err_parm8 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := 'LANTTRPARAM|MODEAMOFIN|' || cDosRub_rec.DRUMONTAGE;
                  aType(nb_element)  := 'S';
                  aDec(nb_element)   := NULL;
               WHEN err_parm9 THEN
                  lOk                := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := 'LANTTRPARAM|CONTRATMONTAGE|' || cDosRub_rec.DRUTYPEMONTAGE;
                  aType(nb_element)  := 'S';
                  aDec(nb_element)   := NULL;
               WHEN err_parma THEN
                  lOk          	     := 0;
                  nb_element   	     := nb_element + 1;
                  nOrdre	     := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUMTORIGINE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nAdosse;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
                  nb_element   	     := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parmb THEN
                  lOk          	     := 0;
		           nb_element   	     := nb_element + 1;
		           nOrdre	     := nb_element;
		           aOrdre(nb_element) := nOrdre;
		           aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
		           aType(nb_element)  := 'N';
		           aDec(nb_element)   := NULL;
		           nb_element   	     := nb_element + 1;
		           aOrdre(nb_element) := nOrdre;
		           aMsg(nb_element)   := TO_CHAR(dDate1,'YYYYMMDD');
		           aType(nb_element)  := 'D';
		           aDec(nb_element)   := NULL;
               WHEN err_parmy THEN
                  lOk          	     := 0;
	               nb_element   	     := nb_element + 1;
		            nOrdre	     := nb_element;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := nAssEmpR;
        	         aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( sDevCode );
	               nb_element   	     := nb_element + 1;
         		   aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := nAdosse;
                  aType(nb_element)  := 'N';
        	         aDec(nb_element)   := F_NbDecimalDansDevise( sDevCode );
		            nb_element   	     := nb_element + 1;
		            aOrdre(nb_element) := nOrdre;
		            aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
         		  aType(nb_element)  := 'N';
         		  aDec(nb_element)   := NULL;
         		  nb_element   	     := nb_element + 1;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := sDosNum;
         		  aType(nb_element)  := 'C';
         		  aDec(nb_element)   := NULL;
               WHEN err_taux THEN
                  lOk          	     := 0;
	                nb_element   	     := nb_element + 1;
         		  nOrdre	     := nb_element;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
              	  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
   	             nb_element   	     := nb_element + 1;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := cDosRub_rec.DRUTAUXFIXE;
                  aType(nb_element)  := 'N';
        	        aDec(nb_element)   := 6;
         		  nb_element   	     := nb_element + 1;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := cDosRub_rec.DRUTAUXNOMINAL;
         		  aType(nb_element)  := 'N';
         		  aDec(nb_element)   := 6;
         		  nb_element   	     := nb_element + 1;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := nAny;
         		  aType(nb_element)  := 'N';
         		  aDec(nb_element)   := 3;
               WHEN err_parmz THEN
                  lOk          	     := 0;
      	          nb_element   	     := nb_element + 1;
         		  nOrdre	     := nb_element;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := nMtRubO;
        	        aType(nb_element)  := 'N';
                 aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
      	          nb_element   	     := nb_element + 1;
		           aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := nAdosse;
                  aType(nb_element)  := 'N';
        	        aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
         		  nb_element   	     := nb_element + 1;
         		  aOrdre(nb_element) := nOrdre;
         		  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
         		  aType(nb_element)  := 'N';
         		  aDec(nb_element)   := NULL;
               WHEN err_parm10 THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUMTRESIDUEL;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
                  nb_element         := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := nDrfMt;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := F_NbDecimalDansDevise( contrat_rec.DEVCODE );
                  nb_element         := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parm11 THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDREMAITRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element         := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
               WHEN err_parmVR then
                   lOk := 0;
                   nb_element         := nb_element + 1;
		             nOrdre             := nb_element;
                   aOrdre(nb_element) := nOrdre;
                   aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                   aType(nb_element)  := 'N';
                   aDec(nb_element)   := NULL;
               -- CV-04082004 FSA-15871
               WHEN err_parmTiers THEN
                   lOk                := 0;
                   nb_element         := nb_element + 1;
		             nOrdre             := nb_element;
                   aOrdre(nb_element) := nOrdre;
                   aMsg(nb_element)   := 'LANROLE|ROLCODE|' || 'ASSUR' ;
                   aType(nb_element)  := 'S';
                   aDec(nb_element)   := NULL;
               --LG FSA 19002 -- NL -- 22/03/05
               WHEN err_parm12 THEN
                  lOk                := 0;
                  nb_element         := nb_element + 1;
                  nOrdre             := nb_element;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDRE;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;
                  nb_element         := nb_element + 1;
                  aOrdre(nb_element) := nOrdre;
                  aMsg(nb_element)   := cDosRub_rec.DRUORDREPREC;
                  aType(nb_element)  := 'N';
                  aDec(nb_element)   := NULL;

            END;
           END LOOP;
         END IF;
         RETURN lOk;
      END;
	END ocDossRubGen;

PROCEDURE GetTypeImposition (
      nDosId   IN DOSSIER.DOSID%TYPE,
      sTacCode IN DOSSIER.TACCODE%TYPE,
      sTaxType IN OUT  TAXE.TAXTYPE%TYPE,
      nTrouve IN OUT NUMBER)  AS
BEGIN
   DECLARE
      nCount NUMBER;
      sTaxType2   TAXE.TAXTYPE%TYPE;
   BEGIN
      IF sTacCode = 'LOCATIF' THEN
         BEGIN
            SELECT DISTINCT TAXTYPE
            INTO   sTaxType
            FROM   TAXE
            WHERE  TAXCODE IN (SELECT TAXCODE FROM DOSRUBRIQUE
                             WHERE DOSID = nDosId
                             AND   TAXCODE != 'MIXTE'
                             AND   DRUCLASSE = 'A'
                             AND   RUBID IN (SELECT RUBID FROM RUBACCES
                                             WHERE RACACCES = 'ACCLOY'))
             GROUP BY TAXTYPE;
            nTrouve:=1;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               nTrouve:=0;
               sTaxType:=NULL;
            WHEN OTHERS THEN
               nTrouve:=2;
               sTaxType:=NULL;
         END;
         nCount:=0;
         BEGIN
            SELECT DISTINCT TAXTYPE
            INTO   sTaxType2
            FROM   TAXE
            WHERE  TAXCODE IN ( SELECT TAXCODE FROM DOSRUBTVATAXE
                               WHERE DOSID = nDosId
                               AND  (DOSID, DRUORDRE) IN (SELECT DOSID, DRUORDRE FROM DOSRUBRIQUE
                                                         WHERE DOSID = nDosId
                                                         AND   TAXCODE = 'MIXTE'
                                                         AND   DRUCLASSE = 'A'
                                                         AND   RUBID IN (SELECT RUBID FROM RUBACCES
                                                                           WHERE RACACCES = 'ACCLOY'))
                             )
            GROUP BY TAXTYPE;
            nCount:=1;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               nCount:=0;
               sTaxType2:=NULL;
            WHEN OTHERS THEN
               nCount:=2;
               sTaxType2:=NULL;
         END;
         IF nCount=2 OR (nCount=1 AND sTaxType != sTaxType2) THEN
               nTrouve:=2;
               sTaxType:=NULL;
         END IF;
      ELSE
         -- CBI et CBMIXTE
         BEGIN
            SELECT DISTINCT TAXTYPE
            INTO   sTaxType
            FROM   TAXE
            WHERE  TAXCODE IN (SELECT TAXCODE FROM DOSRUBRIQUE
                             WHERE DOSID = nDosId
                             AND   TAXCODE != 'MIXTE'
                             AND   DRUTYPE= 'F'
                             AND   DRUCLASSE = 'F')
             GROUP BY TAXTYPE;
            nTrouve:=1;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               nTrouve:=0;
               sTaxType:=NULL;
            WHEN OTHERS THEN
               nTrouve:=2;
               sTaxType:=NULL;
         END;
         nCount:=0;
         BEGIN
            SELECT DISTINCT TAXTYPE
            INTO   sTaxType2
            FROM   TAXE
            WHERE  TAXCODE IN ( SELECT TAXCODE FROM DOSRUBTVATAXE
                               WHERE DOSID = nDosId
                               AND  (DOSID, DRUORDRE) IN (SELECT DOSID, DRUORDRE FROM DOSRUBRIQUE
                                                         WHERE DOSID = nDosId
                                                         AND   TAXCODE = 'MIXTE'
                                                        AND   DRUTYPE= 'F'
                                                        AND   DRUCLASSE = 'F' )     )
            GROUP BY TAXTYPE;
            nCount:=1;
         EXCEPTION
            WHEN NO_DATA_FOUND THEN
               nCount:=0;
               sTaxType2:=NULL;
            WHEN OTHERS THEN
               nCount:=2;
               sTaxType2:=NULL;
         END;
         IF nCount=2 OR (nCount=1 AND sTaxType != sTaxType2) THEN
               nTrouve:=2;
               sTaxType:=NULL;
         END IF;
      END IF;
   END;
END GetTypeImposition;

END PA_DOSRUBCONTROLE2;