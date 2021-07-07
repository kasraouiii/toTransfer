--------------------------------------------------------
--  Fichier cr�� - mercredi-juillet-07-2021   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for View V_PROPOSITION_2
--------------------------------------------------------

  CREATE OR REPLACE FORCE EDITIONABLE VIEW "V_PROPOSITION_2" ("UTI_Impression", "PATH LOGO", "Nom SG", "Capital SG", "Voie SG", "LIBTPGCODE", "CodePoste SG", "Lieu SG", "Ville SG", "CPOSUBREGION SG", "NumRCM SG", "VILLERC SG", "SIRET SG", "Titre DG SG", "PRENOM DG SG", "NOM DG SG", "Titre DGA SG", "PRENOM DGA SG", "NOM DGA SG", "Titre DRC SG", "PRENOM DRC SG", "NOM DRC SG", "Titre DR SG", "PRENOM DR SG", "NOM DR SG", "DOSID", "Date du jour", "HEUREJOUR", "TLCATJUR", "DATETMM", "PECTX", "TXTMM", "Code Contrat", "Code numcassiopae", "DDPRVERSION", "MTINVEST", "MTVR", "FRAIS_DOSSIER", "VRPOURCENT", "Ref Titre credit", "DUREELOYER", "MTFRAIS ETUDE", "TERME", "CRTDUREE", "PERIODICITE", "TPGCODE", "JALCODE", "OBJETFINANCE", "DTCOMITE", "MTCREDIT", "TFRAE", "VR", "TEG", "TXCORRIGE", "REMBMT", "REMBMT en Lettre", "REMBMT2", "REMBMT en Lettre2", "TLCCNOM", "MTASSURANCE", "DEPOTGARANTIE", "ASSPERIODICITE", "TACCODE", "DEMANDEFIN", "SUM_TTC", "sum ttc en Lettre", "SUM_TTC2", "sum ttc2 en Lettre", "DEALTYPE", "SOUMIS_COMPAGNE", "DPRCONCLUSION", "DERNIERE_DECISION", "NOM_AGENCE", "DT_DERNIERE_DECISION", "LABELLE_BAREME", "LAD_DOSSIER", "DATE_CREATION", "NOM_CETUDE", "NOM_COMMERCIAL", "NOM_RECOUVREUR", "RATIO_GROUP_CLT", "RATIO_STARWEB_RETENU", "ENGAGEMENY_SGMA_IMPAYE", "AUTRENGAGEMENT_IMPAYE", "ENGAGEMENTSGLM_CLT_IMPAYE", "ENGAGEMENTSGLM_GRP_IMPAYE", "DEVISE", "DUREE_MOIS", "MT_PREMIER_LOYER_HT", "MT_FRAIS_DOS", "Commission_engagement", "MT_AUTRE_FRAIS", "LIBELLE_ACTIV", "Nature_MAT", "CA CLIENT", "Mt_avance", "Test Env", "Commite", "NOM COM FICHEP") AS 
  SELECT 
------------------------------------SG---------------------
PAV4_JASPER_FO.F_GET_UTI_IMPRESSION (DPR.DOSID) "UTI_Impression",
PAV4_JASPER_FO.F_GET_PATH_LOGO() "PATH LOGO" ,
       ACTSG.ACTNOM AS "Nom SG",
       ACTSG.ACTCAPITAL AS "Capital SG",
       NVL (ADR.ADRVOIE,' ') AS "Voie SG",
       NVL (
          (SELECT MAX (TPGLIBELLE)
             FROM LANTPROFILGESTION
            WHERE     tpgcode = (SELECT MAX (TPGCODE) FROM dossierprospect WHERE DOSID = DPR.DOSID)
                  AND lancode = 'FR'
                  ),
          ' ')
          AS "LIBTPGCODE",
       NVL (ADR.ADRCODEPOST,' ') AS "CodePoste SG",
       NVL (ADR.ADRLIEUDIT, ' ') AS "Lieu SG",
      NVL (ADR.ADRville,' ') AS "Ville SG",
       NVL (COP.CPOSUBREGION,' ')   AS "CPOSUBREGION SG",
       NVL (ACTSG.ACTNUMRCM,' ')   AS "NumRCM SG",
       NVL (pav4_jasper_fo.F_GET_CC_ACT(ACTSG.ACTID,'TFDCCHSID1594'),' ')  AS "VILLERC SG",
       NVL (ACTSG.ACTSIRET,' ') AS "SIRET SG",
       NVL ((SELECT DECODE(ACT.ACOTITRE,'MR', 'M.','M', 'M.','MME', 'MME','MLLE', 'MLLE','DR', 'Dr.','ME', 'ME.',' ') FROM ACTCORRESPONDAnT Act
             WHERE actid = ACTSG.ACTID AND ACOQUALITE = 6 AND ACODTEND IS NULL),' ') AS "Titre DG SG",
       NVL ( (SELECT ACT.ACOPRENOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 6 AND ACODTEND IS NULL),' ')   AS "PRENOM DG SG",
       NVL ( (SELECT ACT.ACONOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 6 AND ACODTEND IS NULL),' ')  AS "NOM DG SG",
       NVL ( (SELECT DECODE(ACT.ACOTITRE,'MR', 'M.','M', 'M.','MME', 'MME','MLLE', 'MLLE','DR', 'Dr.','ME', 'ME.',' ') FROM ACTCORRESPONDAnT Act
              WHERE actid = ACTSG.ACTID AND ACOQUALITE = 7 AND ACODTEND IS NULL),' ') AS "Titre DGA SG",
       NVL ( (SELECT ACT.ACOPRENOM FROM ACTCORRESPONDAnT Act  WHERE actid = ACTSG.ACTID AND ACOQUALITE = 7 AND ACODTEND IS NULL),' ') AS "PRENOM DGA SG",
       NVL ( (SELECT ACT.ACONOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 7 AND ACODTEND IS NULL),' ') AS "NOM DGA SG",
       NVL ((SELECT DECODE(ACT.ACOTITRE,'MR', 'M.','M', 'M.','MME', 'MME','MLLE', 'MLLE','DR', 'Dr.','ME', 'ME.',' ') FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 26 AND ACODTEND IS NULL),' ') AS "Titre DRC SG",
       NVL ( (SELECT ACT.ACOPRENOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 26 AND ACODTEND IS NULL),' ') AS "PRENOM DRC SG",
       NVL ( (SELECT ACT.ACONOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 26 AND ACODTEND IS NULL),' ')  AS "NOM DRC SG",
       NVL ((SELECT DECODE(ACT.ACOTITRE,'MR', 'M.','M', 'M.','MME', 'MME','MLLE', 'MLLE','DR', 'Dr.','ME', 'ME.',' ') FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE =64 AND ACODTEND IS NULL),' ') AS "Titre DR SG",
       NVL ( (SELECT ACT.ACOPRENOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 64 AND ACODTEND IS NULL),' ') AS "PRENOM DR SG",
       NVL ( (SELECT ACT.ACONOM FROM ACTCORRESPONDAnT Act WHERE actid = ACTSG.ACTID AND ACOQUALITE = 64 AND ACODTEND IS NULL),' ')  AS "NOM DR SG",
       DPR.DOSID AS "DOSID",
       TO_CHAR (SYSDATE, 'DD/MM/YYYY') AS "Date du jour",
       TO_CHAR( SYSDATE, 'HH24:MI:SS' ) HEUREJOUR,
       ACTSG.CJUCODE  AS "TLCATJUR",
       NVL ( (SELECT TO_CHAR (TVADTVIGUEUR, 'DD/MM/YYYY') FROM TAUVALEUR  WHERE     taucode = 'TV'  AND TVADTVIGUEUR = (SELECT MAX (TVADTVIGUEUR) FROM TAUVALEUR WHERE taucode = 'TV')),
            SYSDATE) AS "DATETMM",
   NVL (PAV4_JASPER_FO.F_PECTX (DPR.DOSID, DPR.DPRVERSION), 0) AS "PECTX",
       NVL ( (SELECT TVAVAL FROM TAUVALEUR  WHERE     taucode = 'TV' AND TVADTVIGUEUR = (SELECT MAX (TVADTVIGUEUR)
                                           FROM TAUVALEUR  WHERE taucode = 'TV')), 0)  AS "TXTMM",
     DPR.DPRNUMERO AS "Code Contrat",
       NVL(DPR.dprnumcassiopee,' ') as "Code numcassiopae",
       DPR.DPRVERSION AS "DDPRVERSION",
       NVL (PAV4_JASPER_FO.F_MTINVEST (DPR.DOSID, DPR.DPRVERSION), 0) AS "MTINVEST",
       NVL (PAV4_JASPER_FO.F_MTVR (DPR.DOSID, DPR.DPRVERSION), 0) AS "MTVR",
       Nvl(PAV4_JASPER_FO.F_GET_TOT_MT_FRAIS_DOS (DOSID),0) Frais_DOSSIER ,
       NVL (PAV4_JASPER_FO.F_VRPOURCENT (DPR.DOSID, DPR.DPRVERSION), 0) AS "VRPOURCENT",
       PAV4_JASPER_FO.f_get_ref_tit_cred_front (DPR.DOSID) AS "Ref Titre credit",
       NVL (PAV4_JASPER_FO.F_GET_NB_LOYERS (DPR.DOSID), 0) AS "DUREELOYER",
       NVL (PAV4_JASPER_FO.F_MTFRAE (DPR.DOSID, DPR.DPRVERSION), 0) AS "MTFRAIS ETUDE",
       Nvl(PAV4_JASPER_FO.F_GET_TERME(DPR.DOSID),' ') AS "TERME",
       PA_REQUETE_FRONT_TOOL.F_FORMATNUMBERDECIMAL(DPRAPPROXDUREEAN, 0) CRTDUREE,
       DECODE ( (SELECT MIN (PECDTDUE)
             FROM PFIRUBECHEANCIER PFI, DPRPROPFINANCE DR
            WHERE     PFI.PFIID = DR.PFIID
                  AND DR.DOSID = DPR.DOSID
                  AND DR.DPRVERSION = DPR.DPRVERSION),'01', '1er', (SELECT MIN (PECDTDUE)
             FROM PFIRUBECHEANCIER PFI, DPRPROPFINANCE DR WHERE     PFI.PFIID = DR.PFIID  AND DR.DOSID = DPR.DOSID  AND DR.DPRVERSION = DPR.DPRVERSION))  AS "PERIODICITE",
       TPGCODE  TPGCODE,
       NVL((SELECT MAX(dph.jalcode) FROM  dprphase dph WHERE dph.dosid=DPR.DOSID AND DPR.dprversion=dph.dprversion AND dph.dphdtend is null),' ') AS JALCODE,
       NVL(PAV4_BO_TLG_FRONT.f_rech_comment_decision(DPR.DOSID ,'MOTIFI'),' ')OBJETFINANCE,
      (SELECT TO_CHAR( DDC.DDCDT, 'DD/MM/YYYY' ) FROM DPRDATECLE DDC WHERE DDC.DOSID=DPR.DOSID AND DDC.DPRVERSION=DPR.DPRVERSION AND DDC.DCLCODE='DTCOMM'
      AND DDC.DDCORDRE=(SELECT MAX(DDCORDRE) FROM DPRDATECLE WHERE DCLCODE='DTCOMM' AND DOSID=DDC.DOSID AND DPRVERSION=DDC.DPRVERSION) ) as DTCOMITE,
(SELECT max(PFIINVESTISSEMENT)  FROM PROPOSITIONFINANCIERE  WHERE PFIID=(SELECT max(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL ))from dual)) MTCREDIT ,
NVL((SELECT max(PFADOUBLE) FROM PFIATTRIBUT WHERE PFACODE='PFITEG' AND PFIID=(SELECT max(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL ))FROM DUAL)),0) TFRAE,
NVL((SELECT max(PFRVR)   FROM PFIRUBRIQUE  WHERE PFIID=(select MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL ))from dual)),0) as VR ,
nvl(replace(f_calcul_teg_fo(DPR.DOSID,'FR','ORFI'),',','.') ,0) as TEG
 ,NVL((SELECT MAX(PFADOUBLE) FROM PFIATTRIBUT WHERE PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual)AND PFACODE='PFITXAPPARENT'),0) TXCORRIGE,
 NVL((SELECT
Sum (PECMTBASIC) + SUM(PECMTINCIDENTAL) + SUM(PECMTTAX) FROM PFIRUBECHEANCIER WHERE PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual)),0)REMBMT,
  translate_fr ( to_word_en ( ROUND ( NVL((SELECT Sum (PECMTBASIC) + SUM(PECMTINCIDENTAL) + SUM(PECMTTAX) FROM PFIRUBECHEANCIER WHERE PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual)),0),3)))"REMBMT en Lettre",
 ROUND(NVL((SELECT
     SUM(PECMTBASIC) + Sum(PECMTTAX) +SUM(PECMTINCIDENTAL+F_GET_MTTIMBRE(DPR.DOSID))
 FROM PFIRUBECHEANCIER PFI WHERE PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual)AND pectypeelement='LOYER'and (pfpordre not in (select pfpordre from pfiprestation where pfiid=pfi.pfiid and rubcode='FRAE') OR PFPORDRE IS NULL)),0),3)
REMBMT2,
  translate_fr ( to_word_en ( ROUND ( NVL((SELECT
     SUM(PECMTBASIC) + Sum(PECMTTAX) +SUM(PECMTINCIDENTAL+F_GET_MTTIMBRE(DPR.DOSID))
 FROM PFIRUBECHEANCIER PFI WHERE PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual)AND pectypeelement='LOYER'and (pfpordre not in (select pfpordre from pfiprestation where pfiid=pfi.pfiid and rubcode='FRAE') OR PFPORDRE IS NULL)),0),3)))"REMBMT en Lettre2",
  NVL((PAV4_JASPER_FO.F_GET_NOM_PORTFL_COM(DPR.DOSID,DPR.DPRVERSION)),' ') "TLCCNOM" ,
    NVL( (SELECT PA_REQUETE_FRONT_TOOL.F_FORMATNUMBERDECIMAL(MAX(PFI.PFPMT), 3) FROM PFIPRESTATION PFI, DPRPROPFINANCE DPF WHERE  PFI.TPRCODE='ASS'  AND PFI.PFIID=DPF.PFIID AND PFI.PFIID= (SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual) AND DPF.DOSID=DPR.DOSID AND DPF.DPRVERSION=DPR.DPRVERSION AND DPF.DPFFLAGRETENUE=1) ,0)  MTASSURANCE,
NVL((SELECT
MAX(PFI.PFIMTDEPOSIT) FROM   PROPOSITIONFINANCIERE PFI, PFIRUBECHEANCIER PEC WHERE PFI.PFIID=(select max (PFI.PFIID) FROM PFIPRESTATION PFI , DPRPROPFINANCE DPF WHERE  PFI.TPRCODE='ASS' AND PFI.PFIID=DPF.PFIID AND PFI.PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual) AND DPF.DOSID=DPR.DOSID AND DPF.DPRVERSION=DPR.DPRVERSION AND DPF.DPFFLAGRETENUE=1) AND PEC.PFIID=PFI.PFIID
AND PEC.PECTYPEELEMENT='LOYER' AND PEC.PECDTDUE =(SELECT MIN(PECDTDUE) FROM PFIRUBECHEANCIER WHERE PFIID=PFI.PFIID AND PEC.PECTYPEELEMENT='LOYER') AND PEC.PECMTBASIC IS NOT NULL),0) as DEPOTGARANTIE,
NVL( (SELECT MAX( PFI.PFPPERIODICITE) FROM PFIPRESTATION PFI, DPRPROPFINANCE DPF WHERE  PFI.TPRCODE='ASS' AND PFI.PFIID=(SELECT MAX(PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )) from dual) AND PFI.PFIID=DPF.PFIID AND DPF.DOSID=DPR.DOSID AND DPF.DPRVERSION=DPR.DPRVERSION AND DPF.DPFFLAGRETENUE=1) ,0)  ASSPERIODICITE,
DPR.TACCODE as TACCODE,
(SELECT TO_CHAR (MAX(CREDTVALID), 'DD/MM/YYYY')
          FROM CREVT CRE
         WHERE CRE.TMFFONCTION = 'EVF_VALIDER' AND CRE.DOSIDPROSPECT = DPR.DOSID ) as DEMANDEFIN,
       PAV4_JASPER_FO.F_GET_SUM_TTC (DPR.DOSID)  AS sum_ttc ,
         translate_fr ( to_word_en ( ROUND ( NVL(PAV4_JASPER_FO.F_GET_SUM_TTC(DPR.DOSID),0),3)))"sum ttc en Lettre",
          Nvl(PAV4_JASPER_FO.F_GET_SUM_TTC2 (DPR.DOSID),0)  AS sum_ttc2 ,
         translate_fr ( to_word_en ( ROUND ( NVL(PAV4_JASPER_FO.F_GET_SUM_TTC2(DPR.DOSID),0),3)))"sum ttc2 en Lettre",
         NVL((select MAX(TTPLIBELLE) from LANTTRPARAM where TTRNOM='DEALTYPE' and TTPCODE in (select DCODEALTYPE from DPRCOMPLEMENT
       where DOSID=DPR.DOSID and DPRVERSION=Dpr.Dprversion )),' ') DEALTYPE,
       NVL(pav4_JASPER_FO.f_recherche_customchar_dossier (DPR.DOSID ,DPR.DPRVERSION,'CHKCCHSID70903'),0) Soumis_compagne ,
(select max( ADECODE) from dprconclusion where dosid=DPR.DOSID AND DPRVERSION=dpr.dprversion) DPRCONCLUSION,
PAV4_JASPER_FO.F_GET_DERNIER_DECISION(DPR.DOSID) AS DERNIERE_DECISION,
pav4_jasper_fo.F_GET_NOM_AGENCE (DPR.DOSID,'AGENCE')   As Nom_AGENCE,
PAV4_JASPER_FO.F_GET_Date_DECISION(DPR.DOSID) AS DT_DERNIERE_DECISION,
NVL(PAV4_JASPER_FO.F_GET_LABELL_BAREME(DPR.DOSID),' ') AS LABELLE_BAREME,
NVL(PAV4_JASPER_FO.f_get_LAD_dossier(DPR.DOSID),' ') AS LAD_DOSSIER,
DPR.DPRDTCREATION AS DATE_CREATIOn,
NVL(PAV4_JASPER_FO.F_GET_NOM_ChargeEtude(DPR.DOSID),' ') AS Nom_CETUDE,
NVL(PAV4_JASPER_FO.F_GET_NOM_COMM(DPR.DOSID),' ') AS Nom_COMMERCIAL,
NVL(PAV4_JASPER_FO.F_GET_NOM_RECOUVREUR(DPR.DOSID),' ') AS Nom_RECOUVREUR,
PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2018,5) AS RATIO_Group_CLT,
PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2020,5)AS RATIO_Starweb_RETENU,
 to_number(REPLACE(PAV4_JASPER_FO.F_GET_AGRVALUE_Value(DPR.DOSID,5,5000,'Impa'),'.',','))AS ENGAGEMENY_SGMA_IMPAYE,
 to_number(REPLACE(PAV4_JASPER_FO.F_GET_AGRVALUE_Value(DPR.DOSID,5,5002,'Impa'),'.',','))AS AUTRENGAGEMENT_IMPAYE,
 to_number(REPLACE(PAV4_JASPER_FO.F_GET_AGRVALUE_Value(DPR.DOSID,5,6000,'Impa'),'.',','))AS ENGAGEMENTSGLM_CLT_IMPAYE,
 to_number(REPLACE(PAV4_JASPER_FO.F_GET_AGRVALUE_Value(DPR.DOSID,5,6001,'Impa'),'.',','))AS ENGAGEMENTSGLM_GRP_IMPAYE,
DPR.DEVCODE AS "DEVISE",
PAV4_JASPER_FO.F_GET_DUREE_MOIS(DPR.DOSID) AS DUREE_MOIS,
To_number(PAV4_JASPER_FO.F_GET_1ER_LOYER_HORS_TAXE(DPR.DOSID)) AS MT_PREMIER_LOYER_HT,
NVL(PAV4_JASPER_FO.F_GET_FRAIS (DPR.DOSID,'FRADOSS'),0) AS"MT_FRAIS_DOS",
NVL(PAV4_JASPER_FO.F_GET_FRAIS (DPR.DOSID,'COMENGC'),0)AS "Commission_engagement",
NVL(PAV4_JASPER_FO.F_GET_TOT_MT_FRAIS (DPR.DOSID),0)AS "MT_AUTRE_FRAIS",
NVL(PAV4_JASPER_FO.F_GET_libelle_ACTIVITE (DPR.DOSID),' ')AS "LIBELLE_ACTIV",
NVL(PAV4_JASPER_FO.F_GET_Nature_Materiel (DPR.DOSID),' ')AS "Nature_MAT",
DECODE (PAV4_JASPER_FO.F_GET_CC_NUMERIQUE (DPR.DOSID,'TFDCCHSID91'),null,'', PAV4_JASPER_FO.F_GET_CC_NUMERIQUE (DPR.DOSID,'TFDCCHSID91')) AS "CA CLIENT",
NVL((SELECT PFIMTDOWNPAYMENT  FROM PROPOSITIONFINANCIERE  WHERE PFIID=PA_REQUETE_COMMUN.F_PROPOSITION_RETENUE( DPR.DOSID, DPR.DPRVERSION, NULL )),0) AS "Mt_avance"
,PAV4_JASPER_FO.F_GET_TEST_ENV(DPR.DOSID) "Test Env" ,
 decode (pav4_jasper_FO.f_get_cch_dossier (DPR.DOSID,'TFDCCHVAL3009'),4,'SGMA',null,'SGMA','SGL')  "Commite" ,
PAV4_JASPER_FO.F_GET_NOM_COMM_FICHP(DPR.DOSID) "NOM COM FICHEP"
FROM DOSSIERPROSPECT DPR, ACTEUR ACTSG ,ADRESSE ADR, ACTADRESSE AAD, CODEPOSTAL COP
WHERE Dpr.Dprversion =F_DERNIEREVERSIONDOSSIER (DPR.DOSID)
AND AAD.ACTID(+) = ACTSG.ACTID AND ADR.ADRID(+) = AAD.ADRID
AND ADR.ADRMSACODE = COP.MSACODE(+) AND ADR.ADRCODEPOST = COP.CPOCODE(+) AND AAD.AADFLAGSIEGE (+)= 1
AND ACTSG.ACTID=dpr.actid;
