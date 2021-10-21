--------------------------------------------------------
--  Fichier créé - jeudi-octobre-21-2021   
--------------------------------------------------------
--------------------------------------------------------
--  DDL for View V_RATIO
--------------------------------------------------------

  CREATE OR REPLACE FORCE EDITIONABLE VIEW "V_RATIO" ("TEL", "Contentieux", "Redressement Judiciaire", "Saisie Arrêts / ATD", "IMPAYE", "Depassements(> 1 mois)", "Gel du compte (>3 mois)", "Douteux pré-douteux", "Anciennete dans l'activite", "Duree du leasing", "Quotite de financement", "Observations", "Incidents de paiement", "Rapport Credit Bureau", "Analyse financiere", "Mouvement crediteur MCN", "CA CLIENT BILAN", "somme des echeance", "Redev Annu MCN_CA", "Autres garanties", "Anciennete", "Anciennete_profession", "Eligibilté du dossier", "I.S OU PATENTE", "CIN du Gérant", "Client depuis SGL", "Client depuis SGMA", "Capital", "Gestionnaire", "DR", "AvanceTTC", "Siege Social", "Commentaire", "Starweb PRPOPSE", "Starweb VALIDE", "Encours Client", "Encours Groupe", "Risque Net (RiN)", "Fonds Propres Nets (FPN) ALC", "R_DOSID") AS 
  SELECT 
------Ratio Qualitatifs SGM45
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,2009,5),'-') "TEL",
NVL (PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID, 71001,7000), ' ') AS "Contentieux",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71002,7000), ' ') AS "Redressement Judiciaire",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71003,7000), ' ') AS "Saisie Arrêts / ATD",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71004,7000), ' ') AS "IMPAYE",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71005,7000), ' ') AS "Depassements(> 1 mois)",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71006,7000), ' ') AS "Gel du compte (>3 mois)",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71007,7000), ' ') AS "Douteux pré-douteux",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71008,7000), ' ') AS "Anciennete dans l'activite",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71009,7000), ' ') AS "Duree du leasing",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71010,7000), ' ') AS "Quotite de financement",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71012,7000), ' ') AS "Observations",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71011,7000), ' ') AS "Incidents de paiement",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71013,7000), ' ') AS "Rapport Credit Bureau",
NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71014,7000), ' ') AS "Analyse financiere",
DECODE(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71015,7000),null ,' ',(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71015,7000))) AS "Mouvement crediteur MCN",
DECODE(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71016,7000),null ,' ',TO_NUMBER(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71016,7000))) AS "CA CLIENT BILAN",
DECODE(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71017,7000),null ,' ',(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71017,7000))) AS "somme des echeance",
DECODE(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71018,7000),null ,' ',TO_NUMBER(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71018,7000))) AS "Redev Annu MCN_CA",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71019,7000), ' ') AS "Autres garanties",
NVL(PAV4_JASPER_FO.F_GET_libelle_Anciennete (DPR.DOSID),' ') AS "Anciennete",
 NVL(PAV4_JASPER_FO.F_GET_libelle_Anc_profession (DPR.DOSID),' ') AS "Anciennete_profession",

NVL( PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID ,71024,7000), ' ') AS "Eligibilté du dossier",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2006,5),' ') "I.S OU PATENTE",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2007,5),' ') "CIN du Gérant",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2011,5),' ') "Client depuis SGL",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2012,5),' ') "Client depuis SGMA",
NVL(PAV4_JASPER_FO.F_GET_RATIO(DPR.DOSID,2014,5),' ') "Capital",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,1004,5),'-') "Gestionnaire",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,1002,5),'-') "DR",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,1006,5),0) "AvanceTTC",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,2008,5),'-') "Siege Social",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,1000,5),'-') "Commentaire",
NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,2019,5),'-') "Starweb PRPOPSE",

NVL(PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID,2020,5),'-') "Starweb VALIDE",



          
-------------------
              TO_NUMBER (PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID, 10845))
             AS "Encours Client",
          TO_NUMBER (PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID, 10027))
             AS "Encours Groupe",
          TO_NUMBER (PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID, 10028))
             AS "Risque Net (RiN)",
          TO_NUMBER (PAV4_JASPER_FO.F_GET_RATIO (DPR.DOSID, 10029))
             AS "Fonds Propres Nets (FPN) ALC",
 ---------------------------------------------------------------------------------------------------------------------------
          DPR.DOSID AS "R_DOSID"
     FROM DOSSIERPROSPECT DPR
    WHERE DPR.DPRVERSION =
             (SELECT MAX (F_DERNIEREVERSIONDOSSIER (DPR.DOSID)) FROM DUAL);
