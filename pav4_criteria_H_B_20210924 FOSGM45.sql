create or replace PACKAGE pav4_criteria
AS
    FUNCTION F_GET_PFI_MT_INVEST (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_USER_CHEF_AGENCE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_GROCODE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UTI_CONCAT_METIER (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;
----HME SOGELEASE
 FUNCTION F_GET_UTI_CONCAT_GROUPE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;
    /* function F_GET_UTI_DROIT_DEROGTAUX (nDosId number,sUtiCode VARCHAR2) return varchar2;*/

    FUNCTION F_GET_DPRPHASE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPRSTATUT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPR_CONCAT_DECISION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

FUNCTION F_GET_UTIWFSGM (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2 ;

    FUNCTION F_GET_DPR_DERNIERE_DECISION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPR_EXIST_CTRDERO_INI_EC (nDosId      NUMBER,
                                             sUtiCode    VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPR_EXIST_CTRDERO_INI (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    --MTR 2018-03-19
    FUNCTION F_GET_TRAIT_DERO_DISPO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    ---MTR 01/06/2018
    FUNCTION F_DERO_DISPO (NDOSID NUMBER, sUTICODE VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_NIV_DERO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;                                      --MTR 2018-03-19

    FUNCTION F_GET_UTICODE_NIVEAU_DERO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;                                      --MTR 2018-04-13

    FUNCTION F_GET_INI_DERO_UTICODE (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;                                      --MTR 2018-04-16

    FUNCTION F_GET_DPR_EXIST_CTRDERO_EC (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_ANALYSIS_EXIST_EC (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

         FUNCTION F_GET_DTLIMITE (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_ANALYSIS_EXIST_EC_ARCH (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_RETURN_FALSE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UTICODE_CONNECTE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UTICODE_LISTADJOINT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPR_DERNIERE_CONCLUSION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DPR_NB_JRS_FIN_ACCORD (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_EST_DOSUTIL (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_TL_DELEGATION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    /* function F_IS_REMPLACANT_DIRENGA(nDosId number, sUtiCode VARCHAR2)
       return varchar2;*/

    /* function F_IS_REMPLACANT_CC(nDosId number, sUtiCode VARCHAR2)
       return varchar2;*/

    /* function f_existe_aval_conjoint (sDosid number,sUtiCode VARCHAR2)
       return number;*/

    /*  function f_existe_aval (sDosid number,sUtiCode VARCHAR2)
        return number;*/

    ---   function F_GET_UTI_DROIT_DEROGMTVR (nDosId number,sUtiCode VARCHAR2) return VARCHAR2;

    -- function F_GET_UTI_DROIT_DEROGPCTVR (nDosId number,sUtiCode VARCHAR2) return VARCHAR2;

    FUNCTION f_get_dernier_evt_dossier (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_MAXINTERVENANT (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION f_get_UTICODEDERO (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    --    FUNCTION f_get_nbre_condpre_non_real(nDosid NUMBER, sUticode VARCHAR2) RETURN NUMBER;


    FUNCTION F_Last_Exec (ndosid IN DOSSIER.dosid%TYPE, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION f_get_UTICODEDERO_INI_PRO088 (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION f_get_UTICODEDERO_INI (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION f_get_agence (ndosid NUMBER, sUticode VARCHAR2)
        RETURN NUMBER;

    -------MTR 30/05/2018
    FUNCTION F_GET_AG_PDT (ndosid NUMBER, suticode VARCHAR2)
        RETURN VARCHAR2;

    -------MTR 17/05/2018
    FUNCTION f_get_dealtype (ndosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION f_is_CONFCOMP (ndosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UTICODE_APPRO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_NBS_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UTICODE_CONCLU_APPRO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_KAMELK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_DERO_AUT011 (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_ISDEALTRANSFERTMODIFIED (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_ANOUARK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_ABDELFK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_FAKHERK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_NAOUFELB_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_AIDAZ_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_JAL (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_UticodeCreation (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_User_Decision (ndosid NUMBER, suticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_TL_CLASSERISQUE (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;                                         --DBS SCORING

    FUNCTION F_GET_TL_DECISION (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;                                         --DBS SCORING

    FUNCTION F_GET_LANC_SCORE_OK (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
        ---HME SOGELEASE -----
    FUNCTION F_GET_CATEG_MATERIEL (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;

    FUNCTION F_GET_ETAT_MATERIEL (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
           FUNCTION F_GET_CA_ACTEUR (nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER;
    FUNCTION F_GET_LAD (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2 ;  

    FUNCTION F_GET_TPGCODE (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
    FUNCTION F_GET_SEGMENTATION_CLIENT (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
    FUNCTION F_GET_BASOGVENDOR (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2 ;

    FUNCTION F_GET_MONTANTDISPO (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
     FUNCTION F_GET_SCORE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;   
    FUNCTION F_GET_NAPCODE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
    FUNCTION F_GET_MTINV(nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER;
    FUNCTION F_GET_TACCODE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;
    FUNCTION F_GET_NBPERIOD(nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER;
    FUNCTION F_GET_HLAD(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;    
    FUNCTION F_GET_LADWF(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2;    
     ---------Decisions Risk filtering
        FUNCTION F_AV_DECI_RSK (ndosid IN DOSSIER.dosid%TYPE, sUticode VARCHAR2)
      RETURN VARCHAR2     ;
      FUNCTION F_AV_DEC_RISK (ndosid IN DOSSIER.dosid%TYPE, sUticode VARCHAR2)
      RETURN VARCHAR2    ;
      
END PAV4_CRITERIA;
/
create or replace PACKAGE BODY pav4_criteria
AS
    FUNCTION f_get_pfi_mt_invest (ndosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    AS
        l_mt_invest   NUMBER := 0;
    BEGIN
        BEGIN
            SELECT p.pfiinvestissement
              INTO l_mt_invest
              FROM dprpropfinance d, propositionfinanciere p
             WHERE     d.dosid = ndosid
                   AND d.dpfflagretenue = '1'
                   AND d.dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND d.pfiid = p.pfiid;
        EXCEPTION
            WHEN OTHERS
            THEN
                DBMS_OUTPUT.put_line (SQLERRM);
                l_mt_invest := -1;
        END;

        RETURN l_mt_invest;
    END f_get_pfi_mt_invest;

    FUNCTION F_USER_CHEF_AGENCE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN NULL;
    END;

    FUNCTION F_GET_GROCODE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   utilisateur.grocode%TYPE;
    BEGIN
        BEGIN
            SELECT grocode
              INTO l_Result
              FROM utilisateur
             WHERE uticode = suticode;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := NULL;
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_UTI_CONCAT_METIER (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (100);

        CURSOR c_metier
        IS
            SELECT tsmmetier
              FROM utitsm
             WHERE     uticode = sUticode
                   AND TSMSECTGESTION = 'SGMLEA'      -- 'PROD' MAJ POUR CDMLF
                   AND TSMDTEND IS NULL;

        l_metier   utitsm.tsmmetier%TYPE;
    BEGIN
        OPEN c_metier;

        LOOP
            FETCH c_metier INTO l_metier;

            EXIT WHEN c_metier%NOTFOUND;
            l_result := l_result || ';' || l_metier;
        END LOOP;

        CLOSE c_metier;

        RETURN l_result;
    END;

------ HME ---SOGELEASE--
FUNCTION F_GET_UTI_CONCAT_GROUPE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (100);

        CURSOR c_goupe
        IS
            SELECT Grocode
              FROM utilisateur
             WHERE     uticode = sUticode
                   AND Ugecode = 'SGM' ;

        l_grocode   utilisateur.grocode%TYPE;
    BEGIN
        OPEN c_goupe;

        LOOP
            FETCH c_goupe INTO l_grocode;

            EXIT WHEN c_goupe%NOTFOUND;
            l_result := l_result || ';' || l_grocode;
        END LOOP;

        CLOSE c_goupe;

        RETURN l_result;
    END;
    /*---A decommenter apres pour la configuration des baremes

       function F_GET_UTI_DROIT_DEROGTAUX (nDosId number,sUtiCode VARCHAR2) return varchar2
       is
       l_Result varchar2(10):='FALSE';
       l_mtinvest number;
       l_tpgcode varchar2(15);
       l_network varchar2(15);
       l_taux number;
       l_marge number;
       l_duree number;
       l_grocode varchar2(15);
       l_count number;
       begin
       -- cette fonction retourne TRUE ou FALSE selon les droits du user pour deroger sur le TX de financement.
       --1 : recup le tpgcode, le network, le mt fin, la duree, le Tx et la marge nominale de la proposition :
       --2 : recup du grocode du user (d'abord verifier si le gocode present dans lkratgro possede les droits, ensuite, verifier si le usercode en possede aussi.)
       --3 : jointure entre lkratgro et ratingrate
              l_grocode:=pav4_criteria.f_get_grocode(nDosID, suticode);
              dbms_output.put_line('grocode ' || l_grocode);
              begin
              select pf.pfinbperiodes,pf.pfiinvestissement,pr.pfrtxnominal,pr.pfrmargenominale,d.dprreseaucial,d.tpgcode
              into l_duree, l_mtinvest, l_taux, l_marge, l_network, l_tpgcode
              from dossierprospect d, dprpropfinance dp, propositionfinanciere pf,pfirubrique pr
              where d.dosid=nDosID
              and d.dprversion=pa_avcommun.f_derniereversiondossier ( nDosId )
              and d.dosid= dp.dosid
              and d.dprversion= dp.dprversion
              and dp.dpfflagretenue=1
              and dp.pfiid=pf.pfiid
              and pf.pfiid=pr.pfiid
              and pr.pfrordre=1;
              dbms_output.put_line('taux ' || l_taux);
              dbms_output.put_line('marge ' || l_marge);
              dbms_output.put_line('invest ' || l_mtinvest);
              dbms_output.put_line('network ' || l_network);
              dbms_output.put_line('tpgcode ' || l_tpgcode);
              dbms_output.put_line('duree ' || l_duree);
              begin
              -- test si le GROCODE possede les droits :
                  select count(*) into l_count
                  from ratingrate r,
                  lkratgro lk
                  where lk.ratcatnum=r.ratcatnum
                  and lk.ratordre=r.ratordre
                  and lk.grocode=l_grocode
                  and r.tpgcode=l_tpgcode
                  and (r.ratsalenetwork=l_network or r.ratsalenetwork is null)
                  and r.ratdtfin is null
                  and r.ratminmargin<=l_marge
                  and (r.ratmtminimum<=l_mtinvest or r.ratmtminimum is null)
                  and (r.ratmtmaximum>=l_mtinvest or r.ratmtmaximum is null)
                  and (r.ratnbmonthmin<=l_duree or r.ratnbmonthmin is null)
                  and (r.ratnbmonthmax>=l_duree or r.ratnbmonthmax is null);
                  dbms_output.put_line('count ' || l_count);
                  if l_count>0 then
                      l_result:='TRUE';
                  else
                          -- test lkratuti :
                        select count(*) into l_count
                        from ratingrate r,
                        lkratuti lk
                        where lk.ratcatnum=r.ratcatnum
                        and lk.ratordre=r.ratordre
                        and lk.uticode=sUticode
                        and r.tpgcode=l_tpgcode
                        and (r.ratsalenetwork=l_network or r.ratsalenetwork is null)
                        and r.ratdtfin is null
                        and r.ratminmargin<=l_marge
                        and (r.ratmtminimum<=l_mtinvest or r.ratmtminimum is null)
                        and (r.ratmtmaximum>=l_mtinvest or r.ratmtmaximum is null)
                        and (r.ratnbmonthmin<=l_duree or r.ratnbmonthmin is null)
                        and (r.ratnbmonthmax>=l_duree or r.ratnbmonthmax is null);
                        if l_count>0 then
                            l_result:='TRUE';
                        else

                            l_result:='FALSE';
                        end if;
                  end if;

              exception when others then
              l_result:='FALSE';
              end;

              exception when others then
                l_Result:='FALSE';
              end;
       return l_Result;
       end;
    */


    FUNCTION F_GET_DPRPHASE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   dprphase.phacode%TYPE;
    BEGIN
        BEGIN
            l_Result := pa_avcommun.f_derniereversiondossier (nDosId);
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := NULL;
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_DPRSTATUT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   dprphase.jalcode%TYPE;
    BEGIN
        BEGIN
            SELECT jalcode
              INTO l_result
              FROM dprphase
             WHERE     dosid = ndosid
                   AND phacode =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND dphdtend IS NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := NULL;
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_DPR_CONCAT_DECISION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result     VARCHAR2 (100);

        CURSOR c_decision
        IS
            SELECT DISTINCT adecode
              FROM dprdecision
             WHERE dosid = nDosid;

        l_decision   dprdecision.adecode%TYPE;
    BEGIN
        OPEN c_decision;

        LOOP
            FETCH c_decision INTO l_decision;

            EXIT WHEN c_decision%NOTFOUND;
            l_result := l_result || ';' || l_decision;
        END LOOP;

        CLOSE c_decision;

        RETURN l_result;
    END;


    FUNCTION F_GET_DPR_DERNIERE_DECISION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   dprdecision.adecode%TYPE;
    BEGIN
        BEGIN
            SELECT adecode
              INTO l_result
              FROM dprdecision
             WHERE     dosid = ndosid
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND ddeordre =
                       (SELECT MAX (ddeordre)
                          FROM dprdecision
                         WHERE     dosid = ndosid
                               AND dprversion =
                                   pa_avcommun.f_derniereversiondossier (
                                       nDosId));
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := NULL;
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_DPR_EXIST_CTRDERO_INI_EC (nDosId      NUMBER,
                                             sUtiCode    VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result     VARCHAR2 (10) := 'FALSE';
        l_count      NUMBER;
        l_minvalue   VARCHAR2 (100);
    BEGIN
        BEGIN
            SELECT MIN (DCDREQUESTVALUE)
              INTO l_minvalue
              FROM DPRCTRLDERO
             WHERE DOSID = nDosId AND DCDSTATUS IN ('D_DACC');

            IF l_minvalue = '0' OR l_minvalue IS NULL
            THEN
                SELECT COUNT (1)
                  INTO l_count
                  FROM DPRCTRLDERO
                 WHERE DOSID = nDosid AND DCDSTATUS IN ('INI', 'EC');
            ELSE
                SELECT COUNT (1)
                  INTO l_count
                  FROM DPRCTRLDERO
                 WHERE     DOSID = nDosid
                       AND DCDSTATUS IN ('INI', 'EC')
                       AND DCDREQUESTVALUE < l_minvalue;
            END IF;



            IF l_count > 0
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_DPR_EXIST_CTRDERO_INI (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    NUMBER;
    BEGIN
        BEGIN
            SELECT COUNT (1)
              INTO l_count
              FROM DPRCTRLDERO
             WHERE     DOSID = nDosid
                   AND DPRVERSION =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND DCDSTATUS IN ('INI');

            IF l_count > 0
            THEN
                l_result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_DPR_EXIST_CTRDERO_EC (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    NUMBER;
    BEGIN
        BEGIN
            SELECT COUNT (1)
              INTO l_count
              FROM DPRCTRLDERO
             WHERE     DOSID = nDosid
                   AND DPRVERSION =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND DCDSTATUS IN ('EC');

            IF l_count > 0
            THEN
                l_result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_ANALYSIS_EXIST_EC (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        nCount     NUMBER;
        l_Result   VARCHAR2 (10) := 'FALSE';
    BEGIN
        BEGIN
            SELECT COUNT (1)
              INTO nCount
              FROM ANALYSIS
             WHERE     DOSID = nDosId
                   AND ANASTATUS = 'EC'
                   AND DPRVERSION =
                       pa_avcommun.f_derniereversiondossier (nDosId);

            IF nCount >= 1
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := 'FALSE';
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_ANALYSIS_EXIST_EC_ARCH (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        nCount     NUMBER;
        l_Result   VARCHAR2 (10) := 'FALSE';
    BEGIN
        BEGIN
            SELECT COUNT (1)
              INTO nCount
              FROM ANALYSIS
             WHERE DOSID = nDosId AND ANASTATUS IN ('EC', 'ARCH');

            IF nCount >= 1
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := 'FALSE';
        END;

        RETURN l_result;
    END;

    FUNCTION F_RETURN_FALSE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN 'FALSE';
    END;

    FUNCTION F_GET_UTICODE_CONNECTE (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
    BEGIN
        RETURN suticode;
    END;

    FUNCTION F_GET_UTICODE_LISTADJOINT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (100);

        CURSOR c_user
        IS
            SELECT DISTINCT uticode
              FROM utiuserlink
             WHERE uticodelinked = suticode AND ulitype = 'ADJOINT';

        l_user     utiuserlink.uticode%TYPE;
    BEGIN
        OPEN c_user;

        LOOP
            FETCH c_user INTO l_user;

            EXIT WHEN c_user%NOTFOUND;
            l_result := l_result || ';' || l_user;
        END LOOP;

        CLOSE c_user;

        RETURN l_result;
    END;

    FUNCTION F_GET_DPR_DERNIERE_CONCLUSION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   dprconclusion.adecode%TYPE;
    BEGIN
        BEGIN
            SELECT adecode
              INTO l_result
              FROM dprconclusion
             WHERE     dosid = ndosid
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND dcodtcreation =
                       (SELECT MAX (dcodtcreation)
                          FROM dprconclusion
                         WHERE     dosid = ndosid
                               AND dprversion =
                                   pa_avcommun.f_derniereversiondossier (
                                       nDosId))
                   AND DCOORDRE IN
                           (SELECT MAX (DCOORDRE)
                              FROM dprconclusion
                             WHERE     dosid = ndosid
                                   AND dprversion =
                                       pa_avcommun.f_derniereversiondossier (
                                           ndosid));
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := NULL;
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_DPR_NB_JRS_FIN_ACCORD (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   NUMBER;
    BEGIN
        BEGIN
            SELECT   TO_NUMBER (TO_CHAR (SYSDATE, 'J'))
                   - TO_NUMBER (TO_CHAR (dcodtvalid, 'J'))
              INTO l_result
              FROM dprconclusion
             WHERE     dosid = ndosid
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND adecode = 'ACCORD'
                   AND dcoordre =
                       (SELECT MAX (dcoordre)
                          FROM dprconclusion
                         WHERE     dosid = ndosid
                               AND dprversion =
                                   pa_avcommun.f_derniereversiondossier (
                                       nDosId));

            IF l_result IS NULL
            THEN
                l_result := 0;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := 0;
        END;

        RETURN l_result;
    END;

    FUNCTION F_GET_EST_DOSUTIL (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (10);
    BEGIN
        BEGIN
            SELECT NVL (dosidautorisation, 0)
              INTO l_count
              FROM DPRCOMPLEMENT
             WHERE     DOSID = nDosid
                   AND DPRVERSION =
                       pa_avcommun.f_derniereversiondossier (nDosId);

            IF l_count <> 0
            THEN
                l_result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;


    FUNCTION F_GET_TL_DELEGATION (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result        VARCHAR2 (15);
        l_idenveloppe   NUMBER;
    BEGIN
        SELECT MAX (dosidautorisation)
          INTO l_idenveloppe
          FROM dprcomplement
         WHERE dosid = nDosId;

        IF l_idenveloppe IS NOT NULL
        THEN
            BEGIN
                SELECT ratconclusion
                  INTO l_Result
                  FROM lkanarat l, analysis a
                 WHERE     a.dosid = l_idenveloppe
                       AND a.aNAID = l.ANAID
                       AND l.anaiD =
                           (SELECT MAX (anaid)
                              FROM analysis
                             WHERE     dosid = l_idenveloppe
                                   AND dprversion = 'FIN')
                       AND ratid = 3422;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_Result := '';
            END;
        ELSE
            BEGIN
                SELECT ratconclusion
                  INTO l_Result
                  FROM lkanarat l, analysis a
                 WHERE     a.dosid = nDosId
                       AND a.aNAID = l.ANAID
                       AND l.anaiD =
                           (SELECT MAX (anaid)
                              FROM analysis
                             WHERE dosid = nDosId AND dprversion = 'FIN')
                       AND ratid = 3422;
            EXCEPTION
                WHEN OTHERS
                THEN
                    l_Result := '';
            END;
        END IF;



        RETURN l_Result;
    EXCEPTION
        WHEN OTHERS
        THEN
            RETURN NULL;
    END;

    /*
    function F_IS_REMPLACANT_CC(nDosId number, sUtiCode VARCHAR2)
        return varchar2 is
        l_Result varchar2(10) := 'FALSE';
        l_count  varchar2(20);

      begin
        Begin
          SELECT count(*)
            INTO l_count
            FROM ucaremove UR, UTICALENDAR UC
           WHERE UR.UCRUTICODEREMOVE = sUticode
             AND UR.uticode = UC.UTICODE
             AND UR.UCAORDER = UC.UCAORDER
             AND SYSDATE BETWEEN UC.UCADTSTART AND UC.UCADTEND
             and uc.uticode = pav4_workflow.f_get_user_agence(nDosID, 'CC');
          --         and uc.uticode in (select uticode from utilisateur where grocode='DIRENGA')

          if l_count <> '0' then
            l_result := 'TRUE';
          end if;
        EXCEPTION
          WHEN OTHERS THEN
            l_Result := 'FALSE';
        END;
        return l_Result;
      end;
      */
    /*---A decommenter apres pour la configuration des baremes

      function F_IS_REMPLACANT_DIRENGA(nDosId number, sUtiCode VARCHAR2)
        return varchar2 is
        l_Result varchar2(10) := 'FALSE';
        l_count  varchar2(20);

      begin
        Begin
          SELECT count(*)
            INTO l_count
            FROM ucaremove UR, UTICALENDAR UC
           WHERE UR.UCRUTICODEREMOVE = sUticode
             AND UR.uticode = UC.UTICODE
             AND UR.UCAORDER = UC.UCAORDER
             AND SYSDATE BETWEEN UC.UCADTSTART AND UC.UCADTEND
                --         and uc.uticode = pav4_workflow.f_get_user_agence(nDosID,'CC');
             and uc.uticode in
                 (select uticode from utilisateur where grocode = 'DIRENG');

          if l_count <> '0' then
            l_result := 'TRUE';
          end if;
        EXCEPTION
          WHEN OTHERS THEN
            l_Result := 'FALSE';
        END;
        return l_Result;
      end;
    */
    /*
      function f_existe_aval (sDosid number, sUtiCode VARCHAR2) return number
      is
        l_Result NUMBER:=0;
        l_tgacode tgarantie.tgacode %TYPE;

        cursor c_garantie is SELECT tgacode FROM dpgcaracteristique WHERE dosid = sDosid;

        begin
          begin
          open c_garantie;
          loop
          fetch c_garantie into l_tgacode;
          exit when c_garantie%notfound;
          IF l_tgacode='31' THEN
          l_result:=1 ;
          goto end_;
          END IF;
          end loop;
          <<end_>>
          close c_garantie;
          return l_result;
        end;
      end;
    */
    /*
      function f_existe_aval_conjoint (sDosid number,sUtiCode VARCHAR2) return number
      is
        l_Result NUMBER:=0;
        l_tgacode tgarantie.tgacode %TYPE;

        cursor c_garantie is    SELECT tgacode FROM dpgcaracteristique WHERE dosid = sDosid;

        begin
          begin
          open c_garantie;
          loop
          fetch c_garantie into l_tgacode;
          exit when c_garantie%notfound;
          IF l_tgacode='39' THEN
          l_result:=1;
          goto end_;
          END IF;
          end loop;
          <<end_>>
          close c_garantie;
          return l_result;
        end;
      end;
    */
    /*
       function F_GET_UTI_DROIT_DEROGPCTVR (nDosId number,sUtiCode VARCHAR2) return varchar2
       is
       l_Result varchar2(10):='FALSE';
       l_mtinvest number;
       l_tpgcode varchar2(15);
       l_network varchar2(15);
       l_pctvr number;
       l_periodicity varchar2(15);
       l_period number;
       l_grocode varchar2(15);
       l_count number;
       begin

              l_grocode:=pav4_criteria.f_get_grocode(nDosID, suticode);
              dbms_output.put_line('grocode ' || l_grocode);
              begin
              select pf.pfinbperiodes,pf.pfiinvestissement,pf.pfiperiodicite,pr.pfrpctvr , d.dprreseaucial,d.tpgcode
              into l_period , l_mtinvest, l_periodicity , l_pctvr , l_network, l_tpgcode
              from dossierprospect d, dprpropfinance dp, propositionfinanciere pf,pfirubrique pr
              where d.dosid=nDosId
              and d.dprversion=pa_avcommun.f_derniereversiondossier ( nDosId )
              and d.dosid= dp.dosid
              and d.dprversion= dp.dprversion
              and dp.dpfflagretenue=1
              and dp.pfiid=pf.pfiid
              and pf.pfiid=pr.pfiid
              and pr.pfrordre=1;
              dbms_output.put_line('PCT Vr ' || l_pctvr);
              dbms_output.put_line('periodicity ' || l_periodicity);
              dbms_output.put_line('invest ' || l_mtinvest);
              dbms_output.put_line('network ' || l_network);
              dbms_output.put_line('tpgcode ' || l_tpgcode);
              dbms_output.put_line('period ' || l_period);
              begin
              -- test si le GROCODE possede les droits :
                  select count(*) into l_count
                  from ACARV r,
                  LKGROARV  lk
                  where lk.tpgcode = r.tpgcode
                  and lk.acacode=r.acacode
                  and lk.arvnbperiod = r.arvnbperiod
                  AND lk.arvperiodicity = r.arvperiodicity
                  and lk.grocode=l_grocode
                  and r.tpgcode=l_tpgcode
                  and r.arvnbperiod =   l_period
                  and r.arvperiodicity = l_periodicity
                  and (lk.garpctminrv <= l_pctvr or lk.garpctminrv is null)
                  and (lk.garpctmaxrv >= l_pctvr or lk.garpctmaxrv is null);

                  dbms_output.put_line('count ' || l_count);
                  if l_count>0 then
                      l_result:='TRUE';
                  else
                          -- test sur Uticode  :
                        select count(*) into l_count
                        from acarv r,
                        lkutiarv  lk
                        where lk.tpgcode = r.tpgcode
                        and lk.acacode=r.acacode
                        and lk.arvnbperiod = r.arvnbperiod
                        AND lk.arvperiodicity = r.arvperiodicity
                        and lk.uticode = sUtiCode
                        and r.tpgcode=l_tpgcode
                        and r.arvnbperiod =   l_period
                        and r.arvperiodicity = l_periodicity
                        and (r.arvpctminrv <= l_pctvr or r.arvpctminrv is null)
                        and (r.arvpctmaxrv >= l_pctvr or r.arvpctmaxrv is null);
                        if l_count>0 then
                            l_result:='TRUE';
                        else

                            l_result:='FALSE';
                        end if;
                  end if;

              exception when others then
              l_result:='FALSE';
              end;

              exception when others then
                l_Result:='FALSE';
              end;
       return l_Result;
    end;
    */
    /*
    function F_GET_UTI_DROIT_DEROGMTVR (nDosId number,sUtiCode VARCHAR2) return varchar2
       is
       l_Result varchar2(10):='FALSE';
       l_mtinvest number;
       l_tpgcode varchar2(15);
       l_network varchar2(15);
       l_mtvr number;
       l_periodicity varchar2(15);
       l_period number;
       l_grocode varchar2(15);
       l_count number;
       begin

              l_grocode:=pav4_criteria.f_get_grocode(nDosID, suticode);
              dbms_output.put_line('grocode ' || l_grocode);
              begin
              select pf.pfinbperiodes,pf.pfiinvestissement,pf.pfiperiodicite,pr.pfrvr , d.dprreseaucial,d.tpgcode
              into l_period , l_mtinvest, l_periodicity , l_mtvr , l_network, l_tpgcode
              from dossierprospect d, dprpropfinance dp, propositionfinanciere pf,pfirubrique pr
              where d.dosid=nDosId
              and d.dprversion=pa_avcommun.f_derniereversiondossier ( nDosId )
              and d.dosid= dp.dosid
              and d.dprversion= dp.dprversion
              and dp.dpfflagretenue=1
              and dp.pfiid=pf.pfiid
              and pf.pfiid=pr.pfiid
              and pr.pfrordre=1;
              dbms_output.put_line('MT Vr ' || l_mtvr);
              dbms_output.put_line('periodicity ' || l_periodicity);
              dbms_output.put_line('invest ' || l_mtinvest);
              dbms_output.put_line('network ' || l_network);
              dbms_output.put_line('tpgcode ' || l_tpgcode);
              dbms_output.put_line('period ' || l_period);
              begin
              -- test si le GROCODE possede les droits :
                  select count(*) into l_count
                  from ACARV r,
                  LKGROARV  lk
                  where lk.tpgcode = r.tpgcode
                  and lk.acacode=r.acacode
                  and lk.arvnbperiod = r.arvnbperiod
                  AND lk.arvperiodicity = r.arvperiodicity
                  and lk.grocode=l_grocode
                  and r.tpgcode=l_tpgcode
                  and r.arvnbperiod =   l_period
                  and r.arvperiodicity = l_periodicity
                  and (lk.garmtminrv <= l_mtvr or lk.garmtminrv is null)
                  and (lk.garmtmaxrv >= l_mtvr or lk.garmtmaxrv is null);

                  dbms_output.put_line('count ' || l_count);
                  if l_count>0 then
                      l_result:='TRUE';
                  else
                          -- test sur Uticode  :
                        select count(*) into l_count
                        from acarv r,
                        lkutiarv  lk
                        where lk.tpgcode = r.tpgcode
                        and lk.acacode=r.acacode
                        and lk.arvnbperiod = r.arvnbperiod
                        AND lk.arvperiodicity = r.arvperiodicity
                        and lk.uticode = suticode
                        and r.tpgcode=l_tpgcode
                        and r.arvnbperiod =   l_period
                        and r.arvperiodicity = l_periodicity
                        and (r.arvmtminrv <= l_mtvr or r.arvmtminrv is null)
                        and (r.arvmtmaxrv >= l_mtvr or r.arvmtmaxrv is null);
                        if l_count>0 then
                            l_result:='TRUE';
                        else

                            l_result:='FALSE';
                        end if;
                  end if;

              exception when others then
              l_result:='FALSE';
              end;

              exception when others then
                l_Result:='FALSE';
              end;
       return l_Result;
       end;
    */
    FUNCTION f_get_dernier_evt_dossier (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
    BEGIN
        SELECT tmffonction
          INTO l_Result
          FROM crevt
         WHERE     tevdest = 'AVDOSS'
               AND dosidprospect = nDosid                     --EMI 01-03-2018
               AND creid =
                   (SELECT MAX (creid)
                      FROM crevt
                     WHERE tevdest = 'AVDOSS' AND dosidprospect = nDosid);

        RETURN l_Result;
    END;

    FUNCTION F_GET_MAXINTERVENANT (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            SELECT uticode
              INTO l_Result
              FROM dprintervenant
             WHERE     dosid = nDosId
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND dinordre =
                       (SELECT MAX (dinordre)
                          FROM dprintervenant
                         WHERE     dosid = nDosId
                               AND dprversion =
                                   pa_avcommun.f_derniereversiondossier (
                                       nDosId));

            IF l_Result = sUticode
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION f_get_UTICODEDERO (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            SELECT DISTINCT uticode
              INTO l_Result
              FROM DPRCTRLDERO
             WHERE     dosid = nDosId
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   AND DCDSTATUS NOT IN ('D_DACC', 'EC', 'D_RDERO');

            IF l_Result = sUticode
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    /*
    FUNCTION f_get_nbre_condpre_non_real(nDosid NUMBER, sUticode VARCHAR2) RETURN NUMBER IS

    l_Result NUMBER := 0 ;
    l_jalon VARCHAR(250) := NULL;

    BEGIN

      BEGIN

      SELECT jalcode INTO l_jalon FROM dprphase WHERE DOSID = nDosid AND DPHDTEND IS NULL;

      SELECT Count(*) INTO l_Result FROM dprprealable WHERE dosid=nDosid AND jalcode = l_jalon AND DPPFLAGPREALABLEDERO= 0 AND DPPFLAGPREALABLENA= 0 AND DPPDATEREAL IS null;

      exception when others then
                l_Result:= 0;
      END;

    RETURN l_Result;

    END;
    */
    ------------MTR 01/06/2018
    FUNCTION F_DERO_DISPO (NDOSID NUMBER, SUTICODE VARCHAR2)
        RETURN VARCHAR2
    IS
        nCount     NUMBER := 0;
        L_RESULT   VARCHAR2 (10) := '';
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO nCount
              FROM dprctrldero
             WHERE dosid = NDOSID AND DCDSTATUS IN ('INI', 'EC');


            IF (nCount > 0)
            THEN
                L_RESULT := 'TRUE';
            ELSE
                L_RESULT := 'FALSE';
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                L_RESULT := '';
        END;

        RETURN L_RESULT;
    END F_DERO_DISPO;

    ------END MTR 01/06/2018

    --------AKA 31082020
    FUNCTION F_GET_UTIWFSGM (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            select distinct (UTICODEEXEC)
            INTO l_Result
            from dprworstep where
            worcode='WFSGM' and wstorder=1 and wststatus='TER'
            and dosid=nDosid ;

            IF l_Result = sUticode
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;




    ------

    FUNCTION F_GET_NIV_DERO (                   --HBB 20170509 ---MTR 20180315
                             NDOSID NUMBER, SUTICODE VARCHAR2)
        RETURN VARCHAR2
    IS
        L_LIMITE    NUMBER := 0;
        L_TPGCODE   dossierprospect.tpgcode%TYPE;
        L_MARGE     pfirubrique.PFRMARGENOMINALE%TYPE;
        L_RESULT    VARCHAR2 (10) := '';
        L_TTCCODE   VARCHAR2 (10) := '';
    BEGIN
        BEGIN
            SELECT MAX (TTCCODE)
              INTO L_TTCCODE
              FROM dprctrldero
             WHERE dosid = NDOSID AND DCDSTATUS IN ('INI', 'EC');

            IF (L_TTCCODE = 'PRO066')
            THEN
                SELECT MAX (tpgcode)
                  INTO L_TPGCODE
                  FROM dossierprospect
                 WHERE dosid = NDOSID;

                SELECT PFRMARGENOMINALE
                  INTO L_MARGE
                  FROM DPRPROPFINANCE         DPR,
                       PROPOSITIONFINANCIERE  PRO,
                       PFIRUBRIQUE            PRU
                 WHERE     PRO.PFIID = DPR.PFIID
                       AND DPR.PFIID = PRU.PFIID
                       AND DPR.DOSID = ndosid
                       AND DPR.DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = DPR.DOSID)
                       AND DPR.DPFFLAGRETENUE = 1;

                SELECT DECODE (TTPPARAM,
                               'RATE_COM', 'COM',
                               'RATE_CA', 'CA',
                               'RATE_DIRENG', 'DIRENG',
                               'DG')
                  INTO L_RESULT
                  FROM TOPTPGPARAM
                 WHERE     TOPTABLE = 'FRONT_QUOTE'
                       AND TTPPARAM IN ('RATE_COM',
                                        'RATE_CA',
                                        'RATE_DIRENG',
                                        'RATE_DG')
                       AND TPGCODE = L_TPGCODE
                       AND L_MARGE >= TTPNOMBRE
                       AND ttpnombre =
                           (SELECT MAX (TTPNOMBRE)
                              FROM TOPTPGPARAM
                             WHERE     TOPTABLE = 'FRONT_QUOTE'
                                   AND TTPPARAM IN ('RATE_COM',
                                                    'RATE_CA',
                                                    'RATE_DIRENG',
                                                    'RATE_DG')
                                   AND TPGCODE = L_TPGCODE
                                   AND L_MARGE >= TTPNOMBRE);
            ELSIF (L_TTCCODE = 'PRO089')
            THEN
                L_RESULT := 'CA';
            ELSIF ((L_TTCCODE != 'PRO089') AND (L_TTCCODE != 'PRO066'))
            THEN
                L_RESULT := 'DIRENG';
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                L_RESULT := '';
        END;

        RETURN L_RESULT;
    END F_GET_NIV_DERO;

    -----------MTR 13/04/2018
    FUNCTION F_GET_UTICODE_NIVEAU_DERO (nDosid NUMBER, SUTICODE VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
        sMetier    VARCHAR2 (20) := NULL;
        L_NIVDEL   VARCHAR2 (20) := NULL;
        stpgcode   VARCHAR2 (20) := NULL;
        Taux_Dem   NUMBER := 0;
        T_CA       NUMBER := 0;
        T_DIRENG   NUMBER := 0;
        T_DG       NUMBER := 0;
    BEGIN
        BEGIN
            SELECT TO_NUMBER (REPLACE (DCDREQUESTVALUE, '.', ','))
              INTO Taux_Dem
              FROM DPRCTRLDERO
             WHERE     dosid = nDosId
                   AND TTCCODE = 'PRO066'
                   AND DCDSTATUS IN ('INI', 'EC')
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId);

            -- AND UTICODE = sUticode;

            SELECT MIN (TSMMETIER)                            --MTR 01/06/2018
              INTO sMetier
              FROM UTITSM
             WHERE UTICODE = sUticode AND TSMFLAGLEADER = 1;

            SELECT tpgcode
              INTO stpgcode
              FROM dossierprospect
             WHERE     dosid = ndosid
                   AND dprversion =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DOSID);

            SELECT TTPNOMBRE
              INTO T_CA
              FROM TOPTPGPARAM
             WHERE TPGCODE = stpgcode AND TTPPARAM = 'RATE_CA';

            SELECT TTPNOMBRE
              INTO T_DIRENG
              FROM TOPTPGPARAM
             WHERE TPGCODE = stpgcode AND TTPPARAM = 'RATE_DIRENG';

            SELECT TTPNOMBRE
              INTO T_DG
              FROM TOPTPGPARAM
             WHERE TPGCODE = stpgcode AND TTPPARAM = 'RATE_DG';

            L_NIVDEL := F_GET_NIV_DERO (ndosid, SUTICODE);

            IF (sMetier = 'CA') AND (L_NIVDEL = 'CA') AND (Taux_Dem >= T_CA)
            THEN
                l_Result := 'TRUE'; -----------------------------Le WF ne doit pas s'afficher
            ELSIF (    (sMetier = 'DIRENG')
                   AND (L_NIVDEL = 'DIRENG')
                   AND (T_DIRENG <= Taux_Dem)
                   AND (Taux_Dem < T_CA))
            THEN
                l_Result := 'TRUE'; -----------------------------Le WF ne doit pas s'afficher
            ELSIF (    (sMetier = 'DG')
                   AND (L_NIVDEL = 'DG')
                   AND (T_DG <= Taux_Dem)
                   AND (Taux_Dem < T_DIRENG))
            THEN
                l_Result := 'TRUE'; -----------------------------Le WF ne doit pas s'afficher
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                L_RESULT := 'ERREUR';
        END;

        RETURN L_RESULT;
    END F_GET_UTICODE_NIVEAU_DERO;

    -----END 13/04/2018

    -----MTR 16/04/2018
    FUNCTION F_GET_INI_DERO_UTICODE (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result       VARCHAR2 (20) := NULL;
        Uticode_Dero   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            SELECT MAX (uticode)                              --MTR 03/07/2018
              INTO Uticode_Dero
              FROM DPRCTRLDERO
             WHERE     dosid = nDosId
                   AND DCDSTATUS IN ('INI', 'EC')             --MTR 01/06/2018
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId);

            IF Uticode_Dero = sUticode
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END F_GET_INI_DERO_UTICODE;

    -----------END MTR 16/04/2018

    FUNCTION F_GET_TRAIT_DERO_DISPO (                          ---MTR 20180315
                                     NDOSID NUMBER, SUTICODE VARCHAR2)
        RETURN VARCHAR2
    IS
        L_METIER     UTITSM.TSMMETIER%TYPE;
        L_LIMITE     NUMBER := 0;
        nCount       NUMBER := 0;
        L_TPGCODE    dossierprospect.tpgcode%TYPE;
        L_MARGE      pfirubrique.PFRMARGENOMINALE%TYPE;
        L_NIVDELEG   VARCHAR2 (50);
        L_RESULT     VARCHAR2 (10) := '';
        L_TTCCODE    VARCHAR2 (10) := '';
    BEGIN
        BEGIN
            SELECT MAX (TTCCODE)
              INTO L_TTCCODE
              FROM dprctrldero
             WHERE dosid = NDOSID AND DCDSTATUS IN ('INI', 'EC'); --MTR 27/06/2018

            IF (L_TTCCODE = 'PRO066')
            THEN
                SELECT MAX (tpgcode)
                  INTO L_TPGCODE
                  FROM dossierprospect
                 WHERE dosid = NDOSID;

                SELECT PFRMARGENOMINALE
                  INTO L_MARGE
                  FROM DPRPROPFINANCE         DPR,
                       PROPOSITIONFINANCIERE  PRO,
                       PFIRUBRIQUE            PRU
                 WHERE     PRO.PFIID = DPR.PFIID
                       AND DPR.PFIID = PRU.PFIID
                       AND DPR.DOSID = ndosid
                       AND DPR.DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE VDE.DOSID = DPR.DOSID)
                       AND DPR.DPFFLAGRETENUE = 1;

                L_NIVDELEG := F_GET_NIV_DERO (ndosid, SUTICODE);

                SELECT MAX (TSMMETIER)
                  INTO L_METIER
                  FROM utitsm
                 WHERE uticode = SUTICODE AND tsmmetier = L_NIVDELEG;

                IF L_NIVDELEG = 'COM' AND L_METIER IS NOT NULL
                THEN
                    SELECT NVL (TTPNOMBRE, 0)
                      INTO L_LIMITE
                      FROM TOPTPGPARAM
                     WHERE     TOPTABLE = 'FRONT_QUOTE'
                           AND TTPPARAM = 'RATE_COM'
                           AND TPGCODE = L_TPGCODE;
                ELSIF L_NIVDELEG = 'CA' AND L_METIER IS NOT NULL
                THEN
                    SELECT NVL (TTPNOMBRE, 0)
                      INTO L_LIMITE
                      FROM TOPTPGPARAM
                     WHERE     TOPTABLE = 'FRONT_QUOTE'
                           AND TTPPARAM = 'RATE_CA'
                           AND TPGCODE = L_TPGCODE;
                ELSIF L_NIVDELEG = 'DIRENG' AND L_METIER IS NOT NULL
                THEN
                    SELECT NVL (TTPNOMBRE, 0)
                      INTO L_LIMITE
                      FROM TOPTPGPARAM
                     WHERE     TOPTABLE = 'FRONT_QUOTE'
                           AND TTPPARAM = 'RATE_DIRENG'
                           AND TPGCODE = L_TPGCODE;
                ELSIF L_NIVDELEG = 'DG' AND L_METIER IS NOT NULL
                THEN
                    SELECT NVL (TTPNOMBRE, 0)
                      INTO L_LIMITE
                      FROM TOPTPGPARAM
                     WHERE     TOPTABLE = 'FRONT_QUOTE'
                           AND TTPPARAM = 'RATE_DG'
                           AND TPGCODE = L_TPGCODE;
                ELSE
                    RETURN 'FALSE';
                END IF;

                IF L_MARGE >= L_LIMITE
                THEN
                    L_RESULT := 'TRUE';
                ELSE
                    L_RESULT := 'test';
                END IF;
            ELSIF (L_TTCCODE = 'PRO089')
            THEN
                SELECT COUNT (*)
                  INTO nCount
                  FROM utitsm
                 WHERE uticode = SUTICODE AND TSMMETIER = 'CA';

                IF (nCount > 0)
                THEN
                    L_RESULT := 'TRUE';
                ELSE
                    L_RESULT := 'FALSE';
                END IF;
            ELSIF ((L_TTCCODE != 'PRO089') AND (L_TTCCODE != 'PRO066'))
            THEN
                SELECT COUNT (*)
                  INTO nCount
                  FROM utitsm
                 WHERE uticode = SUTICODE AND TSMMETIER = 'DIRENG';

                IF (nCount > 0)
                THEN
                    L_RESULT := 'TRUE';
                ELSE
                    L_RESULT := 'FALSE';
                END IF;
            END IF;
        EXCEPTION
            WHEN NO_DATA_FOUND
            THEN
                L_RESULT := '';
        END;

        RETURN L_RESULT;
    END F_GET_TRAIT_DERO_DISPO;

    FUNCTION f_get_UTICODEDERO_INI_PRO088 (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
        l_count    NUMBER := 0;
    BEGIN
        BEGIN
            SELECT COUNT (uticode)
              INTO l_count
              FROM DPRCTRLDERO
             WHERE     dosid = nDosId
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   --and  DCDSTATUS IN ('INI','EC')  ;
                   AND TTCCODE IN ('PRO088',
                                   'MAT013',
                                   'AUT001',
                                   'AUT011',
                                   'AUT011')
                   AND DCDSTATUS = 'INI';

            IF l_count > 0
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_AG_PDT (ndosid NUMBER, suticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (100);
        stpgcode   VARCHAR2 (10);
        staccode   VARCHAR2 (10);
        nActId     NUMBER;
        l_count    NUMBER;
    BEGIN
        SELECT f_get_agence (ndosid, sUticode) INTO nactid FROM DUAL;

        SELECT tpgcode, taccode
          INTO stpgcode, staccode
          FROM dossierprospect
         WHERE     dosid = ndosid
               AND dprversion = PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DOSID);

        IF staccode IN ('LOCFIN') AND nActId IN (75129, 68330)
        THEN
            l_Result := 'TRUE';
        ELSIF sTPGCODE IN ('CBESTL', 'CBESTLI') AND nActId IN (75129, 68330)
        THEN
            l_Result := 'TRUE';
        ELSE
            l_Result := 'FALSE';
        END IF;

        RETURN l_result;
    END;


    FUNCTION F_Last_Exec (ndosid IN DOSSIER.dosid%TYPE, sUticode VARCHAR2)
        RETURN VARCHAR2                                          --MTR28052013
    IS
    BEGIN
        DECLARE
            sCodeExec   dprworstep.uticodeexec%TYPE;
            sTestCode   VARCHAR2 (100);
        BEGIN
            BEGIN
                SELECT DISTINCT dprw.uticodeexec
                  INTO sCodeExec
                  FROM dprworstep dprw
                 WHERE     dprw.wststatus = 'TER'
                       AND dprw.wstdtexec =
                           (SELECT MAX (wstdtexec)
                              FROM dprworstep
                             WHERE dprw.wststatus = 'TER' AND dosid = ndosid)
                       AND dprw.dosid = ndosid;

                IF sUticode = sCodeExec
                THEN
                    sTestCode := 'TRUE';
                ELSE
                    sTestCode := 'FALSE';
                END IF;
            EXCEPTION
                WHEN OTHERS
                THEN
                    sTestCode := 'FALSE';
            END;

            RETURN sTestCode;
        END;
    END F_Last_Exec;

    FUNCTION f_get_UTICODEDERO_INI (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
        l_count    NUMBER := 0;
    BEGIN
        BEGIN
            SELECT COUNT (uticode)
              INTO l_count
              FROM DPRCTRLDERO
             WHERE     dosid = nDosId
                   AND dprversion =
                       pa_avcommun.f_derniereversiondossier (nDosId)
                   --and  DCDSTATUS IN ('INI','EC')  ;
                   AND TTCCODE != 'PRO088'
                   AND TTCCODE != 'MAT013'
                   AND TTCCODE != 'AUT001'
                   AND TTCCODE != 'PRO066'
                   AND TTCCODE != 'AUT011'
                   AND DCDSTATUS = 'INI';

            IF l_count > 0
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    -----------------MTR 17/05/2018 : R¿cup¿ration Type de la demande
    FUNCTION f_get_dealtype (ndosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    AS
        l_type   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            SELECT MAX (dcodealtype)
              INTO l_type
              FROM dprcomplement
             WHERE     DOSID = ndosid
                   AND DPRVERSION =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DOSID);
        END;


        RETURN l_type;
    END;

    -------------------END MTR 17/05/2018
    FUNCTION f_get_agence (ndosid NUMBER, sUticode VARCHAR2)
        RETURN NUMBER
    AS
        l_actid   VARCHAR2 (30) := NULL;
    BEGIN
        BEGIN
            SELECT MAX (ACTID)
              INTO l_actid
              FROM DPRACTEUR
             WHERE     DOSID = ndosid
                   AND DPRVERSION =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DOSID)
                   AND ROLCODE = 'AGENCE';
        END;


        RETURN l_actid;
    END;

    FUNCTION f_is_CONFCOMP (ndosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    AS
        sExist   VARCHAR2 (15);
    BEGIN
        BEGIN
            SELECT DECODE (COUNT (*), 0, 'FALSE', 'TRUE')
              INTO sExist
              FROM dprdecision d1, dprdecision d2
             WHERE     d1.ADECODE = 'DOCCOM'
                   AND d2.ADECODE = 'DECCONF'
                   AND d1.DOSID = d2.DOSID
                   AND d1.DOSID = ndosid;
        END;

        RETURN sExist;
    END;

    FUNCTION F_GET_UTICODE_APPRO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_result   dprdecision.uticodedecision%TYPE;
    BEGIN
        BEGIN
            SELECT DISTINCT uticodedecision
              INTO l_result
              FROM dprdecision
             WHERE     dosid = ndosid
                   AND adecode = 'APPRO'
                   AND ddeordre =
                       (SELECT MAX (ddeordre)
                          FROM dprdecision
                         WHERE dosid = ndosid AND adecode = 'APPRO');
        EXCEPTION
            WHEN OTHERS
            THEN
                l_result := NULL;
        END;

        RETURN l_result;
    END;

    FUNCTION F_NBS_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'NABILBS';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_UTICODE_CONCLU_APPRO (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM dprconclusion
             WHERE     dosid = ndosid
                   AND adecode = 'APPRO'
                   AND uticode = 'KAMELK';


            IF (l_count <> '0')
            THEN
                l_Result := 'TRUE';
            ELSIF (l_count <> '0')
            THEN
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;


    FUNCTION F_KAMELK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'KAMELK';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_DERO_AUT011 (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM dprctrldero
             WHERE dosid = ndosid AND ttccode = 'AUT011';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_ISDEALTRANSFERTMODIFIED (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        /*isModified varchar2(10) := 'FALSE';
        testModified number:=0;

        BEGIN

         SELECT Decode(Count(*),0,0,1)
            INTO testModified
            FROM DPRWORSTEP dpw, DOSSIERPROSPECT dpr
            WHERE dpw.DOSID =nDosId
            AND dpw.dosid=dpr.dosid
            AND dpw.DPRVERSION = 'NEGO'
            AND dpw.WSTORDER=0
            AND dpw.WORCODE='WFCOMME'
            AND dpr.DPRNUMCASSIOPEE IS NOT null;

            if testModified <> 0 then
            isModified := 'TRUE';
            else
            isModified := 'FALSE';
             end if;

             return isModified;*/
        --Modified By Heythem BLG

        p_dprnumcassiopae   VARCHAR2 (15) := NULL;
        isModified          VARCHAR2 (10) := 'FALSE';
    BEGIN
        SELECT NVL (MAX (DPRNUMCASSIOPEE), 'EMPTY')
          INTO p_dprnumcassiopae
          FROM dossierprospect
         WHERE dosid = nDosId AND DPRNUMCASSIOPEE IS NOT NULL;

        IF (p_dprnumcassiopae != 'EMPTY')
        THEN
            SELECT DECODE (COUNT (DISTINCT DPRNUMERO),
                           0, 'FALSE',
                           1, 'FALSE',
                           'TRUE')
              INTO isModified
              FROM DOSSIERPROSPECT DP, DPRCOMPLEMENT DC
             WHERE     DP.DPRNUMCASSIOPEE = p_dprnumcassiopae
                   AND DP.DOSID = DC.DOSID
                   AND DC.DCODEALTYPE NOT IN ('TRANSFERT', 'RENEGOFROMBEGIN') ----MTR 16/07/2018
                   AND DP.DPRVERSION = DC.DPRVERSION
                   AND DP.DPRVERSION =
                       PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (nDosId);
        END IF;

        RETURN isModified;
    --RETURN 'FALSE';


    END F_ISDEALTRANSFERTMODIFIED;

    FUNCTION F_ANOUARK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'ANOUARK';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_ABDELFK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'ABDELFK';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_FAKHERK_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'FAKHERK';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_NAOUFELB_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'NAOUFELB';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_AIDAZ_IS_ABSENT (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM UTICALENDAR
             WHERE     SYSDATE BETWEEN UCADTSTART AND UCADTEND
                   AND uticode = 'AIDAZ';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    FUNCTION F_GET_JAL (nDosId NUMBER, sUtiCode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (10) := 'FALSE';
        l_count    VARCHAR2 (20);
    BEGIN
        BEGIN
            SELECT COUNT (*)
              INTO l_count
              FROM dprphase
             WHERE dosid = nDosid AND dprversion = 'FIN' AND jalcode = 'DEA';

            IF l_count <> '0'
            THEN
                l_Result := 'TRUE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;


    --RAM: Pour distinguer les dossiers ENNAKL (groupe de l'uticodecreation de la demande doit etre COMAPP ou SUPAPP)
    FUNCTION F_GET_UticodeCreation (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) := NULL;
    BEGIN
        BEGIN
            SELECT DISTINCT grocode
              INTO l_Result
              FROM UTILISATEUR
             WHERE uticode IN (SELECT uticodecreation
                                 FROM dossierprospect
                                WHERE dosid = nDosId AND dprversion = 'NEGO');

            IF l_Result IN ('COMAPP', 'SUPAPP')
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;

    --------------------------- Start DBS 21/04/2017 TLFII-1086

    FUNCTION F_GET_User_Decision (ndosid NUMBER, suticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (100);
        stpgcode   VARCHAR2 (10);
        staccode   VARCHAR2 (10);
        nActId     NUMBER;
        l_count    NUMBER;
    BEGIN
        SELECT f_get_agence (ndosid, sUticode) INTO nactid FROM DUAL;

        SELECT tpgcode, taccode
          INTO stpgcode, staccode
          FROM dossierprospect
         WHERE     dosid = ndosid
               AND dprversion = PA_AVCOMMUN.F_DERNIEREVERSIONDOSSIER (DOSID);

        IF     staccode IN ('LOCFIN')
           AND nActId IN (75129, 68330)
           AND suticode = 'NABILBS'
        THEN
            l_Result := 'TRUE';
        ELSIF     sTPGCODE IN ('CBESTL', 'CBESTLI')
              AND nActId IN (75129, 68330)
              AND suticode = 'NABILBS'
        THEN
            l_Result := 'TRUE';
        ELSIF     staccode NOT IN ('LOCFIN')
              AND sTPGCODE NOT IN ('CBESTL', 'CBESTLI')
              AND nActId IN (75129, 68330)
              AND suticode = 'KAMELK'
        THEN
            l_Result := 'TRUE';
        ELSIF nActId NOT IN (75129, 68330)
        THEN
            SELECT COUNT (*)
              INTO l_count
              FROM utitsm
             WHERE     uticode = suticode
                   AND tsmmetier = 'CA'
                   AND tsmsectgestion = 'PROD';

            IF l_count <> 0
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        ELSE
            l_Result := 'FALSE';
        END IF;

        RETURN l_result;
    END;

    --------------------------- End DBS 21/04/2017 TLFII-1086

    ----------------------------MTR 08/06/2018 FONCTIONS SCORE REPORT TLG-4.0

    --------------------------- TLG-SCORING-classeRisque
    FUNCTION F_GET_TL_CLASSERISQUE (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (15);
        l_classe   VARCHAR2 (15);
    BEGIN
        BEGIN
            SELECT NVL (MAX (ratvalue), 0)
              INTO l_classe
              FROM lkanarat l, analysis a
             WHERE     a.dosid = nDosId
                   AND a.aNAID = l.ANAID
                   AND l.anaiD =
                       (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = nDosId AND dprversion = 'FIN')
                   AND ratid = 3611;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_classe := NULL;
        END;

        l_Result := l_classe;

        RETURN l_Result;
    END;

    -----------------------------TLG-SCORING-decision
    FUNCTION F_GET_TL_DECISION (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result     VARCHAR2 (15);
        l_decision   VARCHAR2 (15);
    BEGIN
        BEGIN
            SELECT ratvalue
              INTO l_decision
              FROM lkanarat l, analysis a
             WHERE     a.dosid = nDosId
                   AND a.aNAID = l.ANAID
                   AND l.anaiD =
                       (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = nDosId AND dprversion = 'FIN')
                   AND ratid = 3426;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_decision := '';
        END;

        l_Result := l_decision;

        RETURN l_Result;
    END;

    ---------------------------
    FUNCTION F_GET_LANC_SCORE_OK (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (15);
    BEGIN
        BEGIN
            SELECT DECODE (COUNT (*), 8, 'TRUE', 'FALSE')
              INTO l_Result
              FROM lkanarat l, analysis a
             WHERE     a.dosid = nDosId
                   AND a.aNAID = l.ANAID
                   AND l.anaiD =
                       (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = nDosId AND dprversion = 'FIN')
                   AND ratid IN (3425,
                                 3426,
                                 3427,
                                 3428,
                                 3608,
                                 3609,
                                 3610,
                                 3611)
                   AND ratvalue IS NOT NULL;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := NULL;
        END;

        RETURN l_Result;
    END;
    --------------HME SOGELEASE F_GET_ASSETCATEGORY MAT-------
    FUNCTION F_GET_CATEG_MATERIEL (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        nacacode   VARCHAR2 (15);
    BEGIN
        BEGIN
            select acacode into nacacode from dprmateriel where dosid=nDosId
            and dprversion = pa_avcommun.f_derniereversiondossier (nDosId);

        EXCEPTION
            WHEN OTHERS
            THEN
                nacacode := NULL;
        END;

        RETURN nacacode;
    END;

    FUNCTION F_GET_ETAT_MATERIEL (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        NDPMFLAGNEUF   NUMBER ;
    BEGIN
        BEGIN
            select DPMFLAGNEUF into NDPMFLAGNEUF from dprmateriel where
            dosid=nDosId
            and   dprversion = pa_avcommun.f_derniereversiondossier (nDosId);

        EXCEPTION
            WHEN OTHERS
            THEN
                NDPMFLAGNEUF := NULL;
        END;

        RETURN NDPMFLAGNEUF;
    END;

    FUNCTION F_GET_CA_ACTEUR (nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER
    IS
        NCA     NUMBER;
        nActId     NUMBER;
    BEGIN
        BEGIN
            select actid into nactid from dpracteur where dosid=nDosId
            and rolcode='CLIENT'
            and DPRVERSION =pa_avcommun.f_derniereversiondossier (nDosId);

             IF (nactid IS NOT NULL )
            THEN
                  select CVANUMERICVALUE into NCA from cchvalue where actid =nactid
                  and CCHSID='TFDCCHSID220';
            END IF;

        EXCEPTION
            WHEN OTHERS
            THEN
                NCA := NULL;
        END;

        RETURN NCA;
    END;
    ----------HME---------ABH update 20210316
    FUNCTION F_GET_LAD (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        nlad     VARCHAR2 (15);
        Autolad   VARCHAR2 (15);
        l_Result  VARCHAR2 (15);

    BEGIN
        BEGIN

         SELECT F_GET_LADWF (ndosid,'ORFI') into Autolad from dual ; --- NB : any updates here require updates in --> PAV4_CCFUNCTIONS.LADSGM


         SELECT    NVL(MAX(CVASTRINGVALUE), 'EMPTY')   into nlad
         from cchvalue   where cchsid='TFDCCHVAL3009' and DOSIDPROSPECT=ndosid
         and DPRVERSION =pa_avcommun.f_derniereversiondossier (nDosId); 


          IF (Autolad IS  NOT NULL)  and (nlad ='EMPTY')  
                THEN 
                l_Result := Autolad; 

          ELSIF  (nlad != 'EMPTY')     

               THEN
                l_Result := nlad ;
               END IF;   



        /*   IF (nlad IS NULL) 
            THEN
                l_Result := Autolad;


            ELSIF (Autolad IS NULL)
               Then 
               l_Result := nlad ;

            ELSIF  (Autolad IS not NULL) and (nlad IS not NULL)
              then
                 l_Result := nlad ; --LGA*/

             --END IF;
              --END IF;

   EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := NULL;

        END;

        RETURN l_Result ;
    END  F_GET_LAD ;
--------
    FUNCTION F_GET_TPGCODE (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        ntpgcode   VARCHAR2 (15);

    BEGIN
        BEGIN
            SELECT TPGCODE into ntpgcode from dossierprospect where DOSID=ndosid
            and DPRVERSION =pa_avcommun.f_derniereversiondossier (nDosId);


        EXCEPTION
            WHEN OTHERS
            THEN
                ntpgcode := NULL;
        END;

        RETURN ntpgcode;
    END;
----------------SEGMENTATION CLIENT----------------------
FUNCTION F_GET_SEGMENTATION_CLIENT (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        NSEGCLT   VARCHAR2(15);
        nActId     NUMBER;
    BEGIN
        BEGIN
            select actid into nactid from dpracteur where dosid=nDosId
            and rolcode='CLIENT'
            and DPRVERSION =pa_avcommun.f_derniereversiondossier (nDosId);

             IF (nactid IS NOT NULL )
            THEN
                  select CVASTRINGVALUE into NSEGCLT from cchvalue where actid =nactid
                  and CCHSID='CMBCCHSID82';
            END IF;

        EXCEPTION
            WHEN OTHERS
            THEN
                NSEGCLT := NULL;
        END;

        RETURN NSEGCLT;
    END;
    ---HME DATE LIMITE pour les enveloppes
   FUNCTION F_GET_DTLIMITE (nDosId NUMBER, sUticode VARCHAR2)       --ABH 09/04/2021
        RETURN VARCHAR2
    IS
        l_Result VARCHAR2(20);
        npfiid     NUMBER;
		DTMATURITY   DATE;
		DTLIMTENV   DATE;
        nbday     NUMBER;
    BEGIN
        BEGIN

        /*   SELECT PFIID
                  INTO npfiid
                  FROM DPRPROPFINANCE

                 WHERE    DOSID = ndosid
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = ndosid)
                       AND DPFFLAGRETENUE = 1;
		--	TRSGM45.DEBUG_TEST('pfiid ='||npfiid);
			select PFADATE INTO DTMATURITY from pfiattribut where  pfiid=npfiid and PFACODE='MATYRITYDATE'; */

      SELECT DPFDTLIMITE
                  INTO DTMATURITY
                  FROM DPRPROPFINANCE

                 WHERE    DOSID = ndosid
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = ndosid)
                       AND DPFFLAGRETENUE = 1;

         --   TRSGM45.DEBUG_TEST('date mat ='||DTMATURITY);
 		 ---   select (TO_DATE(DTMATURITY, 'dd/mm/yy')-1) INTO DTLIMTENV from dual;
         --    TRSGM45.DEBUG_TEST('date limt env ='||DTLIMTENV);
           select (TO_DATE(DTMATURITY, 'dd/mm/yy')) - trunc(sysdate) into nbday from dual;
        --   TRSGM45.DEBUG_TEST('nb jours ='||nbday);
            IF (nbday <=0) THEN
              l_Result := 'true';
            ELSE
            l_Result := 'false';
            END IF;

        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := NULL;
        END;

        RETURN l_Result;
    END;
    -----ich 
    FUNCTION F_GET_BASOGVENDOR (nDosid NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        l_Result   VARCHAR2 (20) ;
		--CLIENTCA   VARCHAR2 (15);
    BEGIN
        BEGIN
		
		-- SELECT F_GET_CA_ACTEUR (ndosid,'ORFI') into CLIENTCA from dual ; 
		
		SELECT DISTINCT grocode
              INTO l_Result
              FROM UTILISATEUR
             WHERE uticode IN (SELECT uticodecreation
                                 FROM dossierprospect
                                WHERE dosid = nDosId AND dprversion = 'NEGO');
								
			

            IF (l_Result IN ('VENDOR', 'SGEC' ,'CC_PRO') OR l_Result ='COMSGL')
            THEN
                l_Result := 'TRUE';
            ELSE
                l_Result := 'FALSE';
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                l_Result := 'FALSE';
        END;

        RETURN l_Result;
    END;


    --HME SGM Si le montant dispo egale a 0
    FUNCTION F_GET_MONTANTDISPO (nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    AS
	    l_Result        VARCHAR2(20);
        NMTDISPO          NUMBER := 0;
        NMONTANTINITIAL   NUMBER;
        NMONTANTUTILISE   NUMBER;
        SDPRVERSION       DOSSIERPROSPECT.DPRVERSION%TYPE;
    BEGIN
        IF nDosId IS NOT NULL
        THEN
            SELECT DPRVERSION
              INTO SDPRVERSION
              FROM V_DEAL
             WHERE DOSID = nDosId;
        END IF;

        SELECT MAX (DPIMTFILIALE)
          INTO NMONTANTINITIAL
          FROM DPRINVESTISSEMENT
         WHERE DOSID = nDosId AND DPRVERSION = SDPRVERSION;

        SELECT SUM (DPI.DPIMT)
          INTO NMONTANTUTILISE
          FROM DOSSIERPROSPECT DPR, DPRCOMPLEMENT DPC, DPRINVESTISSEMENT DPI
         WHERE     DPC.DOSIDAUTORISATION = nDosId
               AND DPR.DPRVERSION = SDPRVERSION
               AND DPR.DOSID = DPC.DOSID
               AND DPR.DPRVERSION = DPC.DPRVERSION
               AND DPR.DPRDTCLASSEMENT IS NULL
               AND DPI.DOSID = DPR.DOSID
               AND DPI.DPRVERSION = DPR.DPRVERSION;

        SELECT  NVL (NMONTANTINITIAL, 0) - NVL (NMONTANTUTILISE, 0) INTO NMTDISPO FROM DUAL;
                IF (NMTDISPO =0) THEN
                 l_Result :='true';
                ELSE
                 l_Result :='false';
                END IF;


        RETURN l_Result;
    END;

    FUNCTION F_GET_SCORE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        o_val2   VARCHAR2(50);
        o_val1   VARCHAR2(50);
		NRESULT   VARCHAR2(50);
    BEGIN
        BEGIN
            SELECT RATVALUE
              INTO   o_val1
              FROM lkanarat
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'R71026')
                   AND anaid = (select max(anaid) from analysis where dosid=nDosId and ANMID=7000);

			SELECT RATVALUE
              INTO   o_val2
              FROM lkanarat
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'R71024')
                   AND anaid = (select max(anaid) from analysis where dosid=nDosId and ANMID=7000);   ----Décision final : calculé sur la base des critédre qualitatif/quantitatif du dossier d'étude 

             IF (o_val1 IS NOT NULL )
            THEN
                NRESULT:=o_val1;
			 ELSE
			    NRESULT:=o_val2;
            END IF;

        EXCEPTION
            WHEN OTHERS
            THEN
                NRESULT := NULL;
        END;

        RETURN NRESULT;
    END;
   FUNCTION F_GET_NAPCODE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        NNAPCODE   VARCHAR2(50);

    BEGIN
        BEGIN
              SELECT max (NAPCODE)
                  INTO NNAPCODE
                  FROM dprmateriel

                 WHERE    DOSID = nDosId
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = nDosId);



        EXCEPTION
            WHEN OTHERS
            THEN
                NNAPCODE := NULL;
        END;

        RETURN NNAPCODE;
    END;
    
       ---------Decisions Risk filtering
   FUNCTION F_AV_DECI_RSK (ndosid IN DOSSIER.dosid%TYPE, sUticode VARCHAR2)
      RETURN VARCHAR2                                            
   IS
   BEGIN
      DECLARE
         nCount  number;
         sResult   VARCHAR2 (100);
      BEGIN
         BEGIN
    
               SELECT COUNT(*) 
               into nCount FROM dprworstep
                    WHERE DOSID = NDOSID
                    AND worcode like 'WFCLICOM' and wstorder=6
                   AND WSTSTATUS='INI';
           
        
           
                If (nCount = 0) then
                sResult:='DECIFA01';
                ELSIF (nCount > 0) then
                sResult:='ACCORD';
                END IF;
         EXCEPTION
            WHEN OTHERS
            THEN
               sResult := 'ERROR';
         END;

         RETURN sResult;
      END;
   END F_AV_DECI_RSK;

 
       ---------Decisions Risk filtering2
   FUNCTION F_AV_DEC_RISK (ndosid IN DOSSIER.dosid%TYPE, sUticode VARCHAR2)
      RETURN VARCHAR2                                            
   IS
   BEGIN
      DECLARE
         nCount  number;
         sResult   VARCHAR2 (100);
      BEGIN
         BEGIN
    
               SELECT COUNT(*) 
               into nCount FROM dprworstep
                    WHERE DOSID = NDOSID
                    AND worcode like 'WFCLIPRO' and wstorder=19
                   AND WSTSTATUS='INI';
           
        
           
                If (nCount = 0) then
                sResult:='DECIFA01';
                ELSIF (nCount > 0) then
                sResult:='ACCORD';
                END IF;
         EXCEPTION
            WHEN OTHERS
            THEN
               sResult := 'ERROR';
         END;

         RETURN sResult;
      END;
   END F_AV_DEC_RISK;





 /*  FUNCTION F_GET_MTINV(nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER
    IS
        NMTINV   NUMBER;

    BEGIN
        BEGIN
         SELECT sum (DPMMTINVEST)  ---abh 12/04/2021
                  INTO NMTINV
                  FROM dprmateriel

                 WHERE    DOSID = nDosId
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = nDosId);



        EXCEPTION
            WHEN OTHERS
            THEN
                NMTINV := NULL;
        END;

        RETURN NMTINV;
    END;*/
    
       FUNCTION F_GET_MTINV(nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER
    IS
        NACTID   NUMBER;
        NMTINV   NUMBER;
        MTIMPAY   NUMBER;
        MTREVP     NUMBER;
        MTENCG    NUMBER;
        MTTOTAL   NUMBER;

    BEGIN
        BEGIN
              SELECT sum (DPMMTINVEST)  ---abh 12/04/2021    montant de la demande actuelle
                  INTO NMTINV
                  FROM dprmateriel

                 WHERE    DOSID = nDosId
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = nDosId);

       -- Dbms_Output.put_line(NMTINV);


             select sum (to_number (replace (agrvalue ,',','.')))  ---abh 15/04/2021      Appl montant des impayes sous dossier d'etude
                      into MTIMPAY from aragrid
                         where ratid in (5000,5002,6001,6000)
                         and  AGRGCOCODE in ('Imp','Impa')
                          and anaid in
                                         (select anaid from ANALYSIS where anmid=5 and dprversion=(SELECT DPRVERSION
                                         FROM V_DEAL VDE
                                          WHERE DOSID = nDosId)and dosid =nDosId);

       -- Dbms_Output.put_line(MTIMPAY);

            select distinct (actid) into NACTID
                          from dpracteur where rolcode='CLIENT' and dosid =nDosId ;

            select PAV4_JASPER_FO.F_GET_RILIQ_ENV_ACT(Nactid ) into MTREVP from dual ; --Reliquat d'enveloppe 	Somme des montants dispo des contrats de type autorisation

            select   PAV4_JASPER_FO.F_GET_ENCOURS_ACTEUR_CTRENG (Nactid) into MTENCG from dual ;--Contrats en cours de deblocage 	Somme des encours des contrats en phase EC et ENG dont la date de creation dans BO

          MTTOTAL := nvl(NMTINV,0) + nvl(MTIMPAY,0) + nvl (MTREVP,0) + nvl(MTENCG,0) ;


     --Dbms_Output.put_line(MTTOTAL);

        EXCEPTION
            WHEN OTHERS
            THEN
               /* MTTOTAL := null;*/
                SELECT sum (DPMMTINVEST)  ---abh 12/04/2021    montant de la demande actuelle
                  INTO NMTINV
                  FROM dprmateriel

                 WHERE    DOSID = nDosId
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = nDosId);
      --  Dbms_Output.put_line(NMTINV);

       select sum (to_number (replace (agrvalue ,'.',',')))   ---abh 15/04/2021     BD montant des impayes sous dossier d'etude
                      into MTIMPAY from aragrid

                         where ratid in (5000,5002,6001,6000)
                         and  AGRGCOCODE in ('Imp','Impa')
                          and anaid in
                                         (select anaid from ANALYSIS where anmid=5 and dprversion=(SELECT DPRVERSION
                                         FROM V_DEAL VDE
                                          WHERE DOSID = nDosId)and dosid =nDosId);

       -- Dbms_Output.put_line(MTIMPAY);

            select distinct (actid) into NACTID
                          from dpracteur where rolcode='CLIENT' and dosid =nDosId ;

            select PAV4_JASPER_FO.F_GET_RILIQ_ENV_ACT(Nactid ) into MTREVP from dual ; --Reliquat d'enveloppe 	Somme des montants dispo des contrats de type autorisation

            select   PAV4_JASPER_FO.F_GET_ENCOURS_ACTEUR_CTRENG (Nactid) into MTENCG from dual ;--Contrats en cours de deblocage 	Somme des encours des contrats en phase EC et ENG dont la date de creation dans BO

          MTTOTAL := nvl(NMTINV,0) + nvl(MTIMPAY,0) + nvl (MTREVP,0) + nvl(MTENCG,0) ;
        END;

        RETURN MTTOTAL;
    END;
    
    

 FUNCTION F_GET_TACCODE(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        NTACCODE   VARCHAR2(50);

    BEGIN
        BEGIN
              SELECT TACCODE
                  INTO NTACCODE
                  FROM dossierprospect

                 WHERE    DOSID = nDosId
                       AND DPRVERSION = (SELECT DPRVERSION
                                               FROM V_DEAL VDE
                                              WHERE DOSID = nDosId);



        EXCEPTION
            WHEN OTHERS
            THEN
                NTACCODE := NULL;
        END;

        RETURN NTACCODE;
    END;

FUNCTION F_GET_NBPERIOD(nDosId NUMBER, sUticode VARCHAR2)
        RETURN NUMBER
    IS
        nbp   NUMBER;

    BEGIN
        BEGIN

         select PFINBPERIODES into nbp from dprpropfinance dprp,propositionfinanciere prop where
         dprp.dosid=nDosId
          and dprp.pfiid=prop.pfiid
         and dprp.DPFFLAGRETENUE=1
         and dprp.dprversion =  pa_avcommun.f_derniereversiondossier (nDosId);      ---abh 12/04/2021


        EXCEPTION
            WHEN OTHERS
            THEN
                nbp := NULL;
        END;

        RETURN nbp;
    END;

    FUNCTION F_GET_HLAD(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        o_val   VARCHAR2(50);

    BEGIN
        BEGIN

         SELECT RATVALUE
              INTO   o_val
              FROM lkanarat
             WHERE     ratid IN (SELECT ratid
                                   FROM ratio
                                  WHERE ratcode = 'LADSGM')
                   AND anaid = (select max(anaid) from analysis where dosid=nDosId and ANMID=5);



        EXCEPTION
            WHEN OTHERS
            THEN
                o_val := NULL;
        END;

        RETURN o_val;
    END;
   ---HME CRITERIA LAD
    FUNCTION F_GET_LADWF(nDosId NUMBER, sUticode VARCHAR2)
        RETURN VARCHAR2
    IS
        NSCORE   VARCHAR2(50);
        NHLAD    VARCHAR2(50);
		NNAP     VARCHAR2(50);
		NMTINV   NUMBER;
		NTACCODE VARCHAR2(50);
		NNBP     NUMBER;
		SRESULT  VARCHAR2(50);

    BEGIN
    BEGIN 
    
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
			      SRESULT:='0';--UNIPERS RMCP

			
			 ELSIF  (NTACCODE='CBM' AND NNAP in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 500001 AND 1000000)  AND (NNBP BETWEEN 36 AND 60)
			 OR NTACCODE='CBM' AND NNAP NOT in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 250001 AND 500000) AND (NNBP BETWEEN 36 AND 60)) THEN
			      SRESULT:='1';-- UNIPERS DG DGA--changed ines
				  
				  
			 ELSIF  (NTACCODE='CBM' AND NNAP in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 1000001 AND 2500000)  AND (NNBP BETWEEN 36 AND 60)
			 OR NTACCODE='CBM' AND NNAP NOT in ('1201','3101','3102','3103','3104','3105') AND (NMTINV BETWEEN 500001 AND 2500000) AND (NNBP BETWEEN 36 AND 60)) THEN
			      SRESULT:='2';--DS RMCP RISQUE --added ines
				  
			 ELSIF (NTACCODE='CBM' AND (NMTINV >= 2500001 ) ) THEN
			     SRESULT:='3';--DS DG RISQUE 	--added ines

			 ELSIF (NTACCODE='CBM' AND (NMTINV BETWEEN 1000000 AND 2500000) AND NNBP<=60) THEN
			     SRESULT:='2';--DS RMCP RISQUE
			 ELSIF (NTACCODE='CBM' AND (NMTINV BETWEEN 2500001 AND 5000000) AND NNBP<=60) THEN
			     SRESULT:='3';--DS DG RISQUE SGM
			 ELSIF 	( NTACCODE='CBI' AND NMTINV<=5000000 AND NNBP<=144) THEN
			     SRESULT:='4';--DS RMCP RISQUE---2 --a changer ines 
		 ELSIF 	 ( NTACCODE='CBI' AND (NMTINV BETWEEN 5000001 AND 10000000)  AND NNBP<=144) THEN
		     SRESULT:='4';--DS DG RISQUE SGM---3  --a changer ines
			 ELSE
			     SRESULT:='4';--DEPUIS DCIIT
			 END IF;

       ELSIF (NSCORE IS NULL OR NSCORE='ND') THEN

		   IF (NTACCODE='CBI' )
          
           THEN
		      SRESULT:='4';--DEPUIS DCIIT

         END IF;

        ELSIF (NSCORE='SCO' OR NSCORE='SCORE ORANGE') THEN

		   IF (NTACCODE='CBM' AND NMTINV<=2500000 AND NNBP<=60)
           --OR (NTACCODE='CBI' AND NMTINV<=5000000 AND NNBP<=144)) -----a changer ines
           THEN
		      SRESULT:='2';--DS RMCP RISQUE

		   ELSIF (NTACCODE='CBM' AND (NMTINV BETWEEN 2500001 AND 5000000) AND NNBP<=60) 
           --OR (NTACCODE='CBI' AND (NMTINV BETWEEN 5000001 AND 10000000) AND NNBP<=144)) -----a changer ines
           THEN

              SRESULT:='3';--DS DG RISQUE SGM
		   ELSE
              SRESULT:='4';--DEPUIS DCIIT
		   END IF;
		  ELSIF (NSCORE='SCR' OR NSCORE='SCORE ROUGE') THEN
		     SRESULT:='5';--CLASSEMENT SANS SUITE
        END IF;
       
            
    ELSE
    SRESULT:='4';--DEPUIS DCIIT
  END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                SRESULT := NULL;
        END;
          
        RETURN SRESULT;
    END;
   
END pav4_criteria;