------EXTSGMAFOT-275


----TPCTACCONTROLE

Insert into TPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,MSGID,UGECODE,RMAID,TTCTYPE) values ('U_DPR','AVDOSS','CBM','U_SCR','3162','SGM',null,null);
Insert into TPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,MSGID,UGECODE,RMAID,TTCTYPE) values ('U_DPR','AVDOSS','GLOBAL','U_SCR','3162','SGM',null,null);
Insert into TPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,MSGID,UGECODE,RMAID,TTCTYPE) values ('U_DPR','AVDOSS','CBI','U_SCR','3162','SGM',null,null);
Insert into TPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,MSGID,UGECODE,RMAID,TTCTYPE) values ('U_DPR','AVDOSS','EMPRUNT','U_SCR','3162','SGM',null,null);


----LANTPCTACCONTROLE
Insert into LANTPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,LANCODE,TTCLIBELLE,TTCMEMO) values ('U_DPR','AVDOSS','GLOBAL','U_SCR','FR','Le score n''est pas calculé',null);
Insert into LANTPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,LANCODE,TTCLIBELLE,TTCMEMO) values ('U_DPR','AVDOSS','CBM','U_SCR','FR','Le score n''est pas calculé',null);
Insert into LANTPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,LANCODE,TTCLIBELLE,TTCMEMO) values ('U_DPR','AVDOSS','CBI','U_SCR','FR','Le score n''est pas calculé',null);
Insert into LANTPCTACCONTROLE (TPCCODE,TPCDEST,TACCODE,TTCCODE,LANCODE,TTCLIBELLE,TTCMEMO) values ('U_DPR','AVDOSS','EMPRUNT','U_SCR','FR','Le score n''est pas calculé',null);


---- LKTTCTEV

Insert into LKTTCTEV (TACCODE,TEVDEST,TPCCODE,TPCDEST,TTCCODE,TMOMODULE,TMFFONCTION,TPGCODE,TCEFLAGACTIF,TCEFLAGDEFAUT,TCEFLAGUSER,TSTID,TCEFLAGUSERROLLBACK,ALICODE) values ('CBI','AVDOSS','U_DPR','AVDOSS','U_SCR','AVDOSS','EVF_VALIDER','CBI01','1',null,'1',null,null,null);
Insert into LKTTCTEV (TACCODE,TEVDEST,TPCCODE,TPCDEST,TTCCODE,TMOMODULE,TMFFONCTION,TPGCODE,TCEFLAGACTIF,TCEFLAGDEFAUT,TCEFLAGUSER,TSTID,TCEFLAGUSERROLLBACK,ALICODE) values ('CBM','AVDOSS','U_DPR','AVDOSS','U_SCR','AVDOSS','EVF_VALIDER','AUT','1',null,'1',null,null,null);
Insert into LKTTCTEV (TACCODE,TEVDEST,TPCCODE,TPCDEST,TTCCODE,TMOMODULE,TMFFONCTION,TPGCODE,TCEFLAGACTIF,TCEFLAGDEFAUT,TCEFLAGUSER,TSTID,TCEFLAGUSERROLLBACK,ALICODE) values ('CBM','AVDOSS','U_DPR','AVDOSS','U_SCR','AVDOSS','EVF_VALIDER','AUT02','1',null,'1',null,null,null);
Insert into LKTTCTEV (TACCODE,TEVDEST,TPCCODE,TPCDEST,TTCCODE,TMOMODULE,TMFFONCTION,TPGCODE,TCEFLAGACTIF,TCEFLAGDEFAUT,TCEFLAGUSER,TSTID,TCEFLAGUSERROLLBACK,ALICODE) values ('CBM','AVDOSS','U_DPR','AVDOSS','U_SCR','AVDOSS','EVF_VALIDER','CBM01','1',null,'1',null,null,null);
Insert into LKTTCTEV (TACCODE,TEVDEST,TPCCODE,TPCDEST,TTCCODE,TMOMODULE,TMFFONCTION,TPGCODE,TCEFLAGACTIF,TCEFLAGDEFAUT,TCEFLAGUSER,TSTID,TCEFLAGUSERROLLBACK,ALICODE) values ('GLOBAL','AVDOSS','U_DPR','AVDOSS','U_SCR','AVDOSS','EVF_VALIDER','TOUT','1',null,'1',null,null,null);


Insert into WSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,WSCACTIONTYPE,WSCACTIONCODE,WSCACTIONMODE,WSCFLAGMAIL,WORCODEDEST) values ('WFCLIPRO','6','8','CONTROLS','U_SCR',null,null,null);

---LANWSTCONSEQUENCE
Insert into LANWSTCONSEQUENCE (WORCODE,WSTORDER,WSCORDER,LANCODE,WSTLABEL,WSTDESCRIPTION) values ('WFCLIPRO','6','8','FR','Controls',null);





COMMIT ;