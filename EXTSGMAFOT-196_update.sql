
----EXTSGMAFOT-196


update groupe set grointitule ='Direction Générale' where grocode ='DG_SGL'  and grointitule='Directeur Général Sogelease';


UPDATE utilisateur  set grocode='DG_SGL' where uticode ='DGASGL'  and uticode ='DGASGL' and grocode='DGA_SGL' ;


delete  from wstconsequence   where worcode='WFCLIPRO' and wstorder=8  and  WSCORDER=11 ;
delete from lanwstconsequence   where worcode='WFCLIPRO' and wstorder=8  and  WSCORDER=11 ;

----lad ds rmcp risq
delete  from wstconsequence   where worcode='WFCLIPRO' and wstorder=27  and  WSCORDER=6 ;
delete from lanwstconsequence   where worcode='WFCLIPRO' and wstorder=27  and  WSCORDER=6 ;

commit ;