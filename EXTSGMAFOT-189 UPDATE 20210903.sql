
-------EXTSGMAFOT-189
----jump verts transfert 

update wstjump  set rulid =3007138   where worcode='WFCLIPRO' and WSTORDER=18  and WSTORDERDEST=10  and rulid =4009 ;



-----Notification -> ( transférer la demande au BO )


update cddrul set rulid =3007139  where cddordre = 10185  and cseid =108 and rulid=4013  ;



-----habilitation DGA 
update rule set rulcode='GRP' where rulid =3007140 ;
update rule set CRIIDFIRST=4 where rulid =3007140 ;


commit ;