

------EXTSGMAFOT-150


update wstjump   set wstorderdest=19 where  worcode='WFCLIPRO' and wstorder=8 and wstorderdest=13 and rulid =3007120  ;
update wstjump   set wstorderdest=13 where  worcode='WFCLIPRO' and wstorder=13 and wstorderdest=19 and rulid =300762  ;


commit ;