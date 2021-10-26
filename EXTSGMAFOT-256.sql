
----EXTSGMAFOT-256

update lanjalon set jallibelle ='Consommation crée par SogeContact'   where jalcode='CONSSC' ;
update lanjalon set jallibelle ='Consommation crée et validée par SGMA'   where jalcode='CONSSG' ;

update lanjalon set jallibelle ='Consommation crée et validée par SGL'   where jalcode='CONSSGL' ;

update lanjalon set jallibelle ='Demande refusée par le commercial'   where jalcode='DRCOM' ;
update lanjalon set jallibelle ='Demande validée par commercial'   where jalcode='DRCOM' ;

update lanjalon set jallibelle ='Demande validée par commercial'   where jalcode='DAJCOM' ;


commit ;