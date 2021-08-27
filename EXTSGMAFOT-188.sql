
---EXTSGMAFOT-188
delete from lkckegre where ckeid in ( select max(ckeid) from configurationkey where jalcode='DVAC' )
and grecode not in 'dealDetail.dealDetailBodySid.tabPane.secCheckList' ;

commit;

