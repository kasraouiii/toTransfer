
---EXTSGMAFOT-188

delete   from lkckegre  where ckeid in ( select max(ckeid) from configurationkey  where jalcode='DVAC' ) 
and grecode in( 'dealDetail.dealDetailBodySid.tabPane.secAssets',
'dealDetail.dealDetailBodySid.tabPane.secDealActeur' , 'dealDetail.dealDetailBodySid.tabPane.secDecision','dealDetail.dealDetailBodySid.tabPane.secDonneesGenerales' ,
'dealDetail.dealDetailBodySid.tabPane.secQuote') ;  


commit;

