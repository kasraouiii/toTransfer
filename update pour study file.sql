

--update pour study file 
        
delete  from lkckegre where grecode='dealDetail.dealDetailBodySid.tabPane.secStudy'  and ckeid   in ( 2017 ,2018,2024,2066 ,
        2069,2070,2071,2072 ,2073 ,2074  ,2022  ,2075)   ;
        
        update lkckegre set EGRFLAGDISABLED='0' where ckeid='2012' and grecode='dealDetail.dealDetailBodySid.tabPane.secStudy';
		
		
		commit ;