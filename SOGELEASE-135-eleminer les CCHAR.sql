----SOGELEASE-135



delete from LKCCHGRE   where cchsid in ( 'CMBCCHSID196'  , 'TFDCCHSID64' , 'TFDCCHSID182',  'TFDCCHSID186',  'TFDCCHSID188') ;
delete  from CUSTOMCHARACTERISTIC  where cchsid in ( 'CMBCCHSID196'  , 'TFDCCHSID64' , 'TFDCCHSID182',  'TFDCCHSID186',  'TFDCCHSID188') ;

delete  from lanLOCALIZABLEFEATURE   where lfename = 'CUSTCHAR.ACTEUR.CMBCCHSID196' ;
delete  from lanLOCALIZABLEFEATURE   where lfename = 'CUSTCHAR.ACTEUR.TFDCCHSID64' ;
delete from lanLOCALIZABLEFEATURE   where lfename = 'CUSTCHAR.ACTEUR.TFDCCHSID182' ;
delete  from lanLOCALIZABLEFEATURE   where lfename = 'CUSTCHAR.ACTEUR.TFDCCHSID186' ;
delete  from lanLOCALIZABLEFEATURE   where lfename = 'CUSTCHAR.ACTEUR.TFDCCHSID188' ;



commit ; 