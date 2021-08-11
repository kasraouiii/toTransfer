---demande pour eleminer le champ VR de la fiche CB

delete  from langrtcolumn  where GRTCODE='LEASANE'  and gcocode='Drus'  ;
delete  from grtcolumn  where GRTCODE='LEASANE'  and gcocode='Drus' and GCOCOLUMNINDEX =7 ;



commit ;