select * from 
DOSSIERPROSPECT DOS
where DOS.dosid not in ( select dosid from DPMDETAIL)
and DOS.dprversion in (select max (DPV.dprversion) from dprphase DPV where DOS.dosid=DPV.dosid);