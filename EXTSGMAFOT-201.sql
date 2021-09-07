
---EXTSGMAFOT-201


update lanmsg set msglibelle  ='Veuillez spécifier la cible, obligatoire pour rechercher une tâche par phase, jalon et le nom de l''entité' 
where lancode='FR' and msglibelle ='FR - Please specify target, mandatory for searching task on phase, step and entity name';


commit ;