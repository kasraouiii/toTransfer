
--rapport credit bureau

update ratio set RATINPUTMETHOD ='INFO',RATRATIOEVALFUNCTION ='P_GET_RAPPCTB' ,RATLISTVALUESFUNCTION=null   where ratid =71013   ;

update lkancanl   set ANADISPLAYTYPE  ='LABEL',ANACONTENTSTYLE =null  where ratid =71013 ;


commit ;