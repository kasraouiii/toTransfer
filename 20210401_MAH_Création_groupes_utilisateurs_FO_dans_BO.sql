--- Insertion des GROUPES FO inexistants dans BO
INSERT INTO GROUPE
SELECT *
FROM GROUPE@SGM_LINK
WHERE GROCODE NOT IN (SELECT GROCODE FROM GROUPE);
COMMIT;

--- Insertion des UTILISATEURS FO inexistants dans BO
INSERT INTO UTILISATEUR (UTICODE,UGECODE,GROCODE,UTINOM,UTIPRENOM,UTIFLAGINACTIF,UTITELECOM,UTIFLAGPASSWORD,UTIDTUPD,UTIWHODUNNIT,UTICRC,UTICODESUBSTITUTE,UTIPWD,UTIPWDDTCHGD,UTILOCKED,UTIATTEMPTS,UTILOCKTIME,UTILDAPREFERENCE,UTIFLAGPERMANENT)
SELECT UTICODE,UGECODE,GROCODE,UTINOM,UTIPRENOM,UTIFLAGINACTIF,UTITELECOM,UTIFLAGPASSWORD,UTIDTUPD,UTIWHODUNNIT,UTICRC,UTICODESUBSTITUTE,UTIPWD,UTIPWDDTCHGD,UTILOCKED,UTIATTEMPTS,UTILOCKTIME,UTILDAPREFERENCE,UTIFLAGPERMANENT
FROM UTILISATEUR@SGM_LINK
WHERE UTICODE NOT IN (SELECT UTICODE FROM UTILISATEUR)---(UTICODE, GROCODE) NOT IN (SELECT UTICODE, GROCODE FROM UTILISATEUR)
AND UTIFLAGINACTIF IS NULL
ORDER BY 1;
COMMIT;
