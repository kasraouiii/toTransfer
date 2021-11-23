create or replace PACKAGE BODY PAV4_PASSWORD
AS
    ERR_NO_ROWS_CONST        CONSTANT INTEGER := 1;
    ERR_PWD_EQUALS_LOGIN     CONSTANT INTEGER := 11874;
    ERR_PWD_EQUALS_OLD_PWD   CONSTANT INTEGER := 11875;
    ERR_PWD_SIZE             CONSTANT INTEGER := 11876;
    ERR_PWD_FORBIDEN_CHAR    CONSTANT INTEGER := 11877;
    ERR_PWD_CASE             CONSTANT INTEGER := 11878;
    ERR_PWD_SPECIAL_CHAR     CONSTANT INTEGER := 11879;
    ERR_PWD_DAYLIMIT         CONSTANT INTEGER := 24465;

    PROCEDURE U_LOGGEDIN (SUTICODE              UTILISATEUR.UTICODE%TYPE,
                          NUTIATTEMPTS          UTILISATEUR.UTIATTEMPTS%TYPE,
                          SUTILOCKED            UTILISATEUR.UTILOCKED%TYPE,
                          NRETURNCODE    IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            DTUTILOCKTIME   UTILISATEUR.UTILOCKTIME%TYPE := NULL;
        BEGIN
            IF (SUTILOCKED = 'Y')
            THEN
                DTUTILOCKTIME := SYSDATE;
            END IF;

            UPDATE UTILISATEUR
               SET UTIATTEMPTS = NUTIATTEMPTS,
                   UTILOCKED = SUTILOCKED,
                   UTILOCKTIME = DTUTILOCKTIME
             WHERE UTICODE = SUTICODE;

            NRETURNCODE := 0;

            IF SQL%ROWCOUNT = 0
            THEN
                NRETURNCODE := ERR_NO_ROWS_CONST;
            END IF;
        EXCEPTION
            WHEN OTHERS
            THEN
                NRETURNCODE := (-1) * SQLCODE;
        END;
    END U_LOGGEDIN;

    FUNCTION VERIFY_PASSWORD (USERNAME          UTILISATEUR.UTICODE%TYPE,
                              PASSWORD          UTILISATEUR.UTIPWD%TYPE,
                              PASSWORDCRYPTE    UTILISATEUR.UTIPWD%TYPE)
        RETURN NUMERIC
    IS
        FIRSTNAME               UTILISATEUR.UTIPRENOM%TYPE;
        LASTNAME                UTILISATEUR.UTINOM%TYPE;
        MINPWDLENGTH            SYSTEMPARAMETER.SPANUMBERVALUE%TYPE;
        MAXPWDLENGTH            SYSTEMPARAMETER.SPANUMBERVALUE%TYPE;
        SPECIAL_CHAR_ARRAY      SYSTEMPARAMETER.SPASTRINGVALUE%TYPE;
        PROHIBITED_CHAR_ARRAY   SYSTEMPARAMETER.SPASTRINGVALUE%TYPE;
        SEPARATOR               SYSTEMPARAMETER.SPASTRINGVALUE%TYPE;
        DAY_LIMIT_COUNT         SYSTEMPARAMETER.SPANUMBERVALUE%TYPE;
        OLD_PASSWORD_COUNT      SYSTEMPARAMETER.SPANUMBERVALUE%TYPE;
        DAYDIFF                 INTEGER := NULL;
    BEGIN
        SELECT UTIPRENOM, UTINOM
          INTO FIRSTNAME, LASTNAME
          FROM UTILISATEUR
         WHERE UTICODE = USERNAME;

        -- MIN LENGTH OF PWD
        BEGIN
            SELECT SPANUMBERVALUE
              INTO MINPWDLENGTH
              FROM SYSTEMPARAMETER
             WHERE SPATYPE = 'SECMINPWDLGTH';
        EXCEPTION
            WHEN OTHERS
            THEN
                MINPWDLENGTH := 0;
        END;

        -- max length of pwd
        BEGIN
            SELECT SPANUMBERVALUE
              INTO MAXPWDLENGTH
              FROM SYSTEMPARAMETER
             WHERE SPATYPE = 'SECMAXPWDLGTH';
        EXCEPTION
            WHEN OTHERS
            THEN
                MAXPWDLENGTH := 0;
        END;

        -- number that decides day limit configured
        -- to block user from changing the pwd.
        -- e.g. if value = 2 then user can not change pwd
        -- within 2 days after changing current pwd
        -- default value = 2
        BEGIN
            SELECT SPANUMBERVALUE
              INTO DAY_LIMIT_COUNT
              FROM SYSTEMPARAMETER
             WHERE SPATYPE = 'SECCHGDELAY';
        EXCEPTION
            WHEN OTHERS
            THEN
                DAY_LIMIT_COUNT := 2;
        END;

        -- number that decides how many old password
        -- should be checked with given new password
        -- user is not allowed to give historical password again
        -- while setting new password
        -- default value = 3
        BEGIN
            SELECT SPANUMBERVALUE
              INTO OLD_PASSWORD_COUNT
              FROM SYSTEMPARAMETER
             WHERE SPATYPE = 'SECHISTLGTH';
        EXCEPTION
            WHEN OTHERS
            THEN
                OLD_PASSWORD_COUNT := 3;
        END;

        BEGIN
            SELECT TRUNC (  SYSDATE
                          - (SELECT MAX (UHIDTCHG)
                               FROM UTIPWDHISTORY
                              WHERE UTICODE = USERNAME))
              INTO DAYDIFF
              FROM DUAL;
        EXCEPTION
            WHEN OTHERS
            THEN
                DAYDIFF := NULL;
        END;

        SELECT SPASTRINGVALUE
          INTO SPECIAL_CHAR_ARRAY
          FROM SYSTEMPARAMETER
         WHERE SPATYPE = 'SECSPECIALCHAR';

        SELECT SPASTRINGVALUE
          INTO PROHIBITED_CHAR_ARRAY
          FROM SYSTEMPARAMETER
         WHERE SPATYPE = 'SECPROHIBITED';

        SELECT SPASTRINGVALUE
          INTO SEPARATOR
          FROM SYSTEMPARAMETER
         WHERE SPATYPE = 'SECSEPARATOR';

        RETURN VERIFY_PASSWORD (USERNAME,
                                PASSWORD,
                                PASSWORDCRYPTE,
                                FIRSTNAME,
                                LASTNAME,
                                MINPWDLENGTH,
                                MAXPWDLENGTH,
                                SPECIAL_CHAR_ARRAY,
                                PROHIBITED_CHAR_ARRAY,
                                SEPARATOR,
                                DAYDIFF,
                                DAY_LIMIT_COUNT,
                                OLD_PASSWORD_COUNT);
    END VERIFY_PASSWORD;

    FUNCTION VERIFY_PASSWORD (
        USERNAME                 UTILISATEUR.UTICODE%TYPE,
        PASSWORD                 UTILISATEUR.UTIPWD%TYPE,
        PASSWORDCRYPTE           UTILISATEUR.UTIPWD%TYPE,
        FIRSTNAME                UTILISATEUR.UTIPRENOM%TYPE,
        LASTNAME                 UTILISATEUR.UTINOM%TYPE,
        MINPWDLENGTH             SYSTEMPARAMETER.SPANUMBERVALUE%TYPE,
        MAXPWDLENGTH             SYSTEMPARAMETER.SPANUMBERVALUE%TYPE,
        SPECIAL_CHAR_ARRAY       SYSTEMPARAMETER.SPASTRINGVALUE%TYPE,
        PROHIBITED_CHAR_ARRAY    SYSTEMPARAMETER.SPASTRINGVALUE%TYPE,
        SEPARATOR                SYSTEMPARAMETER.SPASTRINGVALUE%TYPE,
        DAYDIFF                  INTEGER,
        DAY_LIMIT_COUNT          SYSTEMPARAMETER.SPANUMBERVALUE%TYPE,
        OLD_PASSWORD_COUNT       SYSTEMPARAMETER.SPANUMBERVALUE%TYPE)
        RETURN NUMERIC
    IS
        PWD_SIZE           INTEGER;
        SEPARATOR_SIZE     INTEGER;
        RETURN_CODE        INTEGER;
        IARRAYLENGTH       PLS_INTEGER;
        IS_PROHIBITED      BOOLEAN;
        IS_LOWERCHAR       BOOLEAN;
        IS_UPPERCHAR       BOOLEAN;
        IS_SPECIAL         BOOLEAN;
        IND                INTEGER;
        IND2               INTEGER;
        UPPER_CHAR_ARRAY   VARCHAR2 (30);
        LOWER_CHAR_ARRAY   VARCHAR2 (30);

        CURSOR C_MARCHIN
        IS
              SELECT UHIPWD
                FROM UTIPWDHISTORY
               WHERE UTICODE = USERNAME AND ROWNUM <= OLD_PASSWORD_COUNT
            ORDER BY UHIDTCHG DESC;
    BEGIN
        RETURN_CODE := 0;
        IND := 0;
        IND2 := 0;
        LOWER_CHAR_ARRAY := 'abcdefghijklmnopqrstuvwxyz';
        UPPER_CHAR_ARRAY := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
        SEPARATOR_SIZE := LENGTH (SEPARATOR);
        PWD_SIZE := LENGTH (PASSWORD);

        -- Check if the password is same as the username
        IF NLS_LOWER (PASSWORD) = NLS_LOWER (USERNAME)
        THEN
            RETURN ERR_PWD_EQUALS_LOGIN;
        END IF;

        -- Check if the password is same as the firstname
        IF NLS_LOWER (PASSWORD) = NLS_LOWER (FIRSTNAME)
        THEN
            RETURN ERR_PWD_EQUALS_LOGIN;
        END IF;

        -- Check if the password is same as the lastname
        IF NLS_LOWER (PASSWORD) = NLS_LOWER (LASTNAME)
        THEN
            RETURN ERR_PWD_EQUALS_LOGIN;
        END IF;

        --Check if the password change is less than or equal to 2 days
        IF DAYDIFF <= DAY_LIMIT_COUNT
        THEN
            RETURN ERR_PWD_DAYLIMIT;
        END IF;

        -- Check if the password is same as the n last passwords
        FOR REC_MARCHIN IN C_MARCHIN
        LOOP
            IF PASSWORDCRYPTE = REC_MARCHIN.UHIPWD
            THEN
                RETURN ERR_PWD_EQUALS_OLD_PWD;
            END IF;
        END LOOP;

        -- Check for the minimum length of the password
        IF PWD_SIZE < MINPWDLENGTH
        THEN
            RETURN ERR_PWD_SIZE;
        END IF;

        -- Check for the maximum length of the password
        IF PWD_SIZE > MAXPWDLENGTH
        THEN
            RETURN ERR_PWD_SIZE;
        END IF;

        -- Check if the password contains at least one lowercase letter, one uppercase letter one special character and no prohibited one
        -- 1. Check for prohibited characters
        IS_PROHIBITED := FALSE;

        WHILE IND <> 0
        LOOP
            IND2 := INSTR (PROHIBITED_CHAR_ARRAY, SEPARATOR, IND + 1);

            IF IND = 1
            THEN
                IND := 1 - SEPARATOR_SIZE;
            END IF;

            IF IND2 = 0
            THEN
                IARRAYLENGTH :=
                    1 + LENGTH (PROHIBITED_CHAR_ARRAY) - IND - SEPARATOR_SIZE;
            ELSE
                IARRAYLENGTH := IND2 - IND - SEPARATOR_SIZE;
            END IF;

            FOR J IN 1 .. PWD_SIZE
            LOOP
                IF SUBSTR (PASSWORD, J, IARRAYLENGTH) =
                   SUBSTR (PROHIBITED_CHAR_ARRAY,
                           IND + SEPARATOR_SIZE,
                           IARRAYLENGTH)
                THEN
                    IS_PROHIBITED := TRUE;
                    RETURN ERR_PWD_FORBIDEN_CHAR;
                END IF;
            END LOOP;

            IND := IND2;
        END LOOP;

        IF IS_PROHIBITED = FALSE
        THEN
            GOTO FINDLOWERCHAR;
        END IF;

       -- 2. Check for lower and upper characters
       <<FINDLOWERCHAR>>
        IS_LOWERCHAR := FALSE;
        IS_UPPERCHAR := FALSE;
        IARRAYLENGTH := LENGTH (LOWER_CHAR_ARRAY);

        FOR I IN 1 .. IARRAYLENGTH
        LOOP
            FOR J IN 1 .. PWD_SIZE
            LOOP
                IF SUBSTR (PASSWORD, J, 1) = SUBSTR (LOWER_CHAR_ARRAY, I, 1)
                THEN
                    IS_LOWERCHAR := TRUE;
                ELSIF SUBSTR (PASSWORD, J, 1) =
                      SUBSTR (UPPER_CHAR_ARRAY, I, 1)
                THEN
                    IS_UPPERCHAR := TRUE;
                END IF;

                IF IS_UPPERCHAR = TRUE AND IS_LOWERCHAR = TRUE
                THEN
                    GOTO FINDSPECIAL;
                END IF;
            END LOOP;
        END LOOP;

        RETURN ERR_PWD_CASE;

       -- 3. Check for special characters
       <<FINDSPECIAL>>
        IS_SPECIAL := FALSE;
        IARRAYLENGTH := LENGTH (SPECIAL_CHAR_ARRAY);

        FOR I IN 1 .. IARRAYLENGTH
        LOOP
            FOR J IN 1 .. PWD_SIZE
            LOOP
                IF SUBSTR (PASSWORD, J, 1) =
                   SUBSTR (SPECIAL_CHAR_ARRAY, I, 1)
                THEN
                    IS_SPECIAL := TRUE;
                    GOTO ENDSEARCH;
                END IF;
            END LOOP;
        END LOOP;

        IF IS_SPECIAL = FALSE
        THEN
            RETURN ERR_PWD_SPECIAL_CHAR;
        END IF;

       <<ENDSEARCH>>
        -- Everything is fine; return 0 ;
        RETURN RETURN_CODE;
    END VERIFY_PASSWORD;

    PROCEDURE U_CHANGEPWD (SUTICODE             UTILISATEUR.UTICODE%TYPE,
                           SUTIPWD              UTILISATEUR.UTIPWD%TYPE,
                           NRETURNCODE   IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            NBOLDPWD   INTEGER := 0;
            SOLDPWD    UTILISATEUR.UTIPWD%TYPE;
        BEGIN
            SELECT UTIPWD
              INTO SOLDPWD
              FROM UTILISATEUR
             WHERE UTICODE = SUTICODE;

            IF (SOLDPWD IS NOT NULL)
            THEN
                SELECT COUNT (*)
                  INTO NBOLDPWD
                  FROM UTIPWDHISTORY
                 WHERE UTICODE = SUTICODE;

                IF (NBOLDPWD >= 4)
                THEN
                    UPDATE UTIPWDHISTORY
                       SET UHIDTCHG = SYSDATE, UHIPWD = SOLDPWD
                     WHERE     UTICODE = SUTICODE
                           AND UHIDTCHG = (SELECT MIN (UHIDTCHG)
                                             FROM UTIPWDHISTORY
                                            WHERE UTICODE = SUTICODE);
                ELSE
                    INSERT INTO UTIPWDHISTORY (UTICODE, UHIDTCHG, UHIPWD)
                         VALUES (SUTICODE, SYSDATE, SOLDPWD);
                END IF;
            END IF;

            UPDATE UTILISATEUR
               SET UTIPWD = SUTIPWD,
                   UTIPWDDTCHGD = SYSDATE,
                   UTIWHODUNNIT = SUTICODE,
                   UTIFLAGPASSWORD = 0
             WHERE UTICODE = SUTICODE;

            NRETURNCODE := 0;

            IF SQL%ROWCOUNT = 0
            THEN
                NRETURNCODE := ERR_NO_ROWS_CONST;
            END IF;
        END;
    END U_CHANGEPWD;

    PROCEDURE U_CHANGEPWDSALT (
        SUTICODE             UTILISATEUR.UTICODE%TYPE,
        SUTIPWDSALT          UTILISATEUR.UTIPWDSALT%TYPE,
        NRETURNCODE   IN OUT NUMBER)
    AS
    BEGIN
        BEGIN
            UPDATE UTILISATEUR
               SET UTIPWDSALT = SUTIPWDSALT
             WHERE UTICODE = SUTICODE AND UTIPWDSALT IS NULL;

            NRETURNCODE := 0;

            IF SQL%ROWCOUNT = 0
            THEN
                NRETURNCODE := ERR_NO_ROWS_CONST;
            END IF;
        END;
    END U_CHANGEPWDSALT;

    PROCEDURE U_CHANGEPWD_DBA (
        SUTICODE               UTILISATEUR.UTICODE%TYPE,
        SUTIPWD                UTILISATEUR.UTIPWD%TYPE,
        SUTIWHODUNNIT          UTILISATEUR.UTIWHODUNNIT%TYPE,
        NRETURNCODE     IN OUT NUMBER)
    AS
    BEGIN
        DECLARE
            NBOLDPWD   INTEGER := 0;
            SOLDPWD    UTILISATEUR.UTIPWD%TYPE;
        BEGIN
            SELECT UTIPWD
              INTO SOLDPWD
              FROM UTILISATEUR
             WHERE UTICODE = SUTICODE;

            IF (SOLDPWD IS NOT NULL)
            THEN
                SELECT COUNT (*)
                  INTO NBOLDPWD
                  FROM UTIPWDHISTORY
                 WHERE UTICODE = SUTICODE;

                IF (NBOLDPWD >= 4)
                THEN
                    UPDATE UTIPWDHISTORY
                       SET UHIDTCHG = SYSDATE, UHIPWD = SOLDPWD
                     WHERE     UTICODE = SUTICODE
                           AND UHIDTCHG = (SELECT MIN (UHIDTCHG)
                                             FROM UTIPWDHISTORY
                                            WHERE UTICODE = SUTICODE);
                ELSE
                    INSERT INTO UTIPWDHISTORY (UTICODE, UHIDTCHG, UHIPWD)
                         VALUES (SUTICODE, SYSDATE, SOLDPWD);
                END IF;
            END IF;

            -- SPA - CFS 53240 artf1582853 : Impossible to "unlock" a locked user
            -- When a DBA change password for a user, it is required to reset the UTIATTEMPTS and UTILOCKTIME
            UPDATE UTILISATEUR
               SET UTIPWD = SUTIPWD,
                   UTIDTUPD = SYSDATE,
                   UTIWHODUNNIT = SUTIWHODUNNIT,
                   UTIFLAGPASSWORD = 1,
                   UTILOCKED = 'N',
                   UTIATTEMPTS = 0,
                   UTILOCKTIME = NULL
             WHERE UTICODE = SUTICODE;

            NRETURNCODE := 0;

            IF SQL%ROWCOUNT = 0
            THEN
                NRETURNCODE := ERR_NO_ROWS_CONST;
            END IF;
        END;
    END U_CHANGEPWD_DBA;

    PROCEDURE S_UTILISATEUR (PC_RETURN IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT UTICODE FROM UTILISATEUR;
    END S_UTILISATEUR;

    PROCEDURE S_UTILISATEUR (SUTICODE           UTILISATEUR.UTICODE%TYPE,
                             PC_RETURN   IN OUT T_CURSOR)
    AS
    BEGIN
        OPEN PC_RETURN FOR SELECT UTICODE, UTIPWDSALT
                             FROM UTILISATEUR
                            WHERE UTICODE = SUTICODE;
    END S_UTILISATEUR;

    PROCEDURE U_LOGOFFTIME (sUTICODE             USERACTIVITY.UACUTICODE%TYPE,
                            nReturnCode   IN OUT NUMBER)
    AS
    BEGIN
        UPDATE USERACTIVITY
           SET UACLOGOFFTIME = LOCALTIMESTAMP
         WHERE UACUTICODE = sUTICODE AND UACLOGOFFTIME IS NULL;

        nReturnCode := 0;
    EXCEPTION
        WHEN OTHERS
        THEN
            nReturnCode := (-1) * SQLCODE;
    END U_LOGOFFTIME;

    PROCEDURE U_FLAG_PASSWORD (sUTICODE UTILISATEUR.UTICODE%TYPE)
    AS
    BEGIN
        UPDATE UTILISATEUR
           SET UTIPWDDTCHGD = SYSDATE,
               UTIWHODUNNIT = sUTICODE,
               UTIFLAGPASSWORD = 1
         WHERE UTICODE = sUTICODE;
    END U_FLAG_PASSWORD;
END PAV4_PASSWORD;
