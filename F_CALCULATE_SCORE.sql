CREATE OR REPLACE FUNCTION F_CALCULATE_SCORE (p_dosid NUMBER)
    RETURN NUMBER
IS
    VAL1    VARCHAR2 (100);
    VAL2    VARCHAR2 (100);
    VAL3    VARCHAR2 (100);
    VAL4    VARCHAR2 (100);
    VAL5    VARCHAR2 (100);
    VAL6    VARCHAR2 (100);
    VAL7    VARCHAR2 (100);
    VAL8    VARCHAR2 (100);
    VAL9    VARCHAR2 (100);
    VAL10   VARCHAR2 (100);
    VAL11   VARCHAR2 (100);
    VAL12   VARCHAR2 (100);
    VAL13   VARCHAR2 (100);
    VAL14   VARCHAR2 (100);
    VAL15   VARCHAR2 (100);
BEGIN
    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL1
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71001;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL2
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71002;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL3
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71003;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL4
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71004;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL5
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71005;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL6
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71006;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL7
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71007;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL8
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71008;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL9
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71009;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL10
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71010;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL11
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71011;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL12
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71012;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL13
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71013;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL14
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71014;

    SELECT NVL (MAX (ratvalue), '0')
      INTO VAL15
      FROM lkanarat
     WHERE     anaid = (SELECT MAX (anaid)
                          FROM analysis
                         WHERE dosid = p_dosid AND anmid = 7000)
           AND ratid = 71019;


    IF (    VAL1 != '0'
        AND VAL2 != '0'
        AND VAL3 != '0'
        AND VAL4 != '0'
        AND VAL5 != '0'
        AND VAL6 != '0'
        AND VAL7 != '0'
        AND VAL8 != '0'
        AND VAL9 != '0'
        AND VAL10 != '0'
        AND VAL11 != '0'
        AND VAL12 != '0'
        AND VAL13 != '0'
        AND VAL14 != '0'
        AND VAL15 != '0')
    THEN
        RETURN 1;
    ELSE
        RETURN 0;
    END IF;
END;
/