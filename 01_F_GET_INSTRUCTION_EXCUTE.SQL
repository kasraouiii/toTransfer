 FUNCTION F_GET_INSTRUCTION_EXCUTE (nDosId NUMBER, sUtiCode VARCHAR2)
      RETURN VARCHAR2
   IS
      nCount     NUMBER;
      l_Result   VARCHAR2 (10) := 'FALSE';
   BEGIN
      BEGIN
        SELECT COUNT(*) into nCount FROM DPRPHASE 
         WHERE DOSID = nDosId 
         AND JALCODE = 'DVCLT';

         IF nCount >= 1
         THEN
            l_Result := 'TRUE';
         END IF;
      EXCEPTION
         WHEN OTHERS
         THEN
            l_result := 'FALSE';
      END;

      RETURN l_result;
   END;
