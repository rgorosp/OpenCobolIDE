      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28/04/24
      * Purpose: EXEMPLO COM O COMANDO SECTION(SESSÕES)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SECTION_A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.
       S000-INICIO.
       P000-INI.
             PERFORM S002
             PERFORM S001
             PERFORM S999
             .
       P000-EXIT.
            EXIT.

       S001 SECTION.
       P001-INICIO.
             DISPLAY 'PARAGRAFO 1'.
       P001-FIM.
            EXIT.

       S002 SECTION.
       P002-INICIO.
             DISPLAY 'PARAGRAFO 2'.
       P002-FIM.

       P003-INICIO.
             DISPLAY 'PARAGRAFO 3'.
       P003-FIM.
            EXIT.

       S999 SECTION.
       P999-INICIO.
            DISPLAY "FIM DO PROGRAMA"
            STOP RUN.
       P999-FIM.
            EXIT.
       END PROGRAM SECTION_A.
