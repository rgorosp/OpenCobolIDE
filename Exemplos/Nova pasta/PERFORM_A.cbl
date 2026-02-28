      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 21/02/24
      * Purpose: EXEMPLO COM O COMANDO PERFORM(PARAGRAFOS)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM_A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       PROCEDURE DIVISION.

             PERFORM P003-INICIO THRU P003-FIM.
             PERFORM P001-INICIO THRU P001-FIM.
             PERFORM P002-INICIO.
             PERFORM S001.
             PERFORM P999-INICIO THRU P999-FIM.

       S001 SECTION.
       P001-INICIO.
             DISPLAY 'PARAGRAFO 1'.
       P001-FIM.

       P002-INICIO.
             DISPLAY 'PARAGRAFO 2'.
       P002-FIM.
            EXIT.

       S003 SECTION.
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
       END PROGRAM PERFORM_A.
