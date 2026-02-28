      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 10/04/22
      * Purpose: MEU PRIMEIRO PROGRAMA
      * Tectonics: APRENDA COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OLAMUNDO.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR1             PIC X(10) VALUE 'OLA MUNDO!'.
       PROCEDURE DIVISION.
       000-INICIO.
            DISPLAY "MEU PRIMEIRO PROGRAMA: " WS-VAR1.
            STOP RUN.
       END PROGRAM OLAMUNDO.
