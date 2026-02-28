      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTE-TIPOS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VARIAVEIS.
           05  VALOR-DISPLAY    PIC 9(6)      VALUE 123456.
           05  VALOR-COMP       PIC S9(6) COMP VALUE 123456.
           05  VALOR-COMP3      PIC S9(6) COMP-3 VALUE 123456.

       PROCEDURE DIVISION.
           DISPLAY 'DISPLAY:   ' VALOR-DISPLAY
           DISPLAY 'COMP:      ' VALOR-COMP
           DISPLAY 'COMP-3:    ' VALOR-COMP3
           STOP RUN.
