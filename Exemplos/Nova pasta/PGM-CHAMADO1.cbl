      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 23-09-22
      * Purpose: PROGRAMA CHAMADOR DA SUB-ROTINA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADO1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-PARAMETRO.
           03 WS-NUM1          PIC 9(03) VALUE ZEROS.
           03 WS-NUM2          PIC 9(03) VALUE ZEROS.
           03 WS-RESULT        PIC 9(04) VALUE ZEROS.
       PROCEDURE DIVISION USING WS-PARAMETRO.
       S100-INICIO SECTION.
       P101-INICIO.
            DISPLAY ' '
            DISPLAY '***** PROGRAMA SUB-ROTINA *****'
            DISPLAY 'WS-NUM1 = ' WS-NUM1
            DISPLAY 'WS-NUM2 = ' WS-NUM2

            COMPUTE WS-RESULT = (WS-NUM1 * WS-NUM2)

            DISPLAY "***** TERMINO OK DA SUB-ROTINA *****".
       P101-EXIT.
            EXIT.

       S990-FIM SECTION.
       P999-FIM.
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM PGM-CHAMADO1.
