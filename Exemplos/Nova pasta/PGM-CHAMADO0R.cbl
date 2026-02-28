      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-04-24
      * Purpose: PROGRAMA CHAMADO0R
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADO0R.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-PARM.
           03 WS-PARM1         PIC X(10).
       PROCEDURE DIVISION.
       S100-INICIO SECTION.
       P101-INI.
            DISPLAY "***** PROGRAMA PRINCIPAL *****"
            DISPLAY "DIGITE UM NOME..: "
            ACCEPT WS-PARM1

            CALL "PGM-CHAMADO0" USING WS-PARM

            DISPLAY "PROGRAMA EXECUTADO COM SUCESSO".
       P101-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TER.
            GOBACK.
       P901-EXIT.
            EXIT.
       END PROGRAM PGM-CHAMADO0R.
