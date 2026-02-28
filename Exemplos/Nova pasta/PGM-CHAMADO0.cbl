      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-04-24
      * Purpose: PROGRAMA CHAMADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADO0.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-PARM.
           03 WS-PARM1         PIC X(10).
       PROCEDURE DIVISION USING WS-PARM.
       S100-INICIO SECTION.
       P101-INI.
            DISPLAY "***** SUB-ROTINA PGM-CHAMADO0 *****"
            DISPLAY "NOME RECEBIDO: " WS-PARM1

            DISPLAY "SUB-ROTINA EXECUTADA COM SUCESSO".
       P101-EXIT.
            EXIT.

       S900-FIM SECTION.
       P901-FIM.
            GOBACK.
       P901-EXIT.
            EXIT.
       END PROGRAM PGM-CHAMADO0.
