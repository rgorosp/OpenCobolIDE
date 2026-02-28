      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 04-10-22
      * Purpose: SUBROTINA PROGRAMA CHAMADO2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADO2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-VAR.
           03 WS-NUM1        PIC 9(03).
           03 WS-NUM2        PIC 9(03).
           03 WS-RETURN      PIC Z(06).
       PROCEDURE DIVISION USING WS-VAR.
       S000 SECTION.
       P000-INICIO.
            DISPLAY " "
            DISPLAY ">>>>> SUB-ROTINA CHAMADO2 <<<<<"
            DISPLAY "VARIAVEL 1 RECEBIDA..: " WS-NUM1
            DISPLAY "VARIAVEL 2 RECEBIDA..: " WS-NUM2

            COMPUTE WS-RETURN = (WS-NUM1 * WS-NUM2)

            DISPLAY ">>>> TERMINO DA SUB-ROTINA <<<<"
            DISPLAY " ".
       P000-EXIT.

       S999 SECTION.
       P999-FIM.
            GOBACK.
       P999-EXIT.
       END PROGRAM PGM-CHAMADO2.
