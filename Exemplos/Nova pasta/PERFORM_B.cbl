      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-04-24
      * Purpose: ESTRUTURA DE REPETIÇÃO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-PERFORM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-COUNT       PIC 99.
           03 WS-TOT         PIC 9(02).
           03 WS-IND         PIC 9(02).
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            INITIALIZE WS-VAR
            PERFORM P010-CALCULO THRU P010-EXIT
            PERFORM P020-CALCULO THRU P020-EXIT
            PERFORM P030-CALCULO THRU P030-EXIT
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S010 SECTION.
       P010-CALCULO.
            DISPLAY ">>>>> PARAGRAFO P010-CALCULO <<<<<"
            SET WS-COUNT TO 10
            PERFORM WS-COUNT TIMES
               ADD 1 TO WS-TOT
               DISPLAY "VALOR..: " WS-TOT
            END-PERFORM
            DISPLAY " "
            .
       P010-EXIT.
            EXIT.

       S020 SECTION.
       P020-CALCULO.
            DISPLAY ">>>>> PARAGRAFO P020-CALCULO <<<<<"
            PERFORM WITH TEST BEFORE UNTIL WS-IND = WS-COUNT
               ADD 1 TO WS-IND
               DISPLAY "VALOR..: " WS-IND
            END-PERFORM
            DISPLAY " "
            .
       P020-EXIT.
            EXIT.

       P030-CALCULO.
            DISPLAY ">>>>> PARAGRAFO P030-CALCULO <<<<<"
            DISPLAY "WS-TOT..: " WS-TOT
            DISPLAY "WS-COUNT: " WS-COUNT
            PERFORM VARYING WS-TOT FROM 1 BY 2 UNTIL WS-TOT > WS-COUNT
               DISPLAY "VALOR..: " WS-TOT
            END-PERFORM
            DISPLAY " ".
       P030-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM PGM-PERFORM.
