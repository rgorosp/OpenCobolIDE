******************************************************************
      * Author: EMERSON S MOTTA.
      * Date: 10-07-24.
      * Purpose: RECEBER 3 VARIAVEIS E GERAR CONTROLE DAS VARIAVEIS
      *          EM DESCENDENTE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A02P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1                 PIC 999 VALUE ZEROS.
       77  WS-VAR2                 PIC 999 VALUE ZEROS.
       77  WS-VAR3                 PIC 999 VALUE ZEROS.
       77  WS-CONT                 PIC 999 VALUE ZEROS.
       PROCEDURE DIVISION.
       P000-INICIO.
            DISPLAY "CONTEUDO VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "CONTEUDO VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "CONTEUDO VAR3 = " WS-VAR3 UPON CONSOLE
            DISPLAY "                " UPON CONSOLE.
       P000-EXIT.

       A-VAR1.
            DISPLAY "INSIRA VALOR PARA VAR1 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR1 FROM CONSOLE
            IF WS-VAR1 <= 0
               DISPLAY "VAR1 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR1
            END-IF.
       A-VAR1-EXIT.

       A-VAR2.
            DISPLAY "INSIRA VALOR PARA VAR2 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR2 FROM CONSOLE
            IF WS-VAR2 <= 0
               DISPLAY "VAR2 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR2
            END-IF.
       A-VAR2-EXIT.

       A-VAR3.
            DISPLAY "INSIRA VALOR PARA VAR3 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR3 FROM CONSOLE
            IF WS-VAR3 <= 0
               DISPLAY "VAR3 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR3
            END-IF.
       A-VAR3-EXIT.

       A-CONTROLE.
            IF WS-VAR3 GREATER WS-VAR2
               MOVE WS-VAR3 TO WS-CONT
               MOVE WS-VAR2 TO WS-VAR3
               MOVE WS-CONT TO WS-VAR2
            END-IF

            IF WS-VAR1 LESS WS-VAR2
               MOVE WS-VAR1 TO WS-CONT
               MOVE WS-VAR2 TO WS-VAR1
               MOVE WS-CONT TO WS-VAR2
            END-IF

            IF WS-VAR2 LESS WS-VAR3
               MOVE WS-VAR2 TO WS-CONT
               MOVE WS-VAR3 TO WS-VAR2
               MOVE WS-CONT TO WS-VAR3
            END-IF

            IF WS-CONT    GREATER WS-VAR1
               OR WS-CONT GREATER WS-VAR2
               GO TO A-CONTROLE
            END-IF

            DISPLAY "APOS CONTROLE VAR1 = " WS-VAR1  UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR2 = " WS-VAR2  UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR3 = " WS-VAR3  UPON CONSOLE.
       A-CONTROLE-EXIT.

       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM C3A02P01.
