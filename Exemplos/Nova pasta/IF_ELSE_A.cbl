      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 26-10-24.
      * Purpose: Capítulo 03 - Aula 04 - Prática 01
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A04P01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1                 PIC 999 VALUE ZEROS.
       77  WS-VAR2                 PIC 999 VALUE ZEROS.
       77  WS-VAR3                 PIC 999 VALUE ZEROS.
       77  WS-CONT                 PIC 999 VALUE ZEROS.
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            PERFORM P010-DISPLAY
            PERFORM P020-CONTROLE THRU P020-EXIT
            PERFORM P030-PROCESSO
            PERFORM P090-TERMINO.
       P000-EXIT.
            EXIT.

       S010 SECTION.
       P010-DISPLAY.
            DISPLAY "CONTEUDO VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "CONTEUDO VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "CONTEUDO VAR3 = " WS-VAR3 UPON CONSOLE
            DISPLAY "                "         UPON CONSOLE
            .
       P010-EXIT.
            EXIT.

       S020 SECTION.
       P020-CONTROLE.
            DISPLAY "INSIRA VALOR PARA VAR1 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR1 FROM CONSOLE

            IF WS-VAR1 <= 0
               DISPLAY "VAR1 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO P020-CONTROLE
            END-IF
            .
       P021-CONTROLE.
            DISPLAY "INSIRA VALOR PARA VAR2 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR2 FROM CONSOLE

            IF WS-VAR2 <= 0
               DISPLAY "VAR2 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO P021-CONTROLE
            END-IF
            .
       P022-CONTROLE.
            DISPLAY "INSIRA VALOR PARA VAR3 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR3 FROM CONSOLE

            IF WS-VAR3 <= 0
               DISPLAY "VAR3 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO P022-CONTROLE
            END-IF
            .
       P020-EXIT.
            EXIT.

       S030 SECTION.
       P030-PROCESSO.
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

            IF WS-CONT GREATER WS-VAR1 OR WS-CONT GREATER WS-VAR2
               GO TO P020-CONTROLE
            END-IF.
       P030-EXIT.
            EXIT.

       S090 SECTION.
       P090-TERMINO.
            DISPLAY "APOS CONTROLE VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR3 = " WS-VAR3 UPON CONSOLE
            STOP RUN.
       P090-EXIT.
            EXIT.
       END PROGRAM C3A04P01.
