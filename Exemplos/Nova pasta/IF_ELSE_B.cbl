      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 28-10-24.
      * Purpose: Capítulo 03 - Aula 05 - Prática 01
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A05P01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1                 PIC 9(04) VALUE ZEROS.
       77  WS-VAR2                 PIC 9(04) VALUE ZEROS.
       77  WS-VAR3                 PIC 9(04) VALUE ZEROS.
       77  WS-CONT                 PIC 9(04) VALUE ZEROS.
       PROCEDURE DIVISION.
       A-INI SECTION.
       A-INICIO.
            PERFORM A-DISPLAY
            PERFORM A-VAR1-A UNTIL WS-VAR1 > 0 AND WS-VAR1 < 1000
            PERFORM A-VAR2-A UNTIL WS-VAR2 > 0 AND WS-VAR2 < 1000
            PERFORM A-VAR3-A UNTIL WS-VAR3 > 0 AND WS-VAR3 < 1000
            PERFORM A-CONTROLE.
       A-INICIO-EXIT.
            EXIT.

       A-DIS SECTION.
       A-DISPLAY.
            DISPLAY "CONTEUDO VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "CONTEUDO VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "CONTEUDO VAR3 = " WS-VAR3 UPON CONSOLE
            DISPLAY "                " UPON CONSOLE
            .
       A-DISPLAY-EXIT.
            EXIT.

       A-VA1 SECTION.
       A-VAR1-A.
            DISPLAY "INSIRA VALOR PARA VAR1 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR1 FROM CONSOLE
            .
       A-VAR1-B.
            IF WS-VAR1 <= 0
               DISPLAY "VAR1 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR1-A
            END-IF
            .
       A-VAR1-EXIT.
            EXIT.

       A-VA2 SECTION.
       A-VAR2-A.
            DISPLAY "INSIRA VALOR PARA VAR2 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR2 FROM CONSOLE
            .
       A-VAR2-B.
            IF WS-VAR2 <= 0
               DISPLAY "VAR2 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR2-A
            END-IF.
       A-VAR2-EXIT.
            EXIT.

       A-VA3 SECTION.
       A-VAR3-A.
            DISPLAY "INSIRA VALOR PARA VAR3 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR3 FROM CONSOLE
            .
       A-VAR3-B.
            IF WS-VAR3 <= 0
               DISPLAY "VAR3 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR3-A
            END-IF
            .
       A-VAR3-EXIT.
            EXIT.

       A-CONTROLE SECTION.
       A-CONTROLE-INICIO.
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
               GO TO A-CONTROLE
            END-IF

            DISPLAY "APOS CONTROLE VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "APOS CONTROLE VAR3 = " WS-VAR3 UPON CONSOLE
            STOP RUN
            .
       A-CONTROLE-EXIT.
            EXIT.
       END PROGRAM C3A05P01.
