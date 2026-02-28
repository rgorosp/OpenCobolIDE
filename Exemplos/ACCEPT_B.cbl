      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 17-09-24.
      * Purpose: Capítulo 03 - Aula 03 - Prática 01
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A03P01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1                 PIC 999 VALUE ZEROS.
       77  WS-VAR2                 PIC 999 VALUE ZEROS.
       77  WS-VAR3                 PIC 999 VALUE ZEROS.
       77  WS-CONT                 PIC 999 VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       A-INICIO SECTION.
       A-INI.
            PERFORM A-DISP-P
            PERFORM A-VAR1-P
            PERFORM A-VAR2-P
            PERFORM A-VAR3-P
            PERFORM A-CONTROLE-P.
       A-INI-EXIT.
            EXIT.

       A-DISPLAY SECTION.
       A-DISP-P.
            DISPLAY "CONTEUDO VAR1 = " WS-VAR1 UPON CONSOLE
            DISPLAY "CONTEUDO VAR2 = " WS-VAR2 UPON CONSOLE
            DISPLAY "CONTEUDO VAR3 = " WS-VAR3 UPON CONSOLE
            DISPLAY "                " UPON CONSOLE.
       A-DISP-P-EXIT.
            EXIT.

       A-VAR1 SECTION.
       A-VAR1-P.
            DISPLAY "INSIRA VALOR PARA VAR1 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR1 FROM CONSOLE
            IF WS-VAR1 <= 0
               DISPLAY "VAR1 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR1
            END-IF.
       A-VAR1-P-EXIT.
            EXIT.

       A-VAR2 SECTION.
       A-VAR2-P.
            DISPLAY "INSIRA VALOR PARA VAR2 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR2 FROM CONSOLE
            IF WS-VAR2 <= 0
               DISPLAY "VAR2 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR2
            END-IF.
       A-VAR2-P-EXIT.
            EXIT.

       A-VAR3 SECTION.
       A-VAR3-P.
            DISPLAY "INSIRA VALOR PARA VAR3 DE 1 A 999" UPON CONSOLE
            ACCEPT WS-VAR3 FROM CONSOLE
            IF WS-VAR3 <= 0
               DISPLAY "VAR3 INVALIDO INSIRA NOVAMENTE" UPON CONSOLE
               GO TO A-VAR3
            END-IF.
       A-VAR3-P-EXIT.
            EXIT.

       A-CONTROLE SECTION.
       A-CONTROLE-P.
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

            PERFORM A-DISPLAY
            PERFORM 999-FIM.
       A-CONTROLE-P-EXIT.
            EXIT.

       999-FIM SECTION.
       999-FIM-P.
            STOP RUN.
       999-FIM-P-EXIT.
            EXIT.
       END PROGRAM C3A03P01.
