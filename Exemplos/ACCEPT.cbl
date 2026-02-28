      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 13-05-22
      * Purpose: COMANDO ACCEPT (ACEITAR)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-ACCEPT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DATE-AA        PIC X(06) VALUE SPACES.
       01  WS-DATE-AAAA.
           03 WS-DIA-AA      PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE '/'.
           03 WS-MES-AA      PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE '/'.
           03 WS-ANO-AA      PIC X(02) VALUE SPACES.

       77  WS-DATE-BB        PIC X(08) VALUE SPACES.
       01  WS-DATE-BBBB.
           03 WS-DIA-BB      PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE '/'.
           03 WS-MES-BB      PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE '/'.
           03 WS-ANO-BB      PIC X(04) VALUE SPACES.

       77  WS-DAY            PIC X(05) VALUE SPACES.
       77  WS-SEMANA         PIC X(02) VALUE SPACES.

       77  WS-TIME           PIC X(08) VALUE SPACES.
       01  WS-TIME-AA.
           03 WS-HR          PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE ':'.
           03 WS-MM          PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE ':'.
           03 WS-SS          PIC X(02) VALUE SPACES.

       01  WS-TIME-BB.
           03 WS-HR-BB       PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE ':'.
           03 WS-MM-BB       PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE ':'.
           03 WS-SS-BB       PIC X(02) VALUE SPACES.
           03 FILLER         PIC X VALUE ':'.
           03 WS-ML-BB       PIC X(02) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
            ACCEPT WS-DATE-AA FROM DATE
            DISPLAY "DATA COM 6 CARACTERES = " WS-DATE-AA
            MOVE WS-DATE-AA(5:2) TO WS-DIA-AA
            MOVE WS-DATE-AA(3:2) TO WS-MES-AA
            MOVE WS-DATE-AA(1:2) TO WS-ANO-AA
            DISPLAY "DATA COM 8 CARACTERES FORMATADO = " WS-DATE-AAAA

            ACCEPT WS-DATE-BB FROM DATE YYYYMMDD
            DISPLAY "DATA COM 8 CARACTERES = " WS-DATE-BB
            MOVE WS-DATE-BB(7:2) TO WS-DIA-BB
            MOVE WS-DATE-BB(5:2) TO WS-MES-BB
            MOVE WS-DATE-BB(1:4) TO WS-ANO-BB
            DISPLAY "DATA COM 10 CARACTERES FORMATADO = " WS-DATE-BBBB

            ACCEPT WS-DAY FROM DAY
            DISPLAY "DIA: " WS-DAY

            ACCEPT WS-SEMANA FROM DAY-OF-WEEK
            DISPLAY "DIA DA SEMANA: " WS-SEMANA

            ACCEPT WS-TIME FROM TIME
            DISPLAY "TIME COM 8 DIGITOS: " WS-TIME
            MOVE WS-TIME(1:2) TO WS-HR WS-HR-BB
            MOVE WS-TIME(3:2) TO WS-MM WS-MM-BB
            MOVE WS-TIME(5:2) TO WS-SS WS-SS-BB
            MOVE WS-TIME(7:2) TO WS-ML-BB
            DISPLAY "TIME COM 8 DIGITOS FORMATADO: " WS-TIME-AA
            DISPLAY "TIME COM 11 DIGITOS FORMATADO: " WS-TIME-BB.
       P001-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM PGM-ACCEPT.
