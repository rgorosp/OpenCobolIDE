      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 22/05/24
      * Purpose: COMANDO INSPECT PARA CONTAR E ALTERAR CAMPOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-INSPECT.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DATA           PIC X(08) VALUE SPACES.
       01  WS-DATA-E.
           03 WS-DD          PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WS-MM          PIC 99.
           03 FILLER         PIC X VALUE "/".
           03 WS-AA          PIC 9999.
       77  WS-CONT           PIC 9(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000 SECTION.
       000-INICIO.
            ACCEPT WS-DATA FROM DATE YYYYMMDD

            MOVE WS-DATA(7:2)  TO WS-DD
            MOVE WS-DATA(5:2)  TO WS-MM
            MOVE WS-DATA(1:4)  TO WS-AA

            DISPLAY "DATA...........: " WS-DATA
            DISPLAY "DATA-E.........: " WS-DATA-E

            INSPECT WS-DATA-E TALLYING WS-CONT FOR ALL "/"
            BEFORE WS-AA

            DISPLAY "QTD CONTEM (/).: " WS-CONT

            INSPECT WS-DATA-E REPLACING ALL "/" BY "-" AFTER WS-DD

            DISPLAY "DATA-E ALTERADA: " WS-DATA-E.
       000-EXIT.
            EXIT.

       S900 SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM PGM-INSPECT.
