      ******************************************************************
      * Author: EMERSON
      * Date: 14-09-25
      * Purpose: USO PERFORM VARYING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM_C.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-NUM            PIC 9(03) VALUE ZEROS.
           03 WS-IND            PIC 9(03) VALUE ZEROS.
           03 WS-TOT            PIC 9(05) VALUE ZEROS.
           03 WS-TOT-EDIT       PIC ZZ.Z99.
       01  LK-PARM.
           03 LK-NUM            PIC 999 VALUE 50.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            MOVE LK-NUM         TO WS-NUM

            PERFORM VARYING WS-IND FROM 1 BY 1
               UNTIL WS-IND > WS-NUM
               ADD WS-IND TO WS-TOT
            END-PERFORM

            MOVE WS-TOT   TO WS-TOT-EDIT

            DISPLAY "ADD NUMBERS UNTIL: " WS-NUM
                    " = " WS-TOT-EDIT

            DISPLAY " "

            COMPUTE WS-TOT = WS-NUM * (WS-NUM + 1 ) / 2

            DISPLAY "ADD NUMBERS UNTIL APOS COMPUTE : " WS-NUM
                    " = " WS-TOT-EDIT.
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM PERFORM_C.
