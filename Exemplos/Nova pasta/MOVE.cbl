      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14-05-22
      * Purpose: COMANDO MOVE(MOVER)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-MOVE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DATA     PIC X(08) VALUE SPACES.
       77  WS-DATA-A   PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
             ACCEPT WS-DATA FROM DATE YYYYMMDD
             DISPLAY "DATA: " WS-DATA

             MOVE WS-DATA(7:2) TO WS-DATA-A(1:2)
             MOVE "/"          TO WS-DATA-A(3:1)
                                  WS-DATA-A(6:1)
             MOVE WS-DATA(5:2) TO WS-DATA-A(4:2)
             MOVE WS-DATA(1:4) TO WS-DATA-A(7:4)

             DISPLAY "DATA FORMATADO: " WS-DATA-A

             DISPLAY "DIA: " WS-DATA-A(1:2)
             DISPLAY "MES: " WS-DATA-A(4:2)
             DISPLAY "ANO: " WS-DATA-A(7:4).
       P001-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM PGM-MOVE.
