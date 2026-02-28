      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 01-05-22
      * Purpose: COMANDO MOVE(MOVER)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE_A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DATA           PIC X(08) VALUE SPACES.
       01  WS-DATA-A.
           03 WS-DIA         PIC X(02) VALUE SPACES.
           03 FILLER         PIC X     VALUE '/'.
           03 WS-MES         PIC X(02) VALUE SPACES.
           03 FILLER         PIC X     VALUE '/'.
           03 WS-ANO         PIC X(04) VALUE SPACES.

       77  WS-TIME           PIC X(08) VALUE SPACES.
       01  WS-TIME-A.
           03 WS-HR          PIC X(02) VALUE SPACES.
           03 FILLER         PIC X     VALUE ':'.
           03 WS-MM          PIC X(02) VALUE SPACES.
           03 FILLER         PIC X     VALUE ':'.
           03 WS-SS          PIC X(02) VALUE SPACES.
           03 FILLER         PIC X     VALUE ':'.
           03 WS-MI          PIC X(02) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
            ACCEPT WS-DATA   FROM DATE YYYYMMDD
      *     12345678
      *     AAAAMMDD
            MOVE   WS-DATA(7:2) TO WS-DIA
            MOVE   WS-DATA(5:2) TO WS-MES
            MOVE   WS-DATA(1:4) TO WS-ANO

            DISPLAY 'DATA DE PROCESSAMENTO...: ' WS-DATA-A

            ACCEPT WS-TIME   FROM TIME
      *     12345678
      *     HHMMSSSS
            MOVE WS-TIME(1:2) TO WS-HR
            MOVE WS-TIME(3:2) TO WS-MM
            MOVE WS-TIME(5:2) TO WS-SS
            MOVE WS-TIME(7:2) TO WS-MI

            DISPLAY 'HORARIO DE PROCESSAMENTO: ' WS-TIME-A.
       P001-EXIT.
            EXIT.

       S999-TERMINO SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM MOVE_A.
