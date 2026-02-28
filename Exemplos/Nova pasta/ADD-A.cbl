      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 20/04/24
      * Purpose: CURSO COBOL E MAINFRAME IVEE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C2A01P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR2             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR3             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR4             PIC 9(03)V99 VALUE ZEROS.
       77  WS-TEXTO            PIC X(30)    VALUE SPACES.
       PROCEDURE DIVISION.
       S001-INI SECTION.
       P002-INI.
            DISPLAY "Inserir valor VAR1: "
            ACCEPT WS-VAR1
            DISPLAY "Inserir valor VAR2: "
            ACCEPT WS-VAR2
            DISPLAY "Inserir valor VAR3: "
            ACCEPT WS-VAR3
            DISPLAY "Inserir valor VAR4: "
            ACCEPT WS-VAR4

            MOVE 'DISPLAY ANTES DO COMANDO ADD' TO WS-TEXTO
            PERFORM P010-DISPLAY THRU P010-EXIT
            PERFORM P005-CALCULO THRU P005-EXIT
            MOVE 'DISPLAY APOS O COMANDO ADD' TO WS-TEXTO
            PERFORM P010-DISPLAY THRU P010-EXIT
            PERFORM S900-TERM
            .
       P002-EXIT.
            EXIT.

       S005-CALC SECTION.
       P005-CALCULO.
            ADD WS-VAR1 TO WS-VAR2
            ADD WS-VAR2 TO WS-VAR3
            ADD WS-VAR3 TO WS-VAR4
            .
       P005-EXIT.
            EXIT.

       S010-DIS SECTION.
       P010-DISPLAY.
            DISPLAY '------------------------------'
            DISPLAY WS-TEXTO
            DISPLAY '------------------------------'
            DISPLAY 'VAR1: ' WS-VAR1
            DISPLAY 'VAR2: ' WS-VAR2
            DISPLAY 'VAR3: ' WS-VAR3
            DISPLAY 'VAR4: ' WS-VAR4
            DISPLAY '------------------------------'
            DISPLAY '                              '.
       P010-EXIT.
            EXIT.

       S900-TERM SECTION.
       S999-TERMINO.
            STOP RUN.
       S999-EXIT.
            EXIT.
       END PROGRAM C2A01P01.
