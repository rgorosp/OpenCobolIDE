      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 28-04-24.
      * Purpose: TREX02 CÁLCULOS ARITMÉTICOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C2A05P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1             PIC 9(07) VALUE ZEROS.
       77  WS-VAR2             PIC 9(07) VALUE ZEROS.
       77  WS-VAR3             PIC 9(07) VALUE ZEROS.
       77  WS-VAR4             PIC 9(07) VALUE ZEROS.
       77  WS-VAR1-E           PIC ZZZ.ZZ9,99.
       77  WS-VAR2-E           PIC ZZZ.ZZ9,99.
       77  WS-VAR3-E           PIC ZZZ.ZZ9,99.
       77  WS-VAR4-E           PIC ZZZ.ZZ9,99.
       PROCEDURE DIVISION.
       S000-INICIO.
       P001-INI.
            DISPLAY "Display Variavel 1: " WS-VAR1 UPON CONSOLE
            DISPLAY "Display Variavel 2: " WS-VAR2 UPON CONSOLE
            DISPLAY "Display Variavel 3: " WS-VAR3 UPON CONSOLE
            DISPLAY "Display Variavel 4: " WS-VAR4 UPON CONSOLE
            DISPLAY "                    " UPON CONSOLE.
       P001-EXIT.

       P002-INI.
            DISPLAY "Inserir Valor Variavel 1: " UPON CONSOLE
            ACCEPT WS-VAR1 FROM CONSOLE
            DISPLAY "Inserir Valor Variavel 2: " UPON CONSOLE
            ACCEPT WS-VAR2 FROM CONSOLE
            DISPLAY "Inserir Valor Variavel 3: " UPON CONSOLE
            ACCEPT WS-VAR3 FROM CONSOLE
            DISPLAY "Inserir Valor Variavel 4: " UPON CONSOLE
            ACCEPT WS-VAR4 FROM CONSOLE
            DISPLAY "                    " UPON CONSOLE

            PERFORM P001-INI THRU P001-EXIT.
       P002-EXIT.
            EXIT.

       S020-CALCULO.
       P021-CAL.
            MULTIPLY WS-VAR1 BY WS-VAR2
            MULTIPLY WS-VAR2 BY WS-VAR3
            MULTIPLY WS-VAR3 BY WS-VAR4
            MOVE WS-VAR1 TO WS-VAR1-E
            MOVE WS-VAR2 TO WS-VAR2-E
            MOVE WS-VAR3 TO WS-VAR3-E
            MOVE WS-VAR4 TO WS-VAR4-E
            DISPLAY "                    " UPON CONSOLE

            PERFORM P001-INI THRU P001-EXIT.
       P021-EXIT.
            EXIT.

       S900-TERMINO.
       P901-TER.
           STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM C2A05P01.
