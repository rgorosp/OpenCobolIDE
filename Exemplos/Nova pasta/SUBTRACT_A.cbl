      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-04-24
      * Purpose: PARAMETRO SUBTRACT(SUBTRAIR)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C2A03P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WORK01      PIC 9(03)V99 VALUE ZEROS.
       77  WORK02      PIC 9(03)V99 VALUE ZEROS.
       77  WORK03      PIC 9(03)V99 VALUE ZEROS.
       77  WORK04      PIC 9(03)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO.
       P001-INI.
            INITIALIZE WORK01 WORK02 WORK03 WORK04
            PERFORM P011-DIS THRU P012-EXIT
            PERFORM P051-PRO
            PERFORM P901-TER
            .
       P001-EXIT.
            EXIT.

       S010-DISPLAY.
       P011-DIS.
            DISPLAY "Variavel 1 = " WORK01 UPON CONSOLE
            DISPLAY "Variavel 2 = " WORK02 UPON CONSOLE
            DISPLAY "Variavel 3 = " WORK03 UPON CONSOLE
            DISPLAY "Variavel 4 = " WORK04 UPON CONSOLE
            DISPLAY "             " UPON CONSOLE
            .
       P011-EXIT.

       P012-DIS.
            DISPLAY "Inserir valor Variavel 1" UPON CONSOLE
            ACCEPT WORK01 FROM CONSOLE
            DISPLAY "Inserir valor Variavel 2" UPON CONSOLE
            ACCEPT WORK02 FROM CONSOLE
            DISPLAY "Inserir valor Variavel 3" UPON CONSOLE
            ACCEPT WORK03 FROM CONSOLE
            DISPLAY "Inserir valor Variavel 4" UPON CONSOLE
            ACCEPT WORK04 FROM CONSOLE
            DISPLAY "             " UPON CONSOLE
            .
       P012-EXIT.
            EXIT.

       S050-PROCESSA.
       P051-PRO.
            SUBTRACT WORK01 FROM WORK02
            SUBTRACT WORK02 FROM WORK03
            SUBTRACT WORK03 FROM WORK04

            PERFORM P011-DIS THRU P011-EXIT
            .
       P051-EXIT.
            EXIT.

       S900-TERMINO.
       P901-TER.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM C2A03P01.
