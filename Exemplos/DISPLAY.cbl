      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 26-02-26
      * Purpose: COMANDO DISPLAY GERAR RESULTADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-DISPLAY.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-INICIO        PIC X(10) VALUE "OLA MUNDO".
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
            DISPLAY WS-INICIO.
       P001-EXIT. EXIT.

       S900-TERMINO SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT. EXIT.
       END PROGRAM PGM-DISPLAY.
