      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14-09-25
      * Purpose: USO DO ADD CORRESPONDING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD-CORR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-REG-1.
           03 CODIGO         PIC 9(02).
           03 NOME           PIC X(15).
           03 TELEFONE       PIC X(11).

       01  WS-REG-2.
           03 CODIGO         PIC 9(02).
           03 NOME           PIC X(15).
           03 TELEFONE       PIC X(11).

       01  WS-CODIGO.
           05 CODIGO         PIC 9(02).
           05 FILLER         PIC X(15).
           05 FILLER         PIC X(11).

       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
      *           1212345678901234512345678901
            MOVE '01CARLOS AUGUSTO 11987879898'   TO WS-REG-1
            ADD  CODIGO   OF WS-REG-1 TO CODIGO   OF WS-REG-2
            MOVE NOME     OF WS-REG-1 TO NOME     OF WS-REG-2
            MOVE TELEFONE OF WS-REG-1 TO TELEFONE OF WS-REG-2

            DISPLAY 'WS-REG-1: ' WS-REG-1
            DISPLAY 'WS-REG-2: ' WS-REG-2

            ADD CORR WS-REG-1 TO WS-REG-2

            DISPLAY 'WS-REG-1: ' WS-REG-1
            DISPLAY 'WS-REG-2: ' WS-REG-2

            MOVE 05 TO CODIGO OF WS-CODIGO
            ADD CORR WS-CODIGO TO WS-REG-2

            DISPLAY 'WS-REG-1: ' WS-REG-1
            DISPLAY 'WS-REG-2: ' WS-REG-2
            .
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM ADD-CORR.
