      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 29-10-22
      * Purpose: USO DA CHAMADA DO PROGRAMA COM RECURSIVE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECURS RECURSIVE.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WS-NUMERO PIC 9(4) VALUE ZEROS.
       LOCAL-STORAGE   SECTION.
       77 LS-NUM    PIC 9(4) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000 SECTION.
           MOVE 5         TO WS-NUMERO
           MOVE WS-NUMERO TO LS-NUM.
       P000-INICIO.
           IF WS-NUMERO = 1 THEN
              DISPLAY 'ACABOU'
           ELSE
              SUBTRACT 1 FROM WS-NUMERO
              DISPLAY 'WS-NUMERO: ' WS-NUMERO
      *         DISPLAY 'LS-NUM = ' LS-NUM
              GO TO P000-INICIO
      *        CALL 'RECURS'
           END-IF.
       P000-EXIT.

       S999 SECTION.
       P999-FIM.
           GOBACK.
       P999-EXIT.
       END PROGRAM RECURS.
