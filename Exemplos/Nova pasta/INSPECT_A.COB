      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 22/02/24
      * Purpose: COMANDO INSPECT PARA ALTERAR MAIUSCULA PARA MINUSCULA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT-A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VALOR           PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
       S000 SECTION.
       000-INICIO.
            INITIALIZE WS-VALOR
            MOVE 'SAO PAULO' TO WS-VALOR
            DISPLAY 'ANTES WS-VALOR CONVERTING = ' WS-VALOR

            INSPECT WS-VALOR CONVERTING
            'ABCDEFGHIJKLMNOPQRSTUVXWYZ' TO 'abcdefghijklmnopqrstuvxwyz'
            DISPLAY 'APOS  WS-VALOR CONVERTING = ' WS-VALOR.
       000-EXIT.
            EXIT.

       S900 SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM INSPECT-A.
