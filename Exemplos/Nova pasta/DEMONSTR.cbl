      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14-04-22
      * Purpose: DEMONSTRACAO DO PRIMEIRO PROGRAMA
      * Tectonics: COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMONSTR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-N1       PIC 9(02) VALUE ZEROS.
       77  WS-N2       PIC 9(02) VALUE ZEROS.
       77  WS-N3       PIC Z(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
               PERFORM P011-CALCULO
               PERFORM P901-TERMINO.
       P001-INICIO-EXIT.
            EXIT.

       S010-CALCULO SECTION.
       P011-CALCULO.
               MOVE 9   TO WS-N1
               MOVE 15  TO WS-N2

               COMPUTE WS-N3 = (WS-N1 * WS-N2)
                       ON SIZE ERROR DISPLAY 'ESTOURO DE CALCULO'
                   NOT ON SIZE ERROR DISPLAY 'CALCULO OK!'
               END-COMPUTE

               DISPLAY 'A MULTIPLICACAO DE ' WS-N1 ' * ' WS-N2
                       ' = ' WS-N3.
       P011-CALCULO-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-TERMINO-EXIT.
            EXIT.
       END PROGRAM DEMONSTR.
