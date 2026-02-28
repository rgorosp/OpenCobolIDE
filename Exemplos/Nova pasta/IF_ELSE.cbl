      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 17-09-24
      * Purpose: EXEMPLO IF IS NUMERIC
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-IF.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-NUM1          PIC S9(04)V99 VALUE +5.50.
           03 WS-NUM2          PIC S9(04)V99 VALUE +3.80.
           03 WS-TEXTO         PIC  X(30)    VALUE SPACES.
      *
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            DISPLAY '>>> INICIO DO PROGRAMA <<<'
            INITIALIZE WS-VAR
            PERFORM P100-CALCULO
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S100 SECTION.
       P100-CALCULO.
            IF WS-NUM1 IS NUMERIC THEN
                  DISPLAY 'WS-NUM1 = ' WS-NUM1
                  IF WS-NUM2 IS NUMERIC THEN
                     DISPLAY 'WS-NUM2 = ' WS-NUM2
                     SET WS-NUM2 TO 2
                     COMPUTE WS-NUM1 = WS-NUM1 + (WS-NUM1 * WS-NUM2)
                  ELSE
                     MOVE 'VALOR NAO NUMERICO WS-NUM2' TO WS-TEXTO
                  END-IF
            ELSE
                  MOVE 'VALOR NAO NUMERICO WS-NUM1' TO WS-TEXTO
            END-IF

            DISPLAY 'CALCULO OK..: ' WS-NUM1.
       P100-EXIT.
            EXIT.

       S900 SECTION.
       P900-ERRO.
            DISPLAY 'WS-TEXTO = ' WS-TEXTO
            PERFORM P999-FIM.
       P900-EXIT.

       S999 SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM PGM-IF.
