      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 16/07/25
      * Purpose: COMANDO OCCURS ARRAY ESTATICO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMOCCUR.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-FINANCIAMENTO.
           03 WS-CLIENTE       PIC X(20)     VALUE SPACES.
           03 WS-OBJETO        PIC X(20)     VALUE SPACES.
           03 WS-VALOR         PIC 9(06)V99  VALUE ZEROS.
           03 WS-PARCELAS      PIC $$.$$9,99 OCCURS 12 TIMES.

       01  WS-VAR.
           03 WS-VLR-PARCELAS  PIC 9(5)V99  VALUE ZEROS.
           03 WS-INDICE        PIC 99 VALUE ZEROS.
           03 WS-CONTROLE      PIC 99 VALUE ZEROS.

       PROCEDURE DIVISION.
       S010 SECTION.
       P011-INICIO.

            INITIALIZE WS-FINANCIAMENTO REPLACING ALPHANUMERIC BY SPACES
                                                  NUMERIC      BY ZEROS
            INITIALIZE WS-VAR           REPLACING ALPHANUMERIC BY SPACES
                                                  NUMERIC      BY ZEROS

            SET WS-CONTROLE TO 12

            DISPLAY "Informe o nome do Cliente: "
            ACCEPT WS-CLIENTE
            DISPLAY " "
            DISPLAY "Informe o nome do Objeto: "
            ACCEPT WS-OBJETO
            DISPLAY " "
            DISPLAY "Informe o valor do " WS-OBJETO
            ACCEPT WS-VALOR
            DISPLAY " "

            COMPUTE WS-VLR-PARCELAS = WS-VALOR / WS-CONTROLE
                     ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                 NOT ON SIZE ERROR DISPLAY " "
            END-COMPUTE

            PERFORM UNTIL WS-INDICE EQUAL WS-CONTROLE
               ADD 1 TO WS-INDICE
               MOVE WS-VLR-PARCELAS TO WS-PARCELAS(WS-INDICE)
            END-PERFORM

            PERFORM VARYING WS-INDICE FROM 1 BY 1
                      UNTIL WS-INDICE > WS-CONTROLE
               DISPLAY "PARCELA: " WS-INDICE
                       " VALOR: " WS-PARCELAS(WS-INDICE)
            END-PERFORM.
       P011-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM PGMOCCUR.
