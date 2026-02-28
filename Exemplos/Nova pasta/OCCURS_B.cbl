      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-08-23
      * Purpose: TRABALHANDO COM MATRIZES BIDIMENSIONAIS
      *          INDEXADAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_B.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  FILLER                   PIC X(40).
           88 WS-INICIO             VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       01  WS-MATRIZ.
           03 WS-LINHA              OCCURS  3 TIMES INDEXED BY I.
              05 WS-LINHAS          PIC X(06) VALUE 'LINHA'.
              05 WS-COLUNA          OCCURS  5 TIMES INDEXED BY J.
                 07 WS-CELULA       PIC X(06) VALUE 'CELULA'.

       01  FILLER                   PIC X(40).
           88 WS-FINAL              VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            PERFORM VARYING I FROM 1 BY 1
                      UNTIL I GREATER 3
                    DISPLAY ' '
                PERFORM VARYING J FROM 1 BY 1
                          UNTIL J GREATER 5
                    DISPLAY WS-CELULA(I,J)
                        ' ' I ':' J
                END-PERFORM

            END-PERFORM
            .
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN
            .
       P901-EXIT.
            EXIT.
       END PROGRAM OCCURS_B.
