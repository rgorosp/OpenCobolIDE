      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 09-08-25
      * Purpose: TRABALHANDO COM MATRIZES SIMPLES BIDIMENSIONAIS
      *          NAO INDEXADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  FILLER                    PIC X(40).
           88 WS-INICIO              VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       01  WS-MATRIZ.
           03 WS-LINHA               OCCURS  3 TIMES.
              05 WS-LINHAS           PIC X(06) VALUE 'LINHA'.
              05 WS-COLUNA           OCCURS  5 TIMES.
                 07 WS-CELULA        PIC X(06) VALUE 'CELULA'.

       77  WS-CONTR-L                PIC 99    VALUE ZEROS.
       77  WS-CONTR-C                PIC 99    VALUE ZEROS.

       01  FILLER                    PIC X(40).
           88 WS-FINAL               VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            PERFORM VARYING WS-CONTR-L FROM 1 BY 1
                    UNTIL WS-CONTR-L GREATER 3
                    DISPLAY ' '
                PERFORM VARYING WS-CONTR-C FROM 1 BY 1
                        UNTIL WS-CONTR-C GREATER 5
                    DISPLAY WS-CELULA(WS-CONTR-L,WS-CONTR-C)
                        ' ' WS-CONTR-L ':' WS-CONTR-C
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
       END PROGRAM OCCURS_A.
