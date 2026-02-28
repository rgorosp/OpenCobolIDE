      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 01-09-25
      * Purpose: TRABALHANDO COM MATRIZES BIDIMENSIONAIS
      *          INDEXADAS - BUSCA POR CHAVE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_C.
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
           03 WS-REGISTRO           OCCURS  4 TIMES
                                    ASCENDING KEY IS WS-CHAVE
                                    INDEXED BY I.
              05 WS-CHAVE           PIC 99.
              05 WS-NOME            PIC X(06).

       77  WS-CODIGO                PIC 99.

       01  FILLER                   PIC X(40).
           88 WS-FINAL              VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.

            MOVE '01MARCOS02CARLOS03MARINA04LIVIA ' TO WS-MATRIZ

            DISPLAY WS-MATRIZ
            DISPLAY ' '

            DISPLAY 'DIGITE A CHAVE DE PROCURA: '
            ACCEPT WS-CODIGO

            SEARCH ALL WS-REGISTRO
               AT END
                  DISPLAY 'CHAVE NAO ENCONTRADA: ' WS-CODIGO
                  MOVE 8 TO RETURN-CODE
               WHEN WS-CHAVE(I) = WS-CODIGO
                  DISPLAY 'CHAVE ENCONTRADA: '
                  WS-CHAVE(I) ' - ' WS-NOME(I)
                  ' POSICAO: ' I
            END-SEARCH
            .
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN
            .
       P901-EXIT.
            EXIT.
       END PROGRAM OCCURS_C.
