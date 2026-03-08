      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 08-03-26
      * Purpose: Criar uma tabela com 12 ocorrências, inicializar a tabela
      * e carrega-la com valores múltiplos de 11. Solicitar a posição da
      * ocorrência na console e apresentar o conteúdo da ocorrência na console.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AMOST51.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
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
           03 WS-LINHA               PIC 9(03) OCCURS 12 TIMES.

       77  WS-VALOR                  PIC 999   VALUE ZEROS.
       77  WS-INDICE                 PIC 99    VALUE ZEROS.
       77  WS-CODIGO                 PIC 99    VALUE ZEROS.

       01  FILLER                    PIC X(40).
           88 WS-FINAL               VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            MOVE 11 TO WS-VALOR
            PERFORM VARYING WS-INDICE FROM 1 BY 1
                    UNTIL WS-INDICE > 12
                DISPLAY ' '
                MOVE WS-VALOR TO WS-LINHA(WS-INDICE)
                DISPLAY "VALOR MULTIPO 11 => " WS-LINHA(WS-INDICE)
                " - INDICE: " WS-INDICE
                ADD 11 TO WS-VALOR
            END-PERFORM

            DISPLAY ' '
            DISPLAY "QUAL INDICE VC DESEJA VERIFICAR O VALOR? "
            ACCEPT WS-CODIGO FROM CONSOLE

            DISPLAY "VALOR: " WS-LINHA(WS-CODIGO)
            .
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN
            .
       P901-EXIT.
            EXIT.
       END PROGRAM AMOST51.
