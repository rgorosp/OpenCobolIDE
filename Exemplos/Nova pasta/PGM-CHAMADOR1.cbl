      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 23-09-22
      * Purpose: PROGRAMA CHAMADOR DA SUB-ROTINA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADOR1.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-PARAMETRO.
           03 WS-NUM1          PIC 9(03) VALUE ZEROS.
           03 WS-NUM2          PIC 9(03) VALUE ZEROS.
           03 WS-RESULT        PIC 9(04) VALUE ZEROS.
       PROCEDURE DIVISION.
       S100-INICIO SECTION.
       P101-INICIO.
            DISPLAY '***** PROGRAMA PRINCIPAL *****'
            DISPLAY "DIGITE VALOR PARA WS-NUM1..: "
            ACCEPT WS-NUM1
            DISPLAY "DIGITE VALOR PARA WS-NUM2..: "
            ACCEPT WS-NUM2

            CALL 'PGM-CHAMADO1' USING WS-PARAMETRO

            DISPLAY ' '
            DISPLAY "RESULTADO DA MULTIPLICACAO..: " WS-RESULT

            DISPLAY "***** TERMINO PROGRAMA PRINCIPAL *****".
       P101-EXIT.
            EXIT.

       S990-FIM SECTION.
       P991-FIM.
            GOBACK.
       P991-EXIT.
            EXIT.
       END PROGRAM PGM-CHAMADOR1.
