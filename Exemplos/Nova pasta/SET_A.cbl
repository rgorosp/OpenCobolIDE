      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 02-05-22
      * Purpose: COMANDO SET(ATRIBUIR A UMA VARIAVEL)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SET_A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM1            PIC 99 VALUE 0.
       77  WS-NUM2            PIC 99 VALUE ZEROS.
       01  WS-PGTO            PIC X  VALUE 'N'.
           88 WS-CONFIRM      VALUE 'S' FALSE 'N'.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
            DISPLAY 'ANTES DO SET WS-NUM1.: ' WS-NUM1
            SET WS-NUM1 TO 5
            DISPLAY 'DEPOIS DO SET WS-NUM1: ' WS-NUM1

            DISPLAY ' '
            DISPLAY 'ANTES DO SET WS-NUM2.: ' WS-NUM2
            SET WS-NUM2 TO WS-NUM1
            DISPLAY 'DEPOIS DO SET WS-NUM2: ' WS-NUM2

            SET WS-NUM1 TO 7
            DISPLAY ' '
            DISPLAY 'VALOR FINAL WS-NUM1..: ' WS-NUM1
            DISPLAY 'VALOR FINAL WS-NUM2..: ' WS-NUM2

            DISPLAY ' '
            DISPLAY 'SITUACAO DO PAGAMENTO: ' WS-PGTO
            SET WS-CONFIRM TO TRUE
            DISPLAY 'SITUACAO DO PAGAMENTO: ' WS-PGTO
            SET WS-CONFIRM TO FALSE
            DISPLAY 'SITUACAO DO PAGAMENTO: ' WS-PGTO.
       P001-EXIT.
            EXIT.

       S900-FIM SECTION.
       P901-FIM.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM SET_A.
