      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 07-03-24
      * Purpose: Trabalhando com "ON SIZE ERROR"
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-ON-SIZE-ERROR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM1         PIC 9(02) VALUE ZEROS.
       77  WS-NUM2         PIC 9(02) VALUE ZEROS.
       77  WS-RESULT       PIC 999   VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INI SECTION.
       P000-INICIO.
            DISPLAY "INSIRA VALOR PARA VARIAVEL 1: "
            ACCEPT WS-NUM1

            DISPLAY "INSIRA VALOR PARA VARIAVEL 2: "
            ACCEPT WS-NUM2

            PERFORM P100-CALCULO.
       P000-EXIT.
            EXIT.

       S100-CALC SECTION.
       P100-CALCULO.
             COMPUTE WS-RESULT = (WS-NUM1 * WS-NUM2)
                     ON SIZE ERROR PERFORM P500-ERRO
                 NOT ON SIZE ERROR PERFORM P101-RESULT
             END-COMPUTE
             .
       P100-EXIT.

       P101-RESULT.
             DISPLAY "VALOR OK >> " WS-RESULT
             PERFORM P999-FIM
             .
       P101-EXIT.
            EXIT.

       P500-ERRO.
             DISPLAY "ESTOURO DE CAMPO WS-RESULT"
             MOVE 12 TO RETURN-CODE
             PERFORM P999-FIM.
       P500-EXIT.

       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM PGM-ON-SIZE-ERROR.
