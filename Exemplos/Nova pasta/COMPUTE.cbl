      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 21-02-24
      * Purpose: TRABALHANDO COM A EXPRESSÃO COMPUTE PARA
      *        CALCULOS MATEMATICOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-COMPUTE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-RESULT           PIC Z(06) VALUE ZEROS.
       77  WS-NUM1             PIC 999   VALUE ZEROS.
       77  WS-NUM2             PIC 999   VALUE ZEROS.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
             INITIALIZE WS-RESULT, WS-NUM1, WS-NUM2
             PERFORM P010-CALCULO THRU P010-EXIT
             PERFORM P999-TERMINO.
       P001-EXIT.
            EXIT.

       S010 SECTION.
       P010-CALCULO.
            DISPLAY "DIGITE VARIAVEL 1: "
            ACCEPT WS-NUM1

            DISPLAY "DIGITE VARIAVEL 2: "
            ACCEPT WS-NUM2

            DISPLAY " "

            COMPUTE WS-RESULT = (WS-NUM1 + WS-NUM2) * 3 +
                   (WS-NUM1 / 2) * (WS-NUM2 * 2) ** 5
                ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
            NOT ON SIZE ERROR DISPLAY "CALCULO NORMAL"

            DISPLAY "RESULTADO: " WS-RESULT.
       P010-EXIT.
            EXIT.

       S900 SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM PGM-COMPUTE.
