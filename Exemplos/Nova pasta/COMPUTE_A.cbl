      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 13-06-22
      * Purpose: EXEMPLO COMANDO COMPUTE(OPERACOES ARITMETICAS)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE-A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM1             PIC 9(02) VALUE ZEROS.
       77  WS-NUM2             PIC 9(02) VALUE ZEROS.
       77  WS-RESULT           PIC 9(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INI SECTION.
       P000-INICIO.
            INITIALIZE WS-NUM1 WS-NUM2 WS-RESULT

            SET WS-NUM1 TO 5
            SET WS-NUM2 TO 10

            COMPUTE WS-RESULT = (WS-NUM1 + WS-NUM2)
                    ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                NOT ON SIZE ERROR DISPLAY "CALCULO OK SOMA"
            END-COMPUTE

            COMPUTE WS-RESULT = (WS-RESULT * 3)
                    ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                NOT ON SIZE ERROR DISPLAY "CALCULO OK MULTIPLICACAO"
            END-COMPUTE

            COMPUTE WS-RESULT = (WS-RESULT - 10)
                    ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                NOT ON SIZE ERROR DISPLAY "CALCULO OK SUBTRACAO"
            END-COMPUTE

            COMPUTE WS-RESULT = (WS-RESULT ** 1)
                    ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                NOT ON SIZE ERROR DISPLAY "CALCULO OK EXPONENCIACAO"
            END-COMPUTE

            DISPLAY 'RESULTADO 1: ' WS-RESULT

            COMPUTE WS-RESULT = ((8*9)+(8*1))

            DISPLAY 'RESULTADO 2: ' WS-RESULT
            .
       P000-FIM.
            EXIT.

       S900-FIM SECTION.
       P999-FIM.
            STOP RUN.
       P999-FIM.
            EXIT.
       END PROGRAM COMPUTE-A.
