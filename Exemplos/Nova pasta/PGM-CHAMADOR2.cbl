      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 04-10-22.
      * Purpose: USO DO PROGRAMA CHAMADOR2
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CHAMADOR2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-NUM1        PIC 9(03).
           03 WS-NUM2        PIC 9(03).
           03 WS-RETURN      PIC Z(06).
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            DISPLAY "***** PROGRAMA CHAMADOR2 *****"
            DISPLAY "Insira valor para Variavel 1: "
            ACCEPT WS-NUM1

            DISPLAY "Insira valor para Variavel 2: "
            ACCEPT WS-NUM2

            CALL 'PGM-CHAMADO2' USING WS-VAR

            DISPLAY "A MULTIPLICACAO DE " WS-NUM1 " * " WS-NUM2 " = "
                    WS-RETURN

            DISPLAY "***** TERMINO CHAMADOR2 *****".
       P000-EXIT.

       S999 SECTION.
       P999-FIM.
            GOBACK.
       P999-EXIT.
       END PROGRAM PGM-CHAMADOR2.
