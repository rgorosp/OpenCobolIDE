      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 20-04-24
      * Purpose: COBOL E MAINFRAME IVEE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C2A02P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-EIXO                 PIC 99       VALUE ZEROS.
       77  WS-FRETE                PIC 99V99    VALUE ZEROS.
       77  WS-ADICIONAL            PIC 9(02)V99 VALUE ZEROS.
       77  WS-PAGAR                PIC Z(03)V99 VALUE ZEROS.
       77  WS-FLAG                 PIC X(01)    VALUE SPACE.
       PROCEDURE DIVISION.
       000-INICIO.
            DISPLAY "                             "
            DISPLAY "Quantos eixos ha no caminhao?"
            ACCEPT WS-EIXO FROM CONSOLE.
            SET WS-FRETE TO 50

            IF WS-EIXO =2 THEN
               DISPLAY 'VALOR DO FRETE: ' WS-FRETE
            ELSE
               SUBTRACT 2 FROM WS-EIXO
               MULTIPLY WS-EIXO BY 20 GIVING WS-ADICIONAL

               DISPLAY 'WS-ADICIONAL..: ' WS-ADICIONAL
               DISPLAY 'WS-FRETE......: ' WS-FRETE

               COMPUTE WS-PAGAR = (WS-ADICIONAL + WS-FRETE)
                       ON SIZE ERROR DISPLAY 'ESTOURO DE CAMPO'
                       MOVE 8 TO RETURN-CODE
               END-COMPUTE

               DISPLAY 'VALOR DO FRETE: ' WS-PAGAR
            END-IF

            DISPLAY "                             "
            DISPLAY "Ha mais caminhão? "
            DISPLAY "Digite S(SIM) ou N(NAO)"
            ACCEPT WS-FLAG FROM CONSOLE

            IF WS-FLAG = 'S' OR WS-FLAG = 's' THEN
                PERFORM 000-INICIO
            ELSE
                CONTINUE
            END-IF
            STOP RUN.
       END PROGRAM C2A02P01.
