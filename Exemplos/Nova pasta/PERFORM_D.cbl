      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14/09/25
      * Purpose: EXEMPLOS DE PERFORM COM INCREMENTO AUTOMATICO
      *    OU INCREMENTO POR ADICAO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM_D.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-CONTAR1          PIC 999 VALUE ZEROS.
       77  WS-CONTAR2          PIC 999 VALUE ZEROS.
       77  WS-CONTAR3          PIC 999 VALUE ZEROS.
       77  WS-CONDICAO         PIC 999 VALUE ZEROS.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY 'DIGITE O VALOR PARA A CONDICAO'
            ACCEPT WS-CONDICAO
            DISPLAY ' '

            DISPLAY 'EXEMPLO 1: AUTOMATICO'
            PERFORM VARYING WS-CONTAR1 FROM 1 BY 1
               UNTIL WS-CONTAR1 GREATER THAN WS-CONDICAO
               DISPLAY WS-CONTAR1
            END-PERFORM
            DISPLAY ' '

            DISPLAY 'EXEMPLO 2: ADICAO COM AFTER NAO AUTOMATICO'
            PERFORM WITH TEST AFTER UNTIL
               WS-CONTAR2 EQUAL WS-CONDICAO
               ADD 1 TO WS-CONTAR2
               DISPLAY WS-CONTAR2
            END-PERFORM
            DISPLAY ' '

            DISPLAY 'EXEMPLO 3: ADICAO COM BEFORE NAO AUTOMATICO'
            MOVE 1 TO WS-CONTAR3
            PERFORM WITH TEST BEFORE UNTIL
               WS-CONTAR3 GREATER THAN WS-CONDICAO
               DISPLAY WS-CONTAR3
               ADD 1 TO WS-CONTAR3
            END-PERFORM

            STOP RUN.
       END PROGRAM PERFORM_D.
