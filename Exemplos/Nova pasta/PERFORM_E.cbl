      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 27-07-23
      * Purpose: ESTRUTURA DE REPETIÇÃO COM OCCURS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM_E.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-TABELA.
           03 WS-REGISTRO  OCCURS 10 TIMES
                           ASCENDING KEY IS WS-CHAVE
                           INDEXED BY WS-INDICE.
              05 WS-CHAVE  PIC 99.
              05 WS-TEXTO  PIC X(03).
       PROCEDURE DIVISION.
       P001-INICIO.
            MOVE 1 TO WS-INDICE

            MOVE
            '01AAA02BBB03CCC04DDD05EEE06FFF07GGG08HHH09III10JJJ'
            TO WS-TABELA

            DISPLAY 'TABELA: ' WS-TABELA
            DISPLAY ' '

            PERFORM UNTIL WS-INDICE > 10

                DISPLAY 'LINHA: ' WS-CHAVE(WS-INDICE)
                        ' - TEXTO: ' WS-TEXTO(WS-INDICE)
                        ' - INDICE: ' WS-INDICE

                ADD 1 TO WS-INDICE

            END-PERFORM.
       P001-EXIT.

       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
       END PROGRAM PERFORM_E.
