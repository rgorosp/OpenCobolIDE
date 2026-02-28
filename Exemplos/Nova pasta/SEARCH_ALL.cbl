      ******************************************************************
      * Author: EMERSON SILVA MOTTA
      * Date: 27-07-23
      * Purpose: BUSCA POR CHAVE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEARCH_ALL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-TABELA.
           03 WS-REGISTRO  OCCURS 4 TIMES
                           ASCENDING KEY IS WS-CHAVE
                           INDEXED BY WS-I.
              05 WS-CHAVE     PIC 9(02).
              05 WS-NOME      PIC X(10).
       77  WS-CODIGO          PIC 99.
       77  WS-PESQ            PIC X.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            MOVE '01EMERSON   02ROSA      03LEILA     04LIVIA     '
               TO WS-TABELA

            DISPLAY 'CONTEUDO: ' WS-TABELA.

       P002-CONTINUE.
            DISPLAY ' '
            DISPLAY 'DIGITE O CODIGO DESEJADO PARA PESQUISA: '
            ACCEPT WS-CODIGO

            SEARCH ALL WS-REGISTRO
               AT END
                  DISPLAY 'REGISTRO NAO ENCONTRADO!!!'
               WHEN WS-CHAVE(WS-I) = WS-CODIGO
                  DISPLAY 'CHAVE: ' WS-CHAVE(WS-I)
                          ' - NOME: ' WS-NOME(WS-I)
                          ' - POSICAO: ' WS-I
            END-SEARCH

            DISPLAY ' '
            DISPLAY 'DESEJA REALIZAR OUTRA PESQUISA? '
            DISPLAY '(S) SIM ou (qualquer tecla Sair)'
            ACCEPT WS-PESQ

            EVALUATE WS-PESQ
                WHEN 'S'
                  PERFORM P002-CONTINUE
                WHEN OTHER
                  PERFORM P901-TERMINO
            END-EVALUATE.
       P001-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM SEARCH_ALL.
