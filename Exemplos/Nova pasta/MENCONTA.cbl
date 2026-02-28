      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 24-06-25
      * Purpose: MENU PARA CADASTRO DE CONTATOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENCONTA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-AREA.
           03 WS-MENSAGEM        PIC X(40) VALUE SPACES.
       77  WS-OPCAO              PIC X     VALUE SPACES.
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            INITIALIZE WS-AREA WS-OPCAO
            DISPLAY "-------------------------------------"
            DISPLAY "           MENU DE CONTATOS"
            DISPLAY "-------------------------------------"
            DISPLAY "       DIGITE A OPCAO DESEJADA"
            DISPLAY "-------------------------------------"
            DISPLAY " <1> - LISTAR CONTATOS"
            DISPLAY " <2> - CONSULTAR CONTATOS"
            DISPLAY " <3> - ATUALIZAR CONTATOS"
            DISPLAY " <4> - EXCLUIR CONTATOS"
            DISPLAY " <5> - CADASTRAR CONTATOS"
            DISPLAY " <?> - QUALQUER TECLA SAIR"
            DISPLAY "-------------------------------------"
            MOVE SPACES TO WS-OPCAO
            ACCEPT WS-OPCAO
            EVALUATE WS-OPCAO
                WHEN '1'
                  MOVE '>>>>> LISTA CONTATOS <<<<<'    TO WS-MENSAGEM
                  CALL 'LISCONTA' USING WS-AREA
                WHEN '2'
                  MOVE '>>>>> CONSULTA CONTATOS <<<<<' TO WS-MENSAGEM
                  CALL 'CONCONTA' USING WS-AREA
                WHEN '3'
                  MOVE '>>>>> ATUALIZA CONTATOS <<<<<' TO WS-MENSAGEM
                  CALL 'ATUCONTA' USING WS-AREA
                WHEN '4'
                  MOVE '>>>>> DELETA CONTATOS <<<<<'   TO WS-MENSAGEM
                  CALL 'DELCONTA' USING WS-AREA
                WHEN '5'
                  MOVE '>>>>> CADASTRA CONTATOS <<<<<' TO WS-MENSAGEM
                  CALL 'CADCONTA' USING WS-AREA
                WHEN OTHER
                  PERFORM P999-TERMINO THRU P999-EXIT
            END-EVALUATE

            DISPLAY "DESEJA REALIZAR OUTRA OPERACAO? "
            DISPLAY "DIGITE <S> OU <N>"
            MOVE SPACES TO WS-OPCAO
            ACCEPT WS-OPCAO
            IF WS-OPCAO EQUAL 'S' THEN
              GO TO S000
            ELSE
              PERFORM P999-TERMINO THRU P999-EXIT
            END-IF.
       P000-EXIT.
            EXIT.

       S999 SECTION.
       P999-TERMINO.
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM MENCONTA.
