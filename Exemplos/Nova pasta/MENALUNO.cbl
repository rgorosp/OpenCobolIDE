      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 02-07-25
      * Purpose: MENU PARA CADASTRO DE ALUNOS
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENALUNO.
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
            DISPLAY "           MENU DE ALUNOS"
            DISPLAY "-------------------------------------"
            DISPLAY "       DIGITE A OPCAO DESEJADA"
            DISPLAY "-------------------------------------"
            DISPLAY " <1> - LISTAR ALUNOS"
            DISPLAY " <2> - CONSULTAR ALUNOS"
            DISPLAY " <3> - ATUALIZAR ALUNOS"
            DISPLAY " <4> - EXCLUIR ALUNOS"
            DISPLAY " <5> - CADASTRAR ALUNOS"
            DISPLAY " <?> - QUALQUER TECLA SAIR"
            DISPLAY "-------------------------------------"
            MOVE SPACES TO WS-OPCAO
            ACCEPT WS-OPCAO
            EVALUATE WS-OPCAO
                WHEN '1'
                  MOVE '>>>>> LISTA ALUNOS <<<<<'    TO WS-MENSAGEM
                  CALL 'LISALUNO' USING WS-AREA
                WHEN '2'
                  MOVE '>>>>> CONSULTA ALUNOS <<<<<' TO WS-MENSAGEM
                  CALL 'CONALUNO' USING WS-AREA
                WHEN '3'
                  MOVE '>>>>> ATUALIZA ALUNOS <<<<<' TO WS-MENSAGEM
                  CALL 'ATUALUNO' USING WS-AREA
                WHEN '4'
                  MOVE '>>>>> DELETA ALUNOS <<<<<'   TO WS-MENSAGEM
                  CALL 'DELALUNO' USING WS-AREA
                WHEN '5'
                  MOVE '>>>>> CADASTRA ALUNOS <<<<<' TO WS-MENSAGEM
                  CALL 'CADALUNO' USING WS-AREA
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
       END PROGRAM MENALUNO.
