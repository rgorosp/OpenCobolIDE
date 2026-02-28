      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 15-12-2024
      * Purpose: DELECAO CONTATOS ARQUIVO INDEXADO
      * UPDATE: 24/06/25 - TRANSFORMADO PROGRAMA PARA MODULO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELCONTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-CONTATO
                  FILE STATUS  IS WS-FS-CON.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY REGCONTT.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-CON          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-RW-CON          PIC 9(02) VALUE ZEROS.
       01  WS-REG-CONTATOS       PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CONTATOS.
           03 WS-ID-CONTATO      PIC 99.
           03 WS-NM-CONTATO      PIC X(20).
       77  WS-R-CON-INE          PIC 9(02) VALUE ZEROS.
       77  WS-R-CON              PIC 99    VALUE ZEROS.
       77  WS-DELETE             PIC A     VALUE SPACES.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-MENSAGEM        PIC X(40).

       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY LK-MENSAGEM
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-DELETAR THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN I-O CONTATOS
            IF WS-FS-CON NOT = 00
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-DELETAR.
      *     DISPLAY LK-AREA
            PERFORM UNTIL WS-FLAG = 'N'
            DISPLAY 'DIGITE O ID COM 2 DIGITOS PARA CONSULTA: '
            ACCEPT ID-CONTATO

            READ CONTATOS INTO WS-REG-CONTATOS
                 KEY IS ID-CONTATO
                  INVALID KEY
                       ADD 1 TO WS-R-CON-INE
                       DISPLAY 'CONTATO INEXISTENTE!'
                       DISPLAY ' '
                       PERFORM P300-INVALIDO
                  NOT INVALID KEY
                       ADD 1 TO WS-R-CON
                       DISPLAY 'CONTATOS: ' WS-REG-CONTATOS
                       PERFORM P200-EXCLUIR
            END-READ
            END-PERFORM.
            P100-EXIT.
            EXIT.

       S200-EXCLUIR SECTION.
       P200-EXCLUIR.
            DISPLAY 'VOCE DESEJA EXCLUIR ESSE CONTATO? S(SIM) OU N(NAO)'
            ACCEPT WS-DELETE

            IF WS-DELETE EQUAL 'S' THEN
            DELETE CONTATOS RECORD
                  INVALID KEY
                       DISPLAY 'CONTATO NAO DELETADO'
                       DISPLAY ' '
                  NOT INVALID KEY
                       DISPLAY 'DELECAO EFETUADA!'
            END-DELETE
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                         TO WS-MSG1
               MOVE 'ERRO DELETE ID ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               ADD 1 TO WS-RW-CON
            END-IF
            END-IF.

            DISPLAY ' '
            DISPLAY 'DESEJA REALIZAR OUTRA EXCLUSAO, (S)SIM (N)NAO'
            ACCEPT WS-FLAG.
       P200-EXIT.
            EXIT.

       S300-INVALIDO SECTION.
       P300-INVALIDO.
            DISPLAY ' '
            DISPLAY 'DESEJA SAIR <N> ou <QUALQUER TECLA> para continue'
            ACCEPT WS-FLAG.
       P300-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CONTATOS'    TO WS-MSG2
            PERFORM P800-ERRO THRU P800-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA DELCONTA'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM: ' WS-MSG2
            DISPLAY ' CODIGO..: ' WS-MSG1
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE
            GOBACK.
       P800-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            DISPLAY '--------------------------------------'
            DISPLAY '>>>> PROGRAMA DELCONTA FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE CONTATOS LIDOS.........: ' WS-R-CON
            DISPLAY ' QTDE CONTATOS DELETADO......: ' WS-RW-CON
            DISPLAY ' QTDE CONTATOS NAO ENCONTRADO: ' WS-R-CON-INE
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM DELCONTA.
