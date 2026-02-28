      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 02-07-2025
      * Purpose: ATUALIZA ALUNOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ATUALUNO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CFP001S1 ASSIGN TO "C:/ARQUIVOS/ALUNOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-ALUNO
                  FILE STATUS  IS WS-FS-ALU
                  RESERVE       5 AREAS.
       DATA DIVISION.
       FILE SECTION.
       FD  CFP001S1.
           COPY REGALUNO.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-ALU          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-RW-ALU          PIC 9(02) VALUE ZEROS.

       01  WS-REG-CFP001S1       PIC X(32) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S1.
           03 WS-ID-ALUNO        PIC 999.
           03 WS-NM-ALUNO        PIC X(20).
           03 WS-TL-ALUNO        PIC X(09).

       77  WS-R-ALU-INE          PIC 9(02) VALUE ZEROS.
       77  WS-R-ALU              PIC 99    VALUE ZEROS.
       77  WS-ATUALIZA           PIC A     VALUE SPACES.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-ID-ALUNO        PIC 999.
           03 LK-NM-ALUNO        PIC X(20).
           03 LK-TL-ALUNO        PIC X(09).
           03 LK-MENSAGEM        PIC X(40) VALUE SPACES.
      *
       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-ATUALIZA THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN I-O CFP001S1
            IF WS-FS-ALU NOT = 00
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S1' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-ATUALIZA.
            DISPLAY LK-AREA
            PERFORM UNTIL WS-FLAG = 'N'
            DISPLAY 'DIGITE O ID COM 3 DIGITOS PARA CONSULTA: '
            ACCEPT ID-ALUNO

            READ CFP001S1 INTO WS-REG-CFP001S1
                 KEY IS ID-ALUNO
                  INVALID KEY
                       ADD 1 TO WS-R-ALU-INE
                       DISPLAY 'ALUNO INEXISTENTE!'
                       DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-ALU
                       DISPLAY 'CFP001S1: ' WS-REG-CFP001S1
                       DISPLAY ' '
                       DISPLAY 'VOCE DESEJA ALTERAR ESSE ALUNO? '
                       'S(SIM) OU N(NAO)'
                       ACCEPT WS-ATUALIZA
                       IF WS-ATUALIZA EQUAL 'S' THEN
                          DISPLAY 'DIGITE O NOVO NOME PARA ALUNO: '
                       ACCEPT NM-ALUNO
                          DISPLAY 'DIGITE O TELEFONE DO ALUNO: '
                       ACCEPT TL-ALUNO
                  REWRITE REG-CFP001S1
                  INVALID KEY
                       DISPLAY 'ALUNO NAO ATUALIZADO'
                       DISPLAY ' '
                  NOT INVALID KEY
                       DISPLAY 'ATUALIZACAO EFETUADA!'
                       DISPLAY ' '
                  END-REWRITE
                  IF WS-FS-ALU NOT = 00 THEN
                     MOVE WS-FS-ALU                           TO WS-MSG1
                     MOVE 'ERRO ATUALIZACAO ARQUIVO CFP001S1' TO WS-MSG2
                     PERFORM P800-ERRO THRU P999-EXIT
                  ELSE
                     ADD 1 TO WS-RW-ALU
                  END-IF
            END-IF
            END-READ

            DISPLAY 'DESEJA REALIZAR OUTRA ALTERACAO, (S)SIM (N)NAO'
            ACCEPT WS-FLAG
            END-PERFORM.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CFP001S1
            IF WS-FS-ALU NOT = 00 THEN
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CFP001S1'    TO WS-MSG2
            PERFORM P800-ERRO THRU P800-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA ATUALUNO'
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
            DISPLAY '>>>> PROGRAMA ATUALUNO FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE CFP001S1 LIDOS.........: ' WS-R-ALU
            DISPLAY ' QTDE CFP001S1 ATUALIZADO....: ' WS-RW-ALU
            DISPLAY ' QTDE CFP001S1 NAO ENCONTRADO: ' WS-R-ALU-INE
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM ATUALUNO.
