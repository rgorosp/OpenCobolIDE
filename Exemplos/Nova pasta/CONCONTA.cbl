      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 21-01-2023
      * Purpose: CONSULTA CONTATOS ARQUIVO INDEXADO
      * Update: 24/06/25 - Alteração do Programa para modulo
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCONTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
           SYSIN IS WS-SYSIN.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS
                  ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-CONTATO
                  FILE STATUS  IS WS-FS-CON.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY REGCONTT.
       WORKING-STORAGE SECTION.
      * >>> VARIAVEIS DE CONTROLE
       01  WS-VAR.
           03 WS-FS-CON          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-R-CON           PIC 9(02) VALUE ZEROS.
           03 WS-R-CON-INE       PIC 9(02) VALUE ZEROS.
      * >>> ARQUIVO DE SAIDA
       01  WS-REG-CONTATOS       PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CONTATOS.
           03 WS-ID-CONTATO      PIC 99.
           03 WS-NM-CONTATO      PIC X(20).
       77  WS-ID-CONT            PIC 99    VALUE ZEROS.

      * >>> AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-MENSAGEM        PIC X(40).
      * >>> INICIO DO PROGRAMA
       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY LK-MENSAGEM
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-CONSULTA THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN INPUT CONTATOS
            IF WS-FS-CON NOT = 00
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-CONSULTA.
            DISPLAY LK-AREA
            PERFORM UNTIL WS-FLAG = 'N'
            DISPLAY 'DIGITE O ID COM 2 DIGITOS PARA CONSULTA: '
            ACCEPT ID-CONTATO

            READ CONTATOS INTO WS-REG-CONTATOS
                 KEY IS ID-CONTATO
                  INVALID KEY
                       ADD 1 TO WS-R-CON-INE
                       DISPLAY 'CONTATO INEXISTENTE!'
                       DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-CON
                       DISPLAY 'CONTATOS: ' WS-REG-CONTATOS
                       DISPLAY ' '
            END-READ

            DISPLAY 'DESEJA REALIZAR OUTRA CONSULTA, (S)SIM (N)NAO'
            ACCEPT WS-FLAG
            END-PERFORM.
       P100-EXIT.
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
            DISPLAY '     ERRO PROGRAMA CADCONTA'
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
            DISPLAY '>>>> PROGRAMA CADCONTA FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE CONTATOS EXISTENTE..: ' WS-R-CON
            DISPLAY ' QTDE CONTATOS INEXISTENTE: ' WS-R-CON-INE
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM CONCONTA.
