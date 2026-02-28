      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 30-05-2025
      * Purpose: LISTAR CONTATOS ARQUIVO INDEXADO
      * Update: 24/06/25 - Alteração do Programa para modulo
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISCONTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT CONTATOS ASSIGN TO "C:/ARQUIVOS/CONTATOU.DAT"
      *     SELECT CONTATOS ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS ID-CONTATO
                  FILE STATUS  IS WS-FS-CON
                  RESERVE      10 AREAS.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY REGCONTT.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-CON          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-READ-CONT       PIC 9(05) VALUE ZEROS.

       01  WS-REG-CONTATOS       PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CONTATOS.
           03 WS-ID-CONTATO      PIC 99.
           03 WS-NM-CONTATO      PIC X(20).

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-MENSAGEM        PIC X(40).

       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY LK-MENSAGEM
            INITIALIZE WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-LISTA THRU P100-EXIT UNTIL WS-FS-CON = 10
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN INPUT CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-LISTA.
      *     DISPLAY LK-AREA
            READ CONTATOS INTO WS-REG-CONTATOS
            IF WS-FS-CON NOT EQUAL 00 AND 10 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO LEITURA DO ARQUIVO CONTATOS'  TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               IF WS-FS-CON = 00
                  ADD 1 TO WS-READ-CONT
                  DISPLAY 'REGISTRO: ' WS-READ-CONT
                          ': DADOS: ' WS-REG-CONTATOS
               END-IF
            END-IF.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CONTATOS'    TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA LISCONTA'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM.....: ' WS-MSG2
            DISPLAY ' FILE STATUS..: ' WS-MSG1
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE
            GOBACK.
       P800-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            DISPLAY ' '
            DISPLAY '---------------------------------------'
            DISPLAY '>>>> PROGRAMA LISCONTA FINALIZADO <<<<'
            DISPLAY '---------------------------------------'
            DISPLAY ' QTDE CONTATOS READ.: ' WS-READ-CONT
            DISPLAY '---------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM LISCONTA.
