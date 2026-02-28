      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 26-05-2025
      * Purpose: CADASTRAR CONTATOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA
           SYSIN IS WS-SYSIN.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS
                  ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
      *    SELECT CONTATOS ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-CONTATO
                  FILE STATUS  IS WS-FS-CON.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY REGCONTT.
       WORKING-STORAGE SECTION.
      * >>> VARIAVEIS
       01  WS-VAR.
           03 WS-FS-CON          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-O-CON           PIC 9(02) VALUE ZEROS.

      * >>> ARQUIVO SAIDA
       01  WS-REG-CONTATOS       PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CONTATOS.
           03 WS-ID-CONTATO      PIC 99.
           03 WS-NM-CONTATO      PIC X(20).

      * AREA DE COMUNICACAO
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
            PERFORM P100-CADASTRO THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

      * >>> ABERTURA DE ARQUIVOS
       S050 SECTION.
       P050-ABRIR.
            OPEN I-O CONTATOS
            IF WS-FS-CON = 35 THEN
               OPEN OUTPUT CONTATOS
            ELSE
            IF WS-FS-CON NOT = 00
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF
            END-IF.
       P050-EXIT.
            EXIT.

      * >>> PROCESSAMENTO
       S100 SECTION.
       P100-CADASTRO.
            DISPLAY LK-AREA
            PERFORM UNTIL WS-FLAG = 'N'
               DISPLAY ' '
               DISPLAY 'DIGITE O ID COM 2 DIGITOS! '
               ACCEPT WS-ID-CONTATO
               DISPLAY 'DIGITE O NOME DO CONTATO! '
               ACCEPT WS-NM-CONTATO

            MOVE WS-ID-CONTATO TO ID-CONTATO
            MOVE WS-NM-CONTATO TO NM-CONTATO
            WRITE REG-CONTATOS
                  INVALID KEY
                       DISPLAY 'CONTATO JA CADASTRADO'
                       DISPLAY ' '
                  NOT INVALID KEY
                       DISPLAY 'CADASTO REALIZADO!'
                       DISPLAY ' '
            END-WRITE

            IF WS-FS-CON NOT = 00 AND WS-FS-CON NOT = 22 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO GRAVACAO DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               IF WS-FS-CON = 22
                  CONTINUE
               ELSE
                  ADD 1 TO WS-O-CON
               END-IF
            END-IF

            DISPLAY 'DESEJA REALIZAR OUTRO CADASTRO, (S)SIM (N)NAO'
            ACCEPT WS-FLAG
            END-PERFORM.
       P100-EXIT.
            EXIT.

      * >>> FECHAMENTO DOS ARQUIVOS
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

      * >>> DISPLAY DE ERRO DO PROGRAMA
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

      * >>> DISPLAY DE TERMINO DO PROGRAMA.
       S999 SECTION.
       P999-FIM.
            DISPLAY '--------------------------------------'
            DISPLAY '>>>> PROGRAMA CADCONTA FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE CONTATOS GRAV: ' WS-O-CON
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM CADCONTA.
