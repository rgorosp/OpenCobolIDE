      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 15-05-2025
      * Purpose: CADASTRAR CONTATOS ARQUIVO SEQUENCIAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCONTT.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONTATOS ASSIGN TO "C:/ARQUIVOS/CONTATOS.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  ACCESS  MODE IS SEQUENTIAL
                  FILE STATUS  IS WS-FS-CON.
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOS.
           COPY REGCONTT.
       WORKING-STORAGE SECTION.
      * ----------------------------------------------------------
      * ACUMULADORES E FILE STATUS
      * ----------------------------------------------------------
       01  WS-VAR.
           03 WS-FS-CON          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-O-CON           PIC 9(02) VALUE ZEROS.
      * ----------------------------------------------------------
      * REDEFINIÇÃO DO LAYOUT DE SAIDA
      * ----------------------------------------------------------
       01  WS-REG-CONTATOS       PIC X(22) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CONTATOS.
           03 WS-ID-CONTATO      PIC 99.
           03 WS-NM-CONTATO      PIC X(20).
      * ----------------------------------------------------------
      * INICIO DO PROCESSAMENTO
      * ----------------------------------------------------------
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-CADASTRO UNTIL WS-FLAG = 'N'
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * ABERTURA DOS ARQUIVOS
      * ----------------------------------------------------------
       S050 SECTION.
       P050-ABRIR.
            OPEN OUTPUT CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO
            END-IF.
       P050-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * PROCESSAMENTO DO CADASTRO
      * ----------------------------------------------------------
       S100 SECTION.
       P100-CADASTRO.
            DISPLAY ' '
            DISPLAY 'DIGITE O ID COM 2 DIGITOS! '
            ACCEPT WS-ID-CONTATO
            DISPLAY 'DIGITE O NOME DO CONTATO! '
            ACCEPT WS-NM-CONTATO

            MOVE WS-ID-CONTATO TO ID-CONTATO
            MOVE WS-NM-CONTATO TO NM-CONTATO
            WRITE REG-CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO GRAVACAO DO ARQUIVO CONTATOS' TO WS-MSG2
            PERFORM P800-ERRO
            ELSE
               ADD 1 TO WS-O-CON
            END-IF

            PERFORM P101-VALIDACAO
            .
       P100-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * VALIDAÇÃO DO CADASTRO
      * ----------------------------------------------------------
       S101-VALIDACAO SECTION.
       P101-VALIDACAO.
            MOVE 'X' TO WS-FLAG
            PERFORM UNTIL WS-FLAG = 'S' OR WS-FLAG = 'N'
            DISPLAY 'DESEJA REALIZAR OUTRO CADASTRO, (S)SIM (N)NAO'
            ACCEPT WS-FLAG

            IF WS-FLAG <> 'S' AND WS-FLAG <> 'N' THEN
               DISPLAY "DADOS INVALIDOS!!!"
            ELSE
               CONTINUE
            END-IF
            END-PERFORM.
       P101-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * FECHAMENTOS DOS ARQUIVOS
      * ----------------------------------------------------------
       S700 SECTION.
       P700-FECHAR.
            CLOSE CONTATOS
            IF WS-FS-CON NOT = 00 THEN
               MOVE WS-FS-CON                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CONTATOS'    TO WS-MSG2
            PERFORM P800-ERRO
            END-IF.
       P700-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * MENSAGEM ERRO DO PROGRAMA
      * ----------------------------------------------------------
       S800 SECTION.
       P800-ERRO.
            PERFORM P700-FECHAR THRU P700-EXIT
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA CADCONTT'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM: ' WS-MSG2
            DISPLAY ' CODIGO..: ' WS-MSG1
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE
            STOP RUN.
       P800-EXIT.
            EXIT.
      * ----------------------------------------------------------
      * FIM DO PROGRAMA
      * ----------------------------------------------------------
       S999 SECTION.
       P999-FIM.
            DISPLAY '--------------------------------------'
            DISPLAY '>>>> PROGRAMA CADCONTT FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE CONTATOS GRAV: ' WS-O-CON
            DISPLAY '--------------------------------------'
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM CADCONTT.
