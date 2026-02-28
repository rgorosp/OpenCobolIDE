      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 28-10-24.
      * Purpose: Capítulo 03 - Aula 06 - Prática 01
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A06P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SAIDA ASSIGN TO 'C:\Arquivos\S1-C3A06P01.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-SAI.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  SAIDA
           RECORDING MODE IS F.
       01  REG-SAIDA                 PIC X(41).
      *
       WORKING-STORAGE SECTION.
       01  WS-AREA-AUX.
           05 WS-FS-SAI              PIC 9(02).
           05 WS-FLAG                PIC 9(02).
           05 WS-MSG-A               PIC X(35).
           05 WS-MSG-B               PIC 9(02).
           05 WS-CODIGO              PIC 9(04).
           05 WS-PRECO-KILO          PIC 999V99.
           05 WS-KILOS-ESTOQUE       PIC 9(05).
           05 WS-NOME-VERDURA        PIC X(20).
      *
       01  WS-REG-SAIDA.
           05 S-CODIGO               PIC 9(04).
           05 FILLER                 PIC X.
           05 S-PRECO-KILO           PIC 999V99.
           05 FILLER                 PIC X.
           05 S-KILOS-ESTOQUE        PIC 9(05).
           05 FILLER                 PIC X.
           05 S-NOME-VERDURA         PIC X(20).
      *
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-PROGRAMA.
           PERFORM P001-INICIO
           PERFORM P005-ABERTURA
           PERFORM P010-PROCESSO.
       P000-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE INICIO
      *--------------------------------------------------------------*
       S001 SECTION.
       P001-INICIO.
            INITIALIZE WS-AREA-AUX.
       P001-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ABERTURA
      *--------------------------------------------------------------*
       S005 SECTION.
       P005-ABERTURA.
           OPEN OUTPUT SAIDA
           IF WS-FS-SAI NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO SAIDA'       TO WS-MSG-A
              MOVE WS-FS-SAI                             TO WS-MSG-B
           PERFORM P999-ERRO
           END-IF.
       P005-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE PROCESSAMENTO
      *--------------------------------------------------------------*
       S010 SECTION.
       P010-PROCESSO.
           IF WS-FLAG = '10' THEN
              PERFORM P015-FECHAMENTO
           ELSE
              DISPLAY "                 " UPON CONSOLE
              DISPLAY "Insira o Codigo: " UPON CONSOLE
              ACCEPT WS-CODIGO FROM CONSOLE

              DISPLAY "Insira o Preco do Kilo: " UPON CONSOLE
              ACCEPT WS-PRECO-KILO FROM CONSOLE

              DISPLAY "Insira o Valor no Estoque: " UPON CONSOLE
              ACCEPT WS-KILOS-ESTOQUE FROM CONSOLE

              DISPLAY "Insira o Nome da Verdura: " UPON CONSOLE
              ACCEPT WS-NOME-VERDURA FROM CONSOLE
              ADD 1 TO WS-FLAG
           PERFORM P013-GRAVA
           PERFORM P010-PROCESSO
           END-IF
           .
       P010-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE GRAVACAO
      *--------------------------------------------------------------*
       S013 SECTION.
       P013-GRAVA.
           MOVE WS-CODIGO        TO S-CODIGO
           MOVE WS-PRECO-KILO    TO S-PRECO-KILO
           MOVE WS-KILOS-ESTOQUE TO S-KILOS-ESTOQUE
           MOVE WS-NOME-VERDURA  TO S-NOME-VERDURA
           WRITE REG-SAIDA FROM WS-REG-SAIDA AFTER
                 ADVANCING 1 LINES
           IF WS-FS-SAI NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO ARQUIVO SAIDA'    TO WS-MSG-A
              MOVE WS-FS-SAI                              TO WS-MSG-B
           PERFORM P999-ERRO
           END-IF.
       P013-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ERRO
      *--------------------------------------------------------------*
       S999 SECTION.
       P999-ERRO.
           DISPLAY '============================================'
           DISPLAY '        ERRO NO PROGRAMA C3A06P01           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           MOVE 08 TO RETURN-CODE
           STOP RUN.
       P999-EXIT.
            EXIT.

      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE CLOSE
      *--------------------------------------------------------------*
       S015 SECTION.
       P015-FECHAMENTO.
           CLOSE SAIDA
           IF WS-FS-SAI NOT = '00'
              MOVE 'ERRO NO FECHAMENTO DO ARQUIVO SAIDA'  TO WS-MSG-A
              MOVE WS-FS-SAI                              TO WS-MSG-B
           GO TO P999-ERRO
           END-IF
           STOP RUN.
       P015-EXIT.
            EXIT.
       END PROGRAM C3A06P01.
