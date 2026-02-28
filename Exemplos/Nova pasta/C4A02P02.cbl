       IDENTIFICATION DIVISION.
       PROGRAM-ID. C4A02P02.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO04 - AULA 02 - PRATICA 02
      *---------------------------------------------------------------
      *   LPAR     | V | DESCRICAO                             | DATA
      *   PRE7     |001| MONTAGEM DO PROGRAMA C4A02P02         | 14/10
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - C4P02EN1 | I | ARQUIVO ENTRADA SEQUENCIAL |  - - - - |  020
      * - C4P02SA1 | O | ARQUIVO INDEXADO           |  - - - - |  020
      *---------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           SYSIN IS WS-SYSIN
           .
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E SAIDA DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT C4P02EN1 ASSIGN TO "C:/ARQUIVOS/E1-C4A02P02.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-EN1
           .
           SELECT C4P02SA1 ASSIGN TO "C:/ARQUIVOS/DEPA_CADASTRO.DAT"
                  ORGANIZATION    IS INDEXED
                  ACCESS MODE     IS SEQUENTIAL
                  RECORD KEY      IS KEY-SA1
                  FILE STATUS     IS WS-FS-SA1
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  C4P02SA1.
       01  REG-C4P02SA1.
           05 KEY-SA1                PIC 9(02).
           05 FILLER                 PIC X(16).
           05 FILLER                 PIC X(02).

       FD  C4P02EN1
           RECORDING MODE IS F.
       01  REG-C4P02EN1              PIC X(20).
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO COM AS VARIAVEIS
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01  WS-INICIO                 PIC X(40).
           88 INICIO                 VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       77  WS-SPACES                 PIC X(132).
      * --> 77 - NAO HA NECESSIDADE DO NIVEL 01 E VALUE

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA-AUX.
           05 WS-I-EN1               PIC 9(11) VALUE ZEROS.
           05 WS-O-SA1               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-EN1              PIC 9(02) VALUE ZEROS.
           05 WS-FS-SA1              PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A MENSAGEM
           05 WS-MSG-A               PIC X(35) VALUE SPACES.
           05 WS-MSG-B               PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A DATA DE PROCESSAMENTO
           05 DTSYS                  PIC 9(06) VALUE ZEROS.
           05 DTEDI                  PIC X(10) VALUE SPACES.
           05 DTEDI-I                PIC X(10) VALUE SPACES.
           05 HRSYS                  PIC 9(08) VALUE ZEROS.
           05 HREDI                  PIC X(11) VALUE SPACES.
           05 HREDI-I                PIC X(11) VALUE SPACES.
      *==> AREA DESTINADA A SYSIN
           05 WS-AMBIENTE            PIC X(10) VALUE SPACES.
           05 WS-RETURN              PIC 9(04) VALUE ZEROS.
      *==> AREA DESTINADA A ENTRADA.
       01  WS-REG-C4P02EN1.
           05 DEPTO-EN1              PIC 9(02).
           05 NOMEDEPTO-EN1          PIC X(16).
           05 RESERVA-EN1            PIC X(02).
      *==> AREA DESTINADA A SAIDA.
       01  WS-REG-C4P02SA1.
           05 DEPTO-SA1              PIC 9(02).
           05 NOMEDEPTO-SA1          PIC X(16).
           05 RESERVA-SA1            PIC X(02).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C4A02P02 SECTION.
       000-C4A02P02-INI.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 015-ABRIR
           PERFORM 020-LEITURA
           PERFORM 050-PROCESSO UNTIL WS-FS-EN1 = '10'
           PERFORM 100-TERMINO
           STOP RUN
           .
       000-C4A02P02-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INICIO DO PROCESSAMENTO
      *--------------------------------------------------------------*
       005-INICIO SECTION.
       005-INICIO-INI.
           INITIALIZE WS-AREA-AUX
           PERFORM 010-DATA-HORA-DAY
           MOVE DTEDI TO DTEDI-I
           MOVE HREDI TO HREDI-I
           MOVE 'DESKTOP' TO WS-AMBIENTE
           .
       005-INICIO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ABERTURA DOS ARQUIVOS
      *--------------------------------------------------------------*
       015-ABRIR SECTION.
       015-ABRIR-INI.
           OPEN INPUT C4P02EN1
           IF WS-FS-EN1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4P02EN1'    TO WS-MSG-A
              MOVE WS-FS-EN1                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN OUTPUT C4P02SA1
           IF WS-FS-SA1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4P02SA1'    TO WS-MSG-A
              MOVE WS-FS-SA1                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       015-ABRIR-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *   LEITURA DOS DADOS DE ENTRADA
      *--------------------------------------------------------------*
       020-LEITURA SECTION.
       020-LEITURA-INI.
           READ C4P02EN1 INTO WS-REG-C4P02EN1

      D    EXHIBIT NAMED WS-REG-C4P02EN1

           IF WS-FS-EN1 NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO C4P02EN1'  TO WS-MSG-A
              MOVE WS-FS-EN1                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              IF WS-FS-EN1 EQUAL '10' AND WS-I-EN1 = 0
                 MOVE 'ARQUIVO ENTRADA SEM MOVIMENTO'     TO WS-MSG-A
                 MOVE WS-FS-EN1                           TO WS-MSG-B
              GO TO 998-VAZIO
              ELSE
                 IF WS-FS-EN1 EQUAL '00'
                    ADD 1 TO WS-I-EN1
                 END-IF
              END-IF
           END-IF
           .
       020-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050-PROCESSO SECTION.
       050-PROCESSO-INI.
           MOVE WS-REG-C4P02EN1 TO WS-REG-C4P02SA1

      D    EXHIBIT NAMED WS-REG-C4P02EN1

           WRITE REG-C4P02SA1 FROM WS-REG-C4P02SA1
           IF WS-FS-SA1 NOT EQUAL '00'
              MOVE 'ERRO AO GRAVAR O ARQUIVO C4P02SA1'    TO WS-MSG-A
              MOVE WS-FS-SA1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           ADD 1 TO WS-O-SA1
           PERFORM 020-LEITURA
           .
       050-PROCESSO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C4A02P02
      *--------------------------------------------------------------*
       100-TERMINO SECTION.
       100-TERMINO-INI.
           PERFORM 010-DATA-HORA-DAY
           PERFORM 080-FECHAR
           DISPLAY '============= PGM.C4A02P02 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS C4P02EN1 = ' WS-I-EN1
           DISPLAY 'REG.GRAV. C4P02SA1 = ' WS-O-SA1
           DISPLAY '======================================='
           DISPLAY 'INICIO:  ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO: ' DTEDI   '-' HREDI
           DISPLAY '======================================='
           DISPLAY 'DATA JULIANA:  ' DAYSYS
           DISPLAY 'DIA DA SEMANA: ' WS-WEESYS
           DISPLAY '======================================='
           DISPLAY 'AMBIENTE: ' WS-AMBIENTE
           DISPLAY '======================================='
           STOP RUN
           .
       100-TERMINO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - FECHAR OS ARQUIVOS
      *--------------------------------------------------------------*
       080-FECHAR SECTION.
       080-FECHAR-INI.
           CLOSE C4P02EN1
           IF WS-FS-EN1 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4P02EN1'    TO WS-MSG-A
              MOVE WS-FS-EN1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE C4P02SA1
           IF WS-FS-SA1 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4P02SA1'    TO WS-MSG-A
              MOVE WS-FS-SA1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       080-FECHAR-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO ENTRADA SEM MOVIMENTO
      *----------------------------------------------------------------*
       998-VAZIO SECTION.
       998-VAZIO-INI.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     C4A02P02 SEM MOVIMENTO, FV. VERIFICAR.   '
           DISPLAY '=============================================='
           DISPLAY 'MENSAGEM  = ' WS-MSG-A
           DISPLAY 'CODIGO    = ' WS-MSG-B
           DISPLAY '=============================================='
           DISPLAY 'INICIO  DO PROCESSAMENTO = ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO DO PROCESSAMENTO = ' DTEDI   '-' HREDI
           DISPLAY '=============================================='
           DISPLAY 'AMBIENTE: ' WS-AMBIENTE
           DISPLAY 'RETURN-CODE: ' WS-RETURN
           DISPLAY '=============================================='
           MOVE 04 TO RETURN-CODE WS-RETURN
           STOP RUN
           .
       998-VAZIO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ERRO
      *--------------------------------------------------------------*
       999-ERRO SECTION.
       999-ERRO-INI.
           DISPLAY '============================================'
           DISPLAY '        ERRO NO PROGRAMA C4A02P02           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           DISPLAY 'AMBIENTE: ' WS-AMBIENTE
           DISPLAY 'RETURN-CODE: ' WS-RETURN
           DISPLAY '============================================'
           MOVE 08 TO RETURN-CODE WS-RETURN
           STOP RUN
           .
       999-ERRO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - DATA, DATA EDITADA, DATA JULIANA, HORA,
      *                 E O DIA DA SEMANA EM NUMERO E TEXTO
      *--------------------------------------------------------------*
       010-DATA-HORA-DAY SECTION.
       010-DATA-HORA-DAY-INI.
           ACCEPT DTSYS FROM DATE
           STRING DTSYS (5:2) '/'
                  DTSYS (3:2) '/20'
                  DTSYS (1:2)
           DELIMITED BY SIZE INTO DTEDI
           .
           ACCEPT HRSYS FROM TIME
           STRING HRSYS (1:2) ':'
                  HRSYS (3:2) ':'
                  HRSYS (5:2) ':'
                  HRSYS (7:2)
           DELIMITED BY SIZE INTO HREDI
           .
           ACCEPT DAYSYS FROM DAY
           .
           ACCEPT WEESYS FROM DAY-OF-WEEK
           EVALUATE WEESYS
              WHEN  1
              MOVE  'SEGUNDA-FEIRA' TO WS-WEESYS
              WHEN  2
              MOVE  'TERCA-FEIRA'   TO WS-WEESYS
              WHEN  3
              MOVE  'QUARTA-FEIRA'  TO WS-WEESYS
              WHEN  4
              MOVE  'QUINTA-FEIRA'  TO WS-WEESYS
              WHEN  5
              MOVE  'SEXTA-FEIRA'   TO WS-WEESYS
              WHEN  6
              MOVE  'SABADO'        TO WS-WEESYS
              WHEN  7
              MOVE  'DOMINGO'       TO WS-WEESYS
           END-EVALUATE
           .
       010-DATA-HORA-DAY-FIM.
           EXIT.
