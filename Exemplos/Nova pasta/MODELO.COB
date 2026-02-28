      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 17-10-23
      * Purpose:  imprimir o relatório RC5A3P01.txt conforme layout. Gerar uma
      * tabela interna no programa, para imprimir o nome do mês por extenso.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C5A03P12.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
            DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT FD-RC5A3P01
               ASSIGN  TO  "C:/ARQUIVOS/RC5A3P01.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-RC5.

            SELECT FD-S1C5A3P1
               ASSIGN  TO  "C:/ARQUIVOS/S1C5A3P2.txt"
               ORGANIZATION IS  LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-S1C.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FD-S1C5A3P1.
       01  REG-S1C5A3P1.
           03 COD-PROD-S     PIC 999.
           03 NOME-PROD-S    PIC X(13).
           03 COD-MES-S      PIC 99.
           03 VALOR-MES-S    PIC 9(5)V99.

       FD  FD-RC5A3P01
           RECORDING MODE IS F.
       01  REG-RC5A3P01      PIC X(080).
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO COM AS VARIAVEIS
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01  WS-INICIO                 PIC X(40).
           88 INICIO                 VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA.
           05 WS-I-S1C               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
           05 WS-COD-PROD-S          PIC 999.
           05 WS-TOTAL-TOT1          PIC 9(06)V99 VALUE ZEROS.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-S1C              PIC 9(02) VALUE ZEROS.
           05 WS-FS-RC5              PIC 9(02) VALUE ZEROS.
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
      *==> AREAD DESTINADA A HEADER
       01  HDR1.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 FILLER         PIC X(37) VALUE
              'VENDAS POR PRODUTO (ORDEM ALFABETICA)'.
           03 FILLER         PIC X(18) VALUE SPACES.
           03 FILLER         PIC X(05) VALUE 'PAG: '.
           03 PAG-1          PIC 9(03) VALUE ZEROS.
           03 FILLER         PIC X     VALUE SPACES.

       01  HDR2.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 FILLER         PIC X(07) VALUE 'PRODUTO'.
           03 FILLER         PIC X(09) VALUE SPACES.
           03 FILLER         PIC X(03) VALUE 'MES'.
           03 FILLER         PIC X(17) VALUE SPACES.
           03 FILLER         PIC X(10) VALUE 'QUANTIDADE'.
           03 FILLER         PIC X(18) VALUE SPACES.
      *==> AREAD DESTINADA A REG DETALHES
       01  DET1.
           03 FILLER         PIC X(16) VALUE SPACES.
           03 NOME-PROD-DET1 PIC X(13) VALUE SPACES.
           03 FILLER         PIC X(03) VALUE SPACES.
           03 MES-CORR-DET1  PIC X(09) VALUE SPACES.
           03 FILLER         PIC X(13) VALUE SPACES.
           03 VALOR-DET1     PIC Z.Z99,99.
           03 FILLER         PIC X(17) VALUE SPACES.
      *==> AREAD DESTINADA A TRAILLER
       01  TOT1.
           03 FILLER         PIC X(32) VALUE SPACES.
           03 FILLER         PIC X(13) VALUE 'TOTAL DO ANO:'.
           03 FILLER         PIC X(09) VALUE SPACES.
           03 TOTAL-TOT1     PIC Z.Z99,99.
           03 FILLER         PIC X(17) VALUE SPACES.

       77  WS-SPACES         PIC X(80) VALUE ALL SPACES.

       01  WS-TERMINO        PIC X(40).
           88 TERMINO        VALUE
           '********* TERMINO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       S000-C5A03P12 SECTION.
       P000-C5A03P12.

      D    READY TRACE

           PERFORM P001-INICIO
           PERFORM P021-ABRIR
           PERFORM P031-LEITURA
           PERFORM P041-PROCESSA UNTIL WS-FS-S1C = '10'
           PERFORM P991-TERMINO
           .
       P000-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INICIO DO PROCESSAMENTO
      *--------------------------------------------------------------*
       S000-INICIO SECTION.
       P001-INICIO.
           INITIALIZE WS-AREA
           PERFORM P011-DATA-HORA-DAY
           MOVE DTEDI TO DTEDI-I
           MOVE HREDI TO HREDI-I
           MOVE 65    TO WS-CTLIN
           .
       P001-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ABERTURA DOS ARQUIVOS
      *--------------------------------------------------------------*
       S020-ABRIR SECTION.
       P021-ABRIR.
           OPEN INPUT FD-S1C5A3P1
           IF WS-FS-S1C NOT EQUAL '00'
              MOVE 'ERRO P021-ABRIR FD-S1C5A3P1'      TO WS-MSG-A
              MOVE WS-FS-S1C                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF

           OPEN OUTPUT FD-RC5A3P01
           IF WS-FS-RC5 NOT EQUAL '00'
              MOVE 'ERRO P021-ABRIR FD-RC5A3P01'      TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF
           .
       P021-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *   LEITURA DOS DADOS DE ENTRADA
      *--------------------------------------------------------------*
       S030-LEITURA SECTION.
       P031-LEITURA.
           READ FD-S1C5A3P1

      D    EXHIBIT NAMED REG-S1C5A3P1

           IF WS-FS-S1C NOT EQUAL '00' AND '10'
              MOVE 'ERRO P031-LEITURA FD-S1C5A3P1'    TO WS-MSG-A
              MOVE WS-FS-S1C                          TO WS-MSG-B
           GO TO P901-ERRO
           ELSE
              IF WS-FS-S1C EQUAL '10' AND WS-I-S1C = 0
                 MOVE 'ARQ FD-S1C5A3P1 SEM MOVIMENTO' TO WS-MSG-A
                 MOVE WS-FS-S1C                       TO WS-MSG-B
              GO TO P911-VAZIO
              ELSE
                 IF WS-FS-S1C EQUAL '00'
                    ADD 1 TO WS-I-S1C
                 END-IF
              END-IF
           END-IF.
       P031-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       S040-PROCESSA SECTION.
       P041-PROCESSA.

           IF WS-I-S1C <= 1 THEN
              MOVE COD-PROD-S TO WS-COD-PROD-S
           END-IF

      D    EXHIBIT NAMED DET1

           MOVE COD-PROD-S      TO NOME-PROD-DET1
           EVALUATE COD-MES-S
             WHEN 01
                  MOVE 'JANEIRO'   TO MES-CORR-DET1
             WHEN 02
                  MOVE 'FEVEREIRO' TO MES-CORR-DET1
             WHEN 03
                  MOVE 'MARCO'     TO MES-CORR-DET1
             WHEN 04
                  MOVE 'ABRIL'     TO MES-CORR-DET1
             WHEN 05
                  MOVE 'MAIO'      TO MES-CORR-DET1
             WHEN 06
                  MOVE 'JUNHO'     TO MES-CORR-DET1
             WHEN 07
                  MOVE 'JULHO'     TO MES-CORR-DET1
             WHEN 08
                  MOVE 'AGOSTO'    TO MES-CORR-DET1
             WHEN 09
                  MOVE 'SETEMBRO'  TO MES-CORR-DET1
             WHEN 10
                  MOVE 'OUTUBRO'   TO MES-CORR-DET1
             WHEN 11
                  MOVE 'NOVEMBRO'  TO MES-CORR-DET1
             WHEN 12
                  MOVE 'DEZEMBRO'  TO MES-CORR-DET1
             WHEN OTHER
                  MOVE '????????'  TO MES-CORR-DET1
           END-EVALUATE

           IF COD-PROD-S EQUAL WS-COD-PROD-S THEN
              PERFORM P501-IMPREL
              PERFORM P031-LEITURA
           ELSE
              MOVE COD-PROD-S TO WS-COD-PROD-S
              PERFORM P521-IMPTOT
              PERFORM P501-IMPREL
              PERFORM P031-LEITURA
           END-IF
           .
       P041-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO RELATORIO
      *--------------------------------------------------------------*
       S500-IMPREL SECTION.
       P501-IMPREL.
           IF WS-CTLIN > 60
              PERFORM P511-IMPCAB
           END-IF

           MOVE VALOR-MES-S TO VALOR-DET1
           ADD  VALOR-MES-S TO WS-TOTAL-TOT1
           WRITE REG-RC5A3P01 FROM DET1
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO P501-IMPREL DET1'            TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           ELSE
              ADD 1                                   TO WS-CTLIN
           END-IF
           .
       P501-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       S510-IMPCAB SECTION.
       P511-IMPCAB.
           ADD 1 TO PAG-1
           WRITE REG-RC5A3P01 FROM HDR1
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO P511-IMPCAB HDR1'            TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF

           WRITE REG-RC5A3P01 FROM WS-SPACES
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO P511-IMPCAB WS-SPACES1'      TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF

           WRITE REG-RC5A3P01 FROM HDR2
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'         TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF

           MOVE 03                                    TO WS-CTLIN
           .
       P511-EXIT.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       S520-IMPTOT SECTION.
       P521-IMPTOT.
           MOVE WS-TOTAL-TOT1 TO TOTAL-TOT1
           WRITE REG-RC5A3P01 FROM TOT1
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO P521-IMPTOT TOT1'            TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           ELSE
              ADD 1                                   TO WS-CTLIN
           END-IF

           WRITE REG-RC5A3P01 FROM WS-SPACES
           IF WS-FS-RC5 NOT = '00'
              MOVE 'ERRO P511-IMPCAB WS-SPACES2'      TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           ELSE
           ADD 1                                      TO WS-CTLIN
           END-IF

           MOVE ZEROS TO WS-TOTAL-TOT1
           .
       P521-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - FECHAR OS ARQUIVOS
      *--------------------------------------------------------------*
       S800-FECHAR SECTION.
       P801-FECHAR.
           CLOSE FD-S1C5A3P1
           IF WS-FS-S1C NOT EQUAL '00'
              MOVE 'ERRO P801-FECHAR FD-S1C5A3P1'     TO WS-MSG-A
              MOVE WS-FS-S1C                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF

           CLOSE FD-RC5A3P01
           IF WS-FS-RC5 NOT EQUAL '00'
              MOVE 'ERRO P801-FECHAR FD-RC5A3P01'     TO WS-MSG-A
              MOVE WS-FS-RC5                          TO WS-MSG-B
           GO TO P901-ERRO
           END-IF
           .
       P801-EXIT.
            EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO ENTRADA SEM MOVIMENTO
      *----------------------------------------------------------------*
       S910-VAZIO SECTION.
       P911-VAZIO.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     ARQUIVO  SEM MOVIMENTO, FV. VERIFICAR.   '
           DISPLAY '=============================================='
           DISPLAY 'MENSAGEM  = ' WS-MSG-A
           DISPLAY 'CODIGO    = ' WS-MSG-B
           DISPLAY '=============================================='
           DISPLAY 'INICIO  DO PROCESSAMENTO = ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO DO PROCESSAMENTO = ' DTEDI   '-' HREDI
           DISPLAY '=============================================='
           MOVE 04 TO RETURN-CODE
           STOP RUN
           .
       P911-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ERRO
      *--------------------------------------------------------------*
       S900-ERRO SECTION.
       P901-ERRO.
           DISPLAY '============================================'
           DISPLAY '             ERRO NO PROGRAMA'
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           MOVE 08 TO RETURN-CODE
           STOP RUN
           .
       P901-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA
      *--------------------------------------------------------------*
       S990-TERMINO SECTION.
       P991-TERMINO.
           PERFORM P521-IMPTOT
           PERFORM P801-FECHAR
           PERFORM P011-DATA-HORA-DAY
           DISPLAY '======= TERMINO DO PROCESSAMENTO ======'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS FD-S1C5A3P1 = ' WS-I-S1C
           DISPLAY '======================================='
           DISPLAY 'INICIO:  ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO: ' DTEDI   '-' HREDI
           DISPLAY '======================================='
           DISPLAY 'DATA JULIANA:  ' DAYSYS
           DISPLAY 'DIA DA SEMANA: ' WS-WEESYS
           DISPLAY '======================================='
           STOP RUN
           .
       P991-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - DATA, DATA EDITADA, DATA JULIANA, HORA,
      *                 E O DIA DA SEMANA EM NUMERO E TEXTO
      *--------------------------------------------------------------*
       S010-DATA-HORA-DAY SECTION.
       P011-DATA-HORA-DAY.
           ACCEPT DTSYS FROM DATE
           STRING DTSYS (5:2) '/'
                  DTSYS (3:2) '/20'
                  DTSYS (1:2)
           DELIMITED BY SIZE INTO DTEDI

           ACCEPT HRSYS FROM TIME
           STRING HRSYS (1:2) ':'
                  HRSYS (3:2) ':'
                  HRSYS (5:2) ':'
                  HRSYS (7:2)
           DELIMITED BY SIZE INTO HREDI

           ACCEPT DAYSYS FROM DAY

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
           END-EVALUATE.
       P011-EXIT.
            EXIT.
       END PROGRAM C5A03P12.
