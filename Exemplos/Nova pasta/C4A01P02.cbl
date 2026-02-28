       IDENTIFICATION DIVISION.
       PROGRAM-ID. C4A01P02.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO 04 - AULA 01 - PRATICA 01
      *---------------------------------------------------------------
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - C4A01PE2 | I | ARQUIVO C4A01PE2 SEQUENCIAL|  - - - - |  040
      * - C4A01PR2 | O | ARQUIVO C4A01PR2 RELATORIO |  - - - - |  080
      *---------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA C4A01PE2 E C4A01PR2 DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT C4A01PE2 ASSIGN TO "C:/Arquivos/RANKING_ATP"
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE  IS SEQUENTIAL
                  RELATIVE KEY IS KEY-E02
                  FILE STATUS  IS WS-FS-E02
           .
           SELECT C4A01PR2 ASSIGN TO "C:/Arquivos/R1-C4A01PR2.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-R02
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  C4A01PE2.
       01  REG-C4A01PE2              PIC X(040).

       FD  C4A01PR2
           RECORDING MODE IS F.
       01  REG-C4A01PR2              PIC X(080).
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO COM AS VARIAVEIS
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01  WS-INICIO                 PIC X(40).
           88 INICIO                 VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       77  WS-SPACES                 PIC X(080).
       77  WS-HIFEN                  PIC X(080) VALUE ALL '='.
      * --> 77 - NAO HA NECESSIDADE DO NIVEL 01 E VALUE

      *==> AREA DESTINADA A CHAVE
       01  KEY-E02                   PIC 9(05) BINARY.

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA-AUX.
           05 WS-I-E02               PIC 9(11) VALUE ZEROS.
           05 WS-O-R02               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-TOTALPONTOS         PIC 9(05) VALUE ZEROS.
           05 WS-FLAG                PIC 9(02) VALUE ZEROS.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-E02              PIC 9(02) VALUE ZEROS.
           05 WS-FS-R02              PIC 9(02) VALUE ZEROS.
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
      *==> AREA DESTINADA A C4A01PE2.
       01  WS-REG-C4A01PE2.
           05 POSICAO-RANKING-E02    PIC 9(02).
           05 ULTIMONOME-E02         PIC X(11).
           05 PRIMEIRONOME-E02       PIC X(13).
           05 PAIS-E02               PIC X(03).
           05 TOTALPONTOS-E02        PIC 9(05).
           05 MOVIMENTACAO-E02       PIC X(04).
           05 PARTIDAS-E02           PIC 9(02).

      *==> AREA DESTINADA A CABECALHOS.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(10).
           05 FILLER                 PIC X(19).
           05 FILLER                 PIC X(11) VALUE
              'RANKING ATP'.
           05 FILLER                 PIC X(27).
           05 FILLER                 PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1               PIC 9(02).
           05 FILLER                 PIC X.

       01  WS-CAB2.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'HORA:'.
           05 HORA-CAB2              PIC X(11).
           05 FILLER                 PIC X(63).

       01  WS-CAB3.
           05 FILLER                 PIC X(09).
           05 FILLER                 PIC X(07) VALUE 'POSICAO'.
           05 FILLER                 PIC X(08).
           05 FILLER                 PIC X(21) VALUE
              'NOME(ULTIMO,PRIMEIRO)'.
           05 FILLER                 PIC X(08).
           05 FILLER                 PIC X(04) VALUE 'PAIS'.
           05 FILLER                 PIC X(08).
           05 FILLER                 PIC X(06) VALUE 'PONTOS'.
           05 FILLER                 PIC X(09).

      *==> AREA DESTINADA A DETALHES.
       01  WS-DET1.
           05 FILLER                 PIC X(14).
           05 POSICAO-DET1           PIC 9(02).
           05 FILLER                 PIC X(08).
           05 ULTIMONOME-DET1        PIC X(11).
           05 FILLER                 PIC X VALUE ','.
           05 PRIMEIRONOME-DET1      PIC X(13).
           05 FILLER                 PIC X(05).
           05 PAIS-DET1              PIC X(03).
           05 FILLER                 PIC X(09).
           05 TOTALPONTOS-DET1       PIC 9(05).
           05 FILLER                 PIC X(09).

      *==> AREA DESTINADA A RODAPE.
       01  WS-ROD1.
           05 FILLER                 PIC X(42).
           05 FILLER                 PIC X(24) VALUE
              'TOTAL DE PONTOS(1-10):  '.
           05 TOTALPONTOS-ROD1       PIC 9(05).
           05 FILLER                 PIC X(09).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C4A01P02.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 050-PROCESSO UNTIL WS-FS-E02 = '10'
           PERFORM 100-TERMINO
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INICIO DO PROCESSAMENTO
      *--------------------------------------------------------------*
       005-INICIO SECTION.
       005-INICIO-INI.
           INITIALIZE WS-AREA-AUX
           PERFORM 010-DATA-HORA-DAY
           MOVE DTEDI TO DTEDI-I
           MOVE HREDI TO HREDI-I
           MOVE 60    TO WS-CTLIN

           PERFORM 015-ABRIR
           PERFORM 020-LEITURA
           .
       005-INICIO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ABERTURA DOS ARQUIVOS
      *--------------------------------------------------------------*
       015-ABRIR SECTION.
       015-ABRIR-INI.
           OPEN INPUT C4A01PE2
           IF WS-FS-E02 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4A01PE2'    TO WS-MSG-A
              MOVE WS-FS-E02                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN OUTPUT C4A01PR2
           IF WS-FS-R02 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4A01PR2'    TO WS-MSG-A
              MOVE WS-FS-R02                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       015-ABRIR-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *   LEITURA DOS DADOS DE C4A01PE1
      *--------------------------------------------------------------*
       020-LEITURA SECTION.
       020-LEITURA-INI.
           READ C4A01PE2 INTO WS-REG-C4A01PE2

      D    EXHIBIT NAMED WS-REG-C4A01PE2

           IF WS-FS-E02 NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO C4A01PE2'  TO WS-MSG-A
              MOVE WS-FS-E02                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              IF WS-FS-E02 EQUAL '10' AND WS-I-E02 = 0
                 MOVE 'ARQUIVO C4A01PE2 SEM MOVIMENTO'    TO WS-MSG-A
                 MOVE WS-FS-E02                           TO WS-MSG-B
              GO TO 998-VAZIO
              ELSE
                 IF WS-FS-E02 EQUAL '00'
                    ADD 1 TO WS-I-E02
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

           MOVE POSICAO-RANKING-E02 TO POSICAO-DET1
           MOVE ULTIMONOME-E02      TO ULTIMONOME-DET1
           MOVE PRIMEIRONOME-E02    TO PRIMEIRONOME-DET1
           MOVE PAIS-E02            TO PAIS-DET1
           MOVE TOTALPONTOS-E02     TO TOTALPONTOS-DET1
           IF WS-FLAG <= 09
              ADD TOTALPONTOS-DET1 TO WS-TOTALPONTOS
           END-IF

           PERFORM 051-IMPREL
           PERFORM 020-LEITURA
           .
       050-PROCESSO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO C4A01PS1IO
      *--------------------------------------------------------------*
       051-IMPREL SECTION.
       051-IMPREL-INI.
           IF WS-CTLIN > 22
              PERFORM 052-IMPCAB
           END-IF

           WRITE REG-C4A01PR2 FROM WS-DET1
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4A01PR2 DET1'  TO WS-MSG-A
              MOVE WS-FS-R02                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-R02
              ADD 1                                     TO WS-FLAG
              ADD 1                                     TO WS-CTLIN
           END-IF
           .
       051-IMPREL-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       052-IMPCAB SECTION.
       052-IMPCAB-INI.
           WRITE REG-C4A01PR2 FROM WS-HIFEN
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN1'   TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           ADD 1                                  TO PAG-CAB1
           MOVE DTEDI                             TO DATA-CAB1
           WRITE REG-C4A01PR2 FROM WS-CAB1
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'     TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE HREDI TO HORA-CAB2
           WRITE REG-C4A01PR2 FROM WS-CAB2
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB2'     TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A01PR2 FROM WS-HIFEN
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN2'   TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A01PR2 FROM WS-SPACES
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE1'   TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A01PR2 FROM WS-CAB3
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB3'     TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
              GO TO 999-ERRO
           END-IF

           MOVE 06                                TO WS-CTLIN
           .
       052-IMPCAB-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       053-IMPROD SECTION.
       053-IMPROD-INI.
           WRITE REG-C4A01PR2 FROM WS-SPACES
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE2'   TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE WS-TOTALPONTOS  TO TOTALPONTOS-ROD1
           WRITE REG-C4A01PR2 FROM WS-ROD1
           IF WS-FS-R02 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO ROD1'     TO WS-MSG-A
              MOVE WS-FS-R02                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       053-IMPROD-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C4A01P01
      *--------------------------------------------------------------*
       100-TERMINO SECTION.
       100-TERMINO-INI.
           PERFORM 010-DATA-HORA-DAY
           PERFORM 053-IMPROD.
           PERFORM 080-FECHAR
           DISPLAY '============= PGM.C4A01P02 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS C4A01PE1 = ' WS-I-E02
           DISPLAY 'REG.GRAV. C4A01PR1 = ' WS-O-R02
           DISPLAY '======================================='
           DISPLAY 'INICIO:  ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO: ' DTEDI   '-' HREDI
           DISPLAY '======================================='
           DISPLAY 'DATA JULIANA:  ' DAYSYS
           DISPLAY 'DIA DA SEMANA: ' WS-WEESYS
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
           CLOSE C4A01PE2
           IF WS-FS-E02 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4A01PE2'    TO WS-MSG-A
              MOVE WS-FS-E02                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE C4A01PR2
           IF WS-FS-R02 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4A01PR2'    TO WS-MSG-A
              MOVE WS-FS-R02                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       080-FECHAR-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO C4A01PE1 SEM MOVIMENTO
      *----------------------------------------------------------------*
       998-VAZIO SECTION.
       998-VAZIO-INI.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     C4A01P02 SEM MOVIMENTO, FV. VERIFICAR.   '
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
       998-VAZIO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ERRO
      *--------------------------------------------------------------*
       999-ERRO SECTION.
       999-ERRO-INI.
           DISPLAY '============================================'
           DISPLAY '        ERRO NO PROGRAMA C4A01P02           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           MOVE 08 TO RETURN-CODE
           STOP RUN
           .
       999-ERRO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - DATA, DATA EDITADA, DATA JULIANA, HORA,
      *                 E O DIA DA SEMANA EM NUMERO E TEXTO
      *--------------------------------------------------------------*
       010-DATA-HORA-DAY.
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
           END-EVALUATE.
       END PROGRAM C4A01P02.
