       IDENTIFICATION DIVISION.
       PROGRAM-ID. C4A01P04.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO 04 - AULA 01 - PRATICA 04
      *---------------------------------------------------------------
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - C4A01PE1 | I | ARQUIVO C4P04PE1 SEQUENCIAL|  - - - - |  040
      * - C4A01PE2 |I-O| ARQUIVO C4A04PE2 RELATIVO  |  - - - - |  040
      * - C4A01PR1 | O | ARQUIVO C4A04PR1 RELATORIO |  - - - - |  080
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
           SELECT C4A04PE1 ASSIGN TO
                  "C:/Arquivos/E1-C4A01P04.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-PE1
           .
           SELECT C4A04PE2 ASSIGN TO
                  "C:/Arquivos/RANKING_ATP"
                  ORGANIZATION IS RELATIVE
                  ACCESS MODE  IS DYNAMIC
                  RELATIVE KEY IS KEY-PE2
                  FILE STATUS  IS WS-FS-PE2
           .
           SELECT C4A04PR1 ASSIGN TO
                  "C:/Arquivos/R1-C4P04PR1.TXT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-PR1
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  C4A04PE1
           RECORDING MODE IS F.
       01  REG-C4A04PE1                PIC X(040).

       FD  C4A04PE2.
       01  REG-C4A04PE2                PIC X(040).

       FD  C4A04PR1
           RECORDING MODE IS F.
       01  REG-C4A04PR1                PIC X(080).
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
       01  KEY-PE2                   PIC 9(05) BINARY.

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA-AUX.
           05 WS-I-PE1               PIC 9(11) VALUE ZEROS.
           05 WS-O-PE2               PIC 9(11) VALUE ZEROS.
           05 WS-O-PR1               PIC 9(11) VALUE ZEROS.
           05 WS-DEL-PE2             PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.

      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-PE1              PIC 9(02) VALUE ZEROS.
           05 WS-FS-PE2              PIC 9(02) VALUE ZEROS.
           05 WS-FS-PR1              PIC 9(02) VALUE ZEROS.

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

      *==> AREA DESTINADA A C4A04PE1.
       01  WS-REG-C4A04PE1.
           05 POSICAO-RANKING-PE1    PIC 9(02).
           05 ULTIMONOME-PE1         PIC X(11).
           05 PRIMEIRONOME-PE1       PIC X(13).
           05 PAIS-PE1               PIC X(03).
           05 TOTALPONTOS-PE1        PIC 9(05).
           05 MOVIMENTACAO-PE1       PIC X(04).
           05 PARTIDAS-PE1           PIC 9(02).

      *==> AREA DESTINADA A C4A04PE2.
       01  WS-REG-C4A04PE2.
           05 POSICAO-RANKING-PE2    PIC 9(02).
           05 ULTIMONOME-PE2         PIC X(11).
           05 PRIMEIRONOME-PE2       PIC X(13).
           05 PAIS-PE2               PIC X(03).
           05 TOTALPONTOS-PE2        PIC 9(05).
           05 MOVIMENTACAO-PE2       PIC X(04).
           05 PARTIDAS-PE2           PIC 9(02).

      *==> AREA DESTINADA A CABECALHOS.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(10).
           05 FILLER                 PIC X(13).
           05 FILLER                 PIC X(20) VALUE
              'RELACAO DE INCLUSOES'.
           05 FILLER                 PIC X(24).
           05 FILLER                 PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1               PIC 9(02).
           05 FILLER                 PIC X.

       01  WS-CAB2.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'HORA:'.
           05 HORA-CAB2              PIC X(11).
           05 FILLER                 PIC X(63).

       01  WS-CAB3.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(07) VALUE 'POSICAO'.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(11) VALUE
              'ULTIMO NOME'.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(13) VALUE 'PRIMEIRO NOME'.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(10) VALUE 'OCORRENCIA'.
           05 FILLER                 PIC X(11).

      *==> AREA DESTINADA A DETALHES.
       01  WS-DET1.
           05 FILLER                 PIC X(12).
           05 POSICAO-PR1            PIC 9(02).
           05 FILLER                 PIC X(07).
           05 ULTIMONOME-PR1         PIC X(11).
           05 FILLER                 PIC X(07).
           05 PRIMEIRONOME-PR1       PIC X(13).
           05 FILLER                 PIC X(07).
           05 OCORRENCIA-PR1         PIC X(20).
           05 FILLER                 PIC X(01).

      *==> AREA DESTINADA A RODAPE.
       01  WS-ROD1.
           05 FILLER                 PIC X(41).
           05 FILLER                 PIC X(20) VALUE
              'REGISTROS INCLUIDOS:'.
           05 FILLER                 PIC X(05).
           05 TOT-REG-INCLU          PIC 9(05).
           05 FILLER                 PIC X(09).

      *==> AREA DESTINADA A RODAPE.
       01  WS-ROD2.
           05 FILLER                 PIC X(41).
           05 FILLER                 PIC X(25) VALUE
              'REGISTROS NAO INCLUIDOS: '.
           05 TOT-REG-NAO-INCLU      PIC 9(05).
           05 FILLER                 PIC X(09).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C4A01P04 SECTION.
       000-C4A01P04-INI.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 050-PROCESSO UNTIL WS-FS-PE1 = '10'
           PERFORM 100-TERMINO
           .
       000-C4A01P04-FIM.
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
           MOVE 65    TO WS-CTLIN

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
           OPEN INPUT C4A04PE1
           IF WS-FS-PE1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4A04PE1'    TO WS-MSG-A
              MOVE WS-FS-PE1                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN I-O C4A04PE2
           IF WS-FS-PE2 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4A04PE2'    TO WS-MSG-A
              MOVE WS-FS-PE2                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN OUTPUT C4A04PR1
           IF WS-FS-PR1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4A04PR1'    TO WS-MSG-A
              MOVE WS-FS-PR1                             TO WS-MSG-B
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
           READ C4A04PE1 INTO WS-REG-C4A04PE1

      D    EXHIBIT NAMED WS-REG-C4A04PE1

           IF WS-FS-PE1 NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO C4A04PE1'  TO WS-MSG-A
              MOVE WS-FS-PE1                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              IF WS-FS-PE1 EQUAL '10' AND WS-I-PE1 = 0
                 MOVE 'ARQUIVO C4A04PE1 SEM MOVIMENTO'    TO WS-MSG-A
                 MOVE WS-FS-PE1                           TO WS-MSG-B
              GO TO 998-VAZIO
              ELSE
                 IF WS-FS-PE1 EQUAL '00'
                    ADD 1 TO WS-I-PE1
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
           MOVE POSICAO-RANKING-PE1 TO KEY-PE2
           READ C4A04PE2 INTO WS-REG-C4A04PE2
           IF WS-FS-PE2 = '23'
               MOVE WS-REG-C4A04PE1 TO WS-REG-C4A04PE2
               WRITE REG-C4A04PE2 FROM WS-REG-C4A04PE2
               ADD 1 TO TOT-REG-INCLU
               MOVE 'REGIS INCLUIDO' TO OCORRENCIA-PR1
           ELSE
           IF WS-FS-PE2 NOT = '00' AND WS-FS-PE2 NOT = '23'
                 MOVE 'ERRO NA LEITURA DO ARQUIVO C4A04PE2'  TO WS-MSG-A
                 MOVE WS-FS-PE2                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
           IF WS-FS-PE2 = '00'
               ADD 1 TO WS-DEL-PE2 TOT-REG-NAO-INCLU
               MOVE 'REGIS NAO INCLUIDO' TO OCORRENCIA-PR1
               DISPLAY '*-------------------------------*'
               DISPLAY 'CHAVE ' KEY-PE2 ' JA EXISTENTE'
               DISPLAY '*-------------------------------*'
           END-IF
           END-IF
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
           IF WS-CTLIN > 60
              PERFORM 052-IMPCAB
           END-IF

           MOVE POSICAO-RANKING-PE1 TO POSICAO-PR1
           MOVE ULTIMONOME-PE1      TO ULTIMONOME-PR1
           MOVE PRIMEIRONOME-PE1    TO PRIMEIRONOME-PR1
           WRITE REG-C4A04PR1 FROM WS-DET1
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4A04PR1 DET1'  TO WS-MSG-A
              MOVE WS-FS-PR1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-PR1
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
           WRITE REG-C4A04PR1 FROM WS-HIFEN
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN1'   TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           ADD 1                                  TO PAG-CAB1
           MOVE DTEDI                             TO DATA-CAB1
           WRITE REG-C4A04PR1 FROM WS-CAB1
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'     TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE HREDI TO HORA-CAB2
           WRITE REG-C4A04PR1 FROM WS-CAB2
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB2'     TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-HIFEN
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN2'   TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-SPACES
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE1'   TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-CAB3
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB3'     TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
              GO TO 999-ERRO
           END-IF

           MOVE 06                                TO WS-CTLIN
           .
       052-IMPCAB-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR RODAPE
      *--------------------------------------------------------------*
       053-IMPROD SECTION.
       053-IMPROD-INI.
           WRITE REG-C4A04PR1 FROM WS-SPACES
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE2'   TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-ROD1
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO ROD1'     TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-SPACES
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE3'   TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4A04PR1 FROM WS-ROD2
           IF WS-FS-PR1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO ROD2'     TO WS-MSG-A
              MOVE WS-FS-PR1                      TO WS-MSG-B
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
           DISPLAY '============= PGM.C4A01P04 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS     C4A04PE1 = ' WS-I-PE1
           DISPLAY 'REG.INCLUIDO  C4A04PE2 = ' TOT-REG-INCLU
           DISPLAY 'REG.NAO INCLU C4A04PE2 = ' TOT-REG-NAO-INCLU
           DISPLAY 'REG.GRAVADO   C4A04PR1 = ' WS-O-PR1
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
           CLOSE C4A04PE1
           IF WS-FS-PE1 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4A04PE1'    TO WS-MSG-A
              MOVE WS-FS-PE1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE C4A04PE2
           IF WS-FS-PE2 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4A04PE2'    TO WS-MSG-A
              MOVE WS-FS-PE2                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE C4A04PR1
           IF WS-FS-PR1 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4A04PR1'    TO WS-MSG-A
              MOVE WS-FS-PR1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       080-FECHAR-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO C4A04PE1 SEM MOVIMENTO
      *----------------------------------------------------------------*
       998-VAZIO SECTION.
       998-VAZIO-INI.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     C4A01P03 SEM MOVIMENTO, FV. VERIFICAR.   '
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
           DISPLAY '        ERRO NO PROGRAMA C4A01P03           '
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
       END PROGRAM C4A01P04.
