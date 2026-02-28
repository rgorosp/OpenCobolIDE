       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A09P02.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO 03 - AULA 09 - PRATICA 02
      *---------------------------------------------------------------
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - ENTRADA  | I | ARQUIVO ENTRADA SEQUENCIAL |  - - - - |  054
      * - RELATOR  | O | ARQUIVO RELATOR IMPRESSAO  |  - - - - |  080
      *---------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           SYSIN IS WS-SYSIN
           .
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E RELATOR DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA  ASSIGN TO 'C:\Arquivos\E1-C3A09P02.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-E01
           .
           SELECT RELATOR  ASSIGN TO 'C:\Arquivos\S1-C3A09P02.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-R01
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA
           RECORDING MODE IS F.
       01  REG-ENTRADA               PIC X(054).

       FD  RELATOR
           RECORDING MODE IS F.
       01  REG-RELATOR               PIC X(080).
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

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA-AUX.
           05 WS-I-E01               PIC 9(11) VALUE ZEROS.
           05 WS-O-R01               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-VLREST              PIC 9(07)V99 VALUE ZEROS.
           05 WS-VLR-TOT             PIC 9(07)V99 VALUE ZEROS.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-E01              PIC 9(02) VALUE ZEROS.
           05 WS-FS-R01              PIC 9(02) VALUE ZEROS.
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
      *==> AREA DESTINADA A ENTRADA.
       01  WS-REG-ENTRADA.
           05 COD-PECA-E01           PIC X(04).
           05 FILLER                 PIC X.
           05 NOME-PECA-E01          PIC X(38).
           05 FILLER                 PIC X.
           05 PRECO-E01              PIC 99V99.
           05 FILLER                 PIC X.
           05 QTD-ESTOQUE-E01        PIC 9(05).
      *==> AREA DESTINADA A RELATORIO.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(10).
           05 FILLER                 PIC X(16).
           05 FILLER                 PIC X(17) VALUE
              'R E L A T O R I O'.
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
           05 FILLER                 PIC X(03).
           05 FILLER                 PIC X(06) VALUE 'CODIGO'.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(04) VALUE 'NOME'.
           05 FILLER                 PIC X(37).
           05 FILLER                 PIC X(05) VALUE 'PRECO'.
           05 FILLER                 PIC X(03).
           05 FILLER                 PIC X(07) VALUE 'ESTOQUE'.
           05 FILLER                 PIC X(06).
           05 FILLER                 PIC X(05) VALUE 'VALOR'.
           05 FILLER                 PIC X(03).

       01  WS-DET1.
           05 FILLER                 PIC X(03).
           05 COD-PECA-DET1          PIC X(04).
           05 FILLER                 PIC X(03).
           05 NOME-PECA-DET1         PIC X(38).
           05 FILLER                 PIC X(03).
           05 PRECO-DET1             PIC Z9,99.
           05 FILLER                 PIC X(04).
           05 QTD-ESTOQUE-DET1       PIC 9(05).
           05 FILLER                 PIC X(03).
           05 VALOR-DET1             PIC ZZZZZ9,99.
           05 FILLER                 PIC X(03).

       01  WS-ROD1.
           05 FILLER                 PIC X(43).
           05 FILLER                 PIC X(25) VALUE
              'VALOR TOTAL DO ESTOQUE:  '.
           05 VALOR-TOTAL-ROD1       PIC ZZZZZ9,99.
           05 FILLER                 PIC X(03).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C3A09P02.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 050-PROCESSO UNTIL WS-FS-E01 = '10'
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
           OPEN INPUT ENTRADA
           IF WS-FS-E01 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO ENTRADA'     TO WS-MSG-A
              MOVE WS-FS-E01                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN OUTPUT RELATOR
           IF WS-FS-R01 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO RELATOR'     TO WS-MSG-A
              MOVE WS-FS-R01                             TO WS-MSG-B
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
           READ ENTRADA INTO WS-REG-ENTRADA

      D    EXHIBIT NAMED WS-REG-ENTRADA

           IF WS-FS-E01 NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO ENTRADA'   TO WS-MSG-A
              MOVE WS-FS-E01                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              IF WS-FS-E01 EQUAL '10' AND WS-I-E01 = 0
                 MOVE 'ARQUIVO ENTRADA SEM MOVIMENTO'     TO WS-MSG-A
                 MOVE WS-FS-E01                           TO WS-MSG-B
              GO TO 998-VAZIO
              ELSE
                 IF WS-FS-E01 EQUAL '00'
                    ADD 1 TO WS-I-E01
                 END-IF
              END-IF
           END-IF.
       020-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050-PROCESSO SECTION.
       050-PROCESSO-INI.

           COMPUTE WS-VLREST = (PRECO-E01 * QTD-ESTOQUE-E01)

      D    EXHIBIT NAMED WS-VLREST

           MOVE COD-PECA-E01    TO COD-PECA-DET1
           MOVE NOME-PECA-E01   TO NOME-PECA-DET1
           MOVE PRECO-E01       TO PRECO-DET1
           MOVE QTD-ESTOQUE-E01 TO QTD-ESTOQUE-DET1
           MOVE WS-VLREST       TO VALOR-DET1
           ADD  WS-VLREST       TO WS-VLR-TOT

           PERFORM 051-IMPREL
           PERFORM 020-LEITURA
           .
       050-PROCESSO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO RELATORIO
      *--------------------------------------------------------------*
       051-IMPREL SECTION.
       051-IMPREL-INI.
           IF WS-CTLIN > 60
              PERFORM 052-IMPCAB
           END-IF

           WRITE REG-RELATOR FROM WS-DET1
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO RELATOR DET1'   TO WS-MSG-A
              MOVE WS-FS-R01                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-R01
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
           WRITE REG-RELATOR FROM WS-HIFEN
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN1'   TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           ADD 1                                  TO PAG-CAB1
           MOVE DTEDI                             TO DATA-CAB1
           WRITE REG-RELATOR FROM WS-CAB1
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'     TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE HREDI TO HORA-CAB2
           WRITE REG-RELATOR FROM WS-CAB2
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB2'     TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-HIFEN
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN2'   TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-SPACES
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE1'   TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-CAB3
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB3'     TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
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
           WRITE REG-RELATOR FROM WS-SPACES
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE2'   TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE WS-VLR-TOT      TO VALOR-TOTAL-ROD1
           WRITE REG-RELATOR FROM WS-ROD1
           IF WS-FS-R01 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO ROD1'     TO WS-MSG-A
              MOVE WS-FS-R01                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       053-IMPROD-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C3A09P02
      *--------------------------------------------------------------*
       100-TERMINO SECTION.
       100-TERMINO-INI.
           PERFORM 010-DATA-HORA-DAY
           PERFORM 053-IMPROD.
           PERFORM 080-FECHAR
           DISPLAY '============= PGM.C3A09P02 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS ENTRADA = ' WS-I-E01
           DISPLAY 'REG.GRAV. RELATOR = ' WS-O-R01
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
           CLOSE ENTRADA
           IF WS-FS-E01 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO ENTRADA'     TO WS-MSG-A
              MOVE WS-FS-E01                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE RELATOR
           IF WS-FS-R01 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO RELATOR'     TO WS-MSG-A
              MOVE WS-FS-R01                              TO WS-MSG-B
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
           DISPLAY '     C3A09P02 SEM MOVIMENTO, FV. VERIFICAR.   '
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
           DISPLAY '        ERRO NO PROGRAMA C3A09P02           '
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
       END PROGRAM C3A09P02.
