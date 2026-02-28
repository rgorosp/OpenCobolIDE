      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 10-07-24.
      * Purpose: CÁLCULOS ARITMÉTICOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C2A06P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E SAIDA DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO
             'C:\Arquivos\C2A06P01-E.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-ENT.
      *
           SELECT RELATOR ASSIGN TO
             'C:\Arquivos\C2A06P01-S.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-REL.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA
           RECORDING MODE IS F.
       01  REG-ENTRADA               PIC X(59).

       FD  RELATOR
           RECORDING MODE IS F.
       01  REG-RELATOR               PIC X(133).
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO COM AS VARIAVEIS
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01  WS-INICIO                 PIC X(40).
           88 INICIO                 VALUE
           '********* INICIO DAS VARIAVEIS *********'.

       77  WS-SPACES                 PIC X(133) VALUE SPACES.
       77  WS-LINES                  PIC X(133) VALUE ALL '-'.

      *==> AREA DESTINADA A CONTADORES E ACUMULADORES
       01  WS-AREA-AUX.
           05 TOTVALORLIMITES        PIC 9(11) VALUE ZEROS.
           05 WS-I-ENT               PIC 9(11) VALUE ZEROS.
           05 WS-O-REL               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
           05 WS-IMPR                PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-ENT              PIC 9(02) VALUE ZEROS.
           05 WS-FS-REL              PIC 9(02) VALUE ZEROS.
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
           05 NUMEROCONTA            PIC X(07).
           05 NOMECLIENTE            PIC X(30).
           05 SALDOATUAL             PIC 9(11).
           05 LIMITECREDITO          PIC 9(11).
      *==> AREA DESTINADA A RELATORIO.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(11).
           05 FILLER                 PIC X(110).
           05 FILLER                 PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1               PIC 9(02).
           05 FILLER                 PIC X.

       01  WS-CAB2.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'HORA:'.
           05 HORA-CAB2              PIC X(11).
           05 FILLER                 PIC X(116).

       01  WS-CAB3.
           05 FILLER                 PIC X(57).
           05 FILLER                 PIC X(18) VALUE
              'SEGUE DADOS ABAIXO'.
           05 FILLER                 PIC X(58).

       01  WS-CAB4.
           05 FILLER                 PIC X(57).
           05 FILLER                 PIC X(18) VALUE
              '------------------'.
           05 FILLER                 PIC X(58).

       01  WS-CAB5.
           05 FILLER                 PIC X(14).
           05 FILLER                 PIC X(15) VALUE
              'NUMERO DA CONTA'.
           05 FILLER                 PIC X(14).
           05 FILLER                 PIC X(15) VALUE
              'NOME DO CLIENTE'.
           05 FILLER                 PIC X(14).
           05 FILLER                 PIC X(17) VALUE
              'LIMITE DE CREDITO'.
           05 FILLER                 PIC X(15).
           05 FILLER                 PIC X(11) VALUE
              'SALDO ATUAL'.
           05 FILLER                 PIC X(18).

       01  WS-DET1.
           05 FILLER                 PIC X(18).
           05 NUMEROCONTA-DET1       PIC X(07).
           05 FILLER                 PIC X(18).
           05 NOMECLIENTE-DET1       PIC X(30).
           05 FILLER                 PIC X(06).
           05 LIMITECREDITO-DET1     PIC ZZZ.ZZ9.99.
           05 FILLER                 PIC X(16).
           05 SALDOATUAL-DET1        PIC ZZZ.ZZ9.99.
           05 FILLER                 PIC X(18).

       01  WS-ROD1.
           05 FILLER                 PIC X(14).
           05 FILLER                 PIC X(18) VALUE
              'TOTAL DOS LIMITES:'.
           05 FILLER                 PIC X(11).
           05 VALORLIMITES-ROD1      PIC ZZZ.ZZ9.99.
           05 FILLER                 PIC X(81).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       S000-C2A06P01 SECTION.
       P000-C2A06P01.
            PERFORM P005-INICIO
            PERFORM P050-PROCESSO UNTIL WS-FS-ENT = '10'
            PERFORM P100-TERMINO.
       P000-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INICIO DO PROCESSAMENTO
      *--------------------------------------------------------------*
       S005-INICIO SECTION.
       P005-INICIO.
            INITIALIZE WS-AREA-AUX
            PERFORM P010-DATA-HORA-DAY
            MOVE DTEDI TO DTEDI-I
            MOVE HREDI TO HREDI-I
            MOVE ZEROS TO VALORLIMITES-ROD1
            MOVE 65    TO WS-CTLIN

            PERFORM P015-ABRIR
            PERFORM P020-LEITURA.
       P005-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ABERTURA DOS ARQUIVOS
      *--------------------------------------------------------------*
       S015-ABRIR SECTION.
       P015-ABRIR.
           OPEN INPUT ENTRADA
           IF WS-FS-ENT NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO ENTRADA'     TO WS-MSG-A
              MOVE WS-FS-ENT                             TO WS-MSG-B
           PERFORM P999-ERRO
           END-IF

           OPEN OUTPUT RELATOR
           IF WS-FS-REL NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO RELATOR'     TO WS-MSG-A
              MOVE WS-FS-REL                             TO WS-MSG-B
           PERFORM P999-ERRO
           END-IF
           .
       P015-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *   LEITURA DOS DADOS DE ENTRADA
      *--------------------------------------------------------------*
       S020-LEITURA SECTION.
       P020-LEITURA.
           READ ENTRADA  INTO WS-REG-ENTRADA
           IF NUMEROCONTA = '9999999'
              MOVE 10 TO WS-FS-ENT
           END-IF

           IF WS-FS-ENT NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO ENTRADA'   TO WS-MSG-A
              MOVE WS-FS-ENT                              TO WS-MSG-B
           PERFORM P999-ERRO
           ELSE
              IF WS-FS-ENT EQUAL '10' AND WS-I-ENT = 0
                 MOVE 'ARQUIVO ENTRADA SEM MOVIMENTO'     TO WS-MSG-A
                 MOVE WS-FS-ENT                           TO WS-MSG-B
              PERFORM P998-VAZIO
              ELSE
                 IF WS-FS-ENT EQUAL '00'
                    ADD 1 TO WS-I-ENT
                 END-IF
              END-IF
           END-IF
           .
       P020-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       S050-PROCESSO SECTION.
       P050-PROCESSO.
           IF SALDOATUAL >= 100000 AND SALDOATUAL < 150000
              PERFORM P051-IMPREL
           END-IF

           PERFORM P020-LEITURA.
       P050-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO RELATORIO
      *--------------------------------------------------------------*
       S051-IMPREL SECTION.
       P051-IMPREL.
           IF WS-CTLIN > 60
              PERFORM P052-IMPCAB
           END-IF

           ADD 1 TO WS-O-REL
           ADD  LIMITECREDITO TO TOTVALORLIMITES

           MOVE NUMEROCONTA   TO NUMEROCONTA-DET1
           MOVE NOMECLIENTE   TO NOMECLIENTE-DET1
           MOVE SALDOATUAL    TO SALDOATUAL-DET1
           MOVE LIMITECREDITO TO LIMITECREDITO-DET1
           WRITE REG-RELATOR FROM WS-DET1
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO RELATOR DET1'   TO WS-MSG-A
              MOVE WS-FS-REL                            TO WS-MSG-B
           GO TO P999-ERRO
           ELSE
              ADD 1                               TO WS-IMPR
              ADD 1                               TO WS-CTLIN
           END-IF
           .
       P051-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       S052-IMPCAB SECTION.
       P052-IMPCAB.
           WRITE REG-RELATOR FROM WS-LINES
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO LINES1'   TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           ADD 1                                  TO PAG-CAB1
           MOVE DTEDI                             TO DATA-CAB1
           WRITE REG-RELATOR FROM WS-CAB1 AFTER PAGE
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'     TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           MOVE HREDI TO HORA-CAB2
           WRITE REG-RELATOR FROM WS-CAB2
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB2'     TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-LINES
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO LINES2'   TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-SPACES
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE1'   TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-CAB3
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB3'     TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
              GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-CAB4
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB4'     TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
              GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-SPACES
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE2'   TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           WRITE REG-RELATOR FROM WS-CAB5
           IF WS-FS-REL NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB5'     TO WS-MSG-A
              MOVE WS-FS-REL                      TO WS-MSG-B
              GO TO P999-ERRO
           END-IF

           MOVE 09                                TO WS-CTLIN
           .
       P052-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C2A06P01
      *--------------------------------------------------------------*
       S100-TERMINO SECTION.
       P100-TERMINO.
           WRITE REG-RELATOR FROM WS-SPACES
           IF WS-FS-REL NOT EQUAL '00'
              MOVE 'ERRO AO GRAVAR O RELATOR SPACES3'     TO WS-MSG-A
              MOVE WS-FS-REL                              TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           MOVE TOTVALORLIMITES TO VALORLIMITES-ROD1
           WRITE REG-RELATOR FROM WS-ROD1
           IF WS-FS-REL NOT EQUAL '00'
              MOVE 'ERRO AO GRAVAR O RELATOR ROD1'        TO WS-MSG-A
              MOVE WS-FS-REL                              TO WS-MSG-B
           GO TO P999-ERRO
           END-IF
           PERFORM P010-DATA-HORA-DAY
           PERFORM P080-FECHAR
           DISPLAY '============= PGM.C2A06P01 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS ENTRADA  = ' WS-I-ENT
           DISPLAY 'REG.GRAV. RELATOR  = ' WS-O-REL
           DISPLAY '======================================='
           DISPLAY 'INICIO:  ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO: ' DTEDI   '-' HREDI
           DISPLAY '======================================='
           DISPLAY 'DATA JULIANA:  ' DAYSYS
           DISPLAY 'DIA DA SEMANA: ' WS-WEESYS
           DISPLAY '======================================='
           PERFORM P999-FIM.
       P100-EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - FECHAR OS ARQUIVOS
      *--------------------------------------------------------------*
       S080-FECHAR SECTION.
       P080-FECHAR.
           CLOSE ENTRADA
           IF WS-FS-ENT NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO ENTRADA'     TO WS-MSG-A
              MOVE WS-FS-REL                              TO WS-MSG-B
           GO TO P999-ERRO
           END-IF

           CLOSE RELATOR
           IF WS-FS-REL NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO RELATOR'     TO WS-MSG-A
              MOVE WS-FS-REL                              TO WS-MSG-B
           GO TO P999-ERRO
           END-IF.
       P080-EXIT.
            EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO ENTRADA SEM MOVIMENTO
      *----------------------------------------------------------------*
       S998-VAZIO SECTION.
       P998-VAZIO.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     C2A06P01 SEM MOVIMENTO, FV. VERIFICAR.   '
           DISPLAY '=============================================='
           DISPLAY 'MENSAGEM  = ' WS-MSG-A
           DISPLAY 'CODIGO    = ' WS-MSG-B
           DISPLAY '=============================================='
           DISPLAY 'INICIO  DO PROCESSAMENTO = ' DTEDI-I '-' HREDI-I
           DISPLAY 'TERMINO DO PROCESSAMENTO = ' DTEDI   '-' HREDI
           DISPLAY '=============================================='
           MOVE 04 TO RETURN-CODE
           PERFORM P999-FIM.
       P998-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - ROTINA DE ERRO
      *--------------------------------------------------------------*
       S999-ERRO SECTION.
       P999-ERRO.
           DISPLAY '============================================'
           DISPLAY '        ERRO NO PROGRAMA C2A06P01           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           MOVE 08 TO RETURN-CODE
           PERFORM P999-FIM.
       P999-ERRO-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - DATA, DATA EDITADA, DATA JULIANA, HORA,
      *                 E O DIA DA SEMANA EM NUMERO E TEXTO
      *--------------------------------------------------------------*
       S010-DATA-HORA-DAY SECTION.
       P010-DATA-HORA-DAY.
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
       P010-DATA-HORA-DAY-EXIT.
            EXIT.

       S999-FIM SECTION.
       P999-FIM.
           STOP RUN.
       P999-FIM-EXIT.
            EXIT.
