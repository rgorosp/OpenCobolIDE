       IDENTIFICATION DIVISION.
       PROGRAM-ID. C4A02P03.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO04 - AULA 02 - PRATICA 03
      *---------------------------------------------------------------
      *   LPAR     | V | DESCRICAO                             | DATA
      *   PRE7     |001| MONTAGEM DO PROGRAMA C4A02P03         | 24/10
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - C4P02EN1 | I | ARQUIVO INDEXADO           |  - - - - |  020
      * - C4P02EN2 | I | ARQUIVO INDEXADO           |  - - - - |  040
      * - C4P02RE1 | O | ARQUIVO RELATORIO          |  - - - - |  080
      *---------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
           SYSIN IS WS-SYSIN
           .
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E SAIDA DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT C4P02EN1 ASSIGN TO "C:/ARQUIVOS/DEPA_CADASTRO.DAT"
                  ORGANIZATION    IS INDEXED
                  ACCESS MODE     IS DYNAMIC
                  RECORD KEY      IS KEY-EN1
                  FILE STATUS     IS WS-FS-EN1
           .
           SELECT C4P02EN2 ASSIGN TO "C:/ARQUIVOS/FUNC_CADASTRO.DAT"
                  ORGANIZATION    IS INDEXED
                  ACCESS MODE     IS SEQUENTIAL
                  RECORD KEY      IS KEY-EN2
                  FILE STATUS     IS WS-FS-EN2
           .
           SELECT C4P02RE1 ASSIGN TO "C:/ARQUIVOS/R1-C4A02P03.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-RE1
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  C4P02EN1.
       01  REG-C4P02EN1.
           05 KEY-EN1                PIC 9(02).
           05 FILLER                 PIC X(16).
           05 FILLER                 PIC X(02).

       FD  C4P02EN2.
       01  REG-C4P02EN2.
           05 FILLER                 PIC X(02).
           05 KEY-EN2                PIC 9(03).
           05 FILLER                 PIC X(35).

       FD  C4P02RE1
           RECORDING MODE IS F.
       01  REG-C4P02RE1              PIC X(80).
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
           05 WS-I-EN1               PIC 9(11) VALUE ZEROS.
           05 WS-I-EN2               PIC 9(11) VALUE ZEROS.
           05 WS-O-RE1               PIC 9(11) VALUE ZEROS.
           05 WS-CALC                PIC 9(05) VALUE ZEROS.
           05 WS-TOT-CALC            PIC 9(07) VALUE ZEROS.
           05 WS-DEPTO               PIC 9(02) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-EN1              PIC 9(02) VALUE ZEROS.
           05 WS-FS-EN2              PIC 9(02) VALUE ZEROS.
           05 WS-FS-RE1              PIC 9(02) VALUE ZEROS.
      *==> AREA DESTINADA A MENSAGEM
           05 WS-MSG-A               PIC X(35) VALUE SPACES.
           05 WS-MSG-B               PIC 9(02) VALUE ZEROS.
           05 WS-MSG-C               PIC 9(02) VALUE ZEROS.
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

       01  WS-REG-C4P02EN2.
           05 DEPTO-EN2              PIC 9(02).
           05 MATRICULA-EN2          PIC 9(03).
           05 NOME-EN2               PIC X(20).
           05 SAL-HORA-EN2           PIC 9(03)V99.
           05 HRS-MES-EN2            PIC 9(03)V99.
           05 RESERVA-EN2            PIC X(05).
      *==> AREA DESTINADA A SAIDA.
       01  WS-REG-C4P02RE1.
           05 FILLER                 PIC X(06).
           05 MATRICULA-RE1          PIC 9(03).
           05 FILLER                 PIC X(06).
           05 NOME-RE1               PIC X(20).
           05 FILLER                 PIC X(06).
           05 SAL-HORA-RE1           PIC Z99.99.
           05 FILLER                 PIC X(06).
           05 HRS-MES-RE1            PIC Z99.99.
           05 FILLER                 PIC X(04).
           05 VLR-SALARIO-RE1        PIC ZZZ.ZZ9.99.
           05 FILLER                 PIC X(07).
      *==> AREA DESTINADA A CABECALHOS.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(10).
           05 FILLER                 PIC X(13).
           05 FILLER                 PIC X(23) VALUE
              'RELATORIO DEPARTAMENTAL'.
           05 FILLER                 PIC X(21).
           05 FILLER                 PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1               PIC 9(02).
           05 FILLER                 PIC X.

       01  WS-CAB2.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'HORA:'.
           05 HORA-CAB2              PIC X(11).
           05 FILLER                 PIC X(63).

       01  WS-CAB3.
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(06) VALUE
              'DEPTO:'.
           05 COD-MAT-CAB3           PIC 9(02).
           05 FILLER                 PIC X(03) VALUE
              ' - '.
           05 COD-DEPTO-CAB3         PIC X(16).
           05 FILLER                 PIC X(52).

       01  WS-CAB4.
           05 FILLER                 PIC X(06).
           05 FILLER                 PIC X(03) VALUE
              'MAT'.
           05 FILLER                 PIC X(06).
           05 FILLER                 PIC X(04) VALUE
              'NOME'.
           05 FILLER                 PIC X(17).
           05 FILLER                 PIC X(10) VALUE
              'VALOR-HORA'.
           05 FILLER                 PIC X(03).
           05 FILLER                 PIC X(08) VALUE
              'HORA-MES'.
           05 FILLER                 PIC X(09).
           05 FILLER                 PIC X(07) VALUE
              'SALARIO'.
           05 FILLER                 PIC X(07).

      *==> AREA DESTINADA A RODAPE.
       01  WS-ROD1.
           05 FILLER                 PIC X(44).
           05 FILLER                 PIC X(19) VALUE
              'TOTAL DE SALARIOS: '.
           05 TOT-SALARIO-ROD1       PIC $ZZ.ZZ9.99.
           05 FILLER                 PIC X(07).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C4A02P03 SECTION.
       000-C4A02P03-INI.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 015-ABRIR
           PERFORM 020-LEITURA
           PERFORM 050-PROCESSO UNTIL WS-FS-EN2 = '10'
           PERFORM 100-TERMINO
           STOP RUN
           .
       000-C4A02P03-FIM.
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
           MOVE 65 TO WS-CTLIN
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

           OPEN INPUT C4P02EN2
           IF WS-FS-EN2 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4P02EN2'    TO WS-MSG-A
              MOVE WS-FS-EN2                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN OUTPUT C4P02RE1
           IF WS-FS-RE1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4P02RE1'    TO WS-MSG-A
              MOVE WS-FS-RE1                             TO WS-MSG-B
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
           READ C4P02EN2 INTO WS-REG-C4P02EN2

      D    EXHIBIT NAMED WS-REG-C4P02EN2

           IF WS-FS-EN2 NOT EQUAL '00' AND '10'
              MOVE 'ERRO NA LEITURA DO ARQUIVO C4P02EN2'  TO WS-MSG-A
              MOVE WS-FS-EN2                              TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              IF WS-FS-EN2 EQUAL '10' AND WS-I-EN2 = 0
                 MOVE 'ARQUIVO ENTRADA SEM MOVIMENTO'     TO WS-MSG-A
                 MOVE WS-FS-EN2                           TO WS-MSG-B
              GO TO 998-VAZIO
              ELSE
                 IF WS-FS-EN2 EQUAL '00'
                    ADD 1 TO WS-I-EN2
                    PERFORM 021-READ-C4P02EN1
                 END-IF
              END-IF
           END-IF
           .
       020-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       021-READ-C4P02EN1 SECTION.
       021-READ-C4P02EN1-INI.
           MOVE DEPTO-EN2 TO KEY-EN1
           READ C4P02EN1 INTO WS-REG-C4P02EN1

      D    EXHIBIT NAMED WS-REG-C4P02EN1

           IF WS-FS-EN1 EQUAL '23'
              MOVE 'CHAVE INEXISTENTE' TO WS-MSG-A
              MOVE WS-FS-EN1           TO WS-MSG-B
              MOVE KEY-EN1             TO WS-MSG-C
              PERFORM 9991-ERRO
              PERFORM 020-LEITURA
           ELSE
              IF WS-FS-EN1 EQUAL '00'
                 ADD 1 TO WS-I-EN1
                 MOVE DEPTO-EN1     TO COD-MAT-CAB3
                 MOVE NOMEDEPTO-EN1 TO COD-DEPTO-CAB3
              END-IF
           END-IF
           .
       021-READ-C4P02EN1-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050-PROCESSO SECTION.
       050-PROCESSO-INI.

           MOVE MATRICULA-EN2 TO MATRICULA-RE1
           MOVE NOME-EN2      TO NOME-RE1
           MOVE SAL-HORA-EN2  TO SAL-HORA-RE1
           MOVE HRS-MES-EN2   TO HRS-MES-RE1

           COMPUTE WS-CALC = (SAL-HORA-EN2 * HRS-MES-EN2)
           ADD WS-CALC TO WS-TOT-CALC

           MOVE WS-CALC TO VLR-SALARIO-RE1
           PERFORM 051-IMPREL
           PERFORM 020-LEITURA
           .
       050-PROCESSO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO C4P05PR1
      *--------------------------------------------------------------*
       051-IMPREL SECTION.
       051-IMPREL-INI.
           IF WS-CTLIN > 22
              PERFORM 053-IMPCAB
           END-IF

           IF DEPTO-EN2 NOT = WS-DEPTO AND WS-I-EN2 NOT EQUAL 1
              PERFORM 052-CAB3
           END-IF

           WRITE REG-C4P02RE1 FROM WS-REG-C4P02RE1
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 DET'   TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-RE1
              ADD 1                                     TO WS-CTLIN
           END-IF

           MOVE DEPTO-EN2 TO WS-DEPTO
           .
       051-IMPREL-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO - CAB3
      *--------------------------------------------------------------*
       052-CAB3 SECTION.
       052-CAB3-INI.
           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO WS-SPACES1'     TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-CAB3
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 CAB3'  TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO WS-SPACES2'     TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       052-CAB3-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       053-IMPCAB SECTION.
       053-IMPCAB-INI.
           MOVE ZEROS TO WS-CTLIN
           WRITE REG-C4P02RE1 FROM WS-HIFEN
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN1'   TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           ADD 1                                  TO PAG-CAB1
           MOVE DTEDI                             TO DATA-CAB1
           WRITE REG-C4P02RE1 FROM WS-CAB1
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB1'     TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE HREDI TO HORA-CAB2
           WRITE REG-C4P02RE1 FROM WS-CAB2
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB2'     TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-HIFEN
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO HIFEN2'   TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE3'   TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           IF WS-I-EN2 = 1
           WRITE REG-C4P02RE1 FROM WS-CAB3
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 CAB3-A' TO WS-MSG-A
              MOVE WS-FS-RE1                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO WS-SPACES4'     TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ADD 2 TO WS-CTLIN
           END-IF

           WRITE REG-C4P02RE1 FROM WS-CAB4
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB4'     TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
              GO TO 999-ERRO
           END-IF

           MOVE 06                                TO WS-CTLIN
           .
       053-IMPCAB-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C4A02P02
      *--------------------------------------------------------------*
       100-TERMINO SECTION.
       100-TERMINO-INI.
           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO SPACE5'   TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE WS-TOT-CALC TO TOT-SALARIO-ROD1
           WRITE REG-C4P02RE1 FROM WS-ROD1
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO RODAPE1'   TO WS-MSG-A
              MOVE WS-FS-RE1                       TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           PERFORM 010-DATA-HORA-DAY
           PERFORM 080-FECHAR
           DISPLAY '============= PGM.C4A02P03 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS C4P02EN1 = ' WS-I-EN1
           DISPLAY 'REG.LIDOS C4P02EN2 = ' WS-I-EN2
           DISPLAY 'REG.GRAV. C4P02RE1 = ' WS-O-RE1
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

           CLOSE C4P02EN2
           IF WS-FS-EN2 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4P02EN2'    TO WS-MSG-A
              MOVE WS-FS-EN2                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           CLOSE C4P02RE1
           IF WS-FS-RE1 NOT EQUAL '00'
              MOVE 'ERRO NO CLOSE DO ARQUIVO C4P02RE1'    TO WS-MSG-A
              MOVE WS-FS-RE1                              TO WS-MSG-B
           GO TO 999-ERRO
           END-IF
           .
       080-FECHAR-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * PROCEDIMENTO - MENSAGEM ARQUIVO ENTRADA SEM MOVIMENTO
      * RETURN-CODE = 8 CONSIDERAR COMO NOTOK
      *----------------------------------------------------------------*
       998-VAZIO SECTION.
       998-VAZIO-INI.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '     C4A02P03 SEM MOVIMENTO, FV. VERIFICAR.   '
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
           MOVE 08 TO RETURN-CODE WS-RETURN
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
           DISPLAY '        ERRO NO PROGRAMA C4A02P03           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           DISPLAY 'AMBIENTE: ' WS-AMBIENTE
           DISPLAY 'RETURN-CODE: ' WS-RETURN
           DISPLAY '============================================'
           MOVE 12 TO RETURN-CODE WS-RETURN
           STOP RUN
           .
       999-ERRO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - DISPLAY CHAVE INVALIDA
      *--------------------------------------------------------------*
       9991-ERRO SECTION.
       9991-ERRO-INI.
           DISPLAY '============================================'
           DISPLAY '        DISPLAY NO PROGRAMA C4A02P03           '
           DISPLAY '============================================'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY 'CHAVE    = ' WS-MSG-C
           .
       9991-ERRO-FIM.
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
