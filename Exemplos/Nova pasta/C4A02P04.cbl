       IDENTIFICATION DIVISION.
       PROGRAM-ID. C4A02P04.
      *---------------------------------------------------------------
      * PROGRAMA: CAPITULO04 - AULA 02 - PRATICA 04
      *---------------------------------------------------------------
      *   LPAR     | V | DESCRICAO                             | DATA
      *   PRE7     |001| MONTAGEM DO PROGRAMA C4A02P04         | 10/11
      * ========== |===| ===================================== | =====
      *   DDNAME   |I/O|          DESCRICAO         |   COPY   | LRECL
      * ========== |===| ========================== | ======== | =====
      * - C4P02EN1 | I | ARQUIVO SEQUENCIAL         |  - - - - |  036
      * - C4P02EN2 |I-O| ARQUIVO INDEXADO           |  - - - - |  040
      * - C4P02RE1 | O | ARQUIVO RELATORIO          |  - - - - |  080
      *---------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E SAIDA DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT C4P02EN1 ASSIGN TO
                  "C:/ARQUIVOS/E1-C4A02P04.TXT"
                  ORGANIZATION    IS LINE SEQUENTIAL
                  FILE STATUS     IS WS-FS-EN1
           .
           SELECT C4P02EN2 ASSIGN TO
                  "C:/ARQUIVOS/FUNC_CADASTRO.DAT"
                  ORGANIZATION    IS INDEXED
                  ACCESS MODE     IS DYNAMIC
                  RECORD KEY      IS KEY-EN2
                  FILE STATUS     IS WS-FS-EN2
           .
           SELECT C4P02RE1 ASSIGN TO
                  "C:/ARQUIVOS/R1-C4A02P04.txt"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-RE1
           .
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
       FD  C4P02EN1
           RECORDING MODE IS F.
       01  REG-C4P02EN1              PIC X(36).

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
           05 WS-A-EN2               PIC 9(11) VALUE ZEROS.
           05 WS-I-EN2               PIC 9(11) VALUE ZEROS.
           05 WS-E-EN2               PIC 9(11) VALUE ZEROS.
           05 WS-O-RE1               PIC 9(11) VALUE ZEROS.
           05 DAYSYS                 PIC 9(05) VALUE ZEROS.
           05 WEESYS                 PIC 9(01) VALUE ZEROS.
           05 WS-WEESYS              PIC X(13) VALUE SPACES.
           05 WS-CTLIN               PIC 9(02) VALUE ZEROS.
           05 WS-IMPCAB              PIC 9(02) VALUE ZEROS.
           05 WS-AMBIENTE            PIC X(10) VALUE SPACES.
           05 WS-A-REG		             PIC 9(11) VALUE ZEROS.
           05 WS-I-REG               PIC 9(11) VALUE ZEROS.
           05 WS-E-REG               PIC 9(11) VALUE ZEROS.
           05 WS-A-C4P04EN2          PIC 9(11) VALUE ZEROS.
           05 WS-I-C4P04EN2          PIC 9(11) VALUE ZEROS.
           05 WS-E-C4P04EN2          PIC 9(11) VALUE ZEROS.
           05 WS-CONTR               PIC 9(02) VALUE ZEROS.
           05 WS-RETURN              PIC X(02) VALUE SPACES.

      *==> AREA DESTINADA A FILE STATUS
           05 WS-FS-EN1              PIC 9(02) VALUE ZEROS.
           05 WS-FS-EN2              PIC 9(02) VALUE ZEROS.
           05 WS-FS-RE1              PIC 9(02) VALUE ZEROS.

      *==> AREA DESTINADA A MENSAGEM
           05 WS-MSG-A               PIC X(40) VALUE SPACES.
           05 WS-MSG-B               PIC 9(02) VALUE ZEROS.
           05 WS-MSG-C               PIC 9(02) VALUE ZEROS.

      *==> AREA DESTINADA A DATA DE PROCESSAMENTO
           05 DTSYS                  PIC 9(06) VALUE ZEROS.
           05 DTEDI                  PIC X(10) VALUE SPACES.
           05 DTEDI-I                PIC X(10) VALUE SPACES.
           05 HRSYS                  PIC 9(08) VALUE ZEROS.
           05 HREDI                  PIC X(11) VALUE SPACES.
           05 HREDI-I                PIC X(11) VALUE SPACES.

      *==> AREA DESTINADA A ENTRADA.
       01  WS-REG-C4P02EN1.
           05 TIPO-MANUT-EN1         PIC X.
           05 DEPTO-EN1              PIC 9(02).
           05 MATRICULA-EN1          PIC 9(03).
           05 NOME-EN1               PIC X(20).
           05 SAL-HRS-EN1            PIC 9(03)V99.
           05 HRS-MES-EN1            PIC 9(03)V99.

       01  WS-REG-C4P02EN2.
           05 DEPTO-EN2              PIC 9(02).
           05 MATRICULA-EN2          PIC 9(03).
           05 NOME-EN2               PIC X(20).
           05 SAL-HORA-EN2           PIC 9(03)V99.
           05 HRS-MES-EN2            PIC 9(03)V99.
           05 RESERVA-EN2            PIC X(05).

      *==> AREA DESTINADA A SAIDA.
       01  WS-DET1.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(07) VALUE 'ENTRADA'.
           05 FILLER                 PIC X(08).
           05 TIPO-DET1              PIC X(01).
           05 FILLER                 PIC X(10).
           05 CHAVE-DET1             PIC 9(03).
           05 FILLER                 PIC X(08).
           05 SALARIO-DET1           PIC 9(03)V99.
           05 HRSMES-DET1            PIC 9(03)V99.
           05 FILLER                 PIC X(08).
           05 TIPO-OCORR-DET1        PIC X(12).
           05 FILLER                 PIC X(06).

       01  WS-DET2.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(08) VALUE 'ANTERIOR'.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(10).
           05 CHAVE-DET2             PIC 9(03).
           05 FILLER                 PIC X(08).
           05 SALARIO-DET2           PIC 9(03)V99.
           05 HRSMES-DET2            PIC 9(03)V99.
           05 FILLER                 PIC X(08).
           05 FILLER                 PIC X(12).
           05 FILLER                 PIC X(06).

       01  WS-DET3.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(05) VALUE 'ATUAL'.
           05 FILLER                 PIC X(10).
           05 FILLER                 PIC X(01).
           05 FILLER                 PIC X(10).
           05 CHAVE-DET3             PIC 9(03).
           05 FILLER                 PIC X(08).
           05 SALARIO-DET3           PIC 9(03)V99.
           05 HRSMES-DET3            PIC 9(03)V99.
           05 FILLER                 PIC X(08).
           05 TIPO-OCORR-DET3        PIC X(12).
           05 FILLER                 PIC X(06).

       01  WS-DET4.
           05 FILLER                 PIC X(07).
           05 FILLER                 PIC X(07) VALUE 'ENTRADA'.
           05 FILLER                 PIC X(08).
           05 TIPO-DET4              PIC X(01).
           05 FILLER                 PIC X(10).
           05 CHAVE-DET4             PIC 9(03).
           05 FILLER                 PIC X(08).
           05 SALARIO-DET4           PIC 9(03)V99.
           05 HRSMES-DET4            PIC 9(03)V99.
           05 FILLER                 PIC X(08).
           05 TIPO-OCORR-DET4        PIC X(12).
           05 FILLER                 PIC X(06).

      *==> AREA DESTINADA A CABECALHOS.
       01  WS-CAB1.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'DATA:'.
           05 DATA-CAB1              PIC X(10).
           05 FILLER                 PIC X(12).
           05 FILLER                 PIC X(26) VALUE
              'OCORRENCIAS DE ATUALIZACAO'.
           05 FILLER                 PIC X(19).
           05 FILLER                 PIC X(04) VALUE 'PAG.'.
           05 PAG-CAB1               PIC 9(02).
           05 FILLER                 PIC X.

       01  WS-CAB2.
           05 FILLER                 PIC X.
           05 FILLER                 PIC X(05) VALUE 'HORA:'.
           05 HORA-CAB2              PIC X(11).
           05 FILLER                 PIC X(63).

       01  WS-CAB3.
           05 FILLER                 PIC X(21).
           05 FILLER                 PIC X(04) VALUE
              'TIPO'.
           05 FILLER                 PIC X(06).
           05 FILLER                 PIC X(05) VALUE
              'CHAVE'.
           05 FILLER                 PIC X(10).
           05 FILLER                 PIC X(08) VALUE
              'CONTEUDO'.
           05 FILLER                 PIC X(08).
           05 FILLER                 PIC X(10) VALUE
              'OCORRENCIA'.
           05 FILLER                 PIC X(08).

       01  WS-TERMINO                PIC X(40).
           88 TERMINO                VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       000-C4A02P04 SECTION.
       000-C4A02P04-INI.

      D    READY TRACE

           PERFORM 005-INICIO
           PERFORM 050-PROCESSO UNTIL WS-FS-EN1 = '10'
           PERFORM 100-TERMINO
           STOP RUN
           .
       000-C4A02P04-FIM.
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
           OPEN INPUT C4P02EN1
           IF WS-FS-EN1 NOT EQUAL '00'
              MOVE 'ERRO AO ABRIR O ARQUIVO C4P02EN1'    TO WS-MSG-A
              MOVE WS-FS-EN1                             TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           OPEN I-O C4P02EN2
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
           MOVE TIPO-MANUT-EN1 TO TIPO-DET1
           MOVE MATRICULA-EN1  TO CHAVE-DET1
           MOVE SAL-HRS-EN1    TO SALARIO-DET1
           MOVE HRS-MES-EN1    TO HRSMES-DET1
           IF TIPO-MANUT-EN1 = 'A'
              PERFORM 050A-LEITURA
           ELSE
              IF TIPO-MANUT-EN1 = 'I'
                 PERFORM 050B-LEITURA
              ELSE
                 IF TIPO-MANUT-EN1 = 'E'
                    PERFORM 050C-LEITURA
                 END-IF
              END-IF
           END-IF

           PERFORM 020-LEITURA
           .
       050-PROCESSO-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - LEITTURA DADOS PARA ATUALIZACAO
      *--------------------------------------------------------------*
       050A-LEITURA SECTION.
       050A-LEITURA-INI.
           MOVE MATRICULA-EN1 TO KEY-EN2
           READ C4P02EN2 INTO WS-REG-C4P02EN2
           IF WS-FS-EN2 = '23'
              MOVE TIPO-MANUT-EN1       TO TIPO-DET4
              MOVE MATRICULA-EN1        TO CHAVE-DET4
              MOVE SALARIO-DET1         TO SALARIO-DET4
              MOVE HRSMES-DET1          TO HRSMES-DET4
              MOVE 'INEXISTENTE'        TO TIPO-OCORR-DET4
              ADD 1                     TO WS-A-REG WS-CONTR
           ELSE
           IF WS-FS-EN2 NOT = '00' AND WS-FS-EN2 NOT = '23'
                MOVE 'ERRO NA LEITURA DO ARQUIVO C4P02EN2-A' TO WS-MSG-A
                MOVE WS-FS-EN2                               TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
           IF  WS-FS-EN2 = '00'
               PERFORM 050AA-ATUALIZA
           END-IF
           END-IF
           END-IF

           PERFORM 051-IMPREL
           .
       050A-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050AA-ATUALIZA SECTION.
       050AA-ATUALIZA-INI.
           MOVE MATRICULA-EN2        TO CHAVE-DET2
           MOVE SAL-HORA-EN2         TO SALARIO-DET2
           MOVE HRS-MES-EN2          TO HRSMES-DET2

           MOVE MATRICULA-EN1  TO MATRICULA-EN2
           MOVE SAL-HRS-EN1    TO SAL-HORA-EN2
           MOVE HRS-MES-EN1    TO HRS-MES-EN2
           REWRITE REG-C4P02EN2 FROM WS-REG-C4P02EN2
           IF WS-FS-EN2 NOT = '00'
              MOVE 'ERRO NA REGRAVACAO DO C4P02EN2'     TO WS-MSG-A
              MOVE WS-FS-EN2                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              MOVE MATRICULA-EN2        TO CHAVE-DET3
              MOVE SAL-HORA-EN2         TO SALARIO-DET3
              MOVE HRS-MES-EN2          TO HRSMES-DET3
              MOVE 'ALTERADO'           TO TIPO-OCORR-DET3
              ADD 1                     TO WS-A-C4P04EN2
           END-IF
           .
       050AA-ATUALIZA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - LEITTURA DADOS PARA INSERIR DADOS
      *--------------------------------------------------------------*
       050B-LEITURA SECTION.
       050B-LEITURA-INI.
           MOVE MATRICULA-EN1 TO KEY-EN2
           READ C4P02EN2 INTO WS-REG-C4P02EN2
           IF WS-FS-EN2 = '00'
              MOVE TIPO-MANUT-EN1       TO TIPO-DET4
              MOVE MATRICULA-EN1        TO CHAVE-DET4
              MOVE SALARIO-DET1         TO SALARIO-DET4
              MOVE HRSMES-DET1          TO HRSMES-DET4
              MOVE 'JA EXISTENTE'       TO TIPO-OCORR-DET4
              ADD 1                     TO WS-I-REG WS-CONTR
           ELSE
           IF WS-FS-EN2 NOT = '00' AND WS-FS-EN2 NOT = '23'
                MOVE 'ERRO NA LEITURA DO ARQUIVO C4P02EN2-B' TO WS-MSG-A
                MOVE WS-FS-EN2                               TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
           IF  WS-FS-EN2 = '23'
               PERFORM 050BB-INSERIR
           END-IF
           END-IF
           END-IF

           PERFORM 051-IMPREL
           .
       050B-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050BB-INSERIR SECTION.
       050BB-INSERIR-INI.
           MOVE MATRICULA-EN2        TO CHAVE-DET2
           MOVE SAL-HORA-EN2         TO SALARIO-DET2
           MOVE HRS-MES-EN2          TO HRSMES-DET2

           MOVE MATRICULA-EN1  TO MATRICULA-EN2
           MOVE SAL-HRS-EN1    TO SAL-HORA-EN2
           MOVE HRS-MES-EN1    TO HRS-MES-EN2
           WRITE REG-C4P02EN2 FROM WS-REG-C4P02EN2
           IF WS-FS-EN2 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02EN2'       TO WS-MSG-A
              MOVE WS-FS-EN2                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              MOVE MATRICULA-EN2        TO CHAVE-DET3
              MOVE SAL-HORA-EN2         TO SALARIO-DET3
              MOVE HRS-MES-EN2          TO HRSMES-DET3
              MOVE 'INCLUIDO'           TO TIPO-OCORR-DET3
              ADD 1                     TO WS-I-C4P04EN2
           END-IF
           .
       050BB-INSERIR-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - LEITURA DADOS PARA EXCLUIR REGISTROS
      *--------------------------------------------------------------*
       050C-LEITURA SECTION.
       050C-LEITURA-INI.
           MOVE MATRICULA-EN1 TO KEY-EN2
           READ C4P02EN2 INTO WS-REG-C4P02EN2
           IF WS-FS-EN2 = '23'
              MOVE TIPO-MANUT-EN1       TO TIPO-DET4
              MOVE MATRICULA-EN1        TO CHAVE-DET4
              MOVE SALARIO-DET1         TO SALARIO-DET4
              MOVE HRSMES-DET1          TO HRSMES-DET4
              MOVE 'INEXISTENTE'        TO TIPO-OCORR-DET4
              ADD 1                     TO WS-E-REG WS-CONTR
           ELSE
           IF WS-FS-EN2 NOT = '00' AND WS-FS-EN2 NOT = '23'
                MOVE 'ERRO NA LEITURA DO ARQUIVO C4P02EN2-C' TO WS-MSG-A
                MOVE WS-FS-EN2                               TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
           IF  WS-FS-EN2 = '00'
               PERFORM 050CC-EXCLUIR
           END-IF
           END-IF
           END-IF
           PERFORM 051-IMPREL
           .
       050C-LEITURA-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       050CC-EXCLUIR SECTION.
       050CC-EXCLUIR-INI.
           MOVE MATRICULA-EN2  TO CHAVE-DET2
           MOVE SAL-HORA-EN2   TO SALARIO-DET2
           MOVE HRS-MES-EN2    TO HRSMES-DET2

           MOVE MATRICULA-EN1  TO MATRICULA-EN2
           MOVE SAL-HRS-EN1    TO SAL-HORA-EN2
           MOVE HRS-MES-EN1    TO HRS-MES-EN2
           DELETE C4P02EN2
           IF WS-FS-EN2 NOT = '00'
              MOVE 'ERRO NO DELETE DO ARQUIVO C4P02EN2' TO WS-MSG-A
              MOVE WS-FS-EN2                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              MOVE MATRICULA-EN2        TO CHAVE-DET3
              MOVE SAL-HORA-EN2         TO SALARIO-DET3
              MOVE HRS-MES-EN2          TO HRSMES-DET3
              MOVE 'EXCLUIDO'           TO TIPO-OCORR-DET3
              ADD 1                     TO WS-E-C4P04EN2
           END-IF
           .
       050CC-EXCLUIR-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR LINHAS DO C4P05PR1
      *--------------------------------------------------------------*
       051-IMPREL SECTION.
       051-IMPREL-INI.
           IF WS-CTLIN > 35
              PERFORM 053-IMPCAB
           END-IF

           IF WS-CONTR = 1
           WRITE REG-C4P02RE1 FROM WS-DET4
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 DET4'  TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-RE1
              ADD 1                                     TO WS-CTLIN
           END-IF
           ELSE
           WRITE REG-C4P02RE1 FROM WS-DET1
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 DET1'  TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-RE1
              ADD 1                                     TO WS-CTLIN
           END-IF
           WRITE REG-C4P02RE1 FROM WS-DET2
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 DET2'  TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-RE1
              ADD 1                                     TO WS-CTLIN
           END-IF
           WRITE REG-C4P02RE1 FROM WS-DET3
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO C4P02RE1 DET3'  TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           ELSE
              ADD 1                                     TO WS-O-RE1
              ADD 1                                     TO WS-CTLIN
           END-IF
           END-IF

           MOVE ZEROS TO WS-CONTR
           .
       051-IMPREL-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - IMPRIMIR CABECALHO
      *--------------------------------------------------------------*
       053-IMPCAB SECTION.
       053-IMPCAB-INI.
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
              MOVE 'ERRO NA GRAVACAO DO SPACE1'   TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-CAB3
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO CAB3'     TO WS-MSG-A
              MOVE WS-FS-RE1                      TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           WRITE REG-C4P02RE1 FROM WS-SPACES
           IF WS-FS-RE1 NOT = '00'
              MOVE 'ERRO NA GRAVACAO DO WS-SPACES2'     TO WS-MSG-A
              MOVE WS-FS-RE1                            TO WS-MSG-B
           GO TO 999-ERRO
           END-IF

           MOVE 07                                      TO WS-CTLIN
           .
       053-IMPCAB-FIM.
           EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROCESSAMENTO DO PROGRAMA C4A02P02
      *--------------------------------------------------------------*
       100-TERMINO SECTION.
       100-TERMINO-INI.
           PERFORM 010-DATA-HORA-DAY
           PERFORM 080-FECHAR
           DISPLAY '============= PGM.C4A02P04 ============'
           DISPLAY ' '.
           DISPLAY 'REG.LIDOS C4P02EN1 = ' WS-I-EN1
           DISPLAY 'REG.LIDOS C4P02EN2 = ' WS-I-EN2
           DISPLAY 'REG.ATUALIZADO     = ' WS-A-C4P04EN2
           DISPLAY 'REG.A-INEXISTENTE  = ' WS-A-REG
           DISPLAY 'REG.INSERIDO       = ' WS-I-C4P04EN2
           DISPLAY 'REG.I-JA EXISTENTE = ' WS-I-REG
           DISPLAY 'REG.EXCLUIDO       = ' WS-E-C4P04EN2
           DISPLAY 'REG-E-INEXISTENTE  - ' WS-E-REG
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
           DISPLAY '     C4A02P04 SEM MOVIMENTO, FV. VERIFICAR.   '
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
           MOVE 08 TO RETURN-CODE
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
           DISPLAY '        ERRO NO PROGRAMA C4A02P04           '
           DISPLAY '============================================'
           DISPLAY 'FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY 'MENSAGEM = ' WS-MSG-A
           DISPLAY 'CODIGO   = ' WS-MSG-B
           DISPLAY '============================================'
           DISPLAY 'AMBIENTE: ' WS-AMBIENTE
           DISPLAY 'RETURN-CODE: ' WS-RETURN
           DISPLAY '============================================'
           MOVE 12 TO RETURN-CODE
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
           DISPLAY '        DISPLAY NO PROGRAMA C4A02P04           '
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
