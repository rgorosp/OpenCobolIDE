      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 06-06-24
      * Purpose: EXEMPLO MODULO 2 EXERCICIO 4
      * Tectonics: COBOL E MAINFRAME IVEE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL00000.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ENTRADA ASSIGN TO 'C:\Arquivos\CBL0000E.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-ENT.
      *
           SELECT SAIDA   ASSIGN TO 'C:\Arquivos\CBL0000S.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-SAI.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ENTRADA
           RECORDING MODE IS F.
       01  REG-ENTRADA         PIC X(028).
       FD  SAIDA
           RECORDING MODE IS F.
       01  REG-SAIDA           PIC X(012).
      *
       WORKING-STORAGE SECTION.
       01  WS-VAR.
      *--->>> AREA DE CONTADORES E ACUMULADORES
           05 WS-R-ENT             PIC 9(03)  VALUE ZEROS.
           05 WS-W-SAI             PIC 9(03)  VALUE ZEROS.
           05 WS-EOF               PIC X      VALUE SPACE.
           05 CALCULO              PIC 9(05)  VALUE ZEROS.
           05 CALCULO-EDIT         PIC 999.99 VALUE ZEROS.
           05 PORCENTAGEM          PIC 999V99 VALUE ZEROS.
      *--->>> AREA DE FILE STATUS
           05 WS-FS-ENT            PIC 9(02).
           05 WS-FS-SAI            PIC 9(02).
      *--->>> AREA DE DATA E HORA EDITADA
           05 DTSYS.
              10 DT-ANO-SYS        PIC 9(02).
              10 DT-MES-SYS        PIC 9(02).
              10 DT-DIA-SYS        PIC 9(02).
           05 DTEDI                PIC X(10).
           05 DTEDI-I              PIC X(10).
           05 HRSYS.
              10 HR-HRM-SYS        PIC 9(04).
              10 HR-SEG-SYS        PIC 9(02).
              10 HR-MIL-SYS        PIC 9(02).
           05 HREDI                PIC X(11).
           05 HREDI-I              PIC X(11).
      *--->>> AREA DE MENSAGEM
           05 WS-MSG-A             PIC X(35).
           05 WS-MSG-B             PIC 9(02).
      *--->>> ARQUIVO ENTRADA
       01  WS-REG-ENTRADA.
           05 CODIGODOPRODUTO      PIC X(05).
           05 QUANTIDADENOTESTOQUE PIC 9(05).
           05 QUANTIDADEMININA     PIC 9(05).
           05 QUANTIDADEMAXIMA     PIC 9(05).
           05 CUSTOUNITARIO        PIC 9(05).
           05 MARGEMDEVENDA        PIC 9(03).
      *--->>> ARQUIVO SAIDA
       01  WS-REG-SAIDA.
           05 CODIGODOPRODUTO-A    PIC X(05).
           05 FILLER               PIC X.
           05 QUANTIDADEACOMPRAR   PIC 999.99.
      *
       PROCEDURE DIVISION.
      *--->>> INICIO DO PROCESSAMENTO
       S000-INICIO SECTION.
       P001-INI.
            PERFORM P011-OPE
            PERFORM P021-LEI
            PERFORM P031-PRO UNTIL WS-FS-ENT = '10'
            PERFORM P999-TER.
       P001-EXIT.
            EXIT.

      *--->>> ABERTURA DOS ARQUIVOS
       S010-OPEN SECTION.
       P011-OPE.
           INITIALIZE WS-VAR
           PERFORM P998-DTHR
           MOVE DTEDI TO DTEDI-I
           MOVE HREDI TO HREDI-I

           OPEN INPUT ENTRADA
           IF WS-FS-ENT NOT EQUAL '00' AND '10'
              MOVE 'ERRO NO OPEN DO ARQUIVO ENTRADA'      TO WS-MSG-A
              MOVE WS-FS-ENT                              TO WS-MSG-B
           PERFORM P997-ERR
           END-IF

           OPEN OUTPUT SAIDA
           IF WS-FS-SAI NOT EQUAL '00' AND '10'
              MOVE 'ERRO NO OPEN DO ARQUIVO SAIDA'        TO WS-MSG-A
              MOVE WS-FS-SAI                              TO WS-MSG-B
           PERFORM P997-ERR
           END-IF.
       P011-EXIT.
            EXIT.

      *--->>> LEITURA DOS ARQUIVOS
       S020-LEITURA SECTION.
       P021-LEI.
           READ ENTRADA INTO WS-REG-ENTRADA
           IF CODIGODOPRODUTO = '99999'
              MOVE 10 TO WS-FS-ENT
           END-IF

           IF WS-FS-ENT NOT = '00' AND '10'
              MOVE 'ERRO NO READ DO ARQUIVO ENTRADA'      TO WS-MSG-A
              MOVE WS-FS-ENT                              TO WS-MSG-B
           PERFORM P997-ERR
           ELSE
              IF WS-FS-ENT = '00'
                 ADD 1 TO WS-R-ENT
              END-IF
           END-IF.
       P021-EXIT.
            EXIT.

      *--->>> PROCESSAMENTO
       S030-PROCESSO SECTION.
       P031-PRO.
           COMPUTE PORCENTAGEM = (MARGEMDEVENDA / QUANTIDADEMAXIMA)
                   ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO-1"
               NOT ON SIZE ERROR DISPLAY "CALCULO NORMAL CAMPO-1"
           END-COMPUTE

           COMPUTE CALCULO = (CUSTOUNITARIO * PORCENTAGEM) +
                   CUSTOUNITARIO
                   ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO-2"
               NOT ON SIZE ERROR DISPLAY "CALCULO NORMAL CAMPO-2"
           END-COMPUTE

           MOVE CALCULO TO CALCULO-EDIT
           MOVE CODIGODOPRODUTO TO CODIGODOPRODUTO-A
           MOVE CALCULO-EDIT TO QUANTIDADEACOMPRAR

           DISPLAY WS-REG-SAIDA
           PERFORM P041-GRAV
           PERFORM P021-LEI.
       P031-EXIT.
            EXIT.

      *--->>> GRAVACAO DOS ARQUIVOS
       S040-GRAVACAO SECTION.
       P041-GRAV.
           WRITE REG-SAIDA FROM WS-REG-SAIDA
                 AFTER ADVANCING 0 LINES
           IF WS-FS-SAI NOT EQUAL '00'
              MOVE 'ERRO AO GRAVAR O ARQUIVO SAIDA'       TO WS-MSG-A
              MOVE WS-FS-SAI                              TO WS-MSG-B
           PERFORM P997-ERR
           ELSE
              ADD 1 TO WS-W-SAI
           END-IF.
       P041-EXIT.
            EXIT.

      *--->>> FECHAMENTO DOS ARQUIVOS
       S996-FECHAMENTO SECTION.
       P996-FEC.
           CLOSE ENTRADA
           IF WS-FS-ENT NOT EQUAL '00'
              MOVE 'ERRO AO FECHAR O ARQUIVO ENTRADA'  TO WS-MSG-A
              MOVE WS-FS-ENT                           TO WS-MSG-B
           DISPLAY '*--------------------------------------------*'
           DISPLAY '*      MENSAGEM DE ERRO NO PROCESSAMENTO     *'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY '* MENSAGEMS = ' WS-MSG-A
           DISPLAY '* CODIGO    = ' WS-MSG-B
           DISPLAY '*--------------------------------------------*'
           MOVE 12 TO RETURN-CODE
           PERFORM P999-FIN THRU P999-EXIT1
           END-IF

           CLOSE SAIDA
           IF WS-FS-SAI NOT EQUAL '00'
              MOVE 'ERRO AO FECHAR O ARQUIVO SAIDA'  TO WS-MSG-A
              MOVE WS-FS-SAI                         TO WS-MSG-B
           DISPLAY '*--------------------------------------------*'
           DISPLAY '*      MENSAGEM DE ERRO NO PROCESSAMENTO     *'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY '* MENSAGEMS = ' WS-MSG-A
           DISPLAY '* CODIGO    = ' WS-MSG-B
           DISPLAY '*--------------------------------------------*'
           MOVE 12 TO RETURN-CODE
           PERFORM P999-FIN THRU P999-EXIT1
           END-IF
           .
       P996-EXIT.
            EXIT.

      *--->>> MENSAGEM DE ERRO NA EXECUCAO DO PROGRAMA
       S997-ERRO SECTION.
       P997-ERR.
           DISPLAY '*--------------------------------------------*'
           DISPLAY '*      MENSAGEM DE ERRO NO PROCESSAMENTO     *'
           DISPLAY '*              PROGRAMA CBL00000             *'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* FAVOR VERIFICAR ERRO NO DISPLAY DO PROGRAMA.'
           DISPLAY '* MENSAGEMS = ' WS-MSG-A
           DISPLAY '* CODIGO    = ' WS-MSG-B
           DISPLAY '*--------------------------------------------*'
           MOVE 12 TO RETURN-CODE
           PERFORM P996-FEC.
       P997-EXIT.
            EXIT.

      *--->>> DATA E HORA DO SISTEMA EDITADA
       S998-DATAHORA SECTION.
       P998-DTHR.
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
           DELIMITED BY SIZE INTO HREDI.
       P998-EXIT.
            EXIT.

      *--->>> TERMINO DO PROCESSAMENTO
       S999-TERMINO SECTION.
       P999-TER.
           PERFORM P996-FEC
           PERFORM P998-DTHR
           DISPLAY '*--------------------------------------------*'
           DISPLAY '*              PROGRAMA CBL00000'
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* READ  ARQ. ENTRADA  = ' WS-R-ENT
           DISPLAY '* WRITE ARQ. SAIDA    = ' WS-W-SAI
           DISPLAY '*--------------------------------------------*'
           DISPLAY '* INICIO  DO PROCESSAMENTO = ' DTEDI-I '-' HREDI-I
           DISPLAY '* TERMINO DO PROCESSAMENTO = ' DTEDI   '-' HREDI
           DISPLAY '*--------------------------------------------*'.
           PERFORM P999-FIN THRU P999-EXIT1.
       P999-EXIT.
            EXIT.

       S999-FINALIZA SECTION.
       P999-FIN.
            STOP RUN.
       P999-EXIT1.
            EXIT.
