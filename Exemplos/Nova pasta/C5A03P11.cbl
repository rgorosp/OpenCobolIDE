       IDENTIFICATION DIVISION.
000000 PROGRAM-ID. C5A03P11.
000000* >>> CLASSIFICAR O ARQUIVO DE ENTRADA PELOS CAMPOS NOME-PROD
      * >>> e COD-MES, GERANDO ARQUIVO DE SAIDA
000000 ENVIRONMENT DIVISION.
000000 INPUT-OUTPUT SECTION.
000000 FILE-CONTROL.
000000     SELECT SD-WORK1
                ASSIGN TO  "C:\Arquivos\Temp1.txt".
000000*         ASSIGN       TO  SORTWK01. * MAINFRAME *
000000     SELECT SD-WORK2
                ASSIGN TO  "C:\Arquivos\Temp2.txt".
000000*         ASSIGN       TO  SORTWK01. * MAINFRAME *
000000     SELECT FD-E1C5A3P1
000000         ASSIGN  TO  "C:/ARQUIVOS/E1C5A3P1.txt"
000000         ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-ENT.
000000     SELECT FD-S1C5A3P1
000000         ASSIGN  TO  "C:/ARQUIVOS/S1C5A3P1.txt"
000000         ORGANIZATION IS  LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-SAI.
000000     SELECT FD-S1C5A3P2
000000         ASSIGN  TO  "C:/ARQUIVOS/S1C5A3P2.txt"
000000         ORGANIZATION IS  LINE SEQUENTIAL
               FILE STATUS  IS WS-FS-SAI2.
000000*
000000 DATA DIVISION.
000000 FILE SECTION.
000000 SD  SD-WORK1.
000000 01  REG-WORK1.
           03 COD-PROD-CH     PIC 999.
           03 CHAVE-01.
              05 NOME-PROD-CH PIC X(13).
              05 COD-MES-CH   PIC 99.
           03 FILLER          PIC 9(07).

       SD  SD-WORK2.
       01  REG-WORK2.
           03 CHAVE-02.
              05 COD-PROD-CH  PIC 999.
           03 NOME-PROD-CH    PIC X(13).
           03 COD-MES-CH      PIC 99.
           03 FILLER          PIC 9(07).
      *
000000 FD  FD-E1C5A3P1.
000000 01  REG-E1C5A3P1.
000000     03 COD-PROD        PIC 999.
           03 NOME-PROD       PIC X(13).
           03 COD-MES         PIC 99.
           03 VALOR-MES       PIC 9(5)V99.
      *
000000 FD  FD-S1C5A3P1.
000000 01  REG-S1C5A3P1.
000000     03 COD-PROD-S      PIC 999.
           03 NOME-PROD-S     PIC X(13).
           03 COD-MES-S       PIC 99.
           03 VALOR-MES-S     PIC 9(5)V99.
000000*
000000 WORKING-STORAGE SECTION.
       01  WS-AREA.
           03 WS-FS-ENT       PIC 9(02) VALUE ZEROS.
           03 WS-FS-SAI       PIC 9(02) VALUE ZEROS.
           03 WS-FS-SAI2      PIC 9(02) VALUE ZEROS.
      *
000000 PROCEDURE DIVISION.
000000 S000-INICIO SECTION.
            PERFORM P101-ABRIR
            PERFORM P051-SORT
            PERFORM P201-FECHAR
            PERFORM P999-TERMINO.
       P001-EXIT.
            EXIT.

       S050-SORT SECTION.
       P051-SORT.
000000     SORT SD-WORK1
000000          ON ASCENDING KEY CHAVE-01
000000          USING  FD-E1C5A3P1
000000          GIVING FD-S1C5A3P1
000000     DISPLAY "FIM DA EXECUCAO WORK1" UPON CONSOLE

000000     SORT SD-WORK2
000000          ON ASCENDING KEY CHAVE-02
000000          USING  FD-S1C5A3P1
000000          GIVING FD-S1C5A3P2
000000     DISPLAY "FIM DA EXECUCAO WORK2" UPON CONSOLE.
       P051-EXIT.
            EXIT.

       S100-ABRIR SECTION.
       P101-ABRIR.
            OPEN INPUT FD-E1C5A3P1
            EVALUATE WS-FS-ENT
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P101-ABRIR FD-E1C5A3P1'
                  DISPLAY 'FILE STATUS: ' WS-FS-ENT
                  STOP RUN
            END-EVALUATE

             OPEN OUTPUT FD-S1C5A3P1
             EVALUATE WS-FS-SAI
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P101-ABRIR FD-S1C5A3P1'
                  DISPLAY 'FILE STATUS: ' WS-FS-SAI
                  STOP RUN
            END-EVALUATE.

             OPEN OUTPUT FD-S1C5A3P2
             EVALUATE WS-FS-SAI2
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P101-ABRIR FD-S1C5A3P2'
                  DISPLAY 'FILE STATUS: ' WS-FS-SAI2
                  STOP RUN
            END-EVALUATE.
       P101-EXIT.
            EXIT.

       S200-FECHAR SECTION.
       P201-FECHAR.
            CLOSE FD-E1C5A3P1
            EVALUATE WS-FS-ENT
                WHEN 0
                  CONTINUE
                WHEN 42
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P201-FECHAR FD-E1C5A3P1'
                  DISPLAY 'FILE STATUS: ' WS-FS-ENT
                  STOP RUN
            END-EVALUATE

             CLOSE FD-S1C5A3P1
             EVALUATE WS-FS-SAI
                WHEN 0
                  CONTINUE
                WHEN 42
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P201-FECHAR FD-S1C5A3P1'
                  DISPLAY 'FILE STATUS: ' WS-FS-SAI
                  STOP RUN
            END-EVALUATE.

             CLOSE FD-S1C5A3P2
             EVALUATE WS-FS-SAI2
                WHEN 0
                  CONTINUE
                WHEN 42
                  CONTINUE
                WHEN OTHER
                  DISPLAY 'ERRO P201-FECHAR FD-S1C5A3P2'
                  DISPLAY 'FILE STATUS: ' WS-FS-SAI2
                  STOP RUN
            END-EVALUATE.
       P201-EXIT.
            EXIT.

       S900-SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
