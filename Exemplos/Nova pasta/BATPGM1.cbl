       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATPGM1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE  ASSIGN TO 'C:\Arquivos\INFILE.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-IN.
           SELECT OUT-FILE ASSIGN TO 'C:\Arquivos\OUTFILE.txt'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FS-OU.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  IN-FILE
           RECORDING MODE IS F.
       01  IN-REG          PIC 9(5).

       FD  OUT-FILE
           RECORDING MODE IS F.
       01  OU-REG          PIC X(40).
      *
       WORKING-STORAGE SECTION.
      * >>> VARIAVEIS
       01  WS-VAR.
           03 WS-NOME      PIC X(30) VALUE SPACES.
           03 WS-LINHA     PIC X(60) VALUE SPACES.
           03 WS-FS-IN     PIC 9(02) VALUE ZEROS.
           03 WS-FS-OU     PIC 9(02) VALUE ZEROS.
           03 WS-MSG1      PIC X(30) VALUE SPACES.
           03 WS-MSG2      PIC 9(02) VALUE ZEROS.
           03 WS-R-IN      PIC 9(05) VALUE ZEROS.
           03 WS-W-OU      PIC 9(05) VALUE ZEROS.
      * >>> ARQUIVO ENTRADA
       01  WS-IN-REG.
           05 WS-IN-ID     PIC 9(05).
      * >>> ARQUIVO SAIDA
        01 WS-OU-REG.
           05 WS-OU-ID     PIC 9(05).
           05 WS-OU-NOME   PIC X(35).
      *
       PROCEDURE DIVISION.
       S001-INICIO SECTION.
       P001-INICIO.
            INITIALIZE WS-VAR
            PERFORM P010-ABERTURA
            PERFORM P020-LEITURA UNTIL WS-FS-IN = 10
            PERFORM P070-FECHAMENTO
            PERFORM P090-TERMINO
            .
       P001-EXIT.
            EXIT.

       S010-ABERTURA SECTION.
       P010-ABERTURA.
            OPEN INPUT IN-FILE
            IF WS-FS-IN NOT = 0
               MOVE 'ERRO P010-ABERTURA IN-FILE: ' TO WS-MSG1
               MOVE WS-FS-IN                       TO WS-MSG2
               PERFORM P080-ERRO
            END-IF.

            OPEN OUTPUT OUT-FILE
            IF WS-FS-OU NOT = 0
               MOVE 'ERRO P010-ABERTURA OU-FILE: ' TO WS-MSG1
               MOVE WS-FS-OU                       TO WS-MSG2
               PERFORM P080-ERRO
            END-IF.
       P010-EXIT.
            EXIT.

       S020-LEITURA SECTION.
       P020-LEITURA.
            READ IN-FILE INTO WS-IN-REG
            IF WS-FS-IN = '10' AND WS-R-IN = ZEROS
               MOVE 'IN-FILE SEM MOVIMENTO' TO WS-MSG1
               MOVE WS-FS-IN                TO WS-MSG2
               PERFORM P085-PREVISTO
            ELSE
               IF WS-FS-IN NOT = '10' AND ZEROS
                  MOVE 'ERRO P020-LEITURA IN-FILE: ' TO WS-MSG1
                  MOVE WS-FS-IN                      TO WS-MSG2
                  PERFORM P080-ERRO
               ELSE
                  IF WS-FS-IN EQUAL ZEROS
                     ADD 1 TO WS-R-IN
                     PERFORM P030-PROCESSA
                  END-IF
               END-IF
            END-IF.
       P020-EXIT.
            EXIT.

       S030-PROCESSA SECTION.
       P030-PROCESSA.
            IF WS-IN-ID = 10001
               MOVE 'CARLOS MEIRELES' TO WS-OU-NOME
            ELSE
               IF WS-IN-ID = 10002
                  MOVE 'ANA LIMA' TO WS-OU-NOME
               ELSE
                  MOVE 'CLIENTE DESCONHECIDO' TO WS-OU-NOME
               END-IF
            END-IF.

            STRING 'ID = ' WS-IN-ID ', NOME=' WS-OU-NOME
                DELIMITED BY SIZE INTO WS-LINHA

            DISPLAY 'DEBUG >> ' WS-LINHA

            WRITE OU-REG FROM WS-OU-REG
            IF WS-FS-OU NOT = 0
               MOVE 'ERRO P030-PROCESSA OU-FILE: ' TO WS-MSG1
               MOVE WS-FS-OU                       TO WS-MSG2
               PERFORM P080-ERRO
            ELSE
               ADD 1 TO WS-W-OU
            END-IF.
       P030-EXIT.
            EXIT.

       S070-FECHAMENTO SECTION.
       P070-FECHAMENTO.
            CLOSE IN-FILE
            IF WS-FS-IN NOT = 0
               MOVE 'ERRO P070-FECHAMENTO IN-FILE: ' TO WS-MSG1
               MOVE WS-FS-IN                         TO WS-MSG2
               PERFORM P080-ERRO
            END-IF.

            CLOSE OUT-FILE
            IF WS-FS-OU NOT = 0
               MOVE 'ERRO P070-FECHAMENTO OU-FILE: ' TO WS-MSG1
               MOVE WS-FS-IN                         TO WS-MSG2
               PERFORM P080-ERRO
            END-IF.
       P070-EXIT.
            EXIT.

       S080-ERRO SECTION.
       P080-ERRO.
            DISPLAY '----------------------------------------'
            DISPLAY '         ERRO PROGRAMA BATPGM1'
            DISPLAY '----------------------------------------'
            DISPLAY ' MENSAGEM...: ' WS-MSG1
            DISPLAY ' FILE STATUS: ' WS-MSG2
            DISPLAY '----------------------------------------'
            MOVE 8 TO RETURN-CODE
            STOP RUN.
       P080-EXIT.
            EXIT.

       S085-PREVISTO SECTION.
       P085-PREVISTO.
            DISPLAY '----------------------------------------'
            DISPLAY '   PROCESSAMENTO NORMAL -  BATPGM1'
            DISPLAY '----------------------------------------'
            DISPLAY ' MENSAGEM...: ' WS-MSG1
            DISPLAY ' FILE STATUS: ' WS-MSG2
            DISPLAY '----------------------------------------'
            MOVE 1 TO RETURN-CODE
            STOP RUN.
       P085-EXIT.
            EXIT.

       S090-TERMINO SECTION.
       P090-TERMINO.
            DISPLAY '----------------------------------------'
            DISPLAY '   PROCESSAMENTO NORMAL -  BATPGM1'
            DISPLAY '----------------------------------------'
            DISPLAY ' IN-FILE QTDE: ' WS-R-IN
            DISPLAY ' OU-FILE QTDE: ' WS-W-OU
            DISPLAY '----------------------------------------'
            STOP RUN.
       P090-EXIT.
            EXIT.
       END PROGRAM BATPGM1.
