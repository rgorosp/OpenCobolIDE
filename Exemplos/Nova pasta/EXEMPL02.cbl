       IDENTIFICATION DIVISION.
       PROGRAM-ID. EXEMPL02.
      * TESTE DE PROGRAMA
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO 'C:\Arquivos\INFILE.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-IN.
           SELECT OUFILE ASSIGN TO 'C:\Arquivos\OUFILE.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS WS-FS-OU.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS F.
       01  IN-FILE-REG       PIC X(05).

       FD  OUFILE
           RECORDING MODE IS F.
       01  OU-FILE-REG       PIC X(20).
      *
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-R-IN      PIC 9(02) VALUE ZEROS.
           03 WS-RW-OU     PIC 9(03) VALUE ZEROS.
           03 WS-NOME      PIC X(20) VALUE SPACES.
           03 WS-FS-IN     PIC 9(02) VALUE ZEROS.
           03 WS-FS-OU     PIC 9(02) VALUE ZEROS.
           03 WS-EOF       PIC X     VALUE SPACES.

       01  WS-IN-FILE-REG.
           05 IN-ID        PIC 9(05).

       01  WS-OU-FILE-REG.
           05 OU-MSG       PIC X(20).
      *
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
           DISPLAY 'P001-INICIO'
           INITIALIZE WS-VAR
           MOVE 'N' TO WS-EOF

           PERFORM P005-OPEN
           PERFORM P011-READ UNTIL WS-EOF = 'S'
           PERFORM P901-TERMINO.
       P001-EXIT.
           EXIT.
      *
       S005-OPEN SECTION.
       P005-OPEN.
           DISPLAY 'P005-OPEN'
           OPEN INPUT INFILE
             IF WS-FS-IN NOT = 0
                DISPLAY 'OPEN IN-FILE ERROR = ' WS-FS-IN
             END-IF
           OPEN OUTPUT OUFILE
             IF WS-FS-IN NOT = 0
                DISPLAY 'OPEN OU-FILE ERROR = ' WS-FS-IN
             END-IF
           .
       P005-EXIT.
           EXIT.
      *
       S010-READ SECTION.
       P011-READ.
           DISPLAY 'P011-READ'
           ADD 1 TO WS-R-IN
           READ INFILE INTO WS-IN-FILE-REG
                AT END
                   MOVE 'S' TO WS-EOF
               NOT AT END
                   PERFORM P051-WRITE
           END-READ
           .
       P011-EXIT.
            EXIT.
      *
       S050-WRITE SECTION.
       P051-WRITE.
           DISPLAY 'P051-WRITE'
           IF IN-ID = 10001
              MOVE 'CARLOS MEIRELLES' TO WS-NOME
           ELSE
              MOVE 'CLIENTE DESCONHECIDO' TO WS-NOME
           END-IF

           MOVE WS-NOME TO OU-MSG
           ADD 1 TO WS-RW-OU
           WRITE OU-FILE-REG FROM WS-OU-FILE-REG
           IF WS-FS-OU NOT = 0
              DISPLAY 'WRITE OU-FILE ERROR = ' WS-FS-OU
           END-IF
           .
       P051-EXIT.
           EXIT.
      *
       S900-TERMINO SECTION.
       P901-TERMINO.
           DISPLAY 'P901-TERMINO'
           CLOSE INFILE
           IF WS-FS-IN NOT = 0
              DISPLAY 'OPEN IN-FILE ERROR = ' WS-FS-IN
           END-IF

           CLOSE OUFILE
           IF WS-FS-IN NOT = 0
              DISPLAY 'OPEN OU-FILE ERROR = ' WS-FS-IN
           END-IF

           DISPLAY '>>> TERMINO DO PROGRAMA <<<'
           DISPLAY ' INFILE: ' WS-R-IN
           DISPLAY ' OUFILE: ' WS-RW-OU
           DISPLAY '---------------------------'

           STOP RUN.
       P901-EXIT.
           EXIT.
      *

