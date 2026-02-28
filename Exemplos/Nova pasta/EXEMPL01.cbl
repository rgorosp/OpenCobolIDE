      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 04-07-24
      * Purpose: MOSTRAR LEITURA DE ARQUIVO NO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGMARQ02.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT ASSIGN TO 'C:\Arquivos\STUDENT.txt'
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS WS-FS-STU.
      * >>> ARQUIVOS
       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT.
           01 FD-STUDENT.
              03 COD-STUDENT         PIC 9(05).
              03 NOM-STUDENT         PIC X(25).
      * >>> VARIAVEIS
       WORKING-STORAGE SECTION.
       01  WS-DADOS                  PIC X(30) VALUE SPACES.
       01  FILLER REDEFINES WS-DADOS.
           03 WS-COD-STUDENT         PIC 9(05).
           03 WS-NOM-STUDENT         PIC X(25).
      * >>> FILE STATUS
       77  WS-FS-STU                 PIC 9(02) VALUE ZEROS.
      * >>> MENSAGENS
       77  WS-MSG-A                  PIC X(35) VALUE SPACES.
       77  WS-MSG-B                  PIC 9(02) VALUE ZEROS.
      * >>> DATA SISTEMA
       77  WS-DATA                   PIC 9(08) VALUE ZEROS.
      * >>> DATA SISTEMA EDITADA
       01  WS-DATA-E.
           03 WS-DD                  PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE '/'.
           03 WS-MM                  PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE '/'.
           03 WS-AA                  PIC 9(04) VALUE ZEROS.
      * >>> HORA SISTEMA
       77  WS-HORA                   PIC 9(08) VALUE ZEROS.
      * >>> HORA SISTEMA EDITADA
       01  WS-HORA-I.
           03 WS-HR-I                PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE ':'.
           03 WS-MI-I                PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE ':'.
           03 WS-SE-I                PIC 9(02) VALUE ZEROS.
       01  WS-HORA-F.
           03 WS-HR-F                PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE ':'.
           03 WS-MI-F                PIC 9(02) VALUE ZEROS.
           03 FILLER                 PIC X     VALUE ':'.
           03 WS-SE-F                PIC 9(02) VALUE ZEROS.
      * >>> CONTADORES E ACUMULADORES
       77  WS-LER-STU                PIC 99    VALUE ZEROS.
       77  WS-EOF                    PIC A     VALUE SPACE.
      * >>> PROCEDURE
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            ACCEPT  WS-DATA FROM DATE YYYYMMDD
            MOVE    WS-DATA(7:2) TO WS-DD
            MOVE    WS-DATA(5:2) TO WS-MM
            MOVE    WS-DATA(1:4) TO WS-AA

            ACCEPT  WS-HORA FROM TIME
            MOVE    WS-HORA(1:2) TO WS-HR-I
            MOVE    WS-HORA(3:2) TO WS-MI-I
            MOVE    WS-HORA(5:2) TO WS-SE-I

            MOVE    'N' TO WS-EOF

            PERFORM P010-ABERTURA
            PERFORM P020-LEITURA UNTIL WS-EOF = 'S'
            PERFORM P800-FECHAR
            PERFORM P950-TERMINO.
       P000-EXIT.
            EXIT.

       S010 SECTION.
       P010-ABERTURA.
            OPEN INPUT STUDENT
            IF WS-FS-STU NOT EQUAL 00
              MOVE 'ERRO NO INPUT DO ARQUIVO STUDENT' TO WS-MSG-A
              MOVE WS-FS-STU                          TO WS-MSG-B
            PERFORM P900-DISPLAY
            END-IF.
       P010-EXIT.
            EXIT.

       S020 SECTION.
       P020-LEITURA.
            READ STUDENT INTO WS-DADOS
                 AT END
                    MOVE 'S' TO WS-EOF
             NOT AT END
                    ADD 1 TO WS-LER-STU
                    DISPLAY WS-COD-STUDENT ' - ' WS-NOM-STUDENT
             END-READ.
       P020-EXIT.
            EXIT.

       S800 SECTION.
       P800-FECHAR.
            CLOSE STUDENT
            IF WS-FS-STU NOT EQUAL 00
              MOVE 'ERRO AO FECHAR O ARQUIVO STUDENT' TO WS-MSG-A
              MOVE WS-FS-STU                          TO WS-MSG-B
            PERFORM P900-DISPLAY
            END-IF.
       P800-EXIT.
            EXIT.

       S900 SECTION.
       P900-DISPLAY.
           DISPLAY '=============================================='
           DISPLAY '     MENSAGEM DO PROCESSAMENTO DO PROGRAMA    '
           DISPLAY '=============================================='
           DISPLAY 'MENSAGEM  = ' WS-MSG-A
           DISPLAY 'CODIGO    = ' WS-MSG-B
           DISPLAY '=============================================='
           DISPLAY 'INICIO  PROCESSAMENTO = ' WS-DATA-E '-' WS-HORA-I
           DISPLAY 'TERMINO PROCESSAMENTO = ' WS-DATA-E '-' WS-HORA-F
           DISPLAY '=============================================='
           MOVE 04 TO RETURN-CODE
           PERFORM P999-FIM.
       P900-EXIT.
           EXIT.

       S950 SECTION.
       P950-TERMINO.
           ACCEPT  WS-HORA FROM TIME
           MOVE    WS-HORA(1:2) TO WS-HR-F
           MOVE    WS-HORA(3:2) TO WS-MI-F
           MOVE    WS-HORA(5:2) TO WS-SE-F
           DISPLAY '=============================================='
           DISPLAY '              TERMINO DO PROGRAMA'
           DISPLAY '=============================================='
           DISPLAY 'REGISTROS STUDENT LIDOS  = ' WS-LER-STU
           DISPLAY '=============================================='
           DISPLAY 'INICIO  PROCESSAMENTO = ' WS-DATA-E '-' WS-HORA-I
           DISPLAY 'TERMINO PROCESSAMENTO = ' WS-DATA-E '-' WS-HORA-F
           DISPLAY '=============================================='
           MOVE 00 TO RETURN-CODE
           PERFORM P999-FIM.
       P950-EXIT.
           EXIT.

       S999 SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM PGMARQ02.
