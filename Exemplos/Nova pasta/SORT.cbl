      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14/03/23
      * Purpose: CLASSIFICACAO INTERNA USANDO O PROGRAMA COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT0001.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
             SELECT WORK       ASSIGN TO "C:\Arquivos\Temp.txt".
             SELECT FD-SORTIN  ASSIGN TO "C:\Arquivos\SORTIN.txt"
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-IN.
             SELECT FD-SORTOUT ASSIGN TO "C:\Arquivos\SORTOUT.txt"
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-OUT.
       DATA DIVISION.
       FILE SECTION.
       FD  FD-SORTIN.
           01 REG-SORTIN.
              05 ESTUDANTE-ID     PIC 9(5).
              05 ESTUDANTE-NAME   PIC A(25).

       FD  FD-SORTOUT.
           01 REG-SORTOUT.
              05 ESTUDANTE-ID-O   PIC 9(5).
              05 ESTUDANTE-NAME-O PIC A(25).

       SD  WORK.
           01 WORK-ESTUDANTE.
              05 ESTUDANTE-ID-W   PIC 9(5).
              05 ESTUDANTE-NAME-W PIC A(25).
       WORKING-STORAGE SECTION.
       01  WS-AREA.
      * --- >> FILE STATUS
           03 WS-FS-IN            PIC 9(02) VALUE ZEROS.
           03 WS-FS-OUT           PIC 9(02) VALUE ZEROS.
      * --- >> MENSAGENS
           03 WS-MSG1             PIC X(40) VALUE SPACES.
           03 WS-MSG2             PIC 9(02) VALUE ZEROS.
           03 WS-MSG3             PIC X(08) VALUE SPACES.
       PROCEDURE DIVISION.
       S100 SECTION.
       P101-INICIA.
            INITIALIZE WS-AREA
            PERFORM P201-ABERTURA
            PERFORM P301-PROCESSA
            PERFORM P901-TERMINO THRU P901-EXIT.
       P101-EXIT.
            EXIT.

       S200 SECTION.
       P201-ABERTURA.
            OPEN INPUT FD-SORTIN
            EVALUATE WS-FS-IN
               WHEN 00 CONTINUE
               WHEN OTHER
                    MOVE 'ERRO ABERTURA FD-SORTIN'  TO WS-MSG1
                    MOVE WS-FS-IN                   TO WS-MSG2
                    MOVE 'ANORMAL'                  TO WS-MSG3
            END-EVALUATE

            OPEN OUTPUT FD-SORTOUT
            EVALUATE WS-FS-OUT
               WHEN 00 CONTINUE
               WHEN OTHER
                    MOVE 'ERRO ABERTURA FD-SORTOUT' TO WS-MSG1
                    MOVE WS-FS-OUT                  TO WS-MSG2
                    MOVE 'ANORMAL'                  TO WS-MSG3
            END-EVALUATE.
       P201-EXIT.
            EXIT.

       S300 SECTION.
       P301-PROCESSA.
            SORT WORK ON ASCENDING  KEY ESTUDANTE-ID
      *               ON DESCENDING KEY ESTUDANTE-ID
            USING FD-SORTIN GIVING FD-SORTOUT.
       P301-EXIT.
            EXIT.

       S800 SECTION.
       P801-ERRO.
            DISPLAY '--------------------------------------'
            DISPLAY '      ERRO PROGRAMA - SORT0001'
            DISPLAY '--------------------------------------'
            DISPLAY 'MENSAGEM...: ' WS-MSG1
            DISPLAY 'FILE STATUS: ' WS-MSG2
            DISPLAY 'STATUS.....: ' WS-MSG3
            DISPLAY '--------------------------------------'
            MOVE 8 TO RETURN-CODE
            STOP RUN.
       P801-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM SORT0001.
