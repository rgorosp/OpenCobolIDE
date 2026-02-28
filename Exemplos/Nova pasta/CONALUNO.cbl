      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 02-07-2025
      * Purpose: CONSULTA ALUNOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONALUNO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CFP001S1 ASSIGN TO "C:/ARQUIVOS/ALUNOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-ALUNO
                  FILE STATUS  IS WS-FS-ALU
                  RESERVE      10 AREAS.
       DATA DIVISION.
       FILE SECTION.
       FD  CFP001S1.
           COPY REGALUNO.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-ALU          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-R-ALU           PIC 9(02) VALUE ZEROS.
           03 WS-R-ALU-INE       PIC 9(02) VALUE ZEROS.

       01  WS-REG-CFP001S1       PIC X(32) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S1.
           03 WS-ID-ALUNO        PIC 999.
           03 WS-NM-ALUNO        PIC X(20).
           03 WS-ALUNO           PIC X(09).

       77  WS-ID-CONT            PIC 99    VALUE ZEROS.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-ID-ALUNO        PIC 999.
           03 LK-NM-ALUNO        PIC X(20).
           03 LK-TL-ALUNO        PIC X(09).
           03 LK-MENSAGEM        PIC X(40) VALUE SPACES.
      *
       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-CONSULTA THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN INPUT CFP001S1
            IF WS-FS-ALU NOT = 00
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S1' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-CONSULTA.
            DISPLAY LK-AREA
            IF LK-ID-ALUNO IS NUMERIC THEN
               MOVE LK-ID-ALUNO TO ID-ALUNO
               MOVE 'S'         TO WS-FLAG
            READ CFP001S1 INTO WS-REG-CFP001S1
                 KEY IS ID-ALUNO
                  INVALID KEY
                       ADD 1 TO WS-R-ALU-INE
      *                DISPLAY 'ALUNO INEXISTENTE!'
      *                DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-ALU
      *                DISPLAY 'ALUNOS: ' WS-REG-CFP001S1
      *                DISPLAY ' '
            END-READ
            MOVE WS-ID-ALUNO TO LK-ID-ALUNO
            MOVE WS-NM-ALUNO TO LK-NM-ALUNO
            MOVE WS-ALUNO    TO LK-TL-ALUNO
            ELSE
            PERFORM UNTIL WS-FLAG = 'N'
            DISPLAY 'DIGITE O ID COM 3 DIGITOS PARA CONSULTA: '
            ACCEPT ID-ALUNO

            READ CFP001S1 INTO WS-REG-CFP001S1
                 KEY IS ID-ALUNO
                  INVALID KEY
                       ADD 1 TO WS-R-ALU-INE
                       DISPLAY 'ALUNO INEXISTENTE!'
                       DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-ALU
                       DISPLAY 'ALUNOS: ' WS-REG-CFP001S1
                       DISPLAY ' '
            END-READ

            DISPLAY 'DESEJA REALIZAR OUTRA CONSULTA, (S)SIM (N)NAO'
            ACCEPT WS-FLAG
            END-PERFORM
            END-IF.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CFP001S1
            IF WS-FS-ALU NOT = 00 THEN
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CFP001S1'    TO WS-MSG2
            PERFORM P800-ERRO THRU P800-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA CADALUNO'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM: ' WS-MSG2
            DISPLAY ' CODIGO..: ' WS-MSG1
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE
            GOBACK.
       P800-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            DISPLAY ' '
            DISPLAY '--------------------------------------'
            DISPLAY '>>>> PROGRAMA CADALUNO FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE ALUNOS EXISTENTE..: ' WS-R-ALU
            DISPLAY ' QTDE ALUNOS INEXISTENTE: ' WS-R-ALU-INE
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM CONALUNO.
