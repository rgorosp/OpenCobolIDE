      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-02-2023
      * Purpose: CADASTRO DE MATERIAS DE ALUNO ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADMATER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CFP001S2 ASSIGN TO "C:/ARQUIVOS/MATERIAS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-MATERIA
                  FILE STATUS  IS WS-FS-MAT.
       DATA DIVISION.
       FILE SECTION.
       FD  CFP001S2.
           COPY REGMATER.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-MAT          PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
           03 WS-FLAG            PIC X(01) VALUE 'S'.
           03 WS-O-MAT           PIC 9(02) VALUE ZEROS.
       01  WS-REG-CFP001S2       PIC X(27) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S2.
           03 WS-ID-MATERIA      PIC 999.
           03 WS-NM-MATERIA      PIC X(20).
           03 WS-NT-APROVACAO    PIC 9(02)V99.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-ID-MATERIA      PIC 999.
           03 LK-NM-MATERIA      PIC X(20).
           03 LK-NT-APROVACAO    PIC 9(02)V99.
           03 LK-MENSAGEM        PIC X(40).
      *
       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            DISPLAY WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-CADASTRO THRU P100-EXIT
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN I-O CFP001S2
            IF WS-FS-MAT = 35 THEN
               OPEN OUTPUT CFP001S2
            ELSE
            IF WS-FS-MAT NOT = 00
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S2' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-CADASTRO.
      *     DISPLAY LK-AREA
            PERFORM UNTIL WS-FLAG = 'N'
               DISPLAY ' '
               DISPLAY 'DIGITE O ID COM 3 DIGITOS! '
               ACCEPT WS-ID-MATERIA
               DISPLAY 'DIGITE O NOME DA MATERIA! '
               ACCEPT WS-NM-MATERIA
               DISPLAY 'DIGITE A NOTA PARA APROVACAO'
               ACCEPT WS-NT-APROVACAO

            MOVE WS-ID-MATERIA   TO ID-MATERIA
            MOVE WS-NM-MATERIA   TO NM-MATERIA
            MOVE WS-NT-APROVACAO TO NT-APROVACAO
            WRITE REG-CFP001S2
                  INVALID KEY
                       DISPLAY 'MATERIA JA CADASTRADA'
                       DISPLAY ' '
                  NOT INVALID KEY
                       DISPLAY 'CADASTRO DA MATERIA REALIZADO!'
                       DISPLAY ' '
            END-WRITE

            IF WS-FS-MAT NOT = 00 THEN
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO GRAVACAO DO ARQUIVO CFP001S2' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               ADD 1 TO WS-O-MAT
            END-IF

            DISPLAY 'DESEJA REALIZAR OUTRO CADASTRO, (S)SIM (N)NAO'
            ACCEPT WS-FLAG
            END-PERFORM.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CFP001S2
            IF WS-FS-MAT NOT = 00 THEN
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CFP001S2'    TO WS-MSG2
            PERFORM P800-ERRO THRU P800-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA CADMATER'
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
            DISPLAY '--------------------------------------'
            DISPLAY '>>>> PROGRAMA CADMATER FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE ALUNOS GRAV: ' WS-O-MAT
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM CADMATER.
