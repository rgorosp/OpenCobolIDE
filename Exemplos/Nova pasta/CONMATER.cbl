      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-02-2023
      * Purpose: CONSULTA ALUNOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONMATER.
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
           03 WS-R-MAT           PIC 9(02) VALUE ZEROS.
           03 WS-R-MAT-INE       PIC 9(02) VALUE ZEROS.
       01  WS-REG-CFP001S2       PIC X(27) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S2.
           03 WS-ID-MATERIA      PIC 999.
           03 WS-NM-MATERIA      PIC X(20).
           03 WS-NT-APROVACAO    PIC 9(02)V99.
       77  WS-ID-CONT            PIC 99    VALUE ZEROS.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-ID-MATERIA      PIC 999.
           03 LK-NM-MATERIA      PIC X(20).
           03 LK-NT-APROVACAO    PIC 9(02)V99.
           03 LK-MENSAGEM        PIC X(40).

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
            OPEN INPUT CFP001S2
            IF WS-FS-MAT NOT = 00
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S2' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-CONSULTA.
      *      DISPLAY LK-AREA
            IF LK-ID-MATERIA NOT EQUAL 000 THEN
               MOVE LK-ID-MATERIA TO ID-MATERIA
               MOVE 'S'           TO WS-FLAG
            READ CFP001S2 INTO WS-REG-CFP001S2
                 KEY IS ID-MATERIA
                  INVALID KEY
                       ADD 1 TO WS-R-MAT-INE
      *                 DISPLAY 'MATERIA INEXISTENTE!'
      *                 DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-MAT
      *                 DISPLAY 'MATERIA: ' WS-REG-CFP001S2
      *                 DISPLAY ' '
            END-READ
            MOVE WS-ID-MATERIA   TO LK-ID-MATERIA
            MOVE WS-NM-MATERIA   TO LK-NM-MATERIA
            MOVE WS-NT-APROVACAO TO LK-NT-APROVACAO
            ELSE
            PERFORM UNTIL WS-FLAG = 'N'
            DISPLAY 'DIGITE O ID COM 3 DIGITOS PARA CONSULTA: '
            ACCEPT ID-MATERIA

            READ CFP001S2 INTO WS-REG-CFP001S2
                 KEY IS ID-MATERIA
                  INVALID KEY
                       ADD 1 TO WS-R-MAT-INE
                       DISPLAY 'MATERIA INEXISTENTE!'
                       DISPLAY ' '
                  NOT INVALID KEY
                       ADD 1 TO WS-R-MAT
                       DISPLAY 'MATERIA: ' WS-REG-CFP001S2
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
            DISPLAY '     ERRO PROGRAMA CONMATER'
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
            DISPLAY '>>>> PROGRAMA CONMATER FINALIZADO <<<<'
            DISPLAY '--------------------------------------'
            DISPLAY ' QTDE MATERIA EXISTENTE..: ' WS-R-MAT
            DISPLAY ' QTDE MATERIA INEXISTENTE: ' WS-R-MAT-INE
            DISPLAY '--------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM CONMATER.
