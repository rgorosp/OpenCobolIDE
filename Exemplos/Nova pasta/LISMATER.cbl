      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-02-2022
      * Purpose: LISTAR AS MATERIAS DOS ALUNOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISMATER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CFP001S2 ASSIGN TO "C:/ARQUIVOS/MATERIAS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
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
           03 WS-I-MAT           PIC 9(05) VALUE ZEROS.
       01  WS-REG-CFP001S2       PIC X(27) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S2.
           03 WS-ID-MATERIA      PIC 999.
           03 WS-NM-MATERIA      PIC X(20).
           03 WS-NT-APROVACAO    PIC 9(02)V99.

      *AREA DE COMUNICACAO
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
            INITIALIZE WS-VAR
            PERFORM P050-ABRIR
            PERFORM P100-LISTA THRU P100-EXIT UNTIL WS-FS-MAT = 10
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN INPUT CFP001S2
            IF WS-FS-MAT NOT = 00 THEN
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S2' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-LISTA.
      *      DISPLAY LK-AREA
            READ CFP001S2 INTO WS-REG-CFP001S2
            IF WS-FS-MAT NOT EQUAL 00 AND 10 THEN
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO LEITURA DO ARQUIVO CFP001S2'  TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               IF WS-FS-MAT = 00
                  ADD 1 TO WS-I-MAT
                  DISPLAY 'REGISTRO: ' WS-I-MAT
                          ': DADOS: ' WS-REG-CFP001S2
               END-IF
            END-IF.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CFP001S2
            IF WS-FS-MAT NOT = 00 THEN
               MOVE WS-FS-MAT                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CFP001S2'    TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA LISMATER'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM.....: ' WS-MSG2
            DISPLAY ' FILE STATUS..: ' WS-MSG1
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE.
       P800-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            DISPLAY ' '
            DISPLAY '---------------------------------------'
            DISPLAY '>>>> PROGRAMA LISMATER FINALIZADO <<<<'
            DISPLAY '---------------------------------------'
            DISPLAY ' QTDE CONTATOS READ.: ' WS-I-MAT
            DISPLAY '---------------------------------------'
            GOBACK.
       P999-EXIT.
            EXIT.
       END PROGRAM LISMATER.
