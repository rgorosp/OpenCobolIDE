      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 12-11-23
      * Purpose: Capítulo 05 - Aula 05 - Prática 01
      * Description: Ler o arquivo E1-C3A09P01.txt fazer a chamada do
      * sub-programa C5A5P012 e realizar o calculo do valor unitiario
      * e quantidade mês
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-C5A5P011.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT  E1C3A9P1 ASSIGN TO
                    'C:\Arquivos\E1-C3A09P01.txt'
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-A.

            SELECT  S1C5A5P1 ASSIGN TO
                    'C:\Arquivos\S1-C5A5P011.txt'
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-B.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  E1C3A9P1.
       COPY E1C3A9P1.

       FD  S1C5A5P1.
       01  REG-S1C5A5P1.
           03 COD-PROD-S1   PIC 9999.
           03 FILLER        PIC X.
           03 NOME-PROD-S1  PIC X(39).
           03 FILLER        PIC X.
           03 VLR-PROD-S1   PIC ZZ.Z99,99.

       WORKING-STORAGE SECTION.
       01  WS-PARM.
           03 VLR-PARM      PIC 9(02)V99.
           03 QTDE-PARM     PIC 9(5).
           03 TOT-PARM      PIC 9(05)V99.

       01  WS-VAR.
           03 WS-FS-A       PIC 9(02).
           03 WS-FS-B       PIC 9(02).
           03 WS-MSG1       PIC X(30).
           03 WS-MSG2       PIC 9(02).
           03 WS-MSG3       PIC X(10).
           03 WS-RETURN     PIC 9(02).
           03 WS-R-E1C3A9P1 PIC 9(05).
           03 WS-W-S1C5A5P1 PIC 9(05).
      *
       PROCEDURE DIVISION.
       INICIO SECTION.
            PERFORM P001-ABRIR THRU P002-EXIT
            PERFORM P101-LEITURA
            PERFORM P201-PROCESSAR UNTIL WS-FS-A = 10
            PERFORM P901-TERMINO.

       S000 SECTION.
       P001-ABRIR.
            OPEN INPUT E1C3A9P1
            EVALUATE WS-FS-A
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P001-ABRIR E1C3A9P1' TO WS-MSG1
                 MOVE WS-FS-A                    TO WS-MSG2
                 MOVE 'ANORMAL'                  TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P001-EXIT.

       P002-ABRIR.
            OPEN OUTPUT S1C5A5P1
            EVALUATE WS-FS-B
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P001-ABRIR S1C5A5P1' TO WS-MSG1
                 MOVE WS-FS-B                    TO WS-MSG2
                 MOVE 'ANORMAL'                  TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P002-EXIT.
            EXIT.

       S100 SECTION.
       P101-LEITURA.
            READ E1C3A9P1
            EVALUATE WS-FS-A
                WHEN 0
                 ADD 1 TO WS-R-E1C3A9P1
                 CONTINUE
                WHEN 10
                 IF WS-R-E1C3A9P1 = 0
                    MOVE 'ARQ E1C3A9P1 VAZIO' TO WS-MSG1
                    MOVE WS-FS-A              TO WS-MSG2
                    MOVE 'NORMAL'             TO WS-MSG3
                    PERFORM P701-FECHAR    THRU P702-FECHAR
                    MOVE 1 TO WS-RETURN
                    PERFORM P801-STATUS
                 ELSE
                    CONTINUE
                 END-IF
                WHEN OTHER
                 MOVE 'ERRO P101-LEITURA - E1C3A9P1' TO WS-MSG1
                 MOVE WS-FS-A                        TO WS-MSG2
                 MOVE 'ANORMAL'                      TO WS-MSG3
                 PERFORM P701-FECHAR    THRU P702-FECHAR
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P101-EXIT.
            EXIT.

       S200 SECTION.
       P201-PROCESSAR.
            MOVE COD-PROD  TO COD-PROD-S1
            MOVE NOME-PROD TO NOME-PROD-S1
            MOVE VLR-PROD  TO VLR-PARM
            MOVE QTDE-PROD TO QTDE-PARM

            CALL "PGM-C5A5P012" USING WS-PARM

            MOVE TOT-PARM  TO VLR-PROD-S1

            WRITE REG-S1C5A5P1
            ADD 1 TO WS-W-S1C5A5P1
            EVALUATE WS-FS-B
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO P201-PROCESSAR S1C5A5P1' TO WS-MSG1
                  MOVE WS-FS-B                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P702-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            MOVE ZEROS TO WS-PARM

            PERFORM P101-LEITURA.
       P201-EXIT.
            EXIT.

       S700 SECTION.
       P701-FECHAR.
            CLOSE E1C3A9P1
            EVALUATE WS-FS-A
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P701-FECHAR E1C3A9P1' TO WS-MSG1
                 MOVE WS-FS-A                     TO WS-MSG2
                 MOVE 'ANORMAL'                   TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P701-EXIT.

       P702-FECHAR.
            CLOSE S1C5A5P1
            EVALUATE WS-FS-B
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P702-FECHAR S1C5A5P1' TO WS-MSG1
                 MOVE WS-FS-B                     TO WS-MSG2
                 MOVE 'ANORMAL'                   TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P702-EXIT.
            EXIT.

       S800 SECTION.
       P801-STATUS.
            DISPLAY '----------------------------------'
            DISPLAY '         STATUS DO PROGRAMA'
            DISPLAY '----------------------------------'
            DISPLAY 'MENSAGEM...: ' WS-MSG1
            DISPLAY 'FILE STATUS: ' WS-MSG2
            DISPLAY 'STATUS.....: ' WS-MSG3
            DISPLAY '----------------------------------'
            MOVE WS-RETURN TO RETURN-CODE
            STOP RUN.
       P801-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            PERFORM P701-FECHAR    THRU P702-FECHAR
            DISPLAY '----------------------------------'
            DISPLAY '         FIM DO PROGRAMA'
            DISPLAY '----------------------------------'
            DISPLAY 'LEITURA  E1C3A9P1: ' WS-R-E1C3A9P1
            DISPLAY 'GRAVACAO S1C5A5P1: ' WS-W-S1C5A5P1
            DISPLAY '----------------------------------'
            MOVE 0 TO RETURN-CODE
            STOP RUN.
       P901-EXIT.

       P902-STOP.
            GOBACK.
       P902-EXIT.
            EXIT.
       END PROGRAM PGM-C5A5P011.
