      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 10-09-23
      * Purpose: Capítulo 05 - Aula 02 - Prática 01
      * Description: Ler o arquivo E1C5A2P1.txt e carregar em uma tabela.
      * Ler o arquivo E2C5A2P1.txt e carregar em uma tabela indexada. Imprimir
      * os total de vendas por produto por mês, com o mês por extenso, conforme
      * layout do relatório abaixo.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C5A02P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT  E1C5A2P1 ASSIGN TO
                    'C:\Arquivos\E1C5A2P1.txt'
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-A.
            SELECT  E2C5A2P1 ASSIGN TO
                    'C:\Arquivos\E2C5A2P1.txt'
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-B.
            SELECT  RC5A2P01 ASSIGN TO
                    'C:\Arquivos\RC5A2P01.txt'
                    ORGANIZATION IS LINE SEQUENTIAL
                    FILE STATUS IS WS-FS-C.
       DATA DIVISION.
       FILE SECTION.
       FD  E1C5A2P1.
       01  REG-E1C5A2P1.
           03 COD-PROD  PIC 99.
           03 COD-UF    PIC 99.
           03 COD-MES   PIC 99.
           03 VALOR-MES PIC 9(5)V99.

       FD  E2C5A2P1.
       01  REG-E2C5A2P1.
           03 CHAVE-E2        PIC 99.
           03 SIGLA-MES-E2    PIC XXX.
           03 NOME-MES-E2     PIC X(10).

       FD  RC5A2P01.
       01  REG-RC5A2P01.
           03 COD-PROD-RC     PIC 99.
           03 COD-UF-RC       PIC 99.
           03 SIGLA-MES-RC    PIC XXX.
           03 NOME-MES-RC     PIC X(10).
           03 VALOR-MES-RC    PIC $Z.Z99,99.

       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-A       PIC 9(02).
           03 WS-FS-B       PIC 9(02).
           03 WS-FS-C       PIC 9(02).
           03 WS-MSG1       PIC X(30).
           03 WS-MSG2       PIC 9(02).
           03 WS-MSG3       PIC X(10).
           03 WS-RETURN     PIC 9(02).
           03 WS-R-E1C5A2P1 PIC 9(05).
           03 WS-R-E2C5A2P1 PIC 9(05).
           03 WS-W-RC5A2P01 PIC 9(05).
           03 WS-COD-PROD   PIC 99.
           03 WS-COD-UF     PIC 99.
           03 WS-IND2       PIC 99.
           03 WS-VALOR-MES  PIC 9(5)V99.

       01  WS-TRAILLER.
           03 FILLER             PIC X(03) VALUE SPACES.
           03 FILLER             PIC X(07) VALUE 'TOTAL: '.
           03 WS-TOTAL           PIC Z.Z99,99.

       77  WS-SPACES             PIC X(30) VALUE ALL SPACES.
      *
       PROCEDURE DIVISION.
       INICIO SECTION.
            PERFORM P001-ABRIR THRU P003-EXIT
            PERFORM P101-LEITURA
            PERFORM P201-PROCESSAR UNTIL WS-FS-A = 10
            PERFORM P901-TERMINO.

       S000 SECTION.
       P001-ABRIR.
            DISPLAY 'P001-ABRIR'
            OPEN INPUT E1C5A2P1
            EVALUATE WS-FS-A
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P001-ABRIR E1C5A2P1' TO WS-MSG1
                 MOVE WS-FS-A                    TO WS-MSG2
                 MOVE 'ANORMAL'                  TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P001-EXIT.

       P002-ABRIR.
            OPEN INPUT E2C5A2P1
            EVALUATE WS-FS-B
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P001-ABRIR E2C5A2P1' TO WS-MSG1
                 MOVE WS-FS-B                    TO WS-MSG2
                 MOVE 'ANORMAL'                  TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P002-EXIT.

       P003-ABRIR.
            OPEN OUTPUT RC5A2P01
            EVALUATE WS-FS-C
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P001-ABRIR RC5A2P01' TO WS-MSG1
                 MOVE WS-FS-C                    TO WS-MSG2
                 MOVE 'ANORMAL'                  TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P003-EXIT.
            EXIT.

       S100 SECTION.
       P101-LEITURA.
            DISPLAY 'P101-LEITURA'
            READ E1C5A2P1
            EVALUATE WS-FS-A
                WHEN 0
                 ADD 1 TO WS-R-E1C5A2P1
                 CONTINUE
                WHEN 10
                 IF WS-R-E1C5A2P1 = 0
                    MOVE 'ARQ E1C5A2P1 VAZIO' TO WS-MSG1
                    MOVE WS-FS-A              TO WS-MSG2
                    MOVE 'NORMAL'             TO WS-MSG3
                    PERFORM P701-FECHAR    THRU P703-FECHAR
                    MOVE 1 TO WS-RETURN
                    PERFORM P801-STATUS
                 ELSE
                    CONTINUE
                 END-IF
                WHEN OTHER
                 MOVE 'ERRO P101-LEITURA - E1C5A2P1' TO WS-MSG1
                 MOVE WS-FS-A                        TO WS-MSG2
                 MOVE 'ANORMAL'                      TO WS-MSG3
                 PERFORM P701-FECHAR    THRU P703-FECHAR
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P101-EXIT.

       P102-MESES.
            DISPLAY 'P102-MESES'
            READ E2C5A2P1
            DISPLAY 'REG-E2C5A2P1 = ' REG-E2C5A2P1
            DISPLAY 'COD-UF = ' COD-UF
            DISPLAY 'CHAVE-E2 = ' CHAVE-E2
            EVALUATE WS-FS-B
                WHEN 0
                 IF COD-MES = CHAVE-E2 THEN
                    ADD 1 TO  WS-R-E2C5A2P1
                    MOVE COD-UF       TO COD-UF-RC
                    MOVE SIGLA-MES-E2 TO SIGLA-MES-RC
                    MOVE NOME-MES-E2  TO NOME-MES-RC
                    PERFORM P702-FECHAR THRU P702-EXIT
                    PERFORM P002-ABRIR  THRU P002-EXIT
                 ELSE
                    GO TO P102-MESES
                 END-IF
                WHEN 10
                 IF WS-R-E2C5A2P1 = 0
                    MOVE 'ARQ E2C5A2P1 VAZIO' TO WS-MSG1
                    MOVE WS-FS-A              TO WS-MSG2
                    MOVE 'ANORMAL'            TO WS-MSG3
                    PERFORM P701-FECHAR
                    MOVE 8 TO WS-RETURN
                    PERFORM P801-STATUS
                 ELSE
                    CONTINUE
                 END-IF
                WHEN OTHER
                 MOVE 'ERRO P102-MESES - E2C5A2P1' TO WS-MSG1
                 MOVE WS-FS-A                      TO WS-MSG2
                 MOVE 'ANORMAL'                    TO WS-MSG3
                 PERFORM P701-FECHAR
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE

            DISPLAY 'COD-UF-RC = ' COD-UF-RC
            DISPLAY 'SIGLA-MES-RC = ' SIGLA-MES-RC
            DISPLAY 'NOME-MES-RC = ' NOME-MES-RC.
       P102-EXIT.
            EXIT.

       S200 SECTION.
       P201-PROCESSAR.
            DISPLAY 'P201-PROCESSAR'

            IF WS-R-E1C5A2P1 <= 1 THEN
               MOVE COD-PROD TO WS-COD-PROD
               MOVE COD-UF   TO WS-COD-UF
            END-IF

            IF COD-PROD EQUAL WS-COD-PROD AND
               COD-UF   EQUAL WS-COD-UF

            DISPLAY 'A = ' COD-PROD
            DISPLAY 'B = ' WS-COD-PROD
            DISPLAY 'C = ' COD-UF
            DISPLAY 'D = ' WS-COD-UF

            MOVE WS-COD-PROD TO COD-PROD-RC
            PERFORM P102-MESES
            MOVE VALOR-MES   TO VALOR-MES-RC
            ADD  VALOR-MES   TO WS-VALOR-MES

            WRITE REG-RC5A2P01
            ADD 1 TO WS-W-RC5A2P01
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO P201-PROCESSAR RC5A2P01' TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE
            ELSE

            MOVE COD-PROD TO WS-COD-PROD
            MOVE COD-UF   TO WS-COD-UF

            WRITE REG-RC5A2P01 FROM WS-SPACES
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO WS-SPACES 1'             TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            MOVE WS-VALOR-MES TO WS-TOTAL
            WRITE REG-RC5A2P01 FROM WS-TRAILLER
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO WS-TRAILLER 1'           TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            WRITE REG-RC5A2P01 FROM WS-SPACES
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO WS-SPACES 2'             TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

              MOVE ZEROS TO WS-TOTAL WS-VALOR-MES
              GO TO P201-PROCESSAR
            END-IF

            PERFORM P101-LEITURA.
        P201-EXIT.
            EXIT.

       S700 SECTION.
       P701-FECHAR.
            DISPLAY 'P701-FECHAR'
            CLOSE E1C5A2P1
            EVALUATE WS-FS-A
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P701-FECHAR E1C5A2P1' TO WS-MSG1
                 MOVE WS-FS-A                     TO WS-MSG2
                 MOVE 'ANORMAL'                   TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P701-EXIT.

       P702-FECHAR.
            CLOSE E2C5A2P1
            EVALUATE WS-FS-B
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P701-FECHAR E2C5A2P1' TO WS-MSG1
                 MOVE WS-FS-B                     TO WS-MSG2
                 MOVE 'ANORMAL'                   TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P702-EXIT.

       P703-FECHAR.
            CLOSE RC5A2P01
            EVALUATE WS-FS-C
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 MOVE 'ERRO P701-FECHAR RC5A2P01' TO WS-MSG1
                 MOVE WS-FS-C                     TO WS-MSG2
                 MOVE 'ANORMAL'                   TO WS-MSG3
                 MOVE 8 TO WS-RETURN
                 PERFORM P801-STATUS
            END-EVALUATE.
       P703-EXIT.
            EXIT.

       S800 SECTION.
       P801-STATUS.
            DISPLAY 'P801-STATUS'
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
            DISPLAY 'P901-TERMINO'
            WRITE REG-RC5A2P01 FROM WS-SPACES
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO P901-TERMINO WS-SPACES 1' TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            MOVE WS-VALOR-MES TO WS-TOTAL
            WRITE REG-RC5A2P01 FROM WS-TRAILLER
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO P901-TERMINO TRAILLER 1' TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            WRITE REG-RC5A2P01 FROM WS-SPACES
            EVALUATE WS-FS-C
                WHEN 0
                  CONTINUE
                WHEN OTHER
                  MOVE 'ERRO P901-TERMINO WS-SPACES 2' TO WS-MSG1
                  MOVE WS-FS-C                        TO WS-MSG2
                  MOVE 'ANORMAL'                      TO WS-MSG3
                  PERFORM P701-FECHAR    THRU P703-FECHAR
                  MOVE 8 TO WS-RETURN
                  PERFORM P801-STATUS
            END-EVALUATE

            PERFORM P701-FECHAR    THRU P703-FECHAR
            DISPLAY '----------------------------------'
            DISPLAY '         FIM DO PROGRAMA'
            DISPLAY '----------------------------------'
            DISPLAY 'LEITURA  E1C5A2P1' WS-R-E1C5A2P1
            DISPLAY 'LEITURA  E2C5A2P1' WS-R-E2C5A2P1
            DISPLAY 'GRAVACAO RC5A2P01' WS-W-RC5A2P01
            DISPLAY '----------------------------------'
            MOVE 0 TO RETURN-CODE
            STOP RUN.
       P901-EXIT.

       P902-STOP.
            STOP RUN.
       P902-EXIT.
            EXIT.
       END PROGRAM C5A02P01.
