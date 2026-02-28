       IDENTIFICATION DIVISION.
       PROGRAM-ID.  CBLAPI02.
       AUTHOR. EMERSON S MOTTA.
       DATE-WRITTEN. TODAY.
       DATE-COMPILED. TODAY.
      * REMARKS. 'CBLAPI02 - Processamento de Requisições API'.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INREQ    ASSIGN INREQ
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-IN.
           SELECT APIRES   ASSIGN APIRES
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-API.
           SELECT OUTFINAL ASSIGN OUTFINAL
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS-OUT.

       DATA DIVISION.
       FILE SECTION.

       FD  INREQ
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  INREQ-REC       PIC X(80).

       FD  APIRES
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  APIRES-REC      PIC X(80).

       FD  OUTFINAL
           RECORDING MODE F
           BLOCK CONTAINS 0 RECORDS.
       01  OUTFINAL-REC    PIC X(80).

       WORKING-STORAGE SECTION.

       77 WS-FS-IN        PIC XX.
       77 WS-FS-API       PIC XX.
       77 WS-FS-OUT       PIC XX.
       01 WS-EOF-IN       PIC X VALUE 'N'.
          88 EOF-IN       VALUE 'Y' FALSE 'N'.

       01 WS-EOF-API      PIC X VALUE 'N'.
          88 EOF-API      VALUE 'Y' FALSE 'N'.

       01 WS-ID           PIC X(08).
       01 WS-STATUS       PIC X(10).
       01 WS-LIMITE       PIC X(10).
       01 WS-LINHA-FINAL  PIC X(80).
       01 WS-DUM1         PIC X(20).
       01 WS-DUM2         PIC X(20).
       01 WS-DUM3         PIC X(20).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           MOVE 'N' TO WS-EOF-IN.
           MOVE SPACES TO WS-ID WS-STATUS WS-LIMITE WS-LINHA-FINAL.

           PERFORM OPEN-FILES.
           PERFORM READ-INREQ.
           PERFORM CLOSE-FILES.

       OPEN-FILES.
           OPEN INPUT INREQ
           IF WS-FS-IN NOT EQUAL '00'
              DISPLAY 'ERRO AO ABRIR ARQUIVO INREQ: ' WS-FS-IN
              STOP RUN
           END-IF.

           OPEN INPUT APIRES
             IF WS-FS-API NOT EQUAL '00'
               DISPLAY 'ERRO AO ABRIR ARQUIVO APIRES: ' WS-FS-API
               STOP RUN
             END-IF.

           OPEN OUTPUT OUTFINAL
             IF WS-FS-OUT NOT EQUAL '00'
                DISPLAY 'ERRO AO ABRIR ARQUIVO OUTFINAL: ' WS-FS-OUT
                STOP RUN
             END-IF.

       READ-INREQ.
           MOVE 'N' TO WS-EOF-IN
           PERFORM UNTIL WS-EOF-IN = 'Y'
               READ INREQ
                   AT END
                       MOVE 'Y' TO WS-EOF-IN
                   NOT AT END
                       PERFORM PROCESSAR-REQ
               END-READ
           END-PERFORM.

        CLOSE-FILES.

           CLOSE INREQ
           CLOSE APIRES
           CLOSE OUTFINAL.
           STOP RUN.

       PROCESSAR-REQ.
           MOVE SPACES TO WS-ID WS-STATUS WS-LIMITE

      * Extrai o ID da requisição (posição 20)
           MOVE INREQ-REC(19:8) TO WS-ID

      * Busca ID no APIRES
           MOVE 'N' TO WS-EOF-API

           PERFORM UNTIL WS-EOF-API = 'Y'
               READ APIRES
                   AT END
                       MOVE 'Y' TO WS-EOF-API
                   NOT AT END
                       IF APIRES-REC(8:8) = WS-ID
                          PERFORM PARSE-API
                          MOVE 'Y' TO WS-EOF-API
                       END-IF
               END-READ
           END-PERFORM

      * Monta linha final
           STRING WS-ID DELIMITED BY SIZE
                  ' '   DELIMITED BY SIZE
                  WS-STATUS DELIMITED BY SIZE
                  ' '   DELIMITED BY SIZE
                  WS-LIMITE DELIMITED BY SIZE
                  INTO WS-LINHA-FINAL
           END-STRING

           WRITE OUTFINAL-REC FROM WS-LINHA-FINAL.

       PARSE-API..
      * Exemplo: {"ID":"00000001","STATUS":"ATIVO","LIMITE":"1500.00"}
           UNSTRING APIRES-REC
               DELIMITED BY '":"' OR '","'
               INTO WS-DUM1
                    WS-ID
                    WS-DUM2
                    WS-STATUS
                    WS-DUM3
                    WS-LIMITE
           END-UNSTRING.
           DISPLAY WS-DUM1.
           DISPLAY WS-ID.
           DISPLAY WS-DUM2.
           DISPLAY WS-STATUS.
           DISPLAY WS-DUM3.
           DISPLAY WS-LIMITE.
      * Remover aspas finais
      * Limpeza de aspas, chaves e vírgulasY SPACES.
           INSPECT WS-ID     REPLACING ALL '"' BY SPACES.
           INSPECT WS-STATUS REPLACING ALL '"' BY SPACES.
           INSPECT WS-LIMITE REPLACING ALL '"' BY SPACES.
           INSPECT WS-ID     REPLACING ALL '}' BY SPACES.
           INSPECT WS-STATUS REPLACING ALL '}' BY SPACES.
           INSPECT WS-LIMITE REPLACING ALL '}' BY SPACES.
           INSPECT WS-ID     REPLACING ALL '{' BY SPACES.
           INSPECT WS-STATUS REPLACING ALL '{' BY SPACES.
           INSPECT WS-LIMITE REPLACING ALL '{' BY SPACES.
