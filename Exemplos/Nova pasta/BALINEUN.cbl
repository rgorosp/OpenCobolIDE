      ******************************************************************
      * Author: EMERSON SILVA MOTTA
      * Date: 02-07-25
      * Purpose: BALANCE LINE - LOGICA DE UNIÃO
      *    GRAVAR ARQUIVO CONTATOU.DAT CLASSIFICADO, SE AS
      *    CHAVES FOREM DUPLICADAS, GRAVAR O ULTIMO ARQUIVO
      *    ATUALIZADO PELO ARQUIVO CONTATOB.DAT
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALINEUN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * >>>>>> ARQUIVO LEITURA ORIGINAL
            SELECT CONTATOA ASSIGN TO "C:/ARQUIVOS/CONTATOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS ID-CONTATO
                  FILE STATUS  IS WS-FS-CONA
                  RESERVE      10 AREAS.
      * >>>>>> ARQUIVO LEITURA ATUALIZADO
            SELECT CONTATOB ASSIGN TO "C:/ARQUIVOS/CONTATOB.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS ID-CONTATOB
                  FILE STATUS  IS WS-FS-CONB
                  RESERVE      10 AREAS.
      * >>>>>> ARQUIVO GRAVACAO BALANCE LINE
            SELECT CONTATOU ASSIGN TO "C:/ARQUIVOS/CONTATOU.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-CONTATOU
                  FILE STATUS  IS WS-FS-CONU
                  RESERVE      05 AREAS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOA.
           COPY REGCONTT.

       FD  CONTATOB.
       01  REG-CONTATOB.
           03 ID-CONTATOB         PIC 99.
           03 NM-CONTATOB         PIC X(20).

       FD  CONTATOU.
       01  REG-CONTATOU.
           03 ID-CONTATOU         PIC 99.
           03 NM-CONTATOU         PIC X(20).
      *
       WORKING-STORAGE SECTION.
       01  WS-VAR.
      * >>> FILE STATUS
           03 WS-FS-CONA         PIC 9(02) VALUE ZEROS.
           03 WS-FS-CONB         PIC 9(02) VALUE ZEROS.
           03 WS-FS-CONU         PIC 9(02) VALUE ZEROS.
      * >>> MENSAGENS
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.
      * >>> CONTADORES E ACUMULADORES
           03 WS-R-CONTA         PIC 9(07) COMP-3 VALUE ZEROS.
           03 WS-R-CONTB         PIC 9(07) COMP-3 VALUE ZEROS.
           03 WS-W-CONTU         PIC 9(07) COMP-3 VALUE ZEROS.
      * >>> END-OF-FILE
           03 WS-EOF-CONA        PIC X(01).
              88 EOF-CONA        VALUE 'S' FALSE 'N'.
           03 WS-EOF-CONB        PIC X(01).
              88 EOF-CONB        VALUE 'S' FALSE 'N'.

       77  WS-CURRENT-DATE       PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-FORMAT   PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-INI      PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-FIM      PIC X(21) VALUE SPACES.
      * >>> INICIO DO PROCESSAMENTO
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            INITIALIZE WS-VAR

            INITIALIZE REG-CONTATOU
                       REPLACING ALPHANUMERIC BY SPACES
                                 NUMERIC      BY ZEROES

            SET EOF-CONA EOF-CONB             TO FALSE

            PERFORM P950-DATA-HORA THRU P950-EXIT
            MOVE WS-DATA-HORA-FORMAT TO WS-DATA-HORA-INI

            PERFORM P101-ABERTURA
            PERFORM P202-LEITURA-A THRU P202-EXIT
            PERFORM P203-LEITURA-B THRU P203-EXIT
            PERFORM P501-PROCESSAR UNTIL EOF-CONA AND EOF-CONB
            PERFORM P901-TERMINO.
       P001-EXIT.
            EXIT.

       S100 SECTION.
       P101-ABERTURA.
            OPEN INPUT CONTATOA
            IF WS-FS-CONA NOT = 00 THEN
               MOVE WS-FS-CONA                         TO WS-MSG1
               MOVE 'ERRO ABERTURA DDNAME CONTATOS'    TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.

            OPEN INPUT CONTATOB
            IF WS-FS-CONB NOT = 00 THEN
               MOVE WS-FS-CONB                         TO WS-MSG1
               MOVE 'ERRO ABERTURA DDNAME CONTATOB'    TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.

            OPEN OUTPUT CONTATOU
            IF WS-FS-CONU NOT = 00 THEN
               MOVE WS-FS-CONU                         TO WS-MSG1
               MOVE 'ERRO ABERTURA DDNAME CONTATOU'    TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.
       P101-EXIT.
            EXIT.

       S200 SECTION.
       P202-LEITURA-A.
            READ CONTATOA
            IF WS-FS-CONA NOT EQUAL 00 AND 10 THEN
               MOVE WS-FS-CONA                         TO WS-MSG1
               MOVE 'ERRO LEITURA DDNAME CONTATOA'     TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            ELSE
               IF WS-FS-CONA EQUAL 10
                  MOVE HIGH-VALUES TO ID-CONTATO
                  SET EOF-CONA TO TRUE
               ELSE
                  IF WS-FS-CONA = 00
                     ADD 1 TO WS-R-CONTA
                  END-IF
               END-IF
            END-IF.
       P202-EXIT.

       P203-LEITURA-B.
            READ CONTATOB
            IF WS-FS-CONB NOT EQUAL 00 AND 10 THEN
               MOVE WS-FS-CONB                         TO WS-MSG1
               MOVE 'ERRO LEITURA DDNAME CONTATOB'     TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            ELSE
               IF WS-FS-CONB EQUAL 10
                  MOVE HIGH-VALUES TO ID-CONTATOB
                  SET EOF-CONB TO TRUE
               ELSE
                  IF WS-FS-CONB = 00
                     ADD 1 TO WS-R-CONTB
                  END-IF
               END-IF
            END-IF.
       P203-EXIT.
            EXIT.

       S500 SECTION.
       P501-PROCESSAR.
            IF ID-CONTATO LESS THAN ID-CONTATOB THEN
               MOVE ID-CONTATO TO ID-CONTATOU
               MOVE NM-CONTATO TO NM-CONTATOU
               PERFORM P701-GRAVAR
               PERFORM P202-LEITURA-A THRU P202-EXIT
            ELSE
               IF ID-CONTATO GREATER THAN ID-CONTATOB
                  MOVE ID-CONTATOB TO ID-CONTATOU
                  MOVE NM-CONTATOB TO NM-CONTATOU
                  PERFORM P701-GRAVAR
                  PERFORM P203-LEITURA-B THRU P203-EXIT
               ELSE
                  MOVE ID-CONTATOB TO ID-CONTATOU
                  MOVE NM-CONTATOB TO NM-CONTATOU
                  PERFORM P701-GRAVAR
                  PERFORM P202-LEITURA-A THRU P202-EXIT
                  PERFORM P203-LEITURA-B THRU P203-EXIT
               END-IF
            END-IF.
       P501-EXIT.
            EXIT.

       S700 SECTION.
       P701-GRAVAR.
            WRITE REG-CONTATOU
            IF WS-FS-CONU NOT = 00 THEN
               MOVE WS-FS-CONU                         TO WS-MSG1
               MOVE 'ERRO GRAVACAO DDNAME CONTATOU'    TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF

            ADD 1 TO WS-W-CONTU.
       P701-EXIT.
            EXIT.

       S800 SECTION.
       P801-ERRO.
            PERFORM P950-DATA-HORA THRU P950-EXIT
            DISPLAY '--------------------------------'
            DISPLAY '    ERRO NO PROGRAMA BALINEUN'
            DISPLAY '--------------------------------'
            DISPLAY ' FILE STATUS: ' WS-MSG2
            DISPLAY ' MENSAGEM...: ' WS-MSG1
            DISPLAY '--------------------------------'
            DISPLAY ' DATA/HORA: ' WS-DATA-HORA-FORMAT
            DISPLAY '--------------------------------'
            MOVE 8 TO RETURN-CODE
            STOP RUN.
       P801-EXIT.
            EXIT.

       S850 SECTION.
       P851-FECHAR.
            CLOSE CONTATOA
            IF WS-FS-CONA NOT = 00 THEN
               MOVE WS-FS-CONA                         TO WS-MSG1
               MOVE 'ERRO FECHAR DDNAME CONTATOS'      TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.

            CLOSE CONTATOB
            IF WS-FS-CONB NOT = 00 THEN
               MOVE WS-FS-CONB                         TO WS-MSG1
               MOVE 'ERRO FECHAR DDNAME CONTATOB'      TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.

            CLOSE CONTATOU
            IF WS-FS-CONU NOT = 00 THEN
               MOVE WS-FS-CONU                         TO WS-MSG1
               MOVE 'ERRO FECHAR DDNAME CONTATOU'      TO WS-MSG2
            PERFORM P801-ERRO THRU P801-EXIT
            END-IF.
       P851-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            PERFORM P851-FECHAR
            PERFORM P950-DATA-HORA THRU P950-EXIT
            MOVE WS-DATA-HORA-FORMAT TO WS-DATA-HORA-FIM
            DISPLAY '--------------------------------'
            DISPLAY '       PROGRAMA BALINEUN'
            DISPLAY '--------------------------------'
            DISPLAY ' REG. ENTRADA CONTATOA: ' WS-R-CONTA
            DISPLAY ' REG. ENTRADA CONTATOB: ' WS-R-CONTB
            DISPLAY ' REG. SAIDA   CONTATOU: ' WS-W-CONTU
            DISPLAY '--------------------------------'
            DISPLAY ' DATA/HORA INICIAL....: ' WS-DATA-HORA-INI
            DISPLAY ' DATA/HORA FINAL......: ' WS-DATA-HORA-FIM
            DISPLAY '--------------------------------'
            STOP RUN.
       P901-EXIT.
            EXIT.

       S950-DATA-HORA SECTION.
       P950-DATA-HORA.
            MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE

            STRING WS-CURRENT-DATE(7:2) '/'                             * Dia
                   WS-CURRENT-DATE(5:2) '/'                             * Mês
                   WS-CURRENT-DATE(1:4) ' - '                           * Ano
                   WS-CURRENT-DATE(9:2) ':'                             * Hora
                   WS-CURRENT-DATE(11:2) ':'                            * Minuto
                   WS-CURRENT-DATE(13:2)                                * Segundo
                   DELIMITED BY SIZE
                   INTO WS-DATA-HORA-FORMAT
            END-STRING.
       P950-EXIT.
            EXIT.
       END PROGRAM BALINEUN.
