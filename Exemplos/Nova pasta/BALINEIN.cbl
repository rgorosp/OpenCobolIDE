      ******************************************************************
      * Author: EMERSON SILVA MOTTA
      * Date: 02-07-25
      * Purpose: BALANCE LINE - LOGICA DE INTERCESSAO
      *    MOSTRAR APENAS VALORES EXISTENTES NOS 2 ARQUIVOS
      *    ATUALIZADO PELOS REGISTROS DO ARQUIVOS CONTATOB
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALINEIN.
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
                  RESERVE 10 AREAS.
      * >>>>>> ARQUIVO LEITURA ATUALIZADO
            SELECT CONTATOB ASSIGN TO "C:/ARQUIVOS/CONTATOB.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS ID-CONTATOB
                  FILE STATUS  IS WS-FS-CONB
                  RESERVE 10 AREAS.
      * >>>>>> ARQUIVO GRAVACAO BALANCE LINE
            SELECT CONTATOC ASSIGN TO "C:/ARQUIVOS/CONTATOC.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS RANDOM
                  RECORD KEY   IS ID-CONTATOC
                  FILE STATUS  IS WS-FS-CONC
                  RESERVE 5 AREAS.
      * <!-- Buffer_de_leitura com RESERVE -->
       DATA DIVISION.
       FILE SECTION.
       FD  CONTATOA.
           COPY REGCONTT.

       FD  CONTATOB.
       01  REG-CONTATOB.
           03 ID-CONTATOB         PIC 99.
           03 NM-CONTATOB         PIC X(20).

       FD  CONTATOC.
       01  REG-CONTATOC.
           03 ID-CONTATOC         PIC 99.
           03 NM-CONTATOC         PIC X(20).
      *
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-FS-CONA         PIC 9(02) VALUE ZEROS.
           03 WS-FS-CONB         PIC 9(02) VALUE ZEROS.
           03 WS-FS-CONC         PIC 9(02) VALUE ZEROS.
           03 WS-MSG1            PIC 9(02) VALUE ZEROS.
           03 WS-MSG2            PIC X(35) VALUE SPACES.

       01  WS-CONTADORES.
      * <!-- COMP-3 para performance -->
           03 WS-R-CONTA         PIC 9(07) COMP-3 VALUE ZEROS.
           03 WS-R-CONTB         PIC 9(07) COMP-3 VALUE ZEROS.
           03 WS-W-CONTC         PIC 9(07) COMP-3 VALUE ZEROS.

       01  WS-FLAGS.
           03 WS-EOF-CONA        PIC X(01).
              88 EOF-CONA        VALUE 'S' FALSE 'N'.
           03 WS-EOF-CONB        PIC X(01).
              88 EOF-CONB        VALUE 'S' FALSE 'N'.

       77  WS-CURRENT-DATE       PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-FORMAT   PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-INI      PIC X(21) VALUE SPACES.
       77  WS-DATA-HORA-FIM      PIC X(21) VALUE SPACES.
      *
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
            INITIALIZE WS-VAR
                       WS-CONTADORES
                       WS-FLAGS

            INITIALIZE REG-CONTATOC
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

            OPEN OUTPUT CONTATOC
            IF WS-FS-CONC NOT = 00 THEN
               MOVE WS-FS-CONC                         TO WS-MSG1
               MOVE 'ERRO ABERTURA DDNAME CONTATOC'    TO WS-MSG2
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
                  MOVE 99  TO ID-CONTATO
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
                  MOVE 99  TO ID-CONTATOB
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
            EVALUATE TRUE
               WHEN ID-CONTATO < ID-CONTATOB
                  PERFORM P202-LEITURA-A THRU P202-EXIT
               WHEN ID-CONTATO > ID-CONTATOB
                  PERFORM P203-LEITURA-B THRU P203-EXIT
               WHEN OTHER
                  PERFORM P701-GRAVAR
            END-EVALUATE.
       P501-EXIT.
            EXIT.

       S700 SECTION.
       P701-GRAVAR.
            MOVE ID-CONTATOB TO ID-CONTATOC
            MOVE NM-CONTATOB TO NM-CONTATOC
            WRITE REG-CONTATOC
            IF WS-FS-CONC NOT = 00 THEN
               MOVE WS-FS-CONC                         TO WS-MSG1
               MOVE 'ERRO GRAVACAO DDNAME CONTATOC'    TO WS-MSG2
               PERFORM P801-ERRO THRU P801-EXIT
            END-IF.

            ADD 1 TO WS-W-CONTC
            PERFORM P202-LEITURA-A THRU P202-EXIT
            PERFORM P203-LEITURA-B THRU P203-EXIT.
       P701-EXIT.
            EXIT.

       S800 SECTION.
       P801-ERRO.
            PERFORM P950-DATA-HORA THRU P950-EXIT
            DISPLAY '--------------------------------'
            DISPLAY '    ERRO NO PROGRAMA BALINEIN'
            DISPLAY '--------------------------------'
            DISPLAY ' FILE STATUS: ' WS-MSG2
            DISPLAY ' MENSAGEM...: ' WS-MSG1
            DISPLAY '--------------------------------'
            DISPLAY ' DEBUG INFO.: '
            DISPLAY ' ID-CONTATO.: ' ID-CONTATO
            DISPLAY ' ID-CONTATOB: ' ID-CONTATOB
            DISPLAY ' WS-R-CONTA.: ' WS-R-CONTA
            DISPLAY ' WS-R-CONTB.: ' WS-R-CONTB
            DISPLAY '--------------------------------'
            DISPLAY ' DATA/HORA..: ' WS-DATA-HORA-FORMAT
            DISPLAY '--------------------------------'
      * <!-- Abend controlado CEE3ABD é específico do IBM z/OS -->
      *      CALL 'CEE3ABD' USING BY VALUE 8 BY VALUE 3
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

            CLOSE CONTATOC
            IF WS-FS-CONC NOT = 00 THEN
               MOVE WS-FS-CONC                         TO WS-MSG1
               MOVE 'ERRO FECHAR DDNAME CONTATOC'      TO WS-MSG2
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
            DISPLAY '       PROGRAMA BALINEIN'
            DISPLAY '--------------------------------'
            DISPLAY ' REG. ENTRADA CONTATOA: ' WS-R-CONTA
            DISPLAY ' REG. ENTRADA CONTATOB: ' WS-R-CONTB
            DISPLAY ' REG. SAIDA   CONTATOC: ' WS-W-CONTC
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
       END PROGRAM BALINEIN.
