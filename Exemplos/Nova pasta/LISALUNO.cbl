      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 02-07-2027
      * Purpose: LISTAR ALUNOS ARQUIVO INDEXADO
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LISALUNO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CFP001S1 ASSIGN TO "C:/ARQUIVOS/ALUNOS.DAT"
                  ORGANIZATION IS INDEXED
                  ACCESS MODE  IS SEQUENTIAL
                  RECORD KEY   IS ID-ALUNO
                  FILE STATUS  IS WS-FS-ALU
                  RESERVE      5  AREAS.
       DATA DIVISION.
       FILE SECTION.
       FD  CFP001S1.
           COPY REGALUNO.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
      * >>> FILE STATUS / MENSAGENS
           03 WS-FS-ALU           PIC 9(02) VALUE ZEROS.
           03 WS-MSG1             PIC 9(02) VALUE ZEROS.
           03 WS-MSG2             PIC X(35) VALUE SPACES.
      * >>> CONTADORES E ACUMULADORES
           03 WS-I-ALU            PIC 9(07) COMP-3 VALUE ZEROS.

       01  WS-REG-CFP001S1        PIC X(32) VALUE SPACES.
       01  FILLER REDEFINES WS-REG-CFP001S1.
           03 WS-ID-ALUNO         PIC 999.
           03 WS-NM-ALUNO         PIC X(20).
           03 WS-TL-ALUNO         PIC X(09).

           77 WS-CURRENT-DATE     PIC X(21) VALUE SPACES.
           77 WS-DATA-HORA-FORMAT PIC X(21) VALUE SPACES.
           77 WS-DATA-HORA-INI    PIC X(21) VALUE SPACES.
           77 WS-DATA-HORA-FIM    PIC X(21) VALUE SPACES.

      * AREA DE COMUNICACAO
       LINKAGE SECTION.
       01  LK-AREA.
           03 LK-ID-ALUNO        PIC 999.
           03 LK-NM-ALUNO        PIC X(20).
           03 LK-TL-ALUNO        PIC X(09).
           03 LK-MENSAGEM        PIC X(40) VALUE SPACES.
      * >>> INICIO DO PROCESSAMENTO
       PROCEDURE DIVISION USING LK-AREA.
       S000 SECTION.
       P000-INICIO.
            INITIALIZE WS-VAR
            PERFORM P950-DATA-HORA THRU P950-EXIT
            MOVE WS-DATA-HORA-FORMAT TO WS-DATA-HORA-INI
            PERFORM P050-ABRIR
            PERFORM P100-LISTA THRU P100-EXIT UNTIL WS-FS-ALU = 10
            PERFORM P700-FECHAR
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S050 SECTION.
       P050-ABRIR.
            OPEN INPUT CFP001S1
            IF WS-FS-ALU NOT = 00 THEN
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO ABERTURA DO ARQUIVO CFP001S1' TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P050-EXIT.
            EXIT.

       S100 SECTION.
       P100-LISTA.
            READ CFP001S1 INTO WS-REG-CFP001S1
            IF WS-FS-ALU NOT EQUAL 00 AND 10 THEN
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO LEITURA DO ARQUIVO CFP001S1'  TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            ELSE
               IF WS-FS-ALU = 00
                  ADD 1 TO WS-I-ALU
                  DISPLAY 'REGISTRO: ' WS-I-ALU
                          ': DADOS: ' WS-REG-CFP001S1
               END-IF
            END-IF.
       P100-EXIT.
            EXIT.

       S700 SECTION.
       P700-FECHAR.
            CLOSE CFP001S1
            IF WS-FS-ALU NOT = 00 THEN
               MOVE WS-FS-ALU                           TO WS-MSG1
               MOVE 'ERRO FECHAR O ARQUIVO CFP001S1'    TO WS-MSG2
            PERFORM P800-ERRO THRU P999-EXIT
            END-IF.
       P700-EXIT.
            EXIT.

       S800 SECTION.
       P800-ERRO.
            PERFORM P950-DATA-HORA THRU P950-EXIT
            DISPLAY '**********************************'
            DISPLAY '     ERRO PROGRAMA LISALUNO'
            DISPLAY '**********************************'
            DISPLAY ' MENSAGEM.....: ' WS-MSG2
            DISPLAY ' FILE STATUS..: ' WS-MSG1
            DISPLAY '**********************************'
            DISPLAY ' DATA-HORA....: ' WS-DATA-HORA-FORMAT
            DISPLAY '**********************************'
            MOVE 8 TO RETURN-CODE.
       P800-EXIT.
            EXIT.

       S999 SECTION.
       P999-FIM.
            PERFORM P950-DATA-HORA THRU P950-EXIT
            MOVE WS-DATA-HORA-FORMAT TO WS-DATA-HORA-FIM
            DISPLAY ' '
            DISPLAY '---------------------------------------'
            DISPLAY '>>>> PROGRAMA LISALUNO FINALIZADO <<<<'
            DISPLAY '---------------------------------------'
            DISPLAY ' QTDE CONTATOS READ.: ' WS-I-ALU
            DISPLAY '---------------------------------------'
            DISPLAY ' DATA-HORA INICIO...: ' WS-DATA-HORA-INI
            DISPLAY ' DATA-HORA FINAL....: ' WS-DATA-HORA-FIM
            DISPLAY '---------------------------------------'
            GOBACK.
       P999-EXIT.
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
       END PROGRAM LISALUNO.
