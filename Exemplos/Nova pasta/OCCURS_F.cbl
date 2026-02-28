      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14-05-23
      * Purpose: TRABALHANDO COM MATRIZES BIDIMENSIONAIS
      *          INDEXADAS - BUSCA POR CHAVE TABELA DE CONTROLE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_F.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT ESTADOS ASSIGN
                   'C:\Arquivos\ARQ_OCCURS_F.txt'
                   ORGANIZATION    IS LINE SEQUENTIAL
                   FILE STATUS     IS WS-FS-EST
            .
       DATA DIVISION.
       FILE SECTION.
       FD  ESTADOS.
           01 WS-ESTADOS.
               03 FILLER            PIC X(378).
      *
       WORKING-STORAGE SECTION.
       01  FILLER                   PIC X(40).
           88 WS-INICIO             VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       01  WS-AREA.
           03 WS-FS-EST             PIC 9(02) VALUE ZEROS.
           03 WS-I-EST              PIC 9(05) VALUE ZEROS.

       01  WS-MATRIZ.
           03 WS-REGISTRO           OCCURS  30 TIMES
                                    ASCENDING KEY IS WS-CHAVE
                                    INDEXED BY I.
              05 WS-CHAVE           PIC 99.
              05 WS-NOME            PIC X(02).

       77  WS-CODIGO                PIC 99.

       01  FILLER                   PIC X(40).
           88 WS-FINAL              VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.
       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.

            INITIALIZE WS-AREA

            PERFORM P101-ABERTURA
            PERFORM P201-LEITURA UNTIL WS-FS-EST = 10
            PERFORM P501-PROCESSA
            PERFORM P801-FECHAMENTO
            PERFORM P901-TERMINO
            .
       P001-EXIT.
            EXIT.

       S100 SECTION.
       P101-ABERTURA.
           OPEN INPUT ESTADOS
           IF WS-FS-EST NOT EQUAL '00'
              DISPLAY 'ERRO AO ABRIR O ARQUIVO ESTADOS'
              DISPLAY 'FS = ' WS-FS-EST
              STOP RUN
           END-IF
           .
       P101-EXIT.
            EXIT.

       S200 SECTION.
       P201-LEITURA.
           READ ESTADOS
           IF WS-FS-EST NOT EQUAL '00' AND '10'
              DISPLAY 'ERRO NA LEITURA DO ARQUIVO ESTADOS'
              DISPLAY 'FS = ' WS-FS-EST
           ELSE
               IF WS-FS-EST EQUAL '00'
                  ADD 1 TO WS-I-EST
                  MOVE WS-ESTADOS TO WS-MATRIZ
               END-IF
           END-IF
           .
       P201-EXIT.
           EXIT.

       S500 SECTION.
       P501-PROCESSA.
            DISPLAY WS-MATRIZ
            DISPLAY ' '
            MOVE 05 TO WS-CODIGO
            SEARCH ALL WS-REGISTRO
               AT END
                  DISPLAY 'CHAVE NAO ENCONTRADA'
                  MOVE 8 TO RETURN-CODE
               WHEN WS-CHAVE(I) = WS-CODIGO
                  DISPLAY 'CHAVE ENCONTRADA: '
                  WS-CHAVE(I) ' - ' WS-NOME(I)
                  ' POSICAO: ' I
            END-SEARCH
           .
       P501-EXIT.
            EXIT.

       S800 SECTION.
       P801-FECHAMENTO.
           CLOSE ESTADOS
           IF WS-FS-EST NOT EQUAL '00'
              DISPLAY 'ERRO AO FECHAR O ARQUIVO ESTADOS'
              DISPLAY 'FS = ' WS-FS-EST
              STOP RUN
           END-IF
           .
       P801-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            DISPLAY ' '
            DISPLAY '-------------------------'
            DISPLAY 'REG.ESTADOS = ' WS-I-EST
            DISPLAY '-------------------------'
            STOP RUN
            .
       P901-EXIT.
            EXIT.
       END PROGRAM OCCURS_F.
