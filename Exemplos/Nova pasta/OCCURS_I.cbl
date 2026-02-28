      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 13-05-23
      * Purpose: TRABALHANDO COM MATRIZES BIDIMENSIONAIS
      *          NAO INDEXADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OCCURS_H.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT S-ENTRADA
           ASSIGN TO  "C:/Arquivos/E1C5A1P01.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS  IS WS-FS-ENTRA.
       DATA DIVISION.
       FILE SECTION.
       FD  S-ENTRADA.
           COPY REGENTRA.
       WORKING-STORAGE SECTION.
       01  FILLER                    PIC X(40).
           88 WS-INICIO              VALUE
           '********* INICIO DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       01  WS-TABELA.
           03 WS-CODIGO              OCCURS 100 TIMES.
              05 WS-CODIGO-TAB       PIC 9(02) VALUE ZEROS.
              05 WS-SIGLA-TAB        PIC X(02) VALUE SPACES.
              05 WS-NOME-TAB         PIC X(06) VALUE SPACES.
              05 VALOR-ENT-TAB       PIC 9(36) VALUE ZEROS.

       01  WS-TABELA2.
              05 VALOR-ENT-TAB2      OCCURS 12 TIMES.
                 07 VLR-ENTRADA      PIC 9(03) VALUE ZEROS.

       77  WS-IND1                   PIC 999 VALUE ZEROS.
       77  WS-IND2                   PIC 999 VALUE ZEROS.
       77  WS-IND3                   PIC 999 VALUE ZEROS.
       77  WS-VALOR-TOT              PIC $Z.Z99,99 VALUE ZEROS.
       77  WS-FS-ENTRA               PIC 9(02) VALUE ZEROS.
       77  WS-LIDOS                  PIC 9(02) VALUE ZEROS.
       77  WS-CODIGO-ENT             PIC 9(02) VALUE ZEROS.
       77  WS-SOMA                   PIC 9(05) VALUE ZEROS.

       01  FILLER                    PIC X(40).
           88 WS-FINAL               VALUE
           '********* FINAL  DAS VARIAVEIS *********'.
      * --> 88 - UTILIZADO PARA VALORES FIXOS.

       PROCEDURE DIVISION.
       S000 SECTION.
       P001-INICIO.
           INITIALIZE WS-TABELA
                      WS-IND1 WS-IND2 WS-IND3
                      WS-VALOR-TOT WS-LIDOS

           PERFORM P051-ABRIR
           PERFORM P052-LEITURA
           PERFORM P101-PROCESSA UNTIL WS-FS-ENTRA = 10
           PERFORM P201-FECHAR
           PERFORM P901-TERMINO.
       P001-EXIT.
            EXIT.

       S050 SECTION.
       P051-ABRIR.
            OPEN INPUT S-ENTRADA
            EVALUATE WS-FS-ENTRA
                WHEN 0
                 CONTINUE
                WHEN 10
                 DISPLAY 'SEM MOVIMENTO, FAVOR VERIFICAR'
                 MOVE 8 TO RETURN-CODE
                 STOP RUN
                WHEN OTHER
                 DISPLAY 'FILE STATUS: ' WS-FS-ENTRA
                 DISPLAY 'PROCESSO ANORMAL NO P051-ABRIR'
                 DISPLAY 'FAVOR VERIFICAR'
                 STOP RUN
            END-EVALUATE
            .
       P051-EXIT.

       P052-LEITURA.
            READ S-ENTRADA
            EVALUATE WS-FS-ENTRA
                WHEN 0
                 MOVE CODIGO-ENT TO WS-CODIGO-ENT
                 ADD 1 TO WS-LIDOS
                WHEN 10
                     CONTINUE
                WHEN OTHER
                 DISPLAY 'FILE STATUS: ' WS-FS-ENTRA
                 DISPLAY 'PROCESSO ANORMAL NO P052-LEITURA!'
                 DISPLAY 'FAVOR VERIFICAR'
                 STOP RUN
            END-EVALUATE
            .
       P052-EXIT.
            EXIT.

       S100 SECTION.
       P101-PROCESSA.
            PERFORM VARYING WS-IND1 FROM 1 BY 1
                      UNTIL WS-FS-ENTRA = 10
                MOVE CODIGO-ENT TO WS-CODIGO-TAB(WS-IND1)
                MOVE SIGLA-ENT  TO WS-SIGLA-TAB(WS-IND1)
                MOVE DESCRI-ENT TO WS-NOME-TAB(WS-IND1)
                MOVE VALOR-ENT  TO VALOR-ENT-TAB(WS-IND1)
                MOVE VALOR-ENT-TAB(WS-IND1) TO WS-TABELA2

                PERFORM VARYING WS-IND2 FROM 1 BY 1
                        UNTIL WS-IND2 GREATER 12
                COMPUTE WS-SOMA = WS-SOMA + VLR-ENTRADA(WS-IND2)
                MOVE WS-SOMA TO WS-VALOR-TOT
                END-PERFORM

                DISPLAY 'CODIGO: ' WS-CODIGO-TAB(WS-IND1)
                        ' - ESTADO: ' WS-SIGLA-TAB(WS-IND1)
                        ' - DESCRITIVO: ' WS-NOME-TAB(WS-IND1)
                        ' - VLR TOTAL: ' WS-VALOR-TOT

                MOVE ZEROS TO WS-SOMA
                PERFORM P052-LEITURA

            END-PERFORM
            .
       P101-EXIT.
            EXIT.

       S200 SECTION.
       P201-FECHAR.
            CLOSE S-ENTRADA
            EVALUATE WS-FS-ENTRA
                WHEN 0
                 CONTINUE
                WHEN OTHER
                 DISPLAY 'FILE STATUS: ' WS-FS-ENTRA
                 DISPLAY 'PROCESSO ANORMAL NO P201-FECHAR'
                 DISPLAY 'FAVOR VERIFICAR'
                 STOP RUN
            END-EVALUATE
            .
       P201-EXIT.
            EXIT.

       S900 SECTION.
       P901-TERMINO.
            DISPLAY ' '
            DISPLAY 'LIDOS ENTRADA: ' WS-LIDOS
            STOP RUN
            .
       P901-EXIT.
            EXIT.
       END PROGRAM OCCURS_H.
