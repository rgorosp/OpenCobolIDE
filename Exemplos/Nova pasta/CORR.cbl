      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-07-22.
      * Purpose: COMANDO CORR PARA CORRESPONDER DE UM LAYOUT PARA OUTRO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-CORR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-LAYOUT-1.
           03 WS-NOME          PIC X(30) VALUE SPACES.
           03 WS-ENDERECO      PIC X(20) VALUE SPACES.
           03 WS-TELEFONE      PIC X(20) VALUE SPACES.
           03 WS-EMAIL         PIC X(20) VALUE SPACES.
           03 WS-CIDADE        PIC X(30) VALUE SPACES.
           03 WS-ESTADO        PIC X(30) VALUE SPACES.

       01  WS-LAYOUT-2.
           03 WS-NOME          PIC X(30) VALUE SPACES.
           03 WS-ENDERECO      PIC X(20) VALUE SPACES.
           03 WS-CIDADE        PIC X(30) VALUE SPACES.
           03 WS-ESTADO        PIC X(30) VALUE SPACES.
           03 WS-EMAIL         PIC X(20) VALUE SPACES.
           03 WS-TELEFONE      PIC X(20) VALUE SPACES.
       PROCEDURE DIVISION.
       S0  SECTION.
       000-INICIO.
            DISPLAY "TAMANHO WS-LAYOUT-1: " LENGTH OF WS-LAYOUT-1
            DISPLAY "TAMANHO WS-LAYOUT-2: " LENGTH OF WS-LAYOUT-2

            MOVE 'EMERSON SOUSA'      TO WS-NOME     OF WS-LAYOUT-1
            MOVE 'RUA. FELICIO RUBAO' TO WS-ENDERECO OF WS-LAYOUT-1
            MOVE '(11) 98122.4345'    TO WS-TELEFONE OF WS-LAYOUT-1
            MOVE 'RGORRRJ@UOL.COM.BR' TO WS-EMAIL    OF WS-LAYOUT-1
            MOVE 'COPACABANA'         TO WS-CIDADE   OF WS-LAYOUT-1
            MOVE 'RIO DE JANEIRO'     TO WS-ESTADO   OF WS-LAYOUT-1

            MOVE CORR WS-LAYOUT-1 TO WS-LAYOUT-2

            DISPLAY "WS-LAYOUT-1: " WS-LAYOUT-1
            DISPLAY "WS-LAYOUT-2: " WS-LAYOUT-2.
       000-EXIT.

       S9-SECTION.
       999-FIM.
            STOP RUN.
       999-EXIT.
       END PROGRAM PGM-CORR.
