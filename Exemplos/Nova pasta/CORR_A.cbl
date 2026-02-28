      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-04-24
      * Purpose: QUINTO PROGRAMA
      * Tectonics: IVEE COBOL E MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVE_CORR.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-GRU-01.
           03 NOME-01          PIC X(10) VALUE SPACES.
           03 SOBRENOME-01     PIC X(10) VALUE SPACES.
       01  WS-GRU-02.
           03 NOME-01          PIC X(10) VALUE 'NOME'.
           03 SOBRENOME-01     PIC X(10) VALUE 'SOBRENOME'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "-------- VALORES NAS VARIAVEIS--------"
            DISPLAY "Grupo 01: " WS-GRU-01
            DISPLAY "Grupo 02: " WS-GRU-02
            DISPLAY "                    "

            DISPLAY "INSERIR O SEU NOME: "
            ACCEPT NOME-01 OF WS-GRU-01

            DISPLAY "INSERIR O SEU SOBRENOME: "
            ACCEPT SOBRENOME-01 OF WS-GRU-01

            MOVE CORRESPONDING WS-GRU-01 TO WS-GRU-02

            DISPLAY "-------- VALORES APOS MOVE--------"
            DISPLAY "Grupo 01: "
                    WS-GRU-01
            DISPLAY "Grupo 02: "
                    WS-GRU-02
            STOP RUN.
       END PROGRAM MOVE_CORR.
