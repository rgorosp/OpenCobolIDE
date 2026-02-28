      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 26-02-26
      * Purpose: MEU TERCEIRO PROGRAMA
      * Tectonics: IVEE COBOL E MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY_C.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-NOME-A    PIC X(30) VALUE SPACES.
       77 WS-NOME-B    PIC X(30) VALUE SPACES.
       77 WS-IDADE-A   PIC 9(02) VALUE ZEROS.
       77 WS-IDADE-B   PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P000-INICIO.
            DISPLAY "DISPLAY WS-NOME-A  = " WS-NOME-A
            DISPLAY "DISPLAY WS-NOME-B  = " WS-NOME-B
            DISPLAY "DISPLAY WS-IDADE-A = " WS-IDADE-A
            DISPLAY "DISPLAY WS-IDADE-B = " WS-IDADE-B
            DISPLAY " "

            DISPLAY "INSERIR SEU NOME: "
            ACCEPT WS-NOME-A

            DISPLAY "INSERIR SUA IDADE: "
            ACCEPT WS-IDADE-A

            MOVE WS-NOME-A  TO WS-NOME-B
            MOVE WS-IDADE-A TO WS-IDADE-B
            DISPLAY " "

            DISPLAY "DISPLAY MOVE WS-NOME-B  = " WS-NOME-B
            DISPLAY "DISPLAY MOVE WS-IDADE-B = " WS-IDADE-B
            STOP RUN.
       P000-EXIT. EXIT.
       END PROGRAM DISPLAY_C.
