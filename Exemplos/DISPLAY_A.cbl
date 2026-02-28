      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 26-02-26
      * Purpose: MEU PRIMEIRO PROGRAMA
      * Tectonics: IVEE COBOL A DISTANCIA
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY_A.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-NM-ALUNO           PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P000-INICIO.
            DISPLAY "DIGITE O NOME DO ALUNO: "
            ACCEPT WS-NM-ALUNO

            DISPLAY 'NOME DO ALUNO: ' WS-NM-ALUNO

            STOP RUN.
       P000-EXIT. EXIT.
       END PROGRAM DISPLAY_A.
