      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 11-12-2024
      * Purpose: realiza a conversão de ASCII para EBCDIC
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ASCII-TO-EBCDIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Tabela de conversão ASCII para EBCDIC
       01  ASCII-TO-EBCDIC-TABLE.
           05 ASCII-CODE  OCCURS 256 TIMES PIC X VALUE LOW-VALUE.
           05 EBCDIC-CODE OCCURS 256 TIMES PIC X VALUE LOW-VALUE.

      * Texto de entrada e saída
       01  ASCII-TEXT  PIC X(50) VALUE 'Texto em ASCII para converter.'.
       01  EBCDIC-TEXT PIC X(50).

       PROCEDURE DIVISION.
           PERFORM INITIALIZE-TABLES
           DISPLAY "Texto original (ASCII): " ASCII-TEXT

           PERFORM CONVERT-ASCII-TO-EBCDIC
           DISPLAY "Texto convertido (EBCDIC): " EBCDIC-TEXT

           STOP RUN.

       INITIALIZE-TABLES.
           MOVE LOW-VALUE TO ASCII-CODE (ALL) EBCDIC-CODE (ALL).
           MOVE "A" TO ASCII-CODE (65)   EBCDIC-CODE (65).
           MOVE "B" TO ASCII-CODE (66)   EBCDIC-CODE (66).
           MOVE "C" TO ASCII-CODE (67)   EBCDIC-CODE (67).
      * (Adicione todos os outros caracteres conforme necessário).

       CONVERT-ASCII-TO-EBCDIC.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LENGTH OF ASCII-TEXT
            MOVE ASCII-TEXT (IDX:1) TO TEMP-CHAR
            PERFORM VARYING IDX-TABLE FROM 1 BY 1 UNTIL IDX-TABLE > 256
                IF TEMP-CHAR = ASCII-CODE (IDX-TABLE)
                    MOVE EBCDIC-CODE (IDX-TABLE) TO EBCDIC-TEXT (IDX:1)
                END-IF
            END-PERFORM
           END-PERFORM.
