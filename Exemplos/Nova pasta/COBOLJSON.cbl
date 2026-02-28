      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 29-10-22
      * Purpose: GERANDO JSON PELO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLJSON.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  DADOS.
           02 LINHA            PIC X(80) VALUE 'LINHA: 1'.
       01  JTEXT NATIONAL      PIC N(2000).
       01  WS-I                PIC 99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            JSON GENERATE JTEXT FROM DADOS COUNT WS-I
                ON EXCEPTION
                   DISPLAY 'ERRO NA CONVERSAO JSON: ' JSON-CODE
                NOT ON EXCEPTION
                   DISPLAY 'JSON CRIADO!'
                   DISPLAY JTEXT(1:WS-I)
                   DISPLAY FUNCTION DISPLAY-OF(JTEXT(1:WS-I))
                   DISPLAY 'VALOR-CONTADOR: ' WS-I
            END-JSON
            STOP RUN.
       END PROGRAM COBOLJSON.
