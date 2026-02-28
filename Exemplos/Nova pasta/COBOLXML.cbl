      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 29-10-22
      * Purpose: GERANDO XML PELO COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOLXML.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  DADOS.
           02 REGISTRO.
              03 NOME          PIC X(20).
              03 ENDERECO      PIC X(40).
       01  XMLTEXT             PIC N(2000).
       01  CONTADOR            PIC 9999.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 'JONAS BLOCK' TO NOME
            MOVE 'AVENIDA IPIRANGA, 130' TO ENDERECO
            XML GENERATE XMLTEXT FROM DADOS COUNT CONTADOR
                ON EXCEPTION
                   DISPLAY 'ERRO NA CONVERSAO XML: ' XML-CODE
                NOT ON EXCEPTION
                   DISPLAY 'XML CRIADO!'
                   DISPLAY XMLTEXT(1:CONTADOR)
                   DISPLAY FUNCTION DISPLAY-OF(XMLTEXT(1:CONTADOR))
                   DISPLAY 'VALOR-CONTADOR: ' CONTADOR
            END-XML
            STOP RUN.
       END PROGRAM COBOLXML.
