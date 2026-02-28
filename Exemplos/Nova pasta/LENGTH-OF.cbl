      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-07-22
      * Purpose: COMANDO LENGTH VERIFICA O COMPRIMENTO DA VARIAVEL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-LENGTH.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-ENDERECO.
           03 WS-RUA           PIC X(30) VALUE SPACES.
           03 WS-BAIRRO        PIC X(20) VALUE SPACES.
           03 WS-CIDADE        PIC X(15) VALUE SPACES.
       77  WS-COMPRIMENTO      PIC 99 VALUE 0.
       PROCEDURE DIVISION.
       000-INICIO.
            DISPLAY "O COMPRIMENTO ENDERECO POSSUI: "
                    LENGTH OF WS-ENDERECO " CARACTERES"

            COMPUTE WS-COMPRIMENTO = FUNCTION LENGTH (WS-ENDERECO)

            DISPLAY "OUTRA FORMA USANDO A FUNCAO..: " WS-COMPRIMENTO
                    " CARACTERES".
       000-EXIT.

       999-FIM.
            STOP RUN.
       999-EXIT.
       END PROGRAM PGM-LENGTH.
