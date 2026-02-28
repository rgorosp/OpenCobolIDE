      ****************************************************************
      * Author: EMERSON S MOTTA
      * Date: 12/04/25.
      * Purpose: TRABALHANDO COM INSPECT REVERSE E FUNCTION
      * Tectonics: cobc
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INSPECT_B.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03  WS-TAM-1        PIC 99 VALUE ZEROS.
           03  WS-TAM-2        PIC 99 VALUE ZEROS.
           03  WS-TAM-3        PIC 99 VALUE ZEROS.
           03  WS-TAM-4        PIC 99 VALUE ZEROS.
       COPY LAYOUT01.
      *
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INI.
            DISPLAY "****** INICIO DO PROGRAMA INSPECT_B *****"
            DISPLAY ' '

            INITIALIZE WS-VAR REPLACING NUMERIC BY ZEROS

            PERFORM P101-PRO
            PERFORM P901-TER
            .
       P001-EXIT.
            EXIT.

       S100-PROCESSA SECTION.
       P101-PRO.
            MOVE "MARIA"         TO WS-PRIMEIRO-NOME

            INSPECT FUNCTION REVERSE(WS-PRIMEIRO-NOME)
               TALLYING WS-TAM-1 FOR LEADING ' '

            MOVE "CAMPOS"        TO WS-ULTIMO-NOME

            DISPLAY "1- NOME COMPLETO: " WS-PRIMEIRO-NOME
               (1:FUNCTION LENGTH(WS-PRIMEIRO-NOME) - WS-TAM-1)
                                         ' '
                                         WS-ULTIMO-NOME

            MOVE "5511981223434" TO WS-TELEFONE
            DISPLAY "2- TELEFONE.....: " '+' WS-PAIS ' '
                                         '(' WS-DDD ') '
                                         WS-PREFIXO
                                         '-'
                                         WS-SUFIXO

            MOVE ZEROS TO WS-TAM-1
            MOVE "RUA DEZ, 03"   TO WS-RUA
            INSPECT FUNCTION REVERSE(WS-RUA)
               TALLYING WS-TAM-1 FOR LEADING ' '

            MOVE "SAO JOSE"      TO WS-BAIRRO
            INSPECT FUNCTION REVERSE(WS-BAIRRO)
               TALLYING WS-TAM-2 FOR LEADING ' '

            MOVE "SAO PAULO"     TO WS-CIDADE
            INSPECT FUNCTION REVERSE(WS-CIDADE)
               TALLYING WS-TAM-3 FOR LEADING ' '

            MOVE "SP"            TO WS-UF

            MOVE "03463"         TO WS-CEP-1
            MOVE "050"           TO WS-CEP-2

            DISPLAY "3- ENDERECO.....: " WS-RUA
               (1:FUNCTION LENGTH(WS-RUA) - WS-TAM-1)
                                 ' - '
                                 WS-BAIRRO
               (1:FUNCTION LENGTH(WS-BAIRRO) - WS-TAM-2)
                                 ' - '
                                 WS-CIDADE
               (1:FUNCTION LENGTH(WS-CIDADE) - WS-TAM-3)
                                 ' - '
                                 WS-UF
                                 ' - '
                  FUNCTION CONCATENATE('CEP: '
                                       WS-CEP-1
                                       '-'
                                       WS-CEP-2)

            MOVE "BRASILEIRA"    TO WS-NACIONALIDADE
            DISPLAY "4- NACIONALIDADE: " WS-NACIONALIDADE

            MOVE "ENFERMEIRA"    TO WS-PROFISSAO
            DISPLAY "5- PROFISSAO....: " WS-PROFISSAO
            .
       P101-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TER.
            DISPLAY ' '
            DISPLAY "****** TERMINO DO PROGRAMA INSPECT_B *****"
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM INSPECT_B.
