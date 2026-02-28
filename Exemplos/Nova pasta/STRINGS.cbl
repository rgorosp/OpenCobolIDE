      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 06-03-24
      * Purpose: TRABALHANDO COM STRINGS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-STRINGS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-TM-1             PIC 99.
       77  WS-TM-2             PIC 99.
       77  WS-TM-3             PIC 99.
       77  WS-TM-4             PIC 99.
       COPY 'LAYOUT01'.
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            MOVE 'MARIA'        TO WS-PRIMEIRO-NOME
            MOVE 'CAMPOS'       TO WS-ULTIMO-NOME
            MOVE '551199115642' TO WS-TELEFONE
            MOVE 'RUA: DEZ, 03' TO WS-RUA
            MOVE 'SAO JOSE'     TO WS-BAIRRO
            MOVE 'SAO PAULO'    TO WS-CIDADE
            MOVE 'SP'           TO WS-UF
            MOVE '1234567'      TO WS-CEP
            MOVE 'BRASILEIRA'   TO WS-NACIONALIDADE
            MOVE 'ENFERMEIRA'   TO WS-PROFISSAO

            MOVE ZEROS          TO WS-TM-1 WS-TM-2 WS-TM-3 WS-TM-4

            INSPECT FUNCTION REVERSE(WS-PRIMEIRO-NOME)
                    TALLYING WS-TM-1 FOR LEADING ' '

            INSPECT FUNCTION REVERSE(WS-RUA)
                    TALLYING WS-TM-2 FOR LEADING ' '

            INSPECT FUNCTION REVERSE(WS-BAIRRO)
                    TALLYING WS-TM-3 FOR LEADING ' '

            INSPECT FUNCTION REVERSE(WS-CIDADE)
                    TALLYING WS-TM-4 FOR LEADING ' '

            DISPLAY '1- NOME COMPLETO: ' WS-PRIMEIRO-NOME
            (1:(FUNCTION LENGTH(WS-PRIMEIRO-NOME) - WS-TM-1))
            ' ' WS-ULTIMO-NOME

            DISPLAY '2- TELEFONE.....: ' '+'WS-PAIS ' (' WS-DDD ')'
                    WS-PREFIXO '-' WS-SUFIXO

            DISPLAY '3- ENDERECO.....: ' WS-RUA
            (1:(FUNCTION LENGTH(WS-RUA) - WS-TM-2)) ' - '
                                         WS-BAIRRO
            (1:(FUNCTION LENGTH(WS-BAIRRO) - WS-TM-3)) ' - '
                                         WS-CIDADE
            (1:(FUNCTION LENGTH(WS-CIDADE) - WS-TM-4)) ' - '
                                         WS-UF ' - '
            FUNCTION CONCATENATE('CEP: ' WS-CEP-1'-'WS-CEP-2)

            DISPLAY '4- NACIONALIDADE: ' WS-NACIONALIDADE
            DISPLAY '5- PROFISSAO....: ' WS-PROFISSAO
            .
       P000-EXIT.

       S999 SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM PGM-STRINGS.
