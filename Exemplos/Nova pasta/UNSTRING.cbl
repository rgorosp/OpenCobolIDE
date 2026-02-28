      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 29-06-22
      * Purpose: COMANDO UNSTRING TRABALHANDO COM SAIDA DE CAMPOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-UNSTRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-COMPLETO         PIC X(30) VALUE SPACES.
       01  WS-NOME.
           03 WS-PRI-NOME      PIC X(10) VALUE SPACES.
           03 WS-MEI-NOME      PIC X(10) VALUE SPACES.
           03 WS-FIM-NOME      PIC X(10) VALUE SPACES.
       77  WS-MOSTRA           PIC X(50) VALUE SPACES.
       01  WS-COUNT.
           03 WS-PONTEIRO      PIC 9(02) VALUE ZEROS.
           03 WS-TOT-CAMPOS    PIC 9(02) VALUE ZEROS.
           03 WS-TAM1          PIC 9(02) VALUE ZEROS.
           03 WS-TAM2          PIC 9(02) VALUE ZEROS.
           03 WS-TAM3          PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INI.
      *      PERFORM P013-EX3 THRU P013-EXIT
            PERFORM S010-EXEMPLOS
            PERFORM P901-TERM.
       P001-EXIT.

       S010-EXEMPLOS SECTION.
       P011-EX1.
      * >>>>>>>>>>>>>>>>>>>>> EXEMPLO 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *     OBS. APENAS UM ESPACO ENTRE AS PALAVRAS
            MOVE 'EMERSON SILVA MOTTA'   TO WS-COMPLETO

            UNSTRING
               WS-COMPLETO
               DELIMITED BY SPACES
               INTO WS-PRI-NOME
                    WS-MEI-NOME
                    WS-FIM-NOME
            END-UNSTRING

            DISPLAY '----------------------------------------'
            DISPLAY ' EXEMPLO: 1'
            DISPLAY '----------------------------------------'
            DISPLAY 'PRIMEIRO NOME: ' WS-PRI-NOME
            DISPLAY 'NOME DO MEIO.: ' WS-MEI-NOME
            DISPLAY 'SOBRENOME....: ' WS-FIM-NOME
            DISPLAY '----------------------------------------'
            DISPLAY ' '
            PERFORM P801-INI THRU P801-EXIT.
       P011-EXIT.
            EXIT.

       P012-EX2.
      * >>>>>>>>>>>>>>>>>>>>> EXEMPLO 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *     OBS. MAIS DE UM PONTO E VIRGULA ENTRE AS PALAVRAS USE ALL
            MOVE 'EMERSON;;;SILVA;MOTTA;'   TO WS-COMPLETO

            UNSTRING
               WS-COMPLETO
               DELIMITED BY ALL ';'
               INTO WS-PRI-NOME
                    WS-MEI-NOME
                    WS-FIM-NOME
            END-UNSTRING

            DISPLAY '----------------------------------------'
            DISPLAY ' EXEMPLO: 2'
            DISPLAY '----------------------------------------'
            DISPLAY 'PRIMEIRO NOME: ' WS-PRI-NOME
            DISPLAY 'NOME DO MEIO.: ' WS-MEI-NOME
            DISPLAY 'SOBRENOME....: ' WS-FIM-NOME
            DISPLAY '----------------------------------------'
            DISPLAY ' '
            PERFORM P801-INI THRU P801-EXIT.
       P012-EXIT.
            EXIT.

       P013-EX3.
      * >>>>>>>>>>>>>>>>>>>>> EXEMPLO 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *     OBS. MAIS DE UM ESPACO ENTRE AS PALAVRAS MAS USANDO
      *     PONTEIROS E TOTAL DE CAMPOS
      *           12345678901234567890123456
            MOVE 'EMERSON DA  SILVA    MOTTA' TO WS-COMPLETO
            SET WS-PONTEIRO                   TO 13

            MOVE WS-COMPLETO(1:7) TO WS-PRI-NOME
            UNSTRING
               WS-COMPLETO
               DELIMITED BY ALL SPACES
               INTO WS-MEI-NOME
                    WS-FIM-NOME
               WITH POINTER WS-PONTEIRO
               TALLYING IN WS-TOT-CAMPOS
            END-UNSTRING

            ADD 1 TO WS-TOT-CAMPOS

            DISPLAY '----------------------------------------'
            DISPLAY ' EXEMPLO: 3'
            DISPLAY '----------------------------------------'
            DISPLAY 'PRIMEIRO NOME: ' WS-PRI-NOME
            DISPLAY 'NOME DO MEIO.: ' WS-MEI-NOME
            DISPLAY 'SOBRENOME....: ' WS-FIM-NOME
            DISPLAY 'PONTEIRO.....: ' WS-PONTEIRO
            DISPLAY 'TOTAL CAMPOS.: ' WS-TOT-CAMPOS
            DISPLAY '----------------------------------------'
            DISPLAY ' '
            PERFORM P801-INI THRU P801-EXIT.
       P013-EXIT.

       P014-EX4.
      * >>>>>>>>>>>>>>>>>>>>> EXEMPLO 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *     OBS. VARIOS TIPOS DE CARACTERES PARA FAZER A SEPARAÇÃO,
      *     PONTEIROS E TOTAL DE CAMPOS
      *           1234567890123456789012345
            MOVE '*EMERSON;DA  SILVA;MOTTA*'   TO WS-COMPLETO
            SET WS-PONTEIRO                    TO 14

            MOVE WS-COMPLETO(2:7) TO WS-PRI-NOME

            UNSTRING
               WS-COMPLETO
               DELIMITED BY ALL SPACES OR ALL ';' OR ALL '*'
               INTO WS-MEI-NOME COUNT IN WS-TAM2
                    WS-FIM-NOME COUNT IN WS-TAM3
               WITH POINTER WS-PONTEIRO
               TALLYING IN WS-TOT-CAMPOS
            END-UNSTRING

            ADD 1 TO WS-TOT-CAMPOS

            COMPUTE WS-TAM1 = FUNCTION LENGTH (WS-COMPLETO(2:7))

            DISPLAY '----------------------------------------'
            DISPLAY ' EXEMPLO: 4'
            DISPLAY '----------------------------------------'
            DISPLAY 'PRIMEIRO NOME: ' WS-PRI-NOME
            DISPLAY 'NOME DO MEIO.: ' WS-MEI-NOME
            DISPLAY 'SOBRENOME....: ' WS-FIM-NOME
            DISPLAY 'PONTEIRO.....: ' WS-PONTEIRO
            DISPLAY 'TOTAL CAMPOS.: ' WS-TOT-CAMPOS
            DISPLAY "TAM CAMPO1...: " WS-TAM1
            DISPLAY "TAM CAMPO2...: " WS-TAM2
            DISPLAY "TAM CAMPO3...: " WS-TAM3
            DISPLAY '----------------------------------------'
            DISPLAY ' '
            PERFORM P801-INI THRU P801-EXIT.
       P014-EXIT.
            EXIT.

       S800-INICIALIZA SECTION.
       P801-INI.
            INITIALIZE WS-COMPLETO
                       WS-NOME
                       WS-MOSTRA
                       WS-COUNT
            .
       P801-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TERM.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM PGM-UNSTRING.
