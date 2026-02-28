      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 06-10-22
      * Purpose: PROGRAMA RECEBE A NOTA DOS ALUNOS PARA VERIFICAR SE
      *    O ALUNO FOI APROVADO OU REPROVADO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH000000A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-N1           PIC 9(02).
           03 WS-N2           PIC 9(02).
           03 WS-N3           PIC 9(02).
           03 WS-N4           PIC 9(02).
           03 WS-RS           PIC 9(02).
           03 WS-MOSTRA       PIC X(09).
       77  WS-ALUNO           PIC X(20).
       77  WS-MATERIA         PIC X(20).
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            DISPLAY "***** INICIO DO PROGRAMA *****"
            INITIALISE WS-VAR
            PERFORM P010-PROCESSA  THRU P010-EXIT
            PERFORM P050-RELATORIO THRU P050-EXIT
            PERFORM P999-FIM       THRU P999-EXIT.
       P000-EXIT.

       S010 SECTION.
       P010-PROCESSA.
            DISPLAY "DIGITE O NOME DO ALUNO.....: "
            ACCEPT WS-ALUNO

            DISPLAY " "
            DISPLAY "DIGITE A MATERIA DO ALUNO..: "
            ACCEPT WS-MATERIA.

       S0N1 SECTION.
            DISPLAY " "
            DISPLAY "DIGITE A PRIMEIRA NOTA..: "
            ACCEPT WS-N1

            IF WS-N1 IS NUMERIC AND WS-N1 NOT EQUAL ZEROS THEN
               DISPLAY "NOTA " WS-N1 " RECEBIDA!"
            ELSE
               PERFORM S0N1
            END-IF.

       S0N2 SECTION.
            DISPLAY " "
            DISPLAY "DIGITE A SEGUNDA NOTA..: "
            ACCEPT WS-N2

            IF WS-N2 IS NUMERIC AND WS-N2 NOT EQUAL ZEROS THEN
               DISPLAY "NOTA " WS-N2 " RECEBIDA!"
            ELSE
               PERFORM S0N2
            END-IF.

       S0N3 SECTION.
            DISPLAY " "
            DISPLAY "DIGITE A TERCEIRA NOTA..: "
            ACCEPT WS-N3

            IF WS-N3 IS NUMERIC AND WS-N3 NOT EQUAL ZEROS THEN
               DISPLAY "NOTA " WS-N3 " RECEBIDA!"
            ELSE
               PERFORM S0N3
            END-IF.

       S0N4 SECTION.
            DISPLAY " "
            DISPLAY "DIGITE A QUARTA NOTA..: "
            ACCEPT WS-N4

            IF WS-N4 IS NUMERIC AND WS-N4 NOT EQUAL ZEROS THEN
               DISPLAY "NOTA " WS-N4 " RECEBIDA!"
            ELSE
               PERFORM S0N4
            END-IF

            CALL 'CH000000B' USING WS-VAR.
       P010-EXIT.

       S050 SECTION.
       P050-RELATORIO.
            DISPLAY " "
            DISPLAY "-------------------------------------"
            DISPLAY "       PROGRAMA: CH000000A"
            DISPLAY "-------------------------------------"
            DISPLAY " ALUNO.......: " WS-ALUNO
            DISPLAY " MATERIA.....: " WS-MATERIA
            DISPLAY " NOTA FINAL..: " WS-RS
            DISPLAY " STATUS......: " WS-MOSTRA
            DISPLAY "-------------------------------------".
       P050-EXIT.


       S999 SECTION.
       P999-FIM.
            DISPLAY "***** FIM DO PROGRAMA *****"
            GOBACK.
       P999-EXIT.
       END PROGRAM CH000000A.
