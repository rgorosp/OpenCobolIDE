      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 12-04-25
      * Purpose: PROGRAMA DE BOLETIM ESCOLAR RECEBE 4 NOTAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOLESCOLA1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
              DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-PARM.
           03 WS-N1           PIC 9(02) VALUE ZEROS.
           03 WS-N2           PIC 9(02) VALUE ZEROS.
           03 WS-N3           PIC 9(02) VALUE ZEROS.
           03 WS-N4           PIC 9(02) VALUE ZEROS.
           03 WS-RES          PIC 9(02) VALUE ZEROS.
           03 WS-MATERIA      PIC X(20) VALUE SPACES.
           03 WS-STATUS       PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       S100-INICIO SECTION.
       P101-INI.
            DISPLAY "***** PROGRAMA PRINCIPAL *****"

            DISPLAY "DIGITE O NOME DA MATERIA: "
            ACCEPT WS-MATERIA.
       P102-N1.
            DISPLAY "DIGITE A PRIMEIRA NOTA..: "
            ACCEPT WS-N1
            IF WS-N1 IS NUMERIC
               CONTINUE
            ELSE
               DISPLAY 'VALOR INVALIDO, DIGITE NOVAMENTE!'
            GO TO P102-N1
            END-IF.
       P103-N2.
            DISPLAY "DIGITE A SEGUNDA NOTA..: "
            ACCEPT WS-N2
            IF WS-N2 IS NUMERIC
               CONTINUE
            ELSE
               DISPLAY 'VALOR INVALIDO, DIGITE NOVAMENTE!'
            GO TO P103-N2
            END-IF.
       P104-N3.
            DISPLAY "DIGITE A TERCEIRA NOTA..: "
            ACCEPT WS-N3
            IF WS-N3 IS NUMERIC
               CONTINUE
            ELSE
               DISPLAY 'VALOR INVALIDO, DIGITE NOVAMENTE!'
            GO TO P104-N3
            END-IF.
       P105-N4.
            DISPLAY "DIGITE A QUARTA NOTA..: "
            ACCEPT WS-N4
            IF WS-N4 IS NUMERIC
               CONTINUE
            ELSE
               DISPLAY 'VALOR INVALIDO, DIGITE NOVAMENTE!'
            GO TO P105-N4
            END-IF

            CALL "BOLESCOLA2" USING WS-PARM

            DISPLAY 'MATERIA: ' WS-MATERIA
            DISPLAY 'NOTA...: ' WS-RES
            DISPLAY 'STATUS.: ' WS-STATUS
            DISPLAY ' '
            DISPLAY "PROGRAMA EXECUTADO COM SUCESSO".
       P101-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TER.
            GOBACK.
       P901-EXIT.
            EXIT.
       END PROGRAM BOLESCOLA1.
