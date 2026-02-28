      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 12-04-25
      * Purpose: PROGRAMA PARA CALCULAR MEDIA ESCOLA E APROVACAO
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOLESCOLA2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-PARM.
           03 WS-NOTA1        PIC 9(02) VALUE ZEROS.
           03 WS-NOTA2        PIC 9(02) VALUE ZEROS.
           03 WS-NOTA3        PIC 9(02) VALUE ZEROS.
           03 WS-NOTA4        PIC 9(02) VALUE ZEROS.
           03 WS-RESULT       PIC 9(02) VALUE ZEROS.
           03 WS-MATERIA      PIC X(20) VALUE SPACES.
           03 WS-STATUS       PIC X(10) VALUE SPACES.
      *
       PROCEDURE DIVISION USING WS-PARM.
       S100-INICIO SECTION.
       P101-INI.
            DISPLAY "***** SUB-ROTINA BOLESCOLA2 *****"
            DISPLAY "NOTAS RECEBIDAS: " WS-NOTA1 ' - '
                                        WS-NOTA2 ' - '
                                        WS-NOTA3 ' - '
                                        WS-NOTA4

            COMPUTE WS-RESULT = (WS-NOTA1 + WS-NOTA2 + WS-NOTA3 +
                                 WS-NOTA4) / 4
                    ON SIZE ERROR DISPLAY "ESTOURO DE CAMPO"
                       STOP RUN
                NOT ON SIZE ERROR
                       CONTINUE
            END-COMPUTE

            IF WS-RESULT >= 7
               MOVE 'APROVADO' TO WS-STATUS
            ELSE
               MOVE 'REPROVADO' TO WS-STATUS
            END-IF

            DISPLAY "SUB-ROTINA EXECUTADA COM SUCESSO".
       P101-EXIT.
            EXIT.

       S900-FIM SECTION.
       P901-FIM.
            GOBACK.
       P901-EXIT.
            EXIT.
       END PROGRAM BOLESCOLA2.
