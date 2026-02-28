      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 19-04-24
      * Purpose: QUARTO PROGRAMA
      * Tectonics: IVEE COBOL E MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE_D.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-NOME                  PIC X(10) VALUE SPACES.
       01 WS-SAL-HORA              PIC 9(05) VALUE ZEROS.
       01 WS-HRS-TRAB              PIC 9(02) VALUE ZEROS.
       01 WS-SAL-BRUTO             PIC 9(07) VALUE ZEROS.
       01 WS-SAL-BRUTO-E           PIC ZZZ.Z99,99.
       01 WS-SAL-LIQUIDO           PIC 9(07) VALUE ZEROS.
       01 WS-SAL-LIQ               PIC 9(07) VALUE ZEROS.
       01 WS-FUNC                  PIC X.
       PROCEDURE DIVISION.
       S000-INI SECTION.
       P001-INI.
            DISPLAY "QUAL O NOME DO FUNCIONARIO:"
            ACCEPT WS-NOME

            DISPLAY "DIGITE O VALOR SALARIO HORA:"
            ACCEPT WS-SAL-HORA

            DISPLAY "DIGITE QUANTIDADE DE HORAS TRABALHADAS:"
            ACCEPT  WS-HRS-TRAB

            COMPUTE WS-SAL-LIQUIDO = (WS-SAL-HORA * WS-HRS-TRAB)
                    ON SIZE ERROR DISPLAY 'COMPUTE1 - ESTOURO'
                    PERFORM P901-FIM
            MULTIPLY WS-SAL-LIQUIDO BY 0.10 GIVING WS-SAL-LIQ
            END-COMPUTE

            COMPUTE WS-SAL-BRUTO = (WS-SAL-LIQUIDO - WS-SAL-LIQ)
                    ON SIZE ERROR DISPLAY 'COMPUTE2 - ESTOURO'
                    PERFORM P901-FIM
            END-COMPUTE

            MOVE WS-SAL-BRUTO TO WS-SAL-BRUTO-E

            DISPLAY "SALARIO LIQUIDO: " WS-SAL-LIQUIDO
            DISPLAY "SALARIO BRUTO..: " WS-SAL-BRUTO-E
            DISPLAY ' '
            DISPLAY "CONSULTAR OUTRO FUNCIONARIO? S OU N"

            ACCEPT WS-FUNC
            IF WS-FUNC = 'S' THEN
               GO TO P001-INI
            ELSE
               CONTINUE
            END-IF.
       P001-EXIT.
            EXIT.

       S900-FIM SECTION.
       P901-FIM.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM COMPUTE_D.
