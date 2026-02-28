      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 20-04-24
      * Purpose: SEXTO EXEMPLO
      * Tectonics: IVEE COBOL E MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C01A10P01.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NOME                 PIC A(10).
       77  WS-MATRICULA            PIC X(05).
       77  WS-SALBASE              PIC 9999.
       77  WS-VEND-MES             PIC 9999.
       77  WS-ABONO                PIC 9999 VALUE ZEROS.
       77  WS-IMPOSTO              PIC 9999 VALUE ZEROS.
       77  WS-SALMENSAL            PIC 9999 VALUE ZEROS.
       77  WS-CONTROLE             PIC X VALUE SPACE.
       PROCEDURE DIVISION.
       S000-INI SECTION.
       P000-INICIO.
            PERFORM P010-DISPLAY
            PERFORM P020-PROCESSO
            PERFORM P999-FIM.
       P000-EXIT.
            EXIT.

       S010-DIS SECTION.
       P010-DISPLAY.
            DISPLAY "DIGITE O NOME DO VENDEDOR....: "
            ACCEPT WS-NOME

            DISPLAY "DIGITE A SUA MATRICULA.......: "
            ACCEPT WS-MATRICULA

            DISPLAY "DIGITE O SALARIO BASE........: "
            ACCEPT WS-SALBASE

            DISPLAY "DIGITE A QUANTIDADE DE VENDAS: "
            ACCEPT WS-VEND-MES.
       P010-EXIT.
            EXIT.

       S020-PRO SECTION.
       P020-PROCESSO.
            IF WS-VEND-MES > 1600 THEN
               MOVE 100 TO WS-ABONO
            ELSE
               MOVE 0   TO WS-ABONO
            END-IF

            IF WS-SALBASE <= 500 THEN
               MULTIPLY WS-SALBASE BY 0.10 GIVING WS-IMPOSTO
            ELSE
               IF WS-SALBASE > 500 AND WS-SALBASE < 700
                  MULTIPLY WS-SALBASE BY 0.20 GIVING WS-IMPOSTO
               ELSE
                  MULTIPLY WS-SALBASE BY 0.30 GIVING WS-IMPOSTO
               END-IF
            END-IF

            COMPUTE WS-SALMENSAL = (WS-SALBASE + WS-ABONO)
                    - WS-IMPOSTO

            DISPLAY "VALOR SALARIO MENSAL: " WS-SALMENSAL
            DISPLAY "EXISTE MAIS REGISTROS S(SIM) N(NAO)! "
            ACCEPT WS-CONTROLE

            IF WS-CONTROLE = 'S' OR WS-CONTROLE = 's' THEN
               GO TO P010-DISPLAY
            ELSE
               CONTINUE
            END-IF.
       P020-EXIT.
            EXIT.

       S999-FIM SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM C01A10P01.
