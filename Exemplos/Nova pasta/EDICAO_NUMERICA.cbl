      ****************************************************************
      * Author: EMERSON S MOTTA
      * Date: 12-04-25
      * Purpose: TRABALHANDO COM EDICAO NUMERICA
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EDICAO_NUMERICA.
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
           03 WS-INT           PIC  9(09).
           03 WS-DEC           PIC  9(09)V99.
           03 WS-SIN           PIC S9(09)V99.
       01  WS-MSK.
           03 WS-MSK-INT-1     PIC 999B999B999.
           03 WS-MSK-INT-2     PIC 999.999.999.
           03 WS-MSK-DEC-1     PIC ***.***.*,**.
           03 WS-MSK-DEC-2     PIC ZZZ.ZZZ.9,99.
           03 WS-MSK-SIN-1     PIC ---.---.9,99.
           03 WS-MSK-SIN-2     PIC $$$.$$$.9,99.
       PROCEDURE DIVISION.
       S000 SECTION.
       P000-INICIO.
            INITIALIZE WS-VAR
            MOVE 1 TO WS-INT WS-DEC WS-SIN

            COMPUTE WS-INT = ((WS-INT / 2) * -1)
            COMPUTE WS-DEC = ((WS-DEC / 2) * -1)
            COMPUTE WS-SIN = ((WS-SIN / 2) * -1)

            DISPLAY '>>>>> FORMATO ORIGINAL <<<<<'
            DISPLAY 'NUMERO INTEIRO...: ' WS-INT
            DISPLAY 'NUMERO DECIMAL...: ' WS-DEC
            DISPLAY 'NUMERO SINALIZADO: ' WS-SIN

            MOVE WS-INT TO WS-MSK-INT-1
                           WS-MSK-INT-2
            MOVE WS-DEC TO WS-MSK-DEC-1
                           WS-MSK-DEC-2
            MOVE WS-SIN TO WS-MSK-SIN-1
                           WS-MSK-SIN-2

            DISPLAY ' '
            DISPLAY '>>>>> FORMATO COM MASKARA <<<<<'
            DISPLAY 'WS-MSK-INT-1.....: ' WS-MSK-INT-1
            DISPLAY 'WS-MSK-INT-2.....: ' WS-MSK-INT-2
            DISPLAY 'WS-MSK-DEC-1.....: ' WS-MSK-DEC-1
            DISPLAY 'WS-MSK-DEC-2.....: ' WS-MSK-DEC-2
            DISPLAY 'WS-MSK-SIN-1.....: ' WS-MSK-SIN-1
            DISPLAY 'WS-MSK-SIN-1.....: ' WS-MSK-SIN-2
            .
       P000-EXIT.

       S999 SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
       END PROGRAM EDICAO_NUMERICA.
