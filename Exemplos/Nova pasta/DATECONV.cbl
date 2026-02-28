      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 23-08-23
      * Purpose: FUNCAO PARA DIMINUIR A DATA EM 1 DIA DA ATUAL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATECONV.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-DT-INP       PIC 9(08).
           03 WS-DT-OP1       PIC 9(07).
           03 WS-DT-OP2       PIC 9(08).
       PROCEDURE DIVISION.
       P000-INICIO SECTION.
            ACCEPT WS-DT-INP FROM DATE YYYYMMDD
            DISPLAY 'DATA: ' WS-DT-INP

            COMPUTE WS-DT-OP1 = FUNCTION DAY-OF-INTEGER (FUNCTION
                    INTEGER-OF-DATE (WS-DT-INP) - 1)
            END-COMPUTE

            COMPUTE WS-DT-OP2 = FUNCTION DATE-OF-INTEGER (FUNCTION
                    INTEGER-OF-DATE (WS-DT-INP) - 1)
            END-COMPUTE

            DISPLAY 'WS-DT-OP1 = ' WS-DT-OP1
            DISPLAY 'WS-DT-OP2 = ' WS-DT-OP2

            STOP RUN.
       END PROGRAM DATECONV.
