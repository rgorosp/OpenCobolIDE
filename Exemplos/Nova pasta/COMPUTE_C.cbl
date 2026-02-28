      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 27-07-23
      * Purpose: EXEMPLO COMANDO COMPUTE(OPERACOES ARITMETICAS)
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE-C.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VARIAVEIS.
           03 CAMOD013-TCTTAXA PIC 9V99     VALUE ZEROS.
           03 WS-VFINANC       PIC 9(05)V99 VALUE ZEROS.
           03 CAMOD013-VTXCORR PIC S9(05)V99 VALUE ZEROS.
           03 CAMOD013-VTXCORR-EDIT PIC $$.$99,99.
       77  WS-ARRED-ESC        PIC S9(05)V99 VALUE ZEROS.
       PROCEDURE DIVISION.
       P000-INICIO.
            INITIALIZE WS-VARIAVEIS

            MOVE 0,15  TO CAMOD013-TCTTAXA
            DISPLAY "TAXA: " CAMOD013-TCTTAXA
            MOVE 52445,12 TO WS-VFINANC
            DISPLAY "FINANCIAMENTO: " WS-VFINANC
            DISPLAY " "

            COMPUTE CAMOD013-VTXCORR = WS-VFINANC *
                   (CAMOD013-TCTTAXA / 100)
                    ON SIZE ERROR
                       DISPLAY "ESTOURO DE CAMPO"
                    NOT ON SIZE ERROR
                       DISPLAY "CAMOD013-VTXCORR - OK"
                       DISPLAY "VALOR: " CAMOD013-VTXCORR
                       DISPLAY " "
            END-COMPUTE

            COMPUTE WS-ARRED-ESC ROUNDED = CAMOD013-VTXCORR
                    DISPLAY "VALOR ARREDONDADO: "
                       WS-ARRED-ESC

            MOVE WS-ARRED-ESC TO CAMOD013-VTXCORR-EDIT
                    DISPLAY 'VALOR EDITADO: '
                       CAMOD013-VTXCORR-EDIT.
       P000-FIM.

       P999-INICIO.
            STOP RUN.
       P999-FIM.
       END PROGRAM COMPUTE-C.
