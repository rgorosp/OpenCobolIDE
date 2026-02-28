      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 06-10-22
      * Purpose: SUB-ROTINA PARA CALCULAR A MEDIA DOS ALUNOS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CH000000B.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-VAR.
           03 WS-N1           PIC 9(02).
           03 WS-N2           PIC 9(02).
           03 WS-N3           PIC 9(02).
           03 WS-N4           PIC 9(02).
           03 WS-RS           PIC 9(02).
           03 WS-MOSTRA       PIC X(09).
       PROCEDURE DIVISION USING WS-VAR.
       P000-INICIO.
            PERFORM P100-PROCESSO THRU P100-EXIT
            PERFORM P999-TERMINO  THRU P999-EXIT.
       P000-EXIT.

       P100-PROCESSO.
            DISPLAY " "
            DISPLAY ">>>>> SUB-ROTINA CH000000B <<<<<"
            DISPLAY "NOTA 1 RECEBIDA..: " WS-N1
            DISPLAY "NOTA 2 RECEBIDA..: " WS-N2
            DISPLAY "NOTA 3 RECEBIDA..: " WS-N3
            DISPLAY "NOTA 4 RECEBIDA..: " WS-N4

            COMPUTE WS-RS = (WS-N1 + WS-N2 + WS-N3 + WS-N4) / 4

            IF WS-RS >= 7 THEN
               MOVE "APROVADO"  TO WS-MOSTRA
            ELSE
               MOVE "REPROVADO" TO WS-MOSTRA
            END-IF.
       P100-EXIT.

       P999-TERMINO.
            DISPLAY ">>>>> TERMINO DA SUB-ROTINA <<<<<"
            GOBACK.
       P999-EXIT.
       END PROGRAM CH000000B.
