      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 14-09-25
      * Purpose: EXEMPLO DE PERFORM COM TIMES
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-PERFORM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-COUNT         PIC 99.
           03 WS-TOTAL         PIC 9(02).
           03 WS-INDEX         PIC 9(02).

       PROCEDURE DIVISION.
       S100 SECTION.
       P100-INICIO.
            PERFORM P800-INICIALIZA THRU P800-EXIT
            PERFORM P300-PROCESSA THRU P300-EXIT 5 TIMES

            PERFORM P800-INICIALIZA THRU P800-EXIT
            DISPLAY ' '
            PERFORM P500-PROCESSA THRU P500-EXIT WITH TEST
                                  BEFORE UNTIL WS-COUNT = 5

            PERFORM P800-INICIALIZA THRU P800-EXIT
            DISPLAY ' '
            PERFORM P700-PROCESSA THRU P700-EXIT
            PERFORM P999-FINALIZA.
       P100-EXIT. EXIT.

       S300 SECTION.
       P300-PROCESSA.
            DISPLAY '>>>>> EXEMPLO: P300-PROCESSA <<<<<'
               ADD 1 TO WS-COUNT
               DISPLAY ' '
               DISPLAY 'CONTAGEM..: ' WS-COUNT

            PERFORM 3 TIMES
               ADD 1 TO WS-TOTAL
               DISPLAY 'TOTAL.....: ' WS-TOTAL
            END-PERFORM.
       P300-EXIT. EXIT.

       S500 SECTION.
       P500-PROCESSA.
            DISPLAY '>>>>> EXEMPLO: P500-PROCESSA <<<<<'
               ADD 1 TO WS-COUNT
               DISPLAY ' '
               DISPLAY 'CONTAGEM..: ' WS-COUNT

            MOVE ZEROS TO WS-TOTAL
            PERFORM WITH TEST BEFORE UNTIL WS-TOTAL = 3
               ADD 1 TO WS-TOTAL
               DISPLAY 'TOTAL.....: ' WS-TOTAL
            END-PERFORM.
       P500-EXIT. EXIT.

       S700 SECTION.
       P700-PROCESSA.
            DISPLAY '>>>>> EXEMPLO: P700-PROCESSA <<<<<'
            PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5
               DISPLAY 'INDEX.....: ' WS-INDEX
            END-PERFORM.
       P700-EXIT. EXIT.

       S800-INICIALIZA SECTION.
       P800-INICIALIZA.
            INITIALIZE WS-VAR REPLACING ALPHANUMERIC BY SPACES
                                        NUMERIC      BY ZEROS.
       P800-EXIT. EXIT.

       S999 SECTION.
       P999-FINALIZA.
            DISPLAY ' '
            DISPLAY '>>>>> EXEMPLO: P999-FINALIZA <<<<<'
            STOP RUN.
       P999-EXIT. EXIT.
       END PROGRAM PGM-PERFORM.
