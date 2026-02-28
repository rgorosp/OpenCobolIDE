      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 24/06/2025
      * Purpose: TESTE PROGRAM UDEMY MAINFRAME COBOL ANIL POLSANI
      *          USING IF-ELSE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPGM1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  CUST-DETAILS-IN.
           03 CUST-ID     PIC X(05).
           03 CUST-NAME   PIC X(20).
           03 CUST-GENDER PIC X(01).

       01  CUST-DETAILS-OUT.
           03 CUST-ID     PIC X(05) VALUE SPACE.
           03 CUST-TITLE  PIC X(10) VALUE SPACE.
           03 CUST-NAME   PIC X(20) VALUE SPACE.
           03 CUST-GENDER PIC X(10) VALUE SPACE.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       S001-INICIO SECTION.
       P001-INICIO.
            DISPLAY 'PROGRAM CUSTPGM1 - STARTED'

            MOVE SPACES TO CUST-ID     OF CUST-DETAILS-IN
                           CUST-NAME   OF CUST-DETAILS-IN
                           CUST-GENDER OF CUST-DETAILS-IN

            DISPLAY 'DIGITE A IDENTIFICACAO COM 5 POSICOES: '
            ACCEPT CUST-ID     OF CUST-DETAILS-IN
            DISPLAY ' '
            DISPLAY 'DIGITE O NOME: '
            ACCEPT CUST-NAME   OF CUST-DETAILS-IN
            DISPLAY ' '
            DISPLAY 'DIGITE (M) PARA MALE OU (F) PARA FEMALE: '
            ACCEPT CUST-GENDER OF CUST-DETAILS-IN
            DISPLAY ' '

            IF CUST-GENDER OF CUST-DETAILS-IN = 'M' OR
               CUST-GENDER OF CUST-DETAILS-IN = 'm'
               MOVE 'MALE' TO CUST-GENDER OF CUST-DETAILS-OUT
               MOVE 'MR'   TO CUST-TITLE  OF CUST-DETAILS-OUT
            ELSE
            IF CUST-GENDER OF CUST-DETAILS-IN = 'F' OR
               CUST-GENDER OF CUST-DETAILS-IN = 'f'
               MOVE 'FEMALE' TO CUST-GENDER OF CUST-DETAILS-OUT
               MOVE 'MRS'    TO CUST-TITLE  OF CUST-DETAILS-OUT
            ELSE
               DISPLAY 'INVALIDO CUST-GENDER'
               MOVE 8 TO RETURN-CODE
               PERFORM P999-FIM
            END-IF
            END-IF.

            MOVE CUST-ID     OF CUST-DETAILS-IN TO
                                CUST-ID OF CUST-DETAILS-OUT
            MOVE CUST-NAME   OF CUST-DETAILS-IN TO
                                CUST-NAME OF CUST-DETAILS-OUT

            DISPLAY 'DETAILS = ' CUST-DETAILS-OUT.
       P001-EXIT.
            EXIT.

       S999-FIM SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM CUSTPGM1.
