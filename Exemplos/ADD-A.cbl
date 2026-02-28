      ******************************************************************
      * Author: Emerson S Motta.
      * Date: 27/02/26
      * Purpose: CURSO COBOL E MAINFRAME IVEE
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADD_A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-VAR1             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR1-TEXTO       PIC X(5)     VALUE SPACES.
       77  WS-VAR2             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR2-TEXTO       PIC X(05)    VALUE SPACES.
       77  WS-VAR3             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR3-TEXTO       PIC X(05)    VALUE SPACES.
       77  WS-VAR4             PIC 9(03)V99 VALUE ZEROS.
       77  WS-VAR4-TEXTO       PIC X(05)    VALUE SPACES.
       77  WS-TEXTO            PIC X(30)    VALUE SPACES.

       01  WS-STATUS.
           03 WS-OK            PIC X VALUE 'N'.
              88 OK            VALUE 'S'.
              88 NOTOK         VALUE 'N'.

       PROCEDURE DIVISION.
       S002-INI SECTION.
       P002-INI.
            PERFORM P003-VALIDA1 THRU P003-VALIDA1-EXIT UNTIL OK
            SET NOTOK TO TRUE
            PERFORM P003-VALIDA2 THRU P003-VALIDA2-EXIT UNTIL OK
            SET NOTOK TO TRUE
            PERFORM P003-VALIDA3 THRU P003-VALIDA3-EXIT UNTIL OK
            SET NOTOK TO TRUE
            PERFORM P003-VALIDA4 THRU P003-VALIDA4-EXIT UNTIL OK

            PERFORM P010-DISPLAY THRU P010-EXIT
            PERFORM P005-CALCULO THRU P005-EXIT
            PERFORM P010-DISPLAY THRU P010-EXIT
            PERFORM P999-TERMINO.
       P002-EXIT. EXIT.

       S003-VALIDA SECTION.    
       P003-VALIDA1.
            SET NOTOK TO TRUE
            DISPLAY 'Inserir valor VAR1: '
            ACCEPT WS-VAR1-TEXTO
            MOVE FUNCTION TRIM (WS-VAR1-TEXTO) TO WS-VAR1-TEXTO
            MOVE WS-VAR1-TEXTO TO WS-VAR1
            IF WS-VAR1 IS NOT NUMERIC  
              DISPLAY 'WS-VAR1 NAO NUMERICO, FAVOR INSERIR NOVAMENTE'
              EXIT PARAGRAPH
            ELSE 
              SET OK TO TRUE
            END-IF            
            EXIT PARAGRAPH.
       P003-VALIDA1-EXIT.

       P003-VALIDA2.
            SET NOTOK TO TRUE
            DISPLAY "Inserir valor VAR2: "
            ACCEPT WS-VAR2-TEXTO
            MOVE FUNCTION TRIM (WS-VAR2-TEXTO) TO WS-VAR2-TEXTO
            MOVE WS-VAR2-TEXTO TO WS-VAR2
            IF WS-VAR2 IS NOT NUMERIC  
              DISPLAY 'WS-VAR2 NAO NUMERICO, FAVOR INSERIR NOVAMENTE'
              EXIT PARAGRAPH
            ELSE
              SET OK TO TRUE
            END-IF
            EXIT PARAGRAPH.
       P003-VALIDA2-EXIT.

       P003-VALIDA3.
            SET NOTOK TO TRUE
            DISPLAY "Inserir valor VAR3: "
            ACCEPT WS-VAR3-TEXTO
            MOVE FUNCTION TRIM (WS-VAR3-TEXTO) TO WS-VAR3-TEXTO
            MOVE WS-VAR3-TEXTO TO WS-VAR3
            IF WS-VAR3 IS NOT NUMERIC  
              DISPLAY 'WS-VAR3 NAO NUMERICO, FAVOR INSERIR NOVAMENTE'
              EXIT PARAGRAPH 
            ELSE  
              SET OK TO TRUE
            END-IF
            EXIT PARAGRAPH.
       P003-VALIDA3-EXIT.

       P003-VALIDA4.
            SET NOTOK TO TRUE
            DISPLAY "Inserir valor VAR4: "
            ACCEPT WS-VAR4-TEXTO 
            MOVE FUNCTION TRIM (WS-VAR4-TEXTO ) TO WS-VAR4-TEXTO 
            MOVE WS-VAR4-TEXTO TO WS-VAR4
            IF WS-VAR4 IS NOT NUMERIC  
              DISPLAY 'WS-VAR4 NAO NUMERICO, FAVOR INSERIR NOVAMENTE'
              EXIT PARAGRAPH 
            ELSE 
              SET OK TO TRUE
            END-IF
            EXIT PARAGRAPH.
       P003-VALIDA4-EXIT.
            EXIT.

       S005-CALC SECTION.
       P005-CALCULO.
            ADD WS-VAR1 TO WS-VAR2
            ADD WS-VAR2 TO WS-VAR3
            ADD WS-VAR3 TO WS-VAR4
            .
       P005-EXIT.
            EXIT.

       S010-DIS SECTION.
       P010-DISPLAY.
            DISPLAY '------------------------------'
            DISPLAY WS-TEXTO
            DISPLAY '------------------------------'
            DISPLAY 'VAR1: ' WS-VAR1
            DISPLAY 'VAR2: ' WS-VAR2
            DISPLAY 'VAR3: ' WS-VAR3
            DISPLAY 'VAR4: ' WS-VAR4
            DISPLAY '------------------------------'
            DISPLAY '                              '.
       P010-EXIT.
            EXIT.

       S900-TERM SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM ADD_A.
