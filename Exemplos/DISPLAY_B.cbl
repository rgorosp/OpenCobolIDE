      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 26-02-26
      * Purpose: SEGUNDO PROGRAMA
      * Tectonics: IVEE COBOL E MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DISPLAY_B.
       ENVIRONMENT DIVISION. 
       CONFIGURATION SECTION. 
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-ESTADO        PIC X(15) VALUE SPACES.
       77 WS-IDADE-TXT     PIC X(03) VALUE SPACES.
       77 WS-IDADE         PIC 9(03) VALUE ZEROES.
       77 WS-IDADE-EDIT    PIC ZZZ   VALUE SPACES.
       77 WS-MSG           PIC X(60) VALUE SPACES.
       
       01 WS-FLAGS.
           03 WS-OK        PIC X VALUE 'N'.
              88 OK        VALUE 'S'.
              88 NOTOK     VALUE 'N'.

       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P000-INICIO.
            DISPLAY '************************************************'
            DISPLAY '            PROGRAMA: DISPLAY_B'
            DISPLAY '************************************************'

            DISPLAY 'AONDE AUTHOR VOCE NASCEU?'
            ACCEPT WS-ESTADO 
            MOVE FUNCTION TRIM(WS-ESTADO) TO WS-ESTADO 

            PERFORM P100-LER-IDADE UNTIL OK 
            PERFORM P999-TERMINO.
       P000-EXIT. EXIT.

       S100-LER-IDADE SECTION.
       P100-LER-IDADE.     
            SET NOTOK TO TRUE 
            DISPLAY 'QUAL A SUA IDADE? (0 A 120)'
            ACCEPT WS-IDADE-TXT

      *>>>  LIMPA ESPACOS E VALIDA SE E NUMERICO
            MOVE FUNCTION TRIM(WS-IDADE-TXT) TO WS-IDADE-TXT
            IF WS-IDADE-TXT = SPACES 
              DISPLAY 'ERRO IDADE VAZIA. TENTE NOVAMENTE!'
              EXIT PARAGRAPH 
            END-IF
            
            MOVE WS-IDADE-TXT TO WS-IDADE
            IF WS-IDADE IS NOT NUMERIC 
              DISPLAY 'ERRO: DIGITE APENAS NUMEROS, EX: 25, 34'
              EXIT PARAGRAPH 
            END-IF 

      *>>> CONVERTE VALIDA A FAIXA
           MOVE WS-IDADE-TXT TO WS-IDADE 
           IF WS-IDADE < 0 OR WS-IDADE > 120
              DISPLAY 'ERRO: IDADE FORA DA FAIXA! (0 A 120)'
              EXIT PARAGRAPH 
           END-IF 

           MOVE WS-IDADE TO WS-IDADE-EDIT 
           SET OK TO TRUE
           EXIT PARAGRAPH.
       P100-EXIT. EXIT. 

       S999-TERMINO SECTION.
       P999-TERMINO.              
            DISPLAY 'MORO EM ' WS-ESTADO 
                    'E TENHO ' WS-IDADE-EDIT ' ANOS.'
            STOP RUN.
       P999-EXIT. EXIT.
       END PROGRAM DISPLAY_B.
