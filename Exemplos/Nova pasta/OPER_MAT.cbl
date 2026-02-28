      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 25-03-24.
      * Purpose: TRABALHAR COM OPERACOES MATEMATICAS ADD, SUBTRACT,
      *     MULTIPLY E DIVIDE
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-OPER.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-NUM1         PIC 99  VALUE 0.
       77  WS-NUM2         PIC 99  VALUE 0.
       77  WS-RESULT       PIC 999 VALUE ZEROS.
       77  WS-MENSAGEM     PIC X(30) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INI.
            PERFORM P010-RECEBA
            PERFORM P050-CALCULO
            PERFORM P999-TERMINO.
       P001-EXIT.
            EXIT.

       S010-RECEBA SECTION.
       P010-RECEBA.
             INITIALIZE WS-NUM1
                        WS-NUM2
                        WS-RESULT

             DISPLAY 'DIGITE VALOR PARA WS-NUM1: '
             ACCEPT WS-NUM1

             DISPLAY 'DIGITE VALOR PARA WS-NUM2: '
             ACCEPT WS-NUM2.
       P010-EXIT.
            EXIT.

       S050-CALCULO SECTION.
       P050-CALCULO.
             DISPLAY 'WS-NUM1: ' WS-NUM1
             DISPLAY 'WS-NUM2: ' WS-NUM2

             ADD WS-NUM1   TO WS-NUM2
                           ON SIZE ERROR
                           MOVE 'ESTOURO DE CAMPO ADD' TO WS-MENSAGEM
                           PERFORM P080-ERRO
             END-ADD

             DISPLAY 'WS-NUM2 APOS ADD: ' WS-NUM2

             SUBTRACT 2    FROM WS-NUM2
                           ON SIZE ERROR
                           MOVE 'ESTOURO DE CAMPO SUBTRACT'
                           TO WS-MENSAGEM
                           PERFORM P080-ERRO
             END-SUBTRACT

             DISPLAY 'WS-NUM2 APOS SUBTRACT: ' WS-NUM2

             MULTIPLY 2    BY WS-NUM2
                           ON SIZE ERROR
                           MOVE 'ESTOURO DE CAMPO MULTIPLY'
                           TO WS-MENSAGEM
                           PERFORM P080-ERRO
             END-MULTIPLY

             DISPLAY 'WS-NUM2 APOS MULTIPLY: ' WS-NUM2

             DIVIDE 3      INTO WS-NUM2
                           ON SIZE ERROR
                           MOVE 'ESTOURO DE CAMPO DIVIDE' TO WS-MENSAGEM
                           PERFORM P080-ERRO
             END-DIVIDE

             DISPLAY 'WS-NUM2 APOS DIVIDE: ' WS-NUM2.
       P050-EXIT.
            EXIT.

       S080-ERRO SECTION.
       P080-ERRO.
            DISPLAY 'DISPLAY: ' WS-MENSAGEM
            PERFORM P999-TERMINO.
       P080-EXIT.
            EXIT.

       S999-TERMINO SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM PGM-OPER.
