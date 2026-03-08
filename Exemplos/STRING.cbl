      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 06-03-26
      * Purpose: COMANDO STRING
      * Tectonics: COBOL MAINFRAME
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-STRING.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-CONTEUDO         PIC X(45) VALUE SPACES.
       77  WS-TEXTO            PIC X(45) VALUE SPACES.
       77  WS-PONTEIRO         PIC 9(02) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INI.
      *       PERFORM P011-EXEMPLO1 THRU P011-EXIT
      *       PERFORM P012-EXEMPLO2 THRU P012-EXIT
      *       PERFORM P013-EXEMPLO3 THRU P013-EXIT
      *       PERFORM P014-EXEMPLO4 THRU P014-EXIT
            PERFORM S010-EXEMPLOS
            PERFORM P901-TERM
            .
       P001-EXIT.
            EXIT.
      *
       S010-EXEMPLOS SECTION.
       P011-EXEMPLO1.
      * >>>>>>>>>>>>>>>>>>> EXEMPLO 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
            STRING
                 'EMERSON'
                 ' '
                 'MOTTA'
                 DELIMITED BY SIZE INTO WS-CONTEUDO
            END-STRING

            DISPLAY 'EXEMPLO1:'
            DISPLAY WS-CONTEUDO
            DISPLAY ' '
            PERFORM P801-INICIALIZA THRU P801-EXIT.
       P011-EXIT.

       P012-EXEMPLO2.
      * >>>>>>>>>>>>>>>>>>> EXEMPLO 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *           12345678901234567890123456789012
            MOVE 'O ANDRE ESTA MINISTRANDO O CURSO' TO WS-TEXTO
            STRING
                 'PROFESSOR'
                 WS-TEXTO(2:7)
                 WS-TEXTO(14:19)
                 " "
                 'COBOL'
                 DELIMITED BY SIZE INTO WS-CONTEUDO
            END-STRING

            DISPLAY 'EXEMPLO2: '
            DISPLAY WS-CONTEUDO
            DISPLAY ' '
            PERFORM P801-INICIALIZA THRU P801-EXIT.
       P012-EXIT.

       P013-EXEMPLO3.
      * >>>>>>>>>>>>>>>>>>> EXEMPLO 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *           12345678901234567890123456789012
            MOVE 'O ANDRE;ESTA MINISTRANDO O CURSO' TO WS-TEXTO
            STRING
                 WS-TEXTO
                 DELIMITED BY ";" INTO WS-CONTEUDO
            END-STRING

            DISPLAY 'EXEMPLO3: '
            DISPLAY WS-CONTEUDO
            DISPLAY ' '
            PERFORM P801-INICIALIZA THRU P801-EXIT.
       P013-EXIT.

       P014-EXEMPLO4.
      * >>>>>>>>>>>>>>>>>>> EXEMPLO 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      *           12345678901234567890123456789012
            MOVE 'O ANDRE ESTA MINISTRANDO O CURSO' TO WS-TEXTO
            SET WS-PONTEIRO                         TO 1

            STRING
                 'PROFESSOR'
                 WS-TEXTO(2:31)
                 DELIMITED BY SIZE INTO WS-CONTEUDO
                 WITH POINTER WS-PONTEIRO
            END-STRING

            DISPLAY 'EXEMPLO4: '
            DISPLAY WS-CONTEUDO
            DISPLAY WS-PONTEIRO
            DISPLAY ' '
            PERFORM P801-INICIALIZA THRU P801-EXIT.
       P014-EXIT.
            EXIT.

       S800-INICIALIZA.
       P801-INICIALIZA.
            INITIALIZE WS-CONTEUDO
                       WS-TEXTO
                       WS-PONTEIRO
            .
       P801-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TERM.
            STOP RUN.
       P901-EXIT.
            EXIT.
       END PROGRAM PGM-STRING.
