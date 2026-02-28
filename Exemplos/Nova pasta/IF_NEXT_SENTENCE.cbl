      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 07-05-24
      * Purpose: PROGRAMA EXEMPLO - USO DE IF... NEXT SENTENCE
      * RETIRADO O NEXT SENTENCE POIS ESTA ARCAICO NOS PROGRAMAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-IF.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 WS-OPE-01   PIC 9(5) VALUE 3.
           03 WS-OPE-02   PIC 9(5) VALUE 6.
           03 WS-CONTROLE PIC 9(03) VALUE ZEROS.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P000-INI.
            PERFORM P010-DIS.
       P000-EXIT.
            EXIT.
      *
       S010-DISPLAY SECTION.
       P010-DIS.
            DISPLAY "CONTEUDO DE OPE-01: " WS-OPE-01
            DISPLAY "CONTEUDO DE OPE-02: " WS-OPE-02
            DISPLAY " "
            DISPLAY "DIGITE VALOR PARA CONTROLE: "
            ACCEPT WS-CONTROLE FROM CONSOLE

            IF WS-OPE-01 LESS WS-CONTROLE THEN
              DISPLAY WS-OPE-01 " MENOR QUE " WS-CONTROLE
              PERFORM P020-CONTINUA THRU P999-EXIT
            ELSE
              DISPLAY WS-OPE-01 " MAIOR OU IGUAL A " WS-CONTROLE
                      UPON CONSOLE
              PERFORM P999-FIM
            END-IF.
       P010-EXIT.
            EXIT.
      *
       S020-CONTINUA SECTION.
       P020-CONTINUA.
            IF WS-OPE-02 GREATER WS-CONTROLE
              DISPLAY WS-OPE-02 " MAIOR QUE " WS-CONTROLE
            ELSE
              DISPLAY WS-OPE-02 " NAO MAIOR QUE " WS-CONTROLE
            END-IF
            .
       P020-EXIT.
            EXIT.
      *
       S999-FIM SECTION.
       P999-FIM.
            STOP RUN.
       P999-EXIT.
            EXIT.
