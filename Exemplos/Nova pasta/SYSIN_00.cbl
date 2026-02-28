      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 10-07-24
      * Purpose: CAPITULO 03 - AULA 01 - PRATICA 01
      * Tectonics: IVEE COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. C3A01P01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           SYSIN IS WS-SYSIN
           .
      *-------------------------------------------------------------*
      *  PROCEDIMENTO - SELECT PARA ENTRADA E SAIDA DE ARQUIVOS
      *-------------------------------------------------------------*
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO PARA LAYOUT DOS ARQUIVOS
      *--------------------------------------------------------------*
       DATA DIVISION.
       FILE SECTION.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INSTRUCAO COM AS VARIAVEIS
      *--------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       77  WS-VAR1                   PIC 99 VALUE ZEROS.
       77  WS-VAR2                   PIC 99 VALUE ZEROS.
       77  WS-FLAG                   PIC 9  VALUE ZERO.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSO INICIAL
      *--------------------------------------------------------------*
       PROCEDURE DIVISION.
       P000-C3A01P01.
           PERFORM P005-INICIO
           PERFORM P050-PROCESSO THRU P051-EXIT
           PERFORM P100-TERMINO
           .
       P000-EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - INICIO DO PROCESSAMENTO
      *--------------------------------------------------------------*
       P005-INICIO.
           INITIALIZE WS-VAR1 WS-VAR2 WS-FLAG
           MOVE 1 TO WS-FLAG
           .
       P005-EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - PROCESSAMENTO
      *--------------------------------------------------------------*
       S050-PROCESSO SECTION.
       P050-PROCESSO.
           DISPLAY 'INSERIR VALOR VAR-1 DOIS DIGITOS'
           ACCEPT WS-VAR1
           DISPLAY 'INSERIR VALOR VAR-2 DOIS DIGITOS'
           ACCEPT WS-VAR2 FROM CONSOLE
           .
       P050-EXIT.
            EXIT.

       P051-PROCESSO.
           IF WS-FLAG EQUAL 3
              DISPLAY 'TERCEIRA TENTATIVA SEM SUCESSO'
              DISPLAY 'TERMINO ANORMAL PROGRAMA FECHADO'
              PERFORM P100-TERMINO
           END-IF

           IF WS-VAR1 EQUAL WS-VAR2 THEN
              DISPLAY 'VALORES IGUAIS INSERIR O NOVAMENTE'
              ADD 1   TO WS-FLAG
              PERFORM P050-PROCESSO THRU P051-EXIT
           ELSE
              IF WS-VAR1 GREATER WS-VAR2
                 DISPLAY WS-VAR1 ' MAIOR QUE ' WS-VAR2
              ELSE
                IF WS-VAR1 LESS WS-VAR2
                   DISPLAY WS-VAR1 ' MENOR QUE ' WS-VAR2
                END-IF
              END-IF
           END-IF
           .
       P051-EXIT.
            EXIT.
      *--------------------------------------------------------------*
      *  PROCEDIMENTO - TERMINO DO PROGRAMA
      *--------------------------------------------------------------*
       P100-TERMINO.
           STOP RUN.
       P100-EXIT.
       END PROGRAM C3A01P01.
      *--------------------------------------------------------------*
