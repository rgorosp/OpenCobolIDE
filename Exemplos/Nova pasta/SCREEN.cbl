      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 24-02-26
      * Purpose: TRABALHAR COM TELAS USANDO SCREEN SECTION
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCREEN1.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DT-SISTEMA           PIC X(08).

       01  WS-DT-EDITADO.
           03 WS-DIA               PIC X(02).
           03 FILLER               PIC X(01) VALUE '/'.
           03 WS-MES               PIC X(02).
           03 FILLER               PIC X(01) VALUE '/'.
           03 WS-ANO               PIC X(04).

       01  WS-CADASTRO.
           03 WS-DT-HOJE           PIC X(10).
           03 WS-CODIGO            PIC X(06).
           03 WS-NOME              PIC X(25).
           03 WS-SALARIO           PIC X(15).
           03 WS-ENDERECO          PIC X(40).
           03 WS-DT-NASC           PIC X(10).
           03 WS-NATURALIDADE      PIC X(20).
           03 WS-DT-CAD            PIC X(10).
           03 WS-DT-LIMITE         PIC X(10).
           03 WS-SAIR              PIC X(01).

       SCREEN SECTION.
      * >>> TELA DE CADASTRO
       01  SCREEN-01.
           03 BLANK SCREEN BACKGROUND-COLOR 7.
           03 LINE 01 COLUMN 01 VALUE '*BANCO MUNDIAL*'
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.

           03 LINE 01 COLUMN 59
              VALUE 'DATA: '
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.
           03 WS-LIN01-COL69        PIC X(10)
              LINE 01 COLUMN 69
              FROM WS-DT-HOJE
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

           03 LINE 02 COLUMN 30
              VALUE 'FICHA DE COLABORADORES'
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.

           03 LINE 04 COLUMN 01
              VALUE 'CODIGO: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN04-COL15        PIC X(06)
              LINE 04 COLUMN 15
              TO WS-CODIGO
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 7.

           03 LINE 06 COLUMN 1
              VALUE 'NOME: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN06-COL15        PIC X(25)
              LINE 06 COLUMN 15
              USING WS-NOME
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 06 COLUMN 44
              VALUE 'SALARIO: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN06-COL55        PIC X(15)
              LINE 06 COLUMN 55
              USING WS-SALARIO
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 08 COLUMN 01
              VALUE 'ENDERECO: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN08-COL15        PIC X(40)
              LINE 08 COLUMN 15
              USING WS-ENDERECO
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 10 COLUMN 01
              VALUE 'NASCIMENTO: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN08-COL15        PIC X(10)
              LINE 10 COLUMN 15
              USING WS-DT-NASC
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 10 COLUMN 30
              VALUE 'NATURALIDADE: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN10-COL44        PIC X(20)
              LINE 10 COLUMN 44
              USING WS-NATURALIDADE
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 12 COLUMN 01 
              VALUE 'DATA CADASTRO: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN12-COL20        PIC X(10)
              LINE 12 COLUMN 20
              USING WS-DT-CAD
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 12 COLUMN 44
              VALUE 'DATA LIMITE: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN12-COL60        PIC X(10)
              LINE 12 COLUMN 60
              USING WS-DT-LIMITE
              FOREGROUND-COLOR 1 BACKGROUND-COLOR 2.

           03 LINE 15 COLUMN 01
              VALUE 'SAIR: '
              FOREGROUND-COLOR 0 BACKGROUND-COLOR 7.
           03 WS-LIN15-COL10        PIC X(01)
              LINE 15 COLUMN 10
              USING WS-SAIR
              FOREGROUND-COLOR 3 BACKGROUND-COLOR 2.

      * >>> INICIO DO PROCESSAMENTO
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P000-INICIO.
            ACCEPT WS-DT-SISTEMA FROM DATE YYYYMMDD.
            MOVE WS-DT-SISTEMA(7:2) TO WS-DIA.
            MOVE WS-DT-SISTEMA(5:2) TO WS-MES.
            MOVE WS-DT-SISTEMA(1:4) TO WS-ANO.
            MOVE WS-DT-EDITADO      TO WS-DT-HOJE
            DISPLAY 'DATA DE PROCESSAMENTO: ' WS-DT-EDITADO.
       P000-EXIT. EXIT.

      * >>> TERMINO DO PROCESSAMENTO
       S999-TERMINO SECTION.
       P999-TERMINO.
            DISPLAY SCREEN-01
            ACCEPT  SCREEN-01
            GOBACK.
       P999-EXIT. EXIT.
       END PROGRAM SCREEN1.
