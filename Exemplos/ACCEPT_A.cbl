      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 01-05-22
      * Purpose: COMANDO ACCEPT(ACEITAR)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCEPT_A.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  WS-DATA           PIC X(06) VALUE SPACES.
       77  WS-DATA-A         PIC X(08) VALUE SPACES.
       77  WS-DIA            PIC X(05) VALUE SPACES.
       77  WS-DIA-A          PIC X(07) VALUE SPACES.
       77  WS-DIA-SEMANA     PIC X(20) VALUE SPACES.
       77  WS-HORA           PIC X(08) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INICIO SECTION.
       P001-INICIO.
            ACCEPT WS-DATA   FROM DATE
            ACCEPT WS-DATA-A FROM DATE YYYYMMDD
            DISPLAY 'EXEMPLO DATA 6 DIGITOS.: ' WS-DATA
            DISPLAY 'EXEMPLO DATA 8 DIGITOS.: ' WS-DATA-A

            ACCEPT WS-DIA    FROM DAY
            ACCEPT WS-DIA-A  FROM DAY YYYYDDD
            DISPLAY 'EXEMPLO DIA 5 DIGITOS..: ' WS-DIA
            DISPLAY 'EXMEPLO DIA 7 DIGITOS..: ' WS-DIA-A

            ACCEPT WS-DIA-SEMANA FROM DAY-OF-WEEK
            DISPLAY 'EXEMPLO DIA DA SEMANA..: ' WS-DIA-SEMANA.

            ACCEPT WS-HORA FROM TIME
            DISPLAY 'EXEMPLO HORAS 8 DIGITOS: ' WS-HORA.
       P001-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P999-TERMINO.
            STOP RUN.
       P999-EXIT.
            EXIT.
       END PROGRAM ACCEPT_A.
