      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 28-04-24
      * Purpose: CALCULAR UM BOLETIM ESCOLAR
      * Descritivo: Imputar e calcular dados de notas, processar
      *             média e gerar display de saída.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAMOD2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-VAR.
           03 NOME-ALUNO         PIC X(20).
           03 MATERIA-ESCOLAR    PIC X(20).
           03 NOTA1              PIC 9(02).
           03 NOTA2              PIC 9(02).
           03 NOTA3              PIC 9(02).
           03 NOTA4              PIC 9(02).
           03 MEDIA              PIC 9(02).
           03 WS-I               PIC 9.
           03 WS-STATUS          PIC X(03) VALUE "SIM".
           03 WS-TEXTO           PIC X(10) VALUE SPACES.
       PROCEDURE DIVISION.
       S000-INI SECTION.
       P000-INICIO.
            INITIALIZE WS-VAR
            PERFORM P001-NOTA1     THRU P004-EXIT
            PERFORM P010-CONFIRMA UNTIL WS-STATUS = "NAO"
            PERFORM P090-FINALIZA.
       P000-EXIT.
            EXIT.

       S001-NOTA SECTION.
       P001-NOTA1.
            DISPLAY " "
            DISPLAY "DIGITE O NOME DO ALUNO....: "
            ACCEPT NOME-ALUNO

            DISPLAY "DIGITE A MATERIA ESCOLAR..: "
            ACCEPT MATERIA-ESCOLAR

            DISPLAY " ".
            DISPLAY "DIGITE A NOTA1..: "
            ACCEPT NOTA1
            IF NOTA1 IS NUMERIC AND NOTA1 GREATER ZEROS
               AND NOTA1 LESS 11 THEN
               DISPLAY "NOTA1 >> OK "
            ELSE
               DISPLAY "NOTA1 INVALIDA, TENTE NOVAMENTE!"
               GO TO P001-NOTA1
            END-IF.
       P001-EXIT.

       P002-NOTA2.
            DISPLAY "DIGITE A NOTA2..: "
            ACCEPT NOTA2
            IF NOTA2 IS NUMERIC AND NOTA2 GREATER ZEROS
               AND NOTA2 LESS 11 THEN
               DISPLAY "NOTA2 >> OK "
            ELSE
               DISPLAY "NOTA2 INVALIDA, TENTE NOVAMENTE!"
               GO TO P002-NOTA2
            END-IF.
       P002-EXIT.

       P003-NOTA3.
            DISPLAY "DIGITE A NOTA3..: "
            ACCEPT NOTA3
            IF NOTA3 IS NUMERIC AND NOTA3 GREATER ZEROS
               AND NOTA3 LESS 11 THEN
               DISPLAY "NOTA3 >> OK "
            ELSE
               DISPLAY "NOTA3 INVALIDA, TENTE NOVAMENTE!"
               GO TO P003-NOTA3
            END-IF.
       P003-EXIT.

       P004-NOTA4.
            DISPLAY "DIGITE A NOTA4..: "
            ACCEPT NOTA4
            IF NOTA4 IS NUMERIC AND NOTA4 GREATER ZEROS
               AND NOTA4 LESS 11 THEN
               DISPLAY "NOTA4 >> OK "
            ELSE
               DISPLAY "NOTA4 INVALIDA, TENTE NOVAMENTE!"
               GO TO P004-NOTA4
            END-IF

            PERFORM P011-CALCULO.
       P004-EXIT.
            EXIT.

       S010-CALCULO SECTION.
       P011-CALCULO.
            COMPUTE MEDIA = (NOTA1 + NOTA2 + NOTA3 + NOTA4) / 4
                    ON SIZE ERROR PERFORM P080-ERRO
            END-COMPUTE

            IF MEDIA >= 7 THEN
               MOVE "APROVADO!"  TO WS-TEXTO
            ELSE
               MOVE "REPROVADO!" TO WS-TEXTO
            END-IF.
       P011-EXIT.
            EXIT.

       S010-CONFIRMA SECTION.
       P010-CONFIRMA.
            DISPLAY " "
            DISPLAY "**********************************"
            DISPLAY "*** RESULTADO DO PROCESSAMENTO ***"
            DISPLAY "**********************************"
            DISPLAY "NOME DO ALUNO..: " NOME-ALUNO
            DISPLAY "MATERIA....... : " MATERIA-ESCOLAR
            DISPLAY "MEDIA..........: " MEDIA
            DISPLAY "STATUS.........: " WS-TEXTO
            DISPLAY "**********************************"
            DISPLAY " "
            DISPLAY "DESEJA AVALIAR OUTRO ALUNO: "
            DISPLAY "DIGITE (SIM) OU (NAO): "
            ACCEPT WS-STATUS

            IF WS-STATUS = "SIM" THEN
               INITIALIZE WS-VAR
               PERFORM P001-NOTA1 THRU P004-EXIT
            END-IF.
       P010-EXIT.
            EXIT.

       S080-ERRO SECTION.
       P080-ERRO.
            DISPLAY "ERRO NO CALCULO DA MEDIA, FV VERIFICAR!"
            MOVE 8 TO RETURN-CODE
            PERFORM P090-FINALIZA.
       P080-EXIT.
            EXIT.

       S090-FINALIZA SECTION.
       P090-FINALIZA.
            STOP RUN.
       P090-EXIT.
            EXIT.
       END PROGRAM DESAMOD2.
