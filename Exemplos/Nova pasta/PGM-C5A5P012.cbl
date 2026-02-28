      ******************************************************************
      * Author: EMERSON S MOTTA
      * Date: 13-11-23
      * Purpose: Capítulo 05 - Aula 05 - Prática 01
      * Description: sub-programa C5A5P012 realizar o calculo do valor
      * unitiario e quantidade mês e geral o total
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM-C5A5P012.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01  WS-PARM.
           03 VLR-PARM      PIC 9(02)V99.
           03 QTDE-PARM     PIC 9(5).
           03 TOT-PARM      PIC 9(05)V99.

       PROCEDURE DIVISION USING WS-PARM.
       S000-INICIO SECTION.
       P001-INICIO.
            DISPLAY 'VALOR RECEBIDO.: ' WS-PARM
            COMPUTE TOT-PARM = (VLR-PARM * QTDE-PARM)
            DISPLAY 'VALOR CALCULADO: ' WS-PARM
            DISPLAY " ".
       P001-INICIO-EXIT.
            EXIT.

       S900-TERMINO SECTION.
       P901-TERMINO.
            GOBACK.
       P901-TERMINO-EXIT.
            EXIT.
       END PROGRAM PGM-C5A5P012.
