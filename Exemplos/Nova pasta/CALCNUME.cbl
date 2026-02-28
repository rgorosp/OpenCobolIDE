*****************************************************************
      * DATA     :  19/05/2025
      * AUTOR    :  VAGNER RENATO BELLACOSA
      * OBJETIVO : PROGRAMA EXEMPLO COMPUTACIONAL-1
      *            COMP-2 COMP-3 COMP-4 COMP-5 E COMP
      *            EXEMPLO DE CALCULO E TAMANHO DE AREA DE MEMORIA
      * CPD      : TESTES PROGRAMACAO
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCNUME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.  DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WSS-VAR-COMP1.
          05 WSS-PI          USAGE IS COMP-1.
          05 WSS-RADIUS      USAGE IS COMP-1.
          05 WSS-AREA        USAGE IS COMP-1.

       01 WSS-VAR-COMP2.
          05 WSS-PI2         USAGE IS COMP-2.
          05 WSS-RADIUS2     USAGE IS COMP-2.
          05 WSS-AREA2       USAGE IS COMP-2.

       01 WSS-VAR-COMP3.
          05 WSS-PI3         PIC 9(03)V9(15) USAGE IS COMP-3.
          05 WSS-RADIUS3     PIC 9(03)V9(15) USAGE IS COMP-3.
          05 WSS-AREA3       PIC 9(03)V9(15) USAGE IS COMP-3.

       01 WSS-VAR-COMP4.
          05 WSS-PI4         PIC 9(03)V9(15) USAGE IS COMP-4.
          05 WSS-RADIUS4     PIC 9(03)V9(15) USAGE IS COMP-4.
          05 WSS-AREA4       PIC 9(03)V9(15) USAGE IS COMP-4.

       01 WSS-VAR-COMP4A.
          05 WSS-PI4A        PIC 9(03)V9(15) USAGE IS COMP.
          05 WSS-RADIUS4A    PIC 9(03)V9(15) USAGE IS COMP.
          05 WSS-AREA4A      PIC 9(03)V9(15) USAGE IS COMP.

       01 WSS-VAR-COMP5.
          05 WSS-PI5         PIC 9(03)V9(15) USAGE IS COMP-5.
          05 WSS-RADIUS5     PIC 9(03)V9(15) USAGE IS COMP-5.
          05 WSS-AREA5       PIC 9(03)V9(15) USAGE IS COMP-5.
      *********************
       PROCEDURE DIVISION.
       S001-INICIO SECTION.
       P001-INICIO.
           DISPLAY ' COBOL022'
           DISPLAY ' EXEMPLO PRATICO COMP  , COMP-1, COMP-2, COMP-3'
           DISPLAY '                 COMP-4 & COMP-5'
           DISPLAY ' '

           PERFORM P002-EXEMPLO-COMP1
           PERFORM P003-EXEMPLO-COMP2
           PERFORM P004-EXEMPLO-COMP3
           PERFORM P005-EXEMPLO-COMP
           PERFORM P006-EXEMPLO-COMP4
           PERFORM P007-EXEMPLO-COMP5
           PERFORM P008-LISTA-VAR
           STOP RUN.
       P001-EXIT.
            EXIT.

      * >>> COMP-1 é ponto flutuante (FLOAT simples, 32 bits)
      * >>> Capaz de representar valores com muitas casas decimais, mas com precisão
      * >>> limitada (±7 dígitos significativos, aproximadamente).
      * >>> Ideal para cálculo científico leve.
       S002-EXEMPLO-COMP1 SECTION.
       P002-EXEMPLO-COMP1.

           DISPLAY 'EXEMPLO-COMP1'

           MOVE 3,14159265358979323846  TO WSS-PI
           MOVE 10                      TO WSS-RADIUS

           COMPUTE WSS-AREA = WSS-PI * (WSS-RADIUS ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA

           PERFORM PULA-LINHA.
       P002-EXIT.
            EXIT.

      * >>> COMP-2 é ponto flutuante de dupla precisão (FLOAT 64 bits)
      * >>> Permite trabalhar com até 15-16 dígitos significativos, o que é muito
      * >>> mais que o COMP-1. Ideal para cálculos de alta precisão como PI,
      * >>> logaritmos, trigonometria, etc.
       S003-EXEMPLO-COMP2 SECTION.
       P003-EXEMPLO-COMP2.

           DISPLAY ' '
           DISPLAY 'EXEMPLO-COMP2'

           MOVE 3,14159265358979323846  TO WSS-PI2
           MOVE 10                      TO WSS-RADIUS2

           COMPUTE WSS-AREA2 = WSS-PI2 * (WSS-RADIUS2 ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA2
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA2

           PERFORM PULA-LINHA.
       P003-EXIT.
            EXIT.

      * >>> USAGE COMP-3 = Packed Decimal (decimal compactado)
      * >>> Armazena os dados em formato BCD, 2 dígitos por byte (exceto o último,
      * >>> que tem o sinal).
       S004-EXEMPLO-COMP3 SECTION.
       P004-EXEMPLO-COMP3.

           DISPLAY ' '
           DISPLAY 'EXEMPLO-COMP3'

           MOVE 3,14159265358979323846  TO WSS-PI3
           MOVE 10                      TO WSS-RADIUS3

           COMPUTE WSS-AREA3 = WSS-PI3 * (WSS-RADIUS3 ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA3
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA3

           PERFORM PULA-LINHA.
       P004-EXIT.
            EXIT.

      * >>> COMP-4 é às vezes tratado como inteiro binário nativo (parecido
      * >>> com COMP ou BINARY). no z/OS você deve usar COMP, COMP-3, COMP-1, COMP-2
       S005-EXEMPLO-COMP SECTION.
       P005-EXEMPLO-COMP.

           DISPLAY ' '
           DISPLAY 'EXEMPLO-COMP '

           MOVE 3,14159265358979323846  TO WSS-PI4A
           MOVE 10                      TO WSS-RADIUS4A

           COMPUTE WSS-AREA4A = WSS-PI4A * (WSS-RADIUS4A ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA4A
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA4A

           PERFORM PULA-LINHA.
       P005-EXIT.
            EXIT.

      *-------------*
       S006-EXEMPLO-COMP4 SECTION.
       P006-EXEMPLO-COMP4.

           DISPLAY ' '
           DISPLAY 'EXEMPLO-COMP4'

           MOVE 3,14159265358979323846  TO WSS-PI4
           MOVE 10                      TO WSS-RADIUS4

           COMPUTE WSS-AREA4 = WSS-PI4 * (WSS-RADIUS4 ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA4
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA4

           PERFORM PULA-LINHA.
       P006-EXIT.
            EXIT.

      *-------------*
       S007-EXEMPLO-COMP5 SECTION.
       P007-EXEMPLO-COMP5.

           DISPLAY ' '
           DISPLAY 'EXEMPLO-COMP5'

           MOVE 3,14159265358979323846  TO WSS-PI5
           MOVE 10                      TO WSS-RADIUS5

           COMPUTE WSS-AREA5 = WSS-PI5 * (WSS-RADIUS5 ** 2)

           DISPLAY "THE AREA OF THE CIRCLE: " WSS-AREA5
           DISPLAY 'LENGTH OF WSS-AREA    : ' LENGTH OF WSS-AREA5

           PERFORM PULA-LINHA.
       P007-EXIT.
            EXIT.

      *-----------*
       PULA-LINHA.
      *-----------*

           DISPLAY ' '.

      *-----------*
       S008-LISTA-VAR SECTION.
       P008-LISTA-VAR.

           PERFORM PULA-LINHA

           DISPLAY 'WSS-VAR-COMP1 = ' WSS-VAR-COMP1
                   ' '      LENGTH OF WSS-VAR-COMP1  ' BYTES '
           DISPLAY ' '

           DISPLAY 'WSS-VAR-COMP2 = ' WSS-VAR-COMP2
                   ' '      LENGTH OF WSS-VAR-COMP2  ' BYTES '
           DISPLAY ' '

           DISPLAY 'WSS-VAR-COMP3 = ' WSS-VAR-COMP3
                   ' '      LENGTH OF WSS-VAR-COMP3  ' BYTES '
           DISPLAY ' '

           DISPLAY 'WSS-VAR-COMP4 = ' WSS-VAR-COMP4
                   ' '      LENGTH OF WSS-VAR-COMP4  ' BYTES '
           DISPLAY ' '

           DISPLAY 'WSS-VAR-COMP  = ' WSS-VAR-COMP4A
                   ' '      LENGTH OF WSS-VAR-COMP4A ' BYTES '
           DISPLAY ' '

           DISPLAY 'WSS-VAR-COMP5 = ' WSS-VAR-COMP5
                   ' '      LENGTH OF WSS-VAR-COMP5  ' BYTES '
           DISPLAY ' '.
       P008-EXIT.
            EXIT.
       END PROGRAM CALCNUME.
      ********************** FIM DO PROGRAMA ***************************
