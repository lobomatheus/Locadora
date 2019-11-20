      ******************************************************************
      * Author: MATHEUS LOBO
      * Date: 05/11/19
      * Purpose: Trabalho - sistema de locadora de videos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ESTOQUE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQ-ESTOQUE
           ASSIGN TO "./estoque.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS SEQUENTIAL
           RECORD KEY IS FILME-TITULO
           FILE STATUS IS WS-FILESTATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-ESTOQUE.
       01 FILME.
           05 FILME-TITULO PIC X(20).
           05 FILME-QTD  PIC 9(02).
           05 FILME-DISP PIC 9(02).

       WORKING-STORAGE SECTION.
            77 WS-OPTION PIC 9(01).
            77 WS-LIXO PIC X(01).
            01 WS-FILME.
               05 WS-FILME-TITULO PIC X(20).
               05 WS-FILME-QTD PIC 9(02).
               05 WS-FILME-DISP PIC 9(02).
            77 ARQ-EOF PIC 9(1) VALUE 0.
            77 WS-FILESTATUS PIC 99.
            77 WS-LA PIC 99 VALUE 1.
            01 WS-FILME-AUX.
               05 WS-TITULO-AUX PIC X(20).
               05 WS-QTD-AUX PIC 99.
            77 WS-EXISTE PIC 9 VALUE 0.
       SCREEN SECTION.
       01 ESTOQUE-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1 COL 1 VALUE "-------------------------------- |".
            02 LINE 2 COL 1 VALUE "-           ESTOQUE            - |".
            02 LINE 3 COL 1 VALUE "-------------------------------- |".
            02 LINE 4 COL 1 VALUE "- 1 - EXIBIR ESTOQUE           - |".
            02 LINE 5 COL 1 VALUE "- 2 - ADICIONAR FILME          - |".
            02 LINE 6 COL 1 VALUE "- 3 - REMOVER FILME            - |".
            02 LINE 7 COL 1 VALUE "-                      0-VOLTAR- |".
            02 LINE 8 COL 1 VALUE "-                              - |".
            02 LINE 8 COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9 COL 1 VALUE "-------------------------------- |".
       01 BLANK-SCREEN.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       ESTOQUE.
           DISPLAY ESTOQUE-SCREEN.
           ACCEPT ESTOQUE-SCREEN.
           EVALUATE WS-OPTION
               WHEN 0
                   GO TO FIM
               WHEN 1
                   GO TO EXIBIR
               WHEN 2
                   GO TO ADICIONAR
               WHEN 3
                   GO TO REMOVER
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA" AT LINE 12
                   DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 13
                   ACCEPT WS-LIXO AT LINE 13 COL 26
                   GO TO ESTOQUE
           END-EVALUATE.

       EXIBIR.
           OPEN INPUT ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           SET ARQ-EOF TO 0.
           PERFORM UNTIL ARQ-EOF = 1
               READ ARQ-ESTOQUE NEXT RECORD
                   AT END SET ARQ-EOF TO 1
                   NOT AT END 
                       MOVE FILME TO WS-FILME
                       DISPLAY "FILME: " AT LINE WS-LA COL 36
                       DISPLAY WS-FILME-TITULO AT LINE WS-LA COL 43
                       DISPLAY " - TOTAL: " AT LINE WS-LA COL 63
                       DISPLAY WS-FILME-QTD AT LINE WS-LA COL 73
                       DISPLAY " - DISP: " AT LINE WS-LA COL 76
                       DISPLAY WS-FILME-DISP AT LINE WS-LA COL 85
                       ADD 1 TO WS-LA
               END-READ
           END-PERFORM.
           CLOSE ARQ-ESTOQUE.
           SET ARQ-EOF TO 0.
           ADD 1 TO WS-LA.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE WS-LA COL 36.
           ACCEPT WS-LIXO AT LINE WS-LA COL 62.
           SET WS-LA TO 1.
           GO TO ESTOQUE. 

       ADICIONAR.
           DISPLAY "DIGITE O NOME DO FILME: " AT LINE 1 COL 36.
           ACCEPT WS-TITULO-AUX AT LINE 1 COL 60.
           DISPLAY "DIGITE A QUANTIDADE: " AT LINE 2 COL 36.
           ACCEPT WS-QTD-AUX AT LINE 2 COL 60.

           OPEN I-O ARQ-ESTOQUE
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           PERFORM UNTIL ARQ-EOF = 1
               READ ARQ-ESTOQUE INTO WS-FILME
                   AT END SET ARQ-EOF TO 1
                   NOT AT END 
                       IF WS-FILME-TITULO = WS-TITULO-AUX
                           SET WS-EXISTE TO 1
                           SET ARQ-EOF TO 1
               END-READ
           END-PERFORM.
           SET ARQ-EOF TO 0.

           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           IF WS-EXISTE = 0
               DISPLAY "a" AT LINE 13
               MOVE WS-TITULO-AUX TO FILME-TITULO
               MOVE WS-QTD-AUX TO FILME-QTD
               MOVE WS-QTD-AUX TO FILME-DISP
               WRITE FILME.
           IF WS-EXISTE = 1
               DISPLAY "B" AT LINE 13
               MOVE WS-FILME-TITULO TO FILME-TITULO
               ADD WS-QTD-AUX TO WS-FILME-QTD GIVING FILME-QTD
               ADD WS-QTD-AUX TO WS-FILME-DISP GIVING FILME-DISP
               WRITE FILME.
           CLOSE ARQ-ESTOQUE.
           SET WS-EXISTE TO 0.           
           DISPLAY "FILME ADICIONADO" AT LINE 11 COL 36.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 12 COL 36.
           ACCEPT WS-LIXO AT LINE 12 COL 62.
           GO TO ESTOQUE.

       REMOVER.
           DISPLAY "DIGITE O NOME DO FILME: " AT LINE 1 COL 36.
           ACCEPT WS-TITULO-AUX AT LINE 1 COL 60.
           DISPLAY "QUANTIDADE A SER REMOVIDA: " AT LINE 2 COL 36.
           ACCEPT WS-QTD-AUX AT LINE 2 COL 60.
           
           SET WS-EXISTE TO 1.
           OPEN INPUT ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           PERFORM UNTIL ARQ-EOF = 1
               READ ARQ-ESTOQUE INTO WS-FILME
                   AT END 
                       SET WS-EXISTE TO 0
                       SET ARQ-EOF TO 1
                   NOT AT END 
                       IF WS-FILME-TITULO = WS-TITULO-AUX
                           SUBTRACT WS-QTD-AUX FROM WS-FILME-QTD
                           SUBTRACT WS-QTD-AUX FROM WS-FILME-DISP
                           DELETE ARQ-ESTOQUE
                           SET ARQ-EOF TO 1
               END-READ
           END-PERFORM.
           SET ARQ-EOF TO 0.
           CLOSE ARQ-ESTOQUE.

           IF WS-EXISTE = 0
               DISPLAY "O FILME NAO FOI CADASTRADO" AT LINE 11 COL 36.              
           IF WS-FILME-DISP < 0
               DISPLAY "EXEMPLARES ALUGADOS" AT LINE 11 COL 36.

       ERROARQ.
           IF WS-FILESTATUS = 35
               DISPLAY "ESTOQUE VAZIO" AT LINE 11 COL 36
               OPEN OUTPUT ARQ-ESTOQUE
               CLOSE ARQ-ESTOQUE
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FILESTATUS AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 12.
           CLOSE ARQ-ESTOQUE.
           GO TO ESTOQUE.
       FIM.
       END PROGRAM ESTOQUE.
       