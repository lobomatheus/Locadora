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
           ACCESS IS RANDOM
           RECORD KEY IS FILME-ID
           ALTERNATE KEY IS FILME-TITULO
           FILE STATUS IS WS-FILESTATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-ESTOQUE.
       01 FILME.
           05 FILME-ID PIC 9(02).
           05 FILME-TITULO PIC X(20).
           05 FILME-QTD  PIC 9(02).
           05 FILME-DISP PIC 9(02).

       WORKING-STORAGE SECTION.
            77 WS-OPTION PIC 9(01).
            77 WS-LIXO PIC X(01).
            01 WS-FILME.
               05 WS-FILME-ID PIC 9(02).
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
            77 WS-ALUGADOS PIC 9 VALUE 0.
       SCREEN SECTION.
       01 ESTOQUE-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1 COL 1 VALUE "-------------------------------- |".
            02 LINE 2 COL 1 VALUE "-           ESTOQUE            - |".
            02 LINE 3 COL 1 VALUE "-------------------------------- |".
            02 LINE 4 COL 1 VALUE "- 1 - BUSCAR FILME             - |".
            02 LINE 5 COL 1 VALUE "- 2 - ADICIONAR FILME          - |".
            02 LINE 6 COL 1 VALUE "- 3 - REMOVER FILME            - |".
            02 LINE 7 COL 1 VALUE "-                      0-VOLTAR- |".
            02 LINE 8 COL 1 VALUE "-                              - |".
            02 LINE 8 COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9 COL 1 VALUE "-------------------------------- |".

       PROCEDURE DIVISION.
       ESTOQUE.
           DISPLAY ESTOQUE-SCREEN.
           ACCEPT ESTOQUE-SCREEN.
           EVALUATE WS-OPTION
               WHEN 0
                   GO TO FIM
               WHEN 1
                   GO TO EXIBIR2
               WHEN 2
                   GO TO GET_ULTIMO_ID
               WHEN 3
                   GO TO REMOVER
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA" AT LINE 12
                   DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 13
                   ACCEPT WS-LIXO AT LINE 13 COL 26
                   GO TO ESTOQUE
           END-EVALUATE.

       *>EXIBIR.
       *>    MOVE 1 TO FILME-ID.
       *>    OPEN INPUT ARQ-ESTOQUE.
       *>    IF WS-FILESTATUS <> 0
       *>        GO TO ERROARQ.
       *>    SET ARQ-EOF TO 0.
       *>    PERFORM UNTIL ARQ-EOF = 1
       *>        READ ARQ-ESTOQUE INTO WS-FILME
       *>            KEY IS FILME-ID
       *>            INVALID KEY
       *>                SET ARQ-EOF TO 1
       *>            NOT INVALID KEY
       *>                DISPLAY "FILME: " AT LINE WS-LA COL 36
       *>                DISPLAY WS-FILME-TITULO AT LINE WS-LA COL 43
       *>                DISPLAY " - TOTAL: " AT LINE WS-LA COL 63
       *>                DISPLAY WS-FILME-QTD AT LINE WS-LA COL 73
       *>                DISPLAY " - DISP: " AT LINE WS-LA COL 76
       *>                DISPLAY WS-FILME-DISP AT LINE WS-LA COL 85
       *>                ADD 1 TO WS-LA
       *>                ADD 1 TO FILME-ID
       *>        END-READ
       *>    END-PERFORM.
       *>    CLOSE ARQ-ESTOQUE.
       *>    SET ARQ-EOF TO 0.
       *>    ADD 1 TO WS-LA.
       *>    DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE WS-LA COL 36.
       *>    ACCEPT WS-LIXO AT LINE WS-LA COL 62.
       *>    SET WS-LA TO 1.
       *>    GO TO ESTOQUE. 
       
       EXIBIR2.
           DISPLAY "DIGITE O NOME DO FILME: " AT LINE 1 COL 36.
           ACCEPT FILME-TITULO AT LINE 1 COL 60.

           OPEN INPUT ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           READ ARQ-ESTOQUE INTO WS-FILME
               KEY IS FILME-TITULO
               INVALID KEY
                   DISPLAY "FILME NAO CADASTRADO" AT LINE 3 COL 36
               NOT INVALID KEY
                   DISPLAY "FILME: " AT LINE 3 COL 36
                   DISPLAY WS-FILME-TITULO AT LINE 3 COL 43
                   DISPLAY " - TOTAL: " AT LINE 3 COL 63
                   DISPLAY WS-FILME-QTD AT LINE 3 COL 73
                   DISPLAY " - DISP: " AT LINE 3 COL 76
                   DISPLAY WS-FILME-DISP AT LINE 3 COL 85
           END-READ
           CLOSE ARQ-ESTOQUE.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 5 COL 36.
           ACCEPT WS-LIXO AT LINE 5 COL 62.
           GO TO ESTOQUE. 

       ADICIONAR.
           DISPLAY "DIGITE O NOME DO FILME: " AT LINE 1 COL 36.
           ACCEPT FILME-TITULO AT LINE 1 COL 60.
           DISPLAY "DIGITE A QUANTIDADE: " AT LINE 2 COL 36.
           ACCEPT WS-QTD-AUX AT LINE 2 COL 60.

           OPEN I-O ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           READ ARQ-ESTOQUE INTO WS-FILME
               KEY IS FILME-TITULO
               INVALID KEY
                   MOVE WS-QTD-AUX TO FILME-QTD
                   MOVE WS-QTD-AUX TO FILME-DISP
                   WRITE FILME
                   DISPLAY " " AT LINE 13 COL 36
               NOT INVALID KEY
                   MOVE WS-FILME-ID TO FILME-ID
                   ADD WS-QTD-AUX TO WS-FILME-QTD GIVING FILME-QTD
                   ADD WS-QTD-AUX TO WS-FILME-DISP GIVING FILME-DISP
                   REWRITE FILME
           END-READ
           CLOSE ARQ-ESTOQUE.
           DISPLAY "FILME ADICIONADO" AT LINE 11 COL 36.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 12 COL 36.
           ACCEPT WS-LIXO AT LINE 12 COL 62.
           GO TO ESTOQUE.

       REMOVER.
           DISPLAY "DIGITE O NOME DO FILME: " AT LINE 1 COL 36.
           ACCEPT FILME-TITULO AT LINE 1 COL 60.
           DISPLAY "QTD A SER REMOVIDA: " AT LINE 2 COL 36.
           ACCEPT WS-QTD-AUX AT LINE 2 COL 60.
           
           OPEN I-O ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           READ ARQ-ESTOQUE INTO WS-FILME
               KEY IS FILME-TITULO
               INVALID KEY
                   DISPLAY "FILME NAO CADASTRADO" AT LINE 11 COL 36
               NOT INVALID KEY
                   IF WS-FILME-QTD > WS-FILME-DISP
                       MOVE 1 TO WS-ALUGADOS
                   END-IF
                   SUBTRACT WS-QTD-AUX FROM WS-FILME-QTD
                   SUBTRACT WS-QTD-AUX FROM WS-FILME-DISP
                   IF WS-FILME-QTD < 1
                       IF WS-ALUGADOS = 1
                           DISPLAY "FILMES ALUGADOS" AT LINE 11 COL 36
                           MOVE 0 TO WS-ALUGADOS
                       END-IF
                       DELETE ARQ-ESTOQUE RECORD
                       END-DELETE
                   ELSE
                       MOVE WS-FILME-ID TO FILME-ID
                       MOVE WS-FILME-QTD TO FILME-QTD
                       MOVE WS-FILME-DISP TO FILME-DISP
                       REWRITE FILME
                   END-IF
                   DISPLAY "EXEMPLARES REMOVIDOS" AT LINE 10 COL 36
           END-READ.
           CLOSE ARQ-ESTOQUE.

           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 12 COL 36.
           ACCEPT WS-LIXO AT LINE 12 COL 62.
           GO TO ESTOQUE.

       ERROARQ.
           IF WS-FILESTATUS = 35
               DISPLAY "ESTOQUE VAZIO" AT LINE 11 COL 36
               CLOSE ARQ-ESTOQUE
               OPEN OUTPUT ARQ-ESTOQUE
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FILESTATUS AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 11 COL 50.
           CLOSE ARQ-ESTOQUE.
           GO TO ESTOQUE.

       GET_ULTIMO_ID.
           MOVE 1 TO FILME-ID.
           OPEN INPUT ARQ-ESTOQUE.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           SET ARQ-EOF TO 0.
           PERFORM UNTIL ARQ-EOF = 1
               READ ARQ-ESTOQUE INTO WS-FILME
                   KEY IS FILME-ID
                   INVALID KEY
                       SET ARQ-EOF TO 1
                   NOT INVALID KEY
                       ADD 1 TO FILME-ID
               END-READ
           END-PERFORM.
           CLOSE ARQ-ESTOQUE.
           SET ARQ-EOF TO 0.
           GO TO ADICIONAR.

       FIM.
       END PROGRAM ESTOQUE.