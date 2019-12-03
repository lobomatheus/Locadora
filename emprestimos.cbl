      ******************************************************************
      * Author: MATHEUS LOBO
      * Date: 05/11/19
      * Purpose: Trabalho - sistema de locadora de videos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRESTIMOS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQ-EMPRESTIMOS
           ASSIGN TO "./emprestimos.dat"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL
           FILE STATUS IS WS-FS.
       SELECT ARQ-CLIENTES
           ASSIGN TO "./clientes.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CLIENTE-NOME
           FILE STATUS IS WS-FS2.
       SELECT ARQ-ESTOQUE
           ASSIGN TO "./estoque.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS FILME-ID
           ALTERNATE KEY IS FILME-TITULO
           FILE STATUS IS WS-FS3.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-EMPRESTIMOS.
       01 EMPRESTIMO.
           05 DATA-INI  PIC X(20).
           05 DATA-FIM PIC X(20).
           05 PESSOA PIC X(20).
           05 FILME-EMP PIC X(20).
       FD ARQ-CLIENTES.
       01 CLIENTE.
           05 CLIENTE-NOME  PIC X(20).
           05 CLIENTE-EMP PIC 9.
       FD ARQ-ESTOQUE.
       01 FILME.
           05 FILME-ID PIC 9(02).
           05 FILME-TITULO  PIC X(20).
           05 FILME-QTD  PIC 99.
           05 FI-DISP PIC 99.

       WORKING-STORAGE SECTION.
            01 WS-OPTION    PIC 9(01).
            01 WS-LIXO PIC X(01).
            01 WS-EMPRESTIMO.
               05 WS-DATA-INI  PIC X(20).
               05 WS-DATA-FIM PIC X(20).
               05 WS-PESSOA PIC X(20).
               05 WS-FILME-EMP PIC X(20).
            01 WS-CLIENTE.
               05 WS-CLIENTE-NOME PIC X(20).
               05 WS-CLIENTE-EMP PIC 9.
            01 WS-FILME.
               05 WS-FILME-ID PIC 9(02).
               05 WS-FILME-TITULO PIC X(20).
               05 WS-FILME-QTD PIC 9(02).
               05 WS-FI-DISP PIC 9(02).
            77 WS-FS PIC 99.
            77 WS-FS2 PIC 99.
            77 WS-FS3 PIC 99.
            77 EOF PIC 9.
            77 WS-LA PIC 99 VALUE 01.
       SCREEN SECTION.
       01 EMPRESTIMOS-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1  COL 1  VALUE "------------------------------ |".
            02 LINE 2  COL 1  VALUE "-         EMPRESTIMOS        - |".
            02 LINE 3  COL 1  VALUE "------------------------------ |".
            02 LINE 4  COL 1  VALUE "- 1 - EXIBIR EMPRESTIMOS:    - |".
            02 LINE 5  COL 1  VALUE "- 2 - FAZER EMPRESTIMO:      - |".
            02 LINE 6  COL 1  VALUE "- 3 - REALIZAR DEVOLUCAO:    - |".
            02 LINE 7  COL 1  VALUE "-                    0-VOLTAR- |".
            02 LINE 8  COL 1  VALUE "-                            - |".
            02 LINE 8  COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9  COL 1  VALUE "------------------------------ |".
       PROCEDURE DIVISION.
       EMPRESTIMOS.
           DISPLAY EMPRESTIMOS-SCREEN.
           ACCEPT EMPRESTIMOS-SCREEN.
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
                   GO TO EMPRESTIMOS
           END-EVALUATE.

       EXIBIR.
           SET EOF TO 0.
           OPEN INPUT ARQ-EMPRESTIMOS.
           IF WS-FS <> 0
               GO TO ERROARQ-EMP.
           PERFORM UNTIL EOF = 1
               READ ARQ-EMPRESTIMOS INTO WS-EMPRESTIMO
                   AT END SET EOF TO 1
                   NOT AT END
                       DISPLAY "DATA EMPRESTIMO: " AT LINE WS-LA COL 36
                       DISPLAY WS-DATA-INI AT LINE WS-LA COL 53
                       DISPLAY "DATA DEVOLUCAO: " AT LINE WS-LA COL 64
                       DISPLAY WS-DATA-FIM AT LINE WS-LA COL 80
                       DISPLAY "FILME:" AT LINE WS-LA COL 91
                       DISPLAY WS-FILME-EMP AT LINE WS-LA COL 97
                       DISPLAY "CLIENTE: " AT LINE WS-LA COL 112
                       DISPLAY WS-PESSOA AT LINE WS-LA COL 121
                       ADD 1 TO WS-LA
               END-READ
           END-PERFORM.
           CLOSE ARQ-EMPRESTIMOS.
           SET EOF TO 0.
           ADD 1 TO WS-LA.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE WS-LA COL 36.
           ACCEPT WS-LIXO AT LINE WS-LA COL 62.
           SET WS-LA TO 1.
           GO TO EMPRESTIMOS.

       ADICIONAR.
           DISPLAY "CLIENTE: " AT LINE 1 COL 36.
           ACCEPT WS-PESSOA AT LINE 1 COL 45.
           DISPLAY "FILME: " AT LINE 2 COL 36.
           ACCEPT WS-FILME-EMP AT LINE 2 COL 43.
           DISPLAY "DATA DE EMPRESTIMO: " AT LINE 3 COL 36.
           ACCEPT WS-DATA-INI AT LINE 3 COL 56.
           DISPLAY "DATA DE DEVOLUCAO: " AT LINE 4 COL 36.
           ACCEPT WS-DATA-FIM AT LINE 4 COL 55.

           OPEN I-O ARQ-ESTOQUE.
           IF WS-FS3 <> 0
               GO TO ERROARQ-EST.
           OPEN I-O ARQ-CLIENTES.
           IF WS-FS2 <> 0
               GO TO ERROARQ-CLI.
           OPEN EXTEND ARQ-EMPRESTIMOS. 
           IF WS-FS <> 0
               GO TO ERROARQ-EMP.
           
           MOVE WS-PESSOA TO CLIENTE-NOME.
           READ ARQ-CLIENTES INTO WS-CLIENTE
               KEY IS CLIENTE-NOME
               INVALID KEY
                   DISPLAY "CLIENTE NAO CADASTRADO" AT LINE 6 COL 36
                   GO TO FECHAPROCEDIMENTO
               NOT INVALID KEY
                   MOVE WS-FILME-EMP TO FILME-TITULO.
                   READ ARQ-ESTOQUE INTO WS-FILME
                       KEY IS FILME-TITULO
                       INVALID KEY
                           DISPLAY "FILME INDISPONIVEL" AT LINE 6 COL 36
                           GO TO FECHAPROCEDIMENTO
                       NOT INVALID KEY
                           IF WS-FI-DISP > 0
                               SUBTRACT 1 FROM WS-FI-DISP GIVING FI-DISP
                               MOVE WS-FILME-ID TO FILME-ID
                               MOVE WS-FILME-QTD TO FILME-QTD
                               REWRITE FILME
                           ELSE
                               DISPLAY "FILME ESGOTADO" AT LINE 6 COL 36
                               GO TO FECHAPROCEDIMENTO
                           END-IF
                   ADD 1 TO WS-CLIENTE-EMP GIVING CLIENTE-EMP
                   REWRITE CLIENTE
           END-READ.
           MOVE WS-PESSOA TO PESSOA.
           MOVE WS-FILME-EMP TO FILME-EMP.
           MOVE WS-DATA-INI TO DATA-INI.
           MOVE WS-DATA-FIM TO DATA-FIM.
           WRITE EMPRESTIMO.
           DISPLAY "EMPRESTIMO REALIZADO COM SUCESSO" AT LINE 6 COL 36.
           GO TO FECHAPROCEDIMENTO.

       REMOVER.
           DISPLAY "CLIENTE: " AT LINE 1 COL 36.
           ACCEPT CLIENTE-NOME AT LINE 1 COL 45.
           DISPLAY "FILME: " AT LINE 2 COL 36.
           ACCEPT FILME-TITULO AT LINE 2 COL 43.

           OPEN I-O ARQ-ESTOQUE.
           IF WS-FS3 <> 0
               GO TO ERROARQ-EST.
           READ ARQ-ESTOQUE INTO WS-FILME
               KEY IS FILME-TITULO
               NOT INVALID KEY
                   ADD 1 TO WS-FI-DISP GIVING FI-DISP
                   MOVE WS-FILME-QTD TO FILME-QTD
                   MOVE WS-FILME-ID TO FILME-ID
                   REWRITE FILME
           END-READ.
           CLOSE ARQ-ESTOQUE.

           OPEN I-O ARQ-CLIENTES.
           IF WS-FS2 <> 0
               GO TO ERROARQ-CLI.
           READ ARQ-CLIENTES INTO WS-CLIENTE
               KEY IS CLIENTE-NOME
               NOT INVALID KEY
                   SUBTRACT 1 FROM WS-CLIENTE-EMP GIVING CLIENTE-EMP
                   REWRITE CLIENTE
           END-READ.
           CLOSE ARQ-CLIENTES.
           
           DISPLAY "DEVOLUCAO FEITA" AT LINE 6 COL 36.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 7 COL 36.
           ACCEPT WS-LIXO AT LINE 7 COL 62.
           GO TO EMPRESTIMOS.

       FECHAPROCEDIMENTO.
           CLOSE ARQ-ESTOQUE.
           CLOSE ARQ-CLIENTES.
           CLOSE ARQ-EMPRESTIMOS.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 7 COL 36.
           ACCEPT WS-LIXO AT LINE 7 COL 62.
           GO TO EMPRESTIMOS.
       
       ERROARQ-EMP.
           IF WS-FS = 35
               DISPLAY "NENHUM REGISTRO" AT LINE 11 COL 36
               CLOSE ARQ-EMPRESTIMOS
               OPEN OUTPUT ARQ-EMPRESTIMOS
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FS AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 11 COL 52.
           CLOSE ARQ-EMPRESTIMOS.
           GO TO EMPRESTIMOS.

       ERROARQ-CLI.
           IF WS-FS2 = 35
               DISPLAY "NENHUM REGISTRO DE CLIENTE" AT LINE 11 COL 36
               CLOSE ARQ-CLIENTES
               OPEN OUTPUT ARQ-CLIENTES
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FS2 AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 11 COL 64.
           CLOSE ARQ-CLIENTES.
           GO TO EMPRESTIMOS.

       ERROARQ-EST.
           IF WS-FS3 = 35
               DISPLAY "NENHUM REGISTRO DE FILME" AT LINE 11 COL 36
               CLOSE ARQ-ESTOQUE
               OPEN OUTPUT ARQ-ESTOQUE
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FS3 AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 11 COL 64.
           CLOSE ARQ-ESTOQUE.
           GO TO EMPRESTIMOS.

       FIM.
       END PROGRAM EMPRESTIMOS.
       