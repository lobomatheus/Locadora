      ******************************************************************
      * Author: MATHEUS LOBO
      * Date: 05/11/19
      * Purpose: Trabalho - sistema de locadora de videos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQ-CLIENTES
           ASSIGN TO "./clientes.dat"
           ORGANIZATION IS INDEXED
           ACCESS IS RANDOM
           RECORD KEY IS CLIENTE-NOME
           FILE STATUS IS WS-FILESTATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ARQ-CLIENTES.
       01 CLIENTE.
           05 CLIENTE-NOME  PIC X(20).
           05 CLIENTE-EMP PIC 9.
       WORKING-STORAGE SECTION.
            01 WS-OPTION    PIC 9(01).
            01 WS-LIXO PIC X(01).
            01 WS-CLIENTE.
               05 WS-CLIENTE-NOME PIC X(20).
               05 WS-CLIENTE-EMP PIC 9.
            77 WS-FILESTATUS PIC 99.
       SCREEN SECTION.
       01 CLIENTES-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1  COL 1  VALUE "------------------------------ |".
            02 LINE 2  COL 1  VALUE "-           CLIENTES           |".
            02 LINE 3  COL 1  VALUE "------------------------------ |".
            02 LINE 4  COL 1  VALUE "- 1 - BUSCAR CLIENTE:        - |".
            02 LINE 5  COL 1  VALUE "- 2 - CADASTRAR CLIENTE:     - |".
            02 LINE 6  COL 1  VALUE "- 3 - REMOVER CLIENTE:       - |".
            02 LINE 7  COL 1  VALUE "-                    0-VOLTAR- |".
            02 LINE 8  COL 1  VALUE "-                            - |".
            02 LINE 8  COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9  COL 1  VALUE "------------------------------ |".
       PROCEDURE DIVISION.
       CLIENTES.
           DISPLAY CLIENTES-SCREEN.
           ACCEPT CLIENTES-SCREEN.
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
                   GO TO CLIENTES
           END-EVALUATE.
       EXIBIR.
           DISPLAY "DIGITE O NOME DO CLIENTE: " AT LINE 1 COL 36.
           ACCEPT CLIENTE-NOME AT LINE 1 COL 62.

           OPEN INPUT ARQ-CLIENTES.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           READ ARQ-CLIENTES INTO WS-CLIENTE
               KEY IS CLIENTE-NOME
               INVALID KEY DISPLAY "NAO CADASTRADO" AT LINE 3 COL 36
               NOT INVALID KEY 
                   DISPLAY WS-CLIENTE-NOME AT LINE 3 COL 36
                   DISPLAY " - EMPRESTIMOS: " AT LINE 3 COL 56
                   DISPLAY WS-CLIENTE-EMP AT LINE 3 COL 72
           END-READ
           CLOSE ARQ-CLIENTES.

           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 12 COL 36.
           ACCEPT WS-LIXO AT LINE 12 COL 62.
           GO TO CLIENTES.

       ADICIONAR.
           DISPLAY "DIGITE O NOME DO CLIENTE: " AT LINE 1 COL 36.
           ACCEPT CLIENTE-NOME AT LINE 1 COL 62.
           SET CLIENTE-EMP TO 0.

           OPEN I-O ARQ-CLIENTES.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           WRITE CLIENTE.
           CLOSE ARQ-CLIENTES.

           DISPLAY "CLIENTE ADICIONADO" AT LINE 11 COL 36.
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 12 COL 36.
           ACCEPT WS-LIXO AT LINE 12 COL 62.
           GO TO CLIENTES.

       REMOVER.
           DISPLAY "DIGITE O NOME DO CLIENTE: " AT LINE 1 COL 36.
           ACCEPT CLIENTE-NOME AT LINE 1 COL 62.

           OPEN I-O ARQ-CLIENTES.
           IF WS-FILESTATUS <> 0
               GO TO ERROARQ.
           DELETE ARQ-CLIENTES RECORD
               INVALID KEY DISPLAY "NAO CADASTRADO" AT LINE 12 COL 36
               NOT INVALID KEY DISPLAY "REMOVIDO" AT LINE 12 COL 36
           END-DELETE.
           CLOSE ARQ-CLIENTES.
           
           DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 13 COL 36.
           ACCEPT WS-LIXO AT LINE 13 COL 62.
           GO TO CLIENTES.

       ERROARQ.
           IF WS-FILESTATUS = 35
               DISPLAY "REGISTRO VAZIO" AT LINE 11 COL 36
               CLOSE ARQ-CLIENTES
               OPEN OUTPUT ARQ-CLIENTES
           ELSE 
               DISPLAY "ERRO NA ABERTURA DO ARQUIVO: " AT LINE 11
               DISPLAY WS-FILESTATUS AT LINE 11 COL 30
           END-IF.
           ACCEPT WS-LIXO AT LINE 11 COL 50.
           CLOSE ARQ-CLIENTES.
           GO TO CLIENTES.

       FIM.
       END PROGRAM CLIENTES.
       