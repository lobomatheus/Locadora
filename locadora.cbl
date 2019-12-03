      ******************************************************************
      * Author: MATHEUS LOBO
      * Date: 05/11/19
      * Purpose: Trabalho - sistema de locadora de videos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCADORA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
            01 WS-OPTION    PIC 9(01).
            01 WS-LIXO PIC X(01).
       SCREEN SECTION.
       01 MENU-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1 COL 1 VALUE "-------------------------------- |".
            02 LINE 2 COL 1 VALUE "-          LOCADORA            - |".
            02 LINE 3 COL 1 VALUE "-------------------------------- |".
            02 LINE 4 COL 1 VALUE "- 1 - ESTOQUE:                 - |".
            02 LINE 5 COL 1 VALUE "- 2 - CLIENTES:                - |".
            02 LINE 6 COL 1 VALUE "- 3 - EMPRESTIMOS:             - |".
            02 LINE 7 COL 1 VALUE "-                        0-SAIR- |".
            02 LINE 8 COL 1 VALUE "-                              - |".
            02 LINE 8 COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9 COL 1 VALUE "-------------------------------- |".
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY MENU-SCREEN.
           ACCEPT MENU-SCREEN.
           EVALUATE WS-OPTION
               WHEN 0
                   STOP RUN
               WHEN 1
                   CALL 'ESTOQUE'
               WHEN 2
                   CALL 'CLIENTES'
               WHEN 3
                   CALL 'EMPRESTIMOS'
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA" AT LINE 12
                   DISPLAY "PRESSIONE QUALQUER TECLA" AT LINE 13
                   ACCEPT WS-LIXO AT LINE 13 COL 26
                   GO TO MAIN
           END-EVALUATE.
       GO TO MAIN.
       END PROGRAM LOCADORA.
