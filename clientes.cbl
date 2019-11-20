      ******************************************************************
      * Author: MATHEUS LOBO
      * Date: 05/11/19
      * Purpose: Trabalho - sistema de locadora de videos
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLIENTES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
            01 WS-OPTION    PIC 9(01).
            01 WS-LIXO PIC X(01).
       SCREEN SECTION.
       01 CLIENTES-SCREEN.
            02 BLANK SCREEN.
            02 LINE 1  COL 1  VALUE "--------------------------------".
            02 LINE 2  COL 1  VALUE "-           CLIENTES           -".
            02 LINE 3  COL 1  VALUE "--------------------------------".
            02 LINE 4  COL 1  VALUE "- 1 - EXIBIR CLIENTES:         -".
            02 LINE 5  COL 1  VALUE "- 2 - CADASTRAR CLIENTE:       -".
            02 LINE 6  COL 1  VALUE "- 3 - REMOVER CLIENTE:         -".
            02 LINE 7  COL 1  VALUE "-                      0-VOLTAR-".
            02 LINE 8  COL 1  VALUE "-                              -".
            02 LINE 8  COL 30 PIC 9(1) TO WS-OPTION.
            02 LINE 9  COL 1  VALUE "--------------------------------".
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
           
       ADICIONAR.
           
       REMOVER.
           
       FIM.
       END PROGRAM CLIENTES.
       