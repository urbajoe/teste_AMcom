       IDENTIFICATION DIVISION.
       PROGRAM-ID. MENU.
       AUTHOR. URBANO
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WOPCAO          PIC 9 VALUE ZERO.
       
       01 LINK-DADOS.
          02 LID-ARQ-CLIENTE.
             03 WID-ARQ-CLIENTE-1             PIC X(50) VALUE 'CLIENTES.DAT'.
       
          02 LID-ARQ-VENDEDOR.
             03 WID-ARQ-VENDEDOR-1            PIC X(50) VALUE 'VENDEDOR.DAT'.
       
           02 LID-ARQ-DISTRIBUICAO.
             03 WID-ARQ-DISTRIBUICAO-1        PIC X(50) VALUE 'DISTRIBUICAO.DAT'.
       
       SCREEN SECTION.
       01 MENU-PRINCIPAL.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "SISTEMA EXEMPLO"
      -.
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 4 COL 33 VALUE "MENU PRINCIPAL".
          02 LINE 7 COL 10 VALUE "[1] CADASTRO CLIENTES".
          02 LINE 8 COL 10 VALUE "[2] CADASTRO VENDEDORES".
          02 LINE 9 COL 10 VALUE "[3] RELATORIO DE CLIENTES".
          02 LINE 10 COL 10 VALUE "[4] RELATORIO DE VENDEDORES".
          02 LINE 11 COL 10 VALUE "[5] EXECUTAR DISTRIBUICAO DE CLIENTE".
          02 LINE 12 COL 10 VALUE "[6] SAIR SISTEMA".
          02 LINE 15 COL 10 "DIGITE A OPCAO DESEJADA[.]".
          02 OPCAO LINE 15 COL 34 PIC 9 USING WOPCAO AUTO.
       PROCEDURE DIVISION.
       INICIO.
          DISPLAY MENU-PRINCIPAL
          ACCEPT MENU-PRINCIPAL
          PERFORM UNTIL WOPCAO 6
          EVALUATE WOPCAO
             WHEN 1
               CALL "clientes" USING LINK-DADOS
             WHEN 2
               CALL "vendedores" USING LINK-DADOS
             WHEN 3
               CALL "relcli" USING LINK-DADOS
             WHEN 4
                CALL "relvend" USING LINK-DADOS
             WHEN 5
                CALL "ExeDistrCli" USING LINK-DADOS
             WHEN 6
                STOP RUN
           END-EVALUATE.
