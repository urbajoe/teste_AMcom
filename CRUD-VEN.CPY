       S-INCLUIR SECTION.
       INCLUIR-1.
           PERFORM UNTIL WCODIGO = 9999999
           DISPLAY TELA-VEN
           ACCEPT TELA-CODIGO.
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO NOT = ZEROES
              PERFORM INCLUIR-2
           END-IF.
           
       INCLUIR-2.
           MOVE WCODIGO TO CODIGO  
           PERFORM LER-VEN
           IF FS-CLI = '00'
              MOVE 'VENDEDOR JÁ EXISTE ST ' TO WTXT
              MOVE FS-VEN TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF. 
       
       INCLUIR-3.
           PERFORM UNTIL WSAIDA = 0
           ACCEPT TELA-VEN
           MOVE 'f'     TO WCGCCPF-1.   
           MOVE WCGCCPF TO LINK-CPFCGC.                             
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM GRAVACAO-1 THRU GRAVACAO-EXIT
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE GRAVAÇÃO ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF.   
       INC-FIM.
             EXIT.
       
       S-ALTERAR SECTION.
       ALTERAR-1.
           PERFORM UNTIL WCODIGO = 9999999
           DISPLAY TELA-CLI
           ACCEPT TELA-CODIGO.
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO NOT = ZEROES
              PERFORM ALTERAR-2
           END-IF.
           
       ALTERAR-2.
           MOVE WCODIGO TO CODIGO  
           PERFORM LER-CLIENTE
           IF FS-CLI NOT = '00'
              MOVE 'CLIENTES NÃO EXISTE ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF. 

       ALTERAR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-CLI
           ACCEPT TELA-CLI
           PERFORM VALIDA-CNPJ THRU VALIDA-CNPJ-EXIT
           PERFORM REGRAVACAO-1 THRU REGRAVACAO-EXIT
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE REGRAVAÇÃO ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF.   
           
       ALT-FIM.
             EXIT.

       S-EXCLUIR SECTION.
       EXCLUIR-1.
           PERFORM UNTIL WCODIGO = 9999999
           DISPLAY TELA-CLI
           ACCEPT TELA-CODIGO.
           PERFORM CHECA-CODIGO THRU CHECA-CODIGO-EXIT
           IF WCODIGO NOT = ZEROES
              PERFORM EXCLUIR-2
           END-IF.
       EXCLUIT-2.
           MOVE WCODIGO TO CODIGO  
           PERFORM LER-CLIENTE
           IF FS-CLI NOT = '00'
              MOVE 'CLIENTES NÃO EXISTE ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF. 

       EXCLUIR-3.
           PERFORM MOVE-DADOS-2.
           PERFORM UNTIL WSAIDA = 0
           DISPLAY TELA-CLI
           PERFORM S-DELETA
           IF FS-CLI NOT = '00'
              MOVE 1 TO WSAIDA
              MOVE 'ERRO DE EXCLUSÃO ST ' TO WTXT
              MOVE FS-CLI TO WST
              DISPAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF.   
       EXCL-FIM.
           EXIT.

       S-GRAVAR SECTION.
       GRAVACAO-1.
           MOVE 'CONFIRMA ?...' TO WTXT
           DISPAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              PERFORM MOVE-DADOS
              WRITE REGISTRO-CLIENTES
           END-IF.
       GRAVACAO-EXIT.
           EXIT.

       S-REGRAVAR SECTION.
       REGRAVACAO-1.
           MOVE 'CONFIRMA ?...' TO WTXT
           DISPAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              PERFORM MOVE-DADOS
              REWRITE REGISTRO-CLIENTES
           END-IF.
       REGRAVACAO-EXIT.
           EXIT.

       S-DELETAR SECTION
       DELETA-1.
           MOVE 'CONFIRMA EXCLUSÃO ?...' TO WTXT
           DISPAY TELA-MENSAGEM
           ACCEPT WX.
           IF WX = 'S' OR 's'
              DELETE CLIENTES
           END-IF.
       DELETA-1-EXIT.
           EXIT.
       
       S-MOVER-DADOS SECTION.
       MOVE-DADOS.
           MOVE WCNPJ      TO CNPJ
           MOVE WRAZAO     TO RAZAO 
           MOVE WLATITUDE  TO LATITUDE 
           MOVE WLONGITUDE TO LONGITUDE.

       MOVE-DADOS-2.
           MOVE CNPJ      TO WCNPJ
           MOVE RAZAO     TO WRAZAO 
           MOVE LATITUDE  TO WLATITUDE 
           MOVE LONGITUDE TO WLONGITUDE.

           

