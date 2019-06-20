       VALIDA-CNPJ.
           MOVE 'J'     TO WCGCCPF-1.   
           MOVE WCNPJ   TO WCGCCPF-2. 
           MOVE WCGCCPF TO LINK-CPFCGC.                             
           CALL   'CNPJCPF' USING 01  LINK-CPFCGC.
           CANCEL 'CNPJCPF' 
           IF LINK-CPFCGC(20:1) = 'I'
                 IF LINK-CPFCGC(1:1) = 'F'
                   MOVE 'CPF inválido...' TO WTXT
                 ELSE
                   MOVE 'CNPJ inválido...' TO WTXT
                 END-IF
                 DISPAY TELA-MENSAGEM
                 ACCEPT WX
                 SETFOCUS WCNPJ
           END-IF.
       VALIDA-CNPJ.
           EXIT.
