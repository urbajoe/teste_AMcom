﻿       
       SCREEN SECTION.
       01 TELA-OPCAO.
          02 BLANK SCREEN.
          02 LINE 1 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 2 COL 21 VALUE "SISTEMA EXEMPLO - RELATÓRIO DE CLIENTES"
      -.
          02 LINE 3 COL 1 VALUE "***************************************
      -"************************************".
          02 LINE 4 COL 33 VALUE "OPÇÃO".
          02 LINE 7 COL 10 VALUE "1) ASCENDENTE(a) ou DESCENDENTE(d).........[  ]".
          02 LINE 8 COL 10 VALUE "2) POR CÓDIGO CLIENTE E RAZÃO SOCIAL(c)....[  ]".
          02 LINE 9 COL 10 VALUE "3) FILTRAR POR CÓDIGO DE CLIENTE(f)........[  ]".
          02 LINE 10 COL 10 VALUE "4) FILTRAR POR RAZÃO SOCIAL(g)............[  ]".
          02 LINE 11 COL 10 VALUE "5) FILTRAR POR CÓDIGO DO VENDEDOR(h)......[  ]".
          02 LINE 12 COL 10 VALUE "[ESC] SAIR".
       
       01 TELA-ACC-OPCAO.
          02 LINE 07 COL 45 PIC X USING WOPCAO1 AUTO.
          02 LINE 08 COL 45 PIC X USING WOPCAO2 AUTO.
          02 LINE 09 COL 45 PIC X USING WOPCAO3 AUTO.
          02 LINE 10 COL 45 PIC X USING WOPCAO4 AUTO.
          02 LINE 11 COL 45 PIC X USING WOPCAO5 AUTO.
       
       01 TELA-MENSAGEM.
          02 LINE 25 COL 10 VALUE "MENSAGEM.....:".
          02 LINE 25 COL 27 PIC X(50) WMENSAGEM.

