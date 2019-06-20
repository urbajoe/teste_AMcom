       IDENTIFICATION DIVISION.
       PROGRAM-ID.  vendedores.
       AUTHOR.      URBANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT VENDEDORES ASSIGN TO  DISK WID-ARQ-VENDEDORES
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-VEN
              ALTERNATE RECORD KEY IS CPF-VEN WITH DUPLICATES
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-VEN.

       SELECT ARQIMPORT  ASSIGN TO DISK WID-ARQ-IMPORT
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMPORT.

       DATA DIVISION.
       FILE SECTION.
       FD VENDEDORES.
       01  REGISTRO-VENDEDORES.
           02 CHAVE-VEN.
              03 CODIGO-VEN             PIC  9(003).
              03 CPF-VEN                PIC  9(011).
           02 NOME-VEN                  PIC  X(040).
           02 LATITUDE-VEN              PIC S9(003)v9(008).
           02 LONGITUDE-VEN             PIC S9(003)v9(008).
       
       FD  ARQIMPORT
       01  ARQIMPORT-REC.
           05  ARQIMPORT-DATA-01   PIC X(00512).

       WORKING-STORAGE SECTION.
       01 WARQIMPORT-REC                 PIC X(512).
       01 WARQIMPORT-2 REDEFINES WARQIMPORT-REC.
           02 WCHAVE-VEN-IMP.
              03 WCODIGO-VEN-IMP         PIC  9(003).
              03 WCPF-VEN-IMP            PIC  9(011).
           02 WNOME-VEN-IMP              PIC  X(040).
           02 WLATITUDE-VEN-IMP          PIC S9(003)v9(008).
           02 WLONGITUDE-VEN-IMP         PIC S9(003)v9(008).
           02 FILLER                     PIC X(429).
 
       01 WREGISTRO-VENDEDORES.
          02 WCHAVE-VEN.
             03 WCODIGO-VEN             PIC  9(003).
             03 WCPF-VEN                PIC  9(011).
          02 WNOME-VEN                  PIC  X(040).
          02 WLATITUDE-VEN              PIC S9(003)v9(008).
          02 WLONGITUDE-VEN             PIC S9(003)v9(008).
            
       01 FS-VEN.
           02 FS-VEN-1                PIC 9.
           02 FS-VEN-2                PIC 9.
           02 FS-VEN-R REDEFINES FS-VEN-2 PIC 99 COMP-X.
       01 FS-IMPORT.
           02 FS-IMPORT-1                PIC 9.
           02 FS-IMPORT-2                PIC 9.
           02 FS-IMPORT-R REDEFINES FS-IMPORT-2 PIC 99 COMP-X.

       01 WID-ARQ-VENDEDOR.
           02 WID-ARQ-VENDEDOR-1            PIC X(50).
       
       01 WID-ARQ-IMPORT.
           02 WID-ARQ-IMPORT-1             PIC X(50).

       01 WCGCCPF.
           02 WCGCCPF-1                    PIC X(01).
           02 WCGCCPF-2                    PIC X(01).

       77 WOPCAO          PIC 9 VALUE ZEROES.
       77 WX              PIC X VALUE SPACES.
       77 WSAIDA          PIC 9 VALUE ZEROES.
      *                  (0=OK, 1=SAIDA)
       77 WCONT           PIC 9(04) VALUE ZEROES.

       01 WMENSAGEM.
          02 WTXT         PIC X(50) VALUE SPACES.
          02 WST          PIC 99 VALUE SPACES.
       
       COPY FUNCAO.CPY.
       COPY TELAS-VEN.CPY.

       COPY L-LINK.CPY
                 
       PROCEDURE DIVISION USING LINK-DADOS.
       ABERTURA.
           INITIALIZE WSAIDA.
           MOVE LID-ARQ-VENDEDOR-1 TO WID-ARQ-VENDEDOR-1
           PERFORM ABRE-VEN.

       ACCEPTA-OPCAO.
           PERFORM UNTIL WOPCAO = 5
                DISPLAY TELA-OPCAO
                ACCEPT TELA-OPCAO
           EVALUATE WOPCAO
             WHEN 1
               PERFORM S-INCLUIR
             WHEN 2
               PERFORM S-ALTERAR
             WHEN 3
               PERFORM S-EXCLUIR
             WHEN 4
               PERFORM S-IMPORTAR
             WHEN 5
               EXIT PROGRAM
           END-EVALUATE.
       ACCEPTA-OPCAO-EXIT.
           EXIT.

       COPY CRUD-VEN.CPY.    

       IMPORTAR-1.
           MOVE 'INFORME O NOME DO ARQUIVO PARA IMPORTAÇÃO:' WTXT
           MOVE FS-CLI TO WST
           DISPLAY TELA-MENSAGEM
           ACCEPT TELA-NOME-ARIMPORT
           IF NOT ESC
              OPEN ARQIMPORT
              IF FS-IMPORT NOT = '00'
                 MOVE 'ERRO NA ABERTURA DO ARQUIVO DE IMPORTAÇÃO ST ' WTXT
                 MOVE FS-IMPORT TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
              ELSE
                 INITIALIZE WCONT
                 PERFORM LER-IMPORT UNTIL FS-IMPORT = '10' 
                 MOVE FS-IMPORT TO WST
                 STRING 
                  'IMPORTAÇÃO CONCLUÍDA ST ' WST ' - REG. IMPORTADOS - ' WCONT
                      DELIMITED BY SIZE INTO WTXT
                 MOVE FS-IMPORT TO WST
                 DISPLAY TELA-MENSAGEM
                 ACCEPT WX
                 CLOSE ARQIMPPORT
              END-IF
           END-IF.
           INITIALIZE TELA-NOME-ARIMPORT. 
       IMP-FIM.
           EXIT.
      
       LER-IMPORT.
           READ ARIMPORT NEXT.
           IF FS-IMPORT = '00'
              MOVE ARQIMPORT-REC TO WARQIMPORT-REC
              MOVE WCODIGO-VEN-IMP     TO WCODIGO-VEN
              MOVE WCPF-VEN-IMP        TO WCPF-VEN      
              MOVE WNOME-VEN-IMP       TO WNOME-VEN     
              MOVE WLATITUDE-VEN-IMP   TO WLATITUDE-VEN  
              MOVE WLONGITUDE-VEN-IMP  TO WLONGITUDE-VEN
              PERFORM LER-VEN
              IF FS-VEN = '23'
                 MOVE WCPF-VEN       TO CPF-VEN      
                 MOVE WNOME-VEN      TO NOME-VEN     
                 MOVE WLATITUDE      TO LATITUDE-VEN  
                 MOVE WLONGITUDE     TO LONGITUDE-VEN
                 PERFORM S-GRAVAR
                 IF FS-CLI = '00'
                    ADD 1 TO WCONT
                 ELSE
                    MOVE 'ERRO NA GRAVAÇÃO DO ARQUIVO DE VENDEDORES ST ' WTXT
                    MOVE FS-VEN TO WST
                    DISPLAY TELA-MENSAGEM
                    MOVE '10' TO FS-IMPORT
                    ACCEPT WX
                 END-IF
              END-IF
 
       ABRE-VEN.
           OPEN I-O VENDEDORES.
           IF FS-CLI NOT = '00'
              MOVE 'ERRO NA ABERTURA DO ARQUIVO DE VENDEDORES ST ' WTXT
              MOVE FS-VEN TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              STOP RUN

       LER-VEN.
           READ VENDEDORES.
       LER-VEN-FIM.
           EXIT.
        
          IF WCODIGO = ZEROES
              MOVE 'CODIGO INVALIDO ' WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              INITIALIZE TELA-MENSAGEM
              DISPLAY TELA-MENSAGEM
              SETFOCUS WCODIGO
           END-IF.
       CHECA-CODIGO-EXIT.
           EXIT.          

       COPY VALIDACPFCNPJ.CPY.
