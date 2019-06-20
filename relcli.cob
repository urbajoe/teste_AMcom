﻿       IDENTIFICATION DIVISION.
       PROGRAM-ID.  relcli.
       AUTHOR.      URBANO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT CLIENTES ASSIGN TO  DISK WID-ARQ-CLIENTE
              ORGANIZATION      IS INDEXED
              ACCESS MODE       IS DYNAMIC
              RECORD KEY        IS CHAVE-CLI
              ALTERNATE RECORD KEY IS CNPJ   WITH DUPLICATES
              ALTERNATE RECORD KEY IS CODIGO WITH DUPLICATES
              ALTERNATE RECORD KEY IS RAZAO  WITH DUPLICATES
              ALTERNATE RECORD KEY IS CH01-CLI-1 =
                                          CODIGO
                                          RAZAO WITH DUPLICATES
              LOCK MODE         IS MANUAL
              FILE STATUS       IS FS-CLI.
       SELECT ARQREL  ASSIGN TO DISK WID-ARQ-RELT
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMPORT.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTES.
       01 REGISTRO-CLIENTES.
          02 CHAVE-CLI.
             03 CODIGO             PIC  9(007).
             03 CNPJ               PIC  9(014).
          02 RAZAO                 PIC  X(040).
          02 LATITUDE              PIC S9(003)v9(008).
          02 LONGITUDE             PIC S9(003)v9(008).
       
       FD  ARQIREL.
       01  ARQREL-REC.
           05  ARQREL-DATA-01   PIC X(00512).

       WORKING-STORAGE SECTION.
       01 WARQIMPORT-REC               PIC X(512).
       01 WARQIMPORT-2 REDEFINES WARQIMPORT-REC.
          02 WCHAVE-CLI-IMP.
             03 WCODIGO-IMP             PIC  9(007) VALUE ZEROES.
             03 WCNPJ-IMP               PIC  9(014) VALUE ZEROES.
          02 WRAZAO-IMP                 PIC  X(040) VALUE ZEROES.
          02 WLATITUDE-IMP              PIC S9(003)v9(008) VALUE ZEROES.
          02 WLONGITUDE-IMP             PIC S9(003)v9(008) VALUE ZEROES.
          02 FILLER                     PIC X(429).
 
      01 WREGISTRO-CLIENTES.
          02 WCHAVE-CLI.
             03 WCODIGO             PIC  9(007) VALUE ZEROES.
             03 WCNPJ               PIC  9(014) VALUE ZEROES.
          02 WRAZAO                 PIC  X(040) VALUE ZEROES.
          02 WLATITUDE              PIC S9(003)v9(008) VALUE ZEROES.
          02 WLONGITUDE             PIC S9(003)v9(008) VALUE ZEROES.
       
       01 FS-CLI.
           02 FS-CLIENTES-1                PIC 9.
           02 FS-CLIENTES-2                PIC 9.
           02 FS-CLIENTE-R REDEFINES FS-CLIENTES-2 PIC 99 COMP-X.
       01 FS-IMPORT.
           02 FS-IMPORT-1                PIC 9.
           02 FS-IMPORT-2                PIC 9.
           02 FS-IMPORT-R REDEFINES FS-IMPORT-2 PIC 99 COMP-X.

       01 WID-ARQ-CLIENTE.
           02 WID-ARQ-CLIENTE-1            PIC X(50).
       
       01 WID-ARQ-IMPORT.
           02 WID-ARQ-IMPORT-1             PIC X(50).

       01 WCGCCPF.
           02 WCGCCPF-1                    PIC X(01).
           02 WCGCCPF-2                    PIC X(01).

       01 WOPCAO1         PIC X VALUE SPACES.
       01 WOPCAO2         PIC X VALUE SPACES.
       01 WOPCAO3         PIC X VALUE SPACES.
       01 WOPCAO4         PIC X VALUE SPACES.
       01 WOPCAO5         PIC X VALUE SPACES.
 
      77 WX              PIC X VALUE SPACES.
       77 WSAIDA          PIC 9 VALUE ZEROES.
      *                  (0=OK, 1=SAIDA)
       77 WCONT           PIC 9(04) VALUE ZEROES.

       01 WMENSAGEM.
          02 WTXT         PIC X(50) VALUE SPACES.
          02 WST          PIC 99 VALUE SPACES.
       
       COPY LAY-REL-CLI.CPY.
       COPY FUNCAO.CPY.
       COPY REL-TELAS-CLI.CPY.

       COPY L-LINK.CPY
                 
       PROCEDURE DIVISION USING LINK-DADOS.
       ABERTURA.
           INITIALIZE WSAIDA.
           MOVE LID-ARQ-CLIENTE-1 TO WID-ARQ-CLIENTE-1
           MOVE LID-ARQ-VENDEDOR-1 TO WID-ARQ-VENDEDOR-1
           PERFORM ABRE-CLIENTE.
               

       ACCEPTA-OPCAO.
           PERFORM UNTIL NOT ESC
                DISPLAY TELA-OPCAO
                ACCEPT TELA-ACC-OPCAO
           IF WOPCAO1
               PERFORM S-OPCAO1
           END-IF
           IF WOPCAO2
               PERFORM S-OPCAO2
           END-IF
           IF WOPCAO3
               PERFORM S-OPCAO3
           END-IF
           IF WOPCAO4
                PERFORM S-OPCAO4
           END-IF.
       ACCEPTA-OPCAO-EXIT.
           EXIT PROGRAM.

       ABRE-CLIENTE.
           OPEN I-O CLIENTES.
           IF FS-CLI NOT = '00'
              MOVE 'ERRO NA ABERTURA DO ARQUIVO DE CLIENTES ST ' WTXT
              MOVE FS-CLI TO WST
              DISPLAY TELA-MENSAGEM
              ACCEPT WX
              STOP RUN


       COPY R-IMP-REL.CPY.    

