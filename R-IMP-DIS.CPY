       S-GERA-DISTRIBUICAO SECTION.
       R-GERA-DISTRIBUICAO-1.
           START DISTRIBUICAO KEY IS NOT LESS THAN CHAVE-DIS
           IF FS-DIS NOT = '0'
                 MOVE 'ERRO START DISTRIBUIÇÃO ST ' TO WTXT
                 MOVE FS-DIS TO WST
                 DISPAY TELA-MENSAGEM
                 ACCEPT WX
                 INITIALIZE TELA-MENSAGEM
                 DISPLAY TELA-MENSAGEM
                 EXIT
           END-IF
           MOVE 'RELDIS1.CSV' TO WID-ARQ-RELT
           OPEN OUTPUT ARQREL
           PERFORM S-IMP-CAB
           PERFORM S-IMPRIME UNTIL FS-DIS = '10'.
       R-GERA-DISTRIBUICAO-1-EXIT.
           EXIT.

       S-IMPRIME SECTION.
       R-IMPRIME-1.
      *-------------CALCULO DLO -------------------------
       01 WDLO-VEN        PIC S9(003)v9(008) VALUE ZEROS.
       01 WDLO-CLI        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (DLO-VEN - DLO-CLI) = RESULT1
       01 WRESULT1        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (RESULT1 / 60) RESULT2
       01 WRESULT2        PIC S9(003)v9(008) VALUE ZEROS.
      *------ (RESULT2 * 1852) RESULT-FINAL
       01 WRESULT-FINAL   PIC S9(003)v9(008) VALUE ZEROS.
      *--------------------------------------------------
           03 RL-CODIGO-CLI       PIC 9(07)  VALUE ZEROES.
           03 RL-RAZAO-CLI        PIC X(040) VALUE SPACES.
           03 RL-CODIGO-VEN       PIC 9(03)  VALUE ZEROES.
           03 RL-NOME-VEN         PIC X(40)  VALUE SPACES.
           03 RL-DISTANCIA-VEN    PIC S9(003)v9(008) VALUE ZEROS.

              03 CODIGO-VEN-DIS         PIC  9(003).
              03 CODIGO-CLI-DIS         PIC  9(007).
 

           READ DISTRIBUICAO NEXT.
           MOVE CODIGO-VEN-DIS TO CODIGO-VEN
           READ VENDEDORES
           IF FS-VEN = '00'
              MOVE NOME-VEN TO RL-NOME-VEN 
              MOVE CODIGO-VEN-DIS TO RL-CODIGO-VEN
              MOVE LONGITUDE-VEN TO  WDLO-VEN
           END-IF
           MOVE CODIGO-CLI-DIS TO CODIGO
           READ CLIENTES
           IF FS-CLI = '00'
              MOVE RAZAO TO RL-RAZAO-CLI
              MOVE CODIGO TO RL-CODIGO-CLI
              MOVE LONGITUDE TO  WDLO-CLI
           END-IF
           COMPUTE WRESULT1 = (DLO-VEN - DLO-CLI)
           COMPUTE WRESULT2 = (WRESULT1 / 60)
           COMPUTE WRESULT-FINAL2 = (RESULT2 * 1852)
           MOVE WRESULT-FINAL2 TO RL-DISTANCIA-VEN. 
           PERFORM S-IMP-DET
       R-IMP-REL-VEN-1-EXIT.
           EXIT.
 
       S-IMP-CAB SECTION.
       R-IMP-CAB.
           MOVE CAB02 TO ARQREL-DATA-01
           WRITE ARQREL-REC
           MOVE CAB02 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
       R-IMP-CAB-EXIT.
           EXIT.

       S-IMP-DET SECTION.
       R-IMP-DET.
           MOVE LIN01 TO ARQREL-DATA-01
           WRITE ARQREL-REC.
       R-IMP-DET-EXIT.
           EXIT.
        

