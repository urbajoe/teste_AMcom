      * LAYOUT DO RELATORIOO DE DISTRIBUIÇÃO
       01  CAB01.
           02 FILLER            PIC X(43) VALUE SPACES.
           02 FILLER            PIC X(54) VALUE '     REL DISTRIBUIÇÃO      '.
           02 FILLER            PIC X(05) VALUE 'Pag: '.
           02 PRT-PAGINA        PIC 9(03) VALUE ZEROS.


       01  CAB02.
           03 F PIC X(16) VALUE 'COD. CLIENTE'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(28) VALUE 'RAZÃO SOCIAL'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE 'COD. VENDEDOR'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE 'NOME VENDEDOR'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE '    DISTÂNCIA EM m'.
       01  CAB03.
           03 F PIC X(16) VALUE IS ALL '-'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(28) VALUE IS ALL '-'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE IS ALL '-'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE IS ALL '-'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE IS ALL '-'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE IS ALL '-'.
       01  LIN01.
           03 RL-CODIGO-CLI       PIC 9(07)  VALUE ZEROES.
           03 F                   PIC X(01)  VALUE SPACES.
           03 RL-RAZAO-CLI        PIC X(040) VALUE SPACES.
           03 F                   PIC X(01)  VALUE SPACES.
           03 RL-CODIGO-VEN       PIC 9(03)  VALUE ZEROES.
           03 F                   PIC X(01)  VALUE SPACES.
           03 RL-NOME-VEN         PIC X(40)  VALUE SPACES.
           03 F                   PIC X(01)  VALUE SPACES.
           03 RL-DISTANCIA-VEN    PIC S9(003)v9(008) VALUE ZEROS.
           03 F                   PIC X(01)  VALUE SPACES.
