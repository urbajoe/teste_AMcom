      * LAYOUT DO RELATORIOO DE CLIENTES 
       01  CAB01.
           02 FILLER            PIC X(43) VALUE SPACES.
           02 FILLER            PIC X(54) VALUE '     REL CLIENTES      '.
           02 FILLER            PIC X(05) VALUE 'Pag: '.
           02 PRT-PAGINA        PIC 9(03) VALUE ZEROS.


       01  CAB02.
           03 F PIC X(16) VALUE 'COD.'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(28) VALUE 'RAZÃO SOCIAL'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE 'CNPJ'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE '    LATITUDE'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE '    LONGITUDE'.
           03 F PIC X(01) VALUE SPACES.
           03 F PIC X(14) VALUE SPACES.
       01  CAB08.
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
           03 RL-CODIGO    PIC  9(007) VALUE ZEROES.
           03 F PIC X(01)  VALUE SPACES.
           03 RL-RAZAO     PIC  X(040) VALUE SPACES.
           03 F PIC X(01)  VALUE SPACES.
           03 RL-CNPJ      PIC  9(014) VALUE ZEROS.
           03 F PIC X(01)  VALUE SPACES.
           03 RL-LATITUDE  PIC S9(003)v9(008) VALUE ZEROS.
           03 F PIC X(01)  VALUE SPACES.
           03 RL-LONGITUDE PIC S9(003)v9(008) VALUE ZEROS.
           03 F PIC X(01)  VALUE SPACES.
