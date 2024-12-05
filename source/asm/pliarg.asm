         MACRO
&NAME    PLIARG &N,&TYPE,&ADDR=,&LEN=
.*
.*       EXTRACT ADDRESS AND LENGTH OF PL/I ARGUMENT
.*             &N   = ARGUMENT NUMBER 1..NUMBER OF ARGS
.*             &TYPE= ARGUMENT TYPE:
.*                       STRING - STRING ARGUMENT
.*                       ...
.*             &ADDR= REGISTER TO CONTAIN ADDRESS OF STRING (2-12)
.*             &LEN = REGISTER TO CONTAIN CURRENT LENGTH OF STRING
.*                                                          (2-12)
.*             WHEN CALLED, R1 MUST POINT TO ARGUMENT LIST
.*
.*       MODIFICATIONS:
.*             3-29-96 FIX PROBLEM WITH VARYING STRING ARGS PRF
.*
         LCLA  &NN
&NN      SETA  (&N-1)*4
&NAME    L     14,&NN.(,1)         LOAD PARAMETER ADDRESS
         AIF   ('&TYPE' EQ 'STRING').STRING
         AGO   .UNDEF
.STRING  ANOP
         AIF   (&N LT 1).ERR1
         TM    6(14),X'80'         Q/ VARYING STRING?       <-+
         LH    &LEN,4(,14)         LOAD L'STRING              |
         L     &ADDR,0(,14)        LOAD A(STRING)             |
         BNO   *+12                .. VARYING, GET MAX LEN  <-+
         LH    &LEN,0(,&ADDR)      .. NON-VARYING, GET ACTUAL
         LA    &ADDR,2(,&ADDR)            SKIP OVER LENGTH
         MEXIT
.UNDEF   MNOTE 8,'UNDEFINED ARGUMENT TYPE &TYPE'
         MEXIT
.ERR1    MNOTE 8,'ARGUMENT NUMBER MUST BE GREATER THAN ONE'
         MEND
