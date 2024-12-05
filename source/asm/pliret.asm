         MACRO
&NAME    PLIRET
.*
.*       PL/I RETURN LINKAGE FROM ASSEMBLER SUBROUTINE
.*
&NAME    LR    0,13                SAVE MY DSA ADDRESS
         L     13,4(,13)           RESTORE CALLER'S R13
         L     14,12(,13)          RESTORE RETURN ADDRESS
         LM    2,12,28(13)         RESTORE CALLERS REGISTERS
         BALR  1,14                RETURN
         MEND
