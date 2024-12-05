         TITLE 'PLIINCR - INCREMENT A COUNTER USING CS'
PLIINCR CSECT
***********************************************************************
*                                                                     *
*        PLIINCR: INCREMENT A COUNTER USING THE 'CS' INSTRUCTION.     *
*                                                                     *
*        AUTHOR: PETER FLASS - NYS LEGISLATIVE BILL DRAFTING COMM     *
*                JUN, 1997                                            *
*                                                                     *
*        FUNCTION:                                                    *
*                INCREMENT A COUNTER USING THE MACHINE 'CS'           *
*                INSTRUCTION TO BE SAFE FOR MULTITASKING.             *
*                                                                     *
*        CALLING SEQUENCE:                                            *
*                DCL  P PTR;                                          *
*                DCL  X FIXED BIN(15);                                *
*                CALL INCR( P, X);                                    *
*                INCREMENTS THE FULLWORD COUNTER POINTED TO BY 'P'    *
*                BY THE AMOUNT 'X'.                                   *
*                                                                     *
***********************************************************************
         EJECT
         REGS  ,                  REGISTER EQUATES
         PLIREGS ,                PL/I EQUATES
CODE     EQU   PLIBASE            CSECT BASE (R2)
         EJECT
PLIINCR RMODE ANY
PLIINCR AMODE 31
PLIINCR CSECT ,                   RESUME CSECT
INCR     PLIENTRY
         LM   R1,R2,0(R1)         LOAD PARAMETER ADDRESSES
         L    R1,0(,R1)           LOAD POINTER
         L    R2,0(,R2)           LOAD INCREMENT AMOUNT

CS       EQU  *
         L    R14,0(,R1)          LOAD THE COUNTER VALUE
         LR   R15,R14             BUMP THE COUNTER
         AR   R15,R2              .
         CS   R14,R15,0(R1)       (MAYBE) UPDATE THE COUNTER
         BNZ  CS                  LOOP IF UPDATE UNSUCCESSFUL

         PLIRET                   RETURN TO CALLER

         LTORG

         END
 MODE AMODE(31),RMODE(ANY)
 ALIAS INCR
 NAME PLIINCR(R)
