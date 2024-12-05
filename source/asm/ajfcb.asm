         TITLE 'RDJFCB SUBROUTINE FOR PL/I PROGRAMS'
PLIJFCB  CSECT
***********************************************************************
*                                                                     *
*              RDJFCB SUBROUTINE FOR PL/I PROGRAMS                    *
*              PETER FLASS -- EMPIRE STATE COLLEGE (SUNY) -- MAR 84   *
*              CALLING SEQUENCE:                                      *
*                   DCL AJFCB ENTRY(*) RETURNS(PTR);                  *
*                   DCL P PTR;                                        *
*                   DCL F FILE;                                       *
*                   P=AJFCB(F);                                       *
*              A STRUCTURE 'IEFJFCBN' IS AVAILABLE TO MAP THE JFCB.   *
*              THE FILE MAY BE OPEN OR CLOSED AT CALL TIME.           *
*                                                                     *
*  MODIFICATIONS:                                                     *
*              06-16-1998 DITTO                               PF980616*
*              10-02-1997 FIX FOR HEAP ABOVE 16M              PF100297*
*                                                                     *
***********************************************************************
         SPACE 3
         REGS                      EQUATE REGISTERS
         PLIREGS
JFCBSIZE EQU   176                 STORAGE REQUIREMENT FOR JFCB
         EJECT
AJFCB    PLIENTRY LOCAL=(DSA,DSALEN)
PLIJFCB  AMODE 31
PLIJFCB  RMODE ANY
         LR    R10,R1              SAVE PARAMETER LIST ADDRESS
         L     R3,0(,R10)          LOAD A(FILE_VARIABLE)
         L     R3,0(,R3)           LOAD A(DCLCB)
         L     R11,0(,R3)          LOAD A(FCB) IF INTERNAL
         OC    0(2,R3),0(R3)       Q/ IS FILE INTERNAL?
         BNZ   TESTOPEN            .. YES
         A     R11,4(,PLITCA)      .. NO, ADD A(PRV)
TESTOPEN EQU   *
         L     R11,0(,R11)         LOAD A(FCB)
         C     R11,X'64'(,PLITCA)  Q/ IS FILE OPEN?
         BE    NOTOPEN             .. NO
         L     R11,X'14'(,R11)     .. YES, LOAD A(ACB/DCB)
         B     RDJFCB              GO ISSUE RDJFCB
NOTOPEN  EQU   *                   FILE NOT OPEN, CREATE DUMMY DCB
         MVC   DUMMYDCB(DUMMYL),DUMMY   MOVE DUMMY DCB+OPENLIST
         MVC   DUMMYDCB+DCBDDNAM-IHADCB(8),=CL8' '
         LH    R14,X'14'(,R3)            LOAD LENGTH OF FILE NAME
         CH    R14,=H'8'           Q/ EIGHT CHAR OR LESS?
         BNH   *+8                 .. YES
         LA    R14,8               .. NO, USE EIGHT
         BCTR  R14,0               DECREMENT FOR EXECUTE
         EX    R14,MVDDN           MOVE DDNAME TO DCB
*        MVC   DUMMYDCB+DCBDDNAM-IHADCB(*-*),X'16'(R3)
         LA    R11,DUMMYDCB        LOAD ADDRESS OF DCB
         SPACE 3
RDJFCB   DS    0H                  SET UP FOR RDJFCB
         ST    R11,OPENLIST        STORE DCB ADDRESS
         MVI   OPENLIST,X'80'      CONSTRUCT OPEN LIST FOR RDJFCB
         LA    R1,MYJFCB           POINT TO JFCB AREA         PF100297
         ST    R1,JFCBA            SAVE ADDRESS OF ALLOCATED STG
         MVI   JFCBA,X'87'         JFCB+LAST FLAGS
         L     R3,DCBEXLST-IHADCB(,R11) SAVE EXIT LIST ADDR IF ANY
         LA    R14,JFCBA           LOAD MY EXIT LIST ADDR
         STCM  R14,B'0111',DCBEXLSA-IHADCB(R11)   STORE INTO DCB
         MVI   JFCBA,X'87'         INDICATE JFCB+LAST
         LA    R1,OPENLIST         LOAD A(OPENLIST)
         RDJFCB ,MF=(E,(1))        EXECUTE RDJFCB
         ST    R3,DCBEXLST-IHADCB(,R11) RESTORE EXIT LIST ADDRESS
         LTR   R15,R15             TEST RETURN CODE FROM RDJFCB
         BZ    MOVEJFCB            .. SUCCESS                 PF100297
         MVC   JFCBA,=X'FF000000'  CONVENTIONALLY 'NULL'
         B     RETURN                                         PF100297
*---------------------------------*                           PF100297
* GET NON-LIFO JFCB STORAGE       *                           PF100297
*---------------------------------*                           PF100297
MOVEJFCB EQU   *                   MOVE THE JFCB              PF100297
         L     R15,X'6C'(,PLITCA)  LOAD A(IBMBPGRA)           PF100297
         LA    R0,JFCBSIZE         LOAD STORAGE SIZE          PF100297
         BALR  R14,R15             GO ALLOCATE STORAGE        PF100297
         LA    R14,MYJFCB                                     PF100297
         MVC   0(JFCBSIZE,R1),0(R14) MOVE JFCB TO NON-LIFO STORAGE0297
         ST    R1,JFCBA            SAVE ADDRESS OF AREA       PF100297
*        MVI   JFCBA,X'00'         CLEAR HOB OF JFCB ADDRESS  PF980616
RETURN   EQU   *                   RETURN TO CALLER
         L     R10,4(,R10)         LOAD ADDRESS OF RETURNED VALUE
         MVC   0(4,R10),JFCBA      RETURN JFCB ADDRESS
         PLIRET                    RETURN TO CALLER
         SPACE 3
         PRINT NOGEN
*        EXECUTED INSTRUCTION
MVDDN    MVC   DUMMYDCB+DCBDDNAM-IHADCB(*-*),X'16'(R3)
         SPACE 3
DUMMY    DCB   DDNAME=NULLFILE,MACRF=(E),DSORG=PS,DEVD=DA
DUMMYL   EQU   *-DUMMY
         LTORG *                   LITERALS HERE
         EJECT
DSA      DSECT ,                   PROGRAM WORKAREA
OPENLIST DS    A                   OPEN LIST FOR RDJFCB
JFCBA    DS    A                   ADDRESS OF JFCB
DUMMYDCB DS    XL(DUMMYL)          DUMMY DCB
MYJFCB   DS    XL(JFCBSIZE)        JFCB READIN AREA
DSALEN   EQU   *-DSA               LENGTH OF DSA
         SPACE 1
IHADCB   DSECT ,
         DCBD  DSORG=XE,DEVD=DA
         SPACE 3
         END   PLIJFCB
 MODE AMODE(31),RMODE(24)
 ALIAS AJFCB
 NAME PLIJFCB(R)
