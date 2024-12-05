         TITLE 'PLISTIME - GET SYSTEM DATE AND TIME FOR PL/I'           00012002
PLISTIME CSECT                                                          00020002
*********************************************************************** 00030001
*                                                                     * 00040001
*        PLISTIME: GET SYSTEM DATE AND TIME.                          * 00050002
*                                                                     * 00051001
*        AUTHOR: PETER FLASS - NYS LEGISLATIVE BILL DRAFTING COMM     * 00052001
*                DECEMBER, 1998.                                      * 00053001
*                                                                     * 00054001
*        FUNCTION: ISSUE 'TIME' MACRO AND RETURN RESULTS.             * 00055001
*                                                                     * 00080001
*        CALLING SEQUENCE:                                            * 00090001
*                DCL DDNAME CHAR(8);      /* DDNAME FOR FILE */       * 00100001
*                                                                     * 00110001
*********************************************************************** 00120001
         SPACE 1                                                        00130001
         REGS  ,                  REGISTER EQUATES                      00140001
         PLIREGS ,                PL/I EQUATES                          00150001
CODE     EQU   R11                CSECT BASE                            00160001
         PRINT NOGEN                                                    00170002
         EJECT                                                          00200001
PLISTIME CSECT ,                   RESUME CSECT                         00202002
PLISTIME AMODE 31                                                       00203002
PLISTIME RMODE ANY                                                      00204002
PLISTIME VERSION 1.0                                                    00220402
*--------------------------------*                                      00220601
*  SET UP AND CHAIN SAVEAREAS    *                                      00220701
*--------------------------------*                                      00220801
SYSTIME  PLIENTRY LOCAL=(LOCAL,LOCALL)                                  00220902
                                                                        00270001
*--------------------------------*                                      00280001
*  GET PARM DATA                 *                                      00290001
*--------------------------------*                                      00300001
         L     R1,0(,R1)          LOAD A(PARM LOCATOR)                  00300102
         L     R4,0(,R1)          LOAD A(STRING)                        00300202
         LH    R3,4(,R1)          LOAD ALLOCATED LENGTH                 00300301
         TM    6(R1),X'80'        Q/ VARYING STRING?                    00300402
         BO    ISVAR              .. YES                                00300501
         CH    R3,=AL2(L'RET)     TEST RESULT LENGTH                    00300601
         BNH   GETTIME            .. EXACT LENGTH OR LESS               00300701
* ASSERTION: LENGTH WILL BE GREATER THAN ONE                            00300801
         LR    R14,R3             LOAD LENGTH                           00300901
         LR    R15,R4             LOAD ADDRESS                          00301002
         MVI   0(R4),C' '         MOVE A BLANK                          00301102
         BCTR  R14,0              SUBTRACT ONE FOR BLANK MOVED          00301201
DOPAD    EQU   *                  NEEDS BLANK PADDING                   00301301
         LR    R1,R14             GET REMAINING LENGTH                  00301401
         CH    R1,=H'256'         COMPARE TO MAX                        00301501
         BNH   *+8                .. OKAY                               00301601
         LH    R1,=H'256'         .. ELSE USE MAX                       00301701
         BCTR  R1,0               DECREMENT FOR EX                      00301801
         EX    R1,MOVEB           ** MVC 1(*-*,15),0(R15)               00301901
         BCTR  R14,0              ADJUST REMAINING LENGTH               00302001
         SR    R14,R1             (DITTO)                               00302101
         LA    R15,1(R14,R15)     ADJUST OUTPUT PTR                     00302201
         LTR   R14,R14            TEST LENGTH                           00302301
         BNZ   DOPAD              .. MORE TO DO                         00302401
         B     GETTIME            .. ALL DONE                           00302501
                                                                        00302601
ISVAR    EQU   *                  'VARYING' STRING                      00302701
         CH    R3,=AL2(L'RET)     TEST RESULT LENGTH                    00302801
         BNH   *+8                .. EXACT LENGTH OR LESS               00302901
         LH    R3,=AL2(L'RET)     ELSE USE MY LENGTH                    00303001
         STH   R3,0(,R4)          STORE LENGTH                          00303102
         LA    R4,2(,R4)          SKIP OVER LENGTH                      00303202
                                                                        00303301
*--------------------------------*                                      00303401
*  GET DATE AND TIME             *                                      00303501
*--------------------------------*                                      00304001
GETTIME  EQU   *                  GET DATE AND TIME                     00305001
         TIME  DEC                GET DATE AND TIME                     00310002
         ST    R1,DATETIME        SAVE DATE PACKED '0CYYDDDS'           00321001
         ST    R0,DATETIME+4      SAVE TIME PACKED 'HHMMSSTT'           00321102
         MVI   DATETIME+8,X'0C'   MAKE TIME SIGNED                      00321202
         SRP   DATETIME+4(5),64-1,0 TIME<-'0HHMMSSTTS'                  00321302
         UNPK  DWD(8),DATETIME(4) UNPACK DATE                           00321402
         OI    DWD+7,C'0'         FIXUP SIGN                            00321502
         MVC   RET(6),DWD+2       MOVE DATE TO RETURN AREA              00327001
         UNPK  DWD(8),DATETIME+4(5) UNPACK TIME                         00327102
         OI    DWD+7,C'0'         FIXUP SIGN                            00327202
         MVC   RET+6(8),DWD       MOVE TIME TO RETURN AREA              00327302
         CLI   RET,C'0'           Q/ IS CENTURY ZERO?                   00327402
         BNE   *+8                .. NO                                 00327502
         MVI   RET,C' '           .. YES, MAKE IT BLANK                 00327602
         LTR   R3,R3              TEST RETURN LENGTH                    00340001
         BZ    RETURN             .. RETURN W/NO DATA                   00350001
         BCTR  R3,0               DECREMENT LENGTH FOR EX               00360001
         EX    R3,MOVE            ** MVC 0(*-*,R4),RET                  00370002
                                                                        00384301
*--------------------------------*                                      00386501
*  RETURN TO CALLER              *                                      00386601
*--------------------------------*                                      00386701
RETURN   EQU   *                  RETURN TO CALLER                      00386801
         PLIRET                   RETURN                                00386902
                                                                        00400001
*        EXECUTED INSTRUCTIONS                                          00410001
MOVE     MVC   0(*-*,R4),RET      MOVE DATE AND TIME                    00411002
MOVEB    MVC   1(*-*,15),0(R15)   BLANK-PAD RESULT FIELD                00412001
                                                                        00420001
*--------------------------------*                                      00430001
*  DYNAMIC STORAGE               *                                      00440001
*--------------------------------*                                      00450001
LOCAL    DSECT                    LOCAL VARIABLE STORAGE                00460001
DWD      DS    CL8                WORKAREA                              00471101
DATETIME DS    CL9                SYSTEM DATE/TIME                      00472001
RET      DS    CL14               RETURNED DATE/TIME                    00472101
LOCALL   EQU   *-LOCAL            LENGTH OF LOCAL STORAGE               00472501
                                                                        00472601
         END                                                            00477001
