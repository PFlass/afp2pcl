PLIVLOC  CSECT                                                          00000010
         TITLE 'VLOCATE -- LOCATE SPECIFIED-LENGTH RECORD'              00000020
*/********************************************************************/ 00000030
*/*                                                                  */ 00000040
*/*      VLOCATE -- LOCATE SPECIFIED-LENGTH RECORD                   */ 00000050
*/*      P.FLASS - EMPIRE STATE COLLEGE (SUNY) -- AUGUST 1985        */ 00000060
*/*                                                                  */ 00000070
*/*      DESCRIPTION:                                                */ 00000080
*/*            MODULE IS A 'FIX' AROUND A PL/I RESTRICTION THAT      */ 00000090
*/*            THE LENGTH OF A BASED VARIABLE USED IN A LOCATE       */ 00000100
*/*            STATEMENT MUST BE A DECIMAL INTEGER CONSTANT.         */ 00000110
*/*                                                                  */ 00000120
*/*      CALLING SEQUENCE:                                           */ 00000130
*/*            DCL F FILE;                                           */ 00000140
*/*            DCL P PTR;                                            */ 00000150
*/*            DCL L FIXED BIN(31); /* RECORD LENGTH */              */ 00000160
*/*            DCL Z CHAR(?) BASED(P); /* RECORD VARIABLE */         */ 00000170
*/*            DCL K CHAR(?); /* KEY */                        PF100285 00000180
*/*            P=VLOCATE(F,L[,K]);                                   */ 00000190
*/*                                                                  */ 00000200
*/*      PROCEDURE:                                                  */ 00000210
*/*            REF: OS PL/I OPTIMIZING COMPILER: EXECUTION LOGIC     */ 00000220
*/*                 (SC33-0025) CHAPTER 8.                           */ 00000230
*/*            PROGRAM BUILDS A RECORD DESCRIPTOR (RD) USING THE     */ 00000240
*/*            REQUESTED LENGTH. A STANDARD CALL IS MADE TO IBMBRIO. */ 00000250
*/*                                                                  */ 00000260
*/********************************************************************/ 00000270
         SPACE 3                                                        00000280
         REGS                                                           00000290
         PLIREGS                                                        00000300
         EJECT                                                          00000310
PLIVLOC  CSECT                                                          00000320
PLIVLOC  AMODE 31                                                       00000320
PLIVLOC  RMODE ANY                                                      00000320
VLOCATE  PLIENTRY LOCAL=(WKSPC,WKSPL)                                   00000330
         MVC   MYPARAM(DMYPRL),DMYPARAM INITIALIZE PARAMETER LIPF100285 00000340
         LA    R14,MYRCB           LOAD RCB ADDRESS            PF100285 00000350
         ST    R14,RCBA-DMYPARAM+MYPARAM STORE INTO PARAM LIST PF100285 00000360
         L     R14,0(,R1)          LOAD A(DCLCB ADDRESS)                00000370
         L     R14,0(,R14)         LOAD A(DCLCB)                        00000380
         ST    R14,DCLCBA-DMYPARAM+MYPARAM   STORE INTO PARAM LIST      00000390
         L     R14,4(,R1)          LOAD A(LENGTH)                       00000400
         L     R14,0(,R14)         LOAD LENGTH                          00000410
         ST    R14,MYRD+4          STORE INTO RD                        00000420
         LA    R1,8(,R1)           POINT TO NEXT PARAMETER     PF100285 00000430
         TM    0(R1),X'80'         Q/ IS IT LAST?              PF100285 00000440
         BO    NOKEY               .. YES, KEY NOT SUPPLIED    PF100285 00000450
         PLIARG 1,STRING,ADDR=R14,LEN=R15 GET KEY INFO         PF100295 00000460
         ST    R14,KEYAD-KD+MYKD STORE INTO KD                 PF100285 00000470
         ST    R15,KEYLEN-KD+MYKD STORE LENGTH                 PF100285 00000480
         MVI   REQ3-RCB+MYRCB,X'20'  SET RCB TO INDICATE 'KEYFRPF100285 00000490
         MVI   RCBTM+1-RCB+MYRCB,X'01' CHANGE STATEMENT TYPE   PF100485
         LA    R14,MYKD            LOAD A(KD)                  PF100285 00000500
         ST    R14,KDA-DMYPARAM+MYPARAM STORE INTO LIST        PF100285 00000510
         LA    R1,4(,R1)           SKIP THIS PARAMETER         PF100285 00000520
NOKEY    EQU   *                   KEY BYPASS                  PF100285 00000530
         L     R14,0(,R1)          LOAD A(RETURNED_PTR)                 00000540
         LA    R14,0(,R14)         CLEAR HOB                            00000550
         ST    R14,MYRD+0          STORE INTO RD                        00000560
         LA    R14,MYRD            LOAD A(RD)                           00000570
         ST    R14,RDA-DMYPARAM+MYPARAM      STORE INTO PARAM LIST      00000580
         LA    R1,MYPARAM          LOAD PARAM LIST ADDR                 00000590
         L     R15,=V(IBMBRIOA)    LOAD A(TRANSMITTER MODULE)           00000600
         BALR  R14,R15             EXECUTE 'LOCATE'                     00000610
         PLIRET ,                  RETURN TO CALLER                     00000620
         SPACE 1                                                        00000630
         LTORG *                                                        00000640
         EJECT                                                          00000650
***************************************************************PF100285 00000660
*                                                              PF100285 00000670
*        CONTENTS OF 'DMYPARAM' AND 'WKSPC' MUST REMAIN IN SYNCPF100285 00000680
*        DO NOT REARRANGE OR INSERT FIELDS IN ONE ONLY         PF100285 00000690
*                                                              PF100285 00000700
***************************************************************PF100285 00000710
DMYPARAM DS    0F                  SKELETON PARAMETER LIST FOR IBMBRIO  00000720
DCLCBA   DC    A(*-*)              .. DUMMY A(DCLCB)                    00000730
RCBA     DC    A(*-*)              .. A(RCB) FOR LOCATE        PF100285 00000740
RDA      DC    A(*-*)              .. DUMMY A(RD)                       00000750
KDA      DC    A(*-*)              .. A(KD)                    PF100285 00000760
         DC    A(0)                .. UNUSED (NULL ARGUMENT)            00000770
         DC    X'80',AL3(0)        .. UNUSED (NULL ARGUMENT)            00000780
PARAML   EQU   *-DMYPARAM          LENGTH OF PARAM LIST        PF100285 00000790
         SPACE 1                                                        00000800
RCB      DS    0F                  REQUEST CONTROL BLOCK FOR LOCATE     00000810
         DC    X'0C'               REQ1 - 'LOCATE'                      00000820
         DC    X'40'               REQ2 - 'SET'                         00000830
REQ3     DC    X'00'               REQ3 - 'KEY' OPTION IF APPLIPF100285 00000840
         DC    X'00'               REQ4 - UNUSED                        00000850
RCBTM    TM    2(2),X'02'          TM INSTR - CHECK FOR LOCATE VALID    00000860
RCBL     EQU   *-RCB               LENGTH OF RCB               PF100285 00000870
         SPACE 1                                               PF100285 00000880
KD       DS    0F                  KEY DESCRIPTOR FOR LOCATE   PF100285 00000890
KEYAD    DC    A(*-*)              KEY ADDRESS                 PF100285 00000900
KEYLEN   DC    F'0'                KEY LENGTH                  PF100285 00000910
         DC    F'0'                (UNUSED)                    PF100285 00000920
KDL      EQU   *-KD                PF100285                             00000930
DMYPRL   EQU   *-DMYPARAM          L'PARAMETER LIST            PF100285 00000940
         SPACE 3                                                        00000950
WKSPC    DSECT ,                   LOCAL WORKSPACE                      00000960
MYPARAM  DS    XL(PARAML)          PARAMETER LIST FOR IBMBRIO  PF100285 00000970
MYRCB    DS    XL(RCBL)            RCD                         PF100285 00000980
MYKD     DS    XL(KDL)             KEY DESCRIPTOR              PF100285 00000990
MYRD     DS    2F                  RECORD DESCRIPTOR           PF100285 00001000
WKSPL    EQU   *-WKSPC             L'WORKSPACE                          00001010
         SPACE 1                                                        00001020
         END   PLIVLOC                                                  00001030
