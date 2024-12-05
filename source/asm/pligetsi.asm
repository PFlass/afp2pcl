         TITLE 'PLIGETSI - PL/I INTERFACE FOR SYSTEM INFORMATION'       00170005
PLIGETSI CSECT                                                          00180000
*********************************************************************** 00190000
*                                                                     * 00200000
*        PLIGETSI: PROVIDE PL/I INTERFACE TO OBTAIN MISCELLANEOUS     * 00210000
*                  OPERATING SYSTEM INFORMATION.                      * 00220000
*                                                                     * 00230000
*        AUTHOR: PETER FLASS - NYS LEGISLATIVE BILL DRAFTING COMM     * 00240000
*                MAY, 1990                                            * 00250000
*                                                                     * 00260000
*        FUNCTION: PROGRAM IS CALLED WITH A CHARACTER STRING          * 00270000
*                  CONTAINING A LIST OF THE DATA ITEMS TO BE          * 00280000
*                  OBTAINED AND THE ADDRESS OF A BUFFER FOR THE       * 00290000
*                  RETURNED INFORMATION.  THE LIST IS SCANNED AND     * 00300000
*                  THE REQUESTED ITEMS ARE MOVED ONE AT A TIME TO     * 00310000
*                  THE CALLERS BUFFER.  UNRECOGNIZED LIST ITEMS ARE   * 00320000
*                  IGNORED.  THE FUNCTION TERMINATES WHEN THE END     * 00330000
*                  OF THE LIST IS ENCOUNTERED OR THE CALLERS BUFFER   * 00340000
*                  IS FULL.                                           * 00350000
*                                                                     * 00360000
*        CALLING SEQUENCE:                                            * 00370000
*                DCL GETSYSI ENTRY( CHAR(*), CHAR(*) )                * 00380016
*                            RETURNS ( FIXED BIN(31) );               * 00390000
*                RC = GETSYSI( LIST_CHAR_STR, BUF );                  * 00400000
*                  RC=0 - ALL ITEMS SUCCESSFULLY EXTRACTED            * 00410000
*                  RC=4 - AN INVALID LIST ITEM WAS ENCOUNTERED OR THE * 00420000
*                    BUFFER SIZE PROVIDED WAS TOO SMALL               * 00430000
*                  LIST_CHAR_STR IS THE LIST OF NAMES OF THE ITEMS    * 00440000
*                    TO BE EXTRACTED, SEPARATED BY COMMAS OR SPACES.  * 00450000
*                  BUF IS THE USERS BUFFER.  ITEMS WILL BE RETURNED   * 00460000
*                    IN THE BUFFER IN THE ORDER REQUESTED.            * 00470000
*                    THE LENGTH OF EACH ITEM IS DEFINED BY THE TABLE  * 00480000
*                    'ITEMTAB' BELOW.                                 * 00490000
*                                                                     * 00491014
*        DATA ITEMS TO BE RETRIEVED:                                  * 00492014
*                  'JOBNAME'                                          * 00500014
*                  'JOBID'                                    PF050897* 00501014
*                                                                     * 00502014
*********************************************************************** 00510000
         EJECT                                                          00520008
         REGS  ,                  REGISTER EQUATES                      00530000
         PLIREGS ,                PL/I EQUATES                          00540000
CODE     EQU   PLIBASE            CSECT BASE (R2)                       00550002
RCADDR   EQU   R4                 A(RETURN CODE)                        00560002
TBPTR    EQU   R5                 COMMAND TABLE PTR                     00570002
BUFA     EQU   R6                 A(BUF)                                00580002
BUFL     EQU   R7                 L'BUF                                 00590002
LISTA    EQU   R8                 A(LIST)                               00600002
LISTL    EQU   R9                 L'LIST                                00610002
         SPACE 3                                                        00620000
LOCAL    DSECT                    LOCAL VARIABLE STORAGE                00630000
ASTRLEN  DS    A                  A(VAR_STR_LEN)                        00640001
FLAGS    DS    BL1                FLAGS BYTE                            00650001
FLVAR    EQU   X'80'              BUFFER IS VARYING STRING              00660001
LOCALL   EQU   *-LOCAL            LENGTH OF LOCAL STORAGE               00670000
         EJECT                                                          00680000
PLIGETSI RMODE ANY                                                      00690013
PLIGETSI AMODE 31                                                       00700013
PLIGETSI CSECT ,                  RESUME CSECT                          00710000
GETSYSI  PLIENTRY ,LOCAL=(LOCAL,LOCALL)                                 00720000
         MVI   FLAGS,X'00'        INITIALIZE FLAGS BYTE                 00730004
         L     RCADDR,08(,PLIPL)  LOAD A(FUNCTION RETURN)               00740002
         XC    0(4,RCADDR),0(RCADDR) SET RC=0                           00750002
         SPACE 1                                                        00760001
*        PROCESS SECOND ARGUMENT (BUFFER)                               00770001
         L     R14,4(,R1)         PICK UP 2ND ARG DESC ADDR             00780001
         L     BUFA,0(,R14)       LOAD A(BUFFER)                        00790002
         LH    BUFL,4(,R14)       LOAD ALLOCATED LENGTH                 00800002
         TM    6(R14),X'80'       Q/ BUFFER IS VARYING STRING?          00810001
         BO    VARSTR             .. YES, INITIALIZE LENGTH             00820000
         LR    R14,BUFA           COPY DESTINATION ADDRESS              00830002
         LTR   R15,BUFL            AND LENGTH                           00840002
         BZ    GETARG1            DONT INITIALIZE IF ZERO LEN           00850002
         SR    R8,R8              ZERO SOURCE ADDRESS                   00860004
         L     R9,=X'40000000'    SOURCE LENGTH AND PAD CHAR            00870004
         MVCL  R14,R8             CLEAR FIXED-LENGTH BUFFER             00880004
         B     GETARG1            GO GET FIRST ARGUMENT                 00890001
VARSTR   EQU   *                  INITIALIZE VARYING STRING             00900001
         OI    FLAGS,FLVAR        INDICATE VARYING STRING               00910001
         XC    0(2,BUFA),0(BUFA)  ZERO CURRENT LENGTH                   00920002
         ST    BUFA,ASTRLEN       SAVE ADDR OF LENGTH FIELD             00930004
         LA    BUFA,2(,BUFA)      SKIP OVER LENGTH                      00940002
         SPACE 1                                                        00950001
*        PROCESS FIRST ARGUMENT (LIST)                                  00960001
GETARG1  EQU   *                                                        00970002
         PLIARG 1,STRING,ADDR=LISTA,LEN=LISTL LOAD ADDR OF LIST         00980002
         B     LISTSCAN           SKIP OVER END OF LOOP                 00990011
         EJECT                                                          01000002
*********************************************************************** 01010002
*             PROCESS ITEM LIST                                       * 01020002
*********************************************************************** 01030002
LISTNEXT EQU   *                                                        01040011
         LA    LISTA,1(,LISTA)    ADVANCE LIST PTR                      01050011
         SPACE 2                                                        01060011
LISTSCAN DS    0H                 SCAN ITEM LIST                        01070002
         BCTR  LISTL,0            DECREMENT LIST LENGTH                 01080011
         LTR   LISTL,LISTL        TEST ARGUMENT LENGTH                  01090002
         BM    EXIT               .. DONE                               01100010
         CLI   0(LISTA),C' '      TEST FOR BLANK                        01110002
         BE    LISTNEXT                                                 01120002
         CLI   0(LISTA),C','      OR COMMA                              01130002
         BE    LISTNEXT                                                 01140011
         SPACE 1                                                        01150011
         LR    R1,LISTA           SAVE ITEM START ADDRESS               01160002
LISTSCNI EQU   *                  SCAN ONE ITEM NAME                    01170002
         LTR   LISTL,LISTL        TEST ARGUMENT LENGTH                  01180002
         BNP   LISTENDI           .. DONE                               01190002
         CLI   1(LISTA),C' '      TEST FOR BLANK                        01200002
         BE    LISTENDI                                                 01210002
         CLI   1(LISTA),C','      OR COMMA                              01220002
         BE    LISTENDI                                                 01230002
         LA    LISTA,1(,LISTA)    ADVANCE LIST PTR                      01240002
         BCTR  LISTL,0            DECREMENT LENGTH                      01250002
         B     LISTSCNI           AND CONTINUE SCAN                     01260002
LISTENDI EQU   *                  ITEM NAME IDENTIFIED                  01270002
         LR    R15,LISTA          GET ITEM END ADDR                     01280008
         SR    R15,R1             SUBTRACT START ADDRESS                01290002
         SPACE 2                                                        01300002
         LA    TBPTR,ITEMTBL      POINT TO ITEM TABLE                   01310002
         USING ITMTBDS,TBPTR                                            01320004
ITEMLOOP EQU   *                  IDENTIFY ITEM                         01330000
         CLI   0(TBPTR),X'FF'     Q/ END OF TABLE                       01340002
         BE    ITEMERR            .. YES, IGNORE THIS ITEM              01350002
         CH    R15,ITMNMLN        Q/ SAME LENGTH?                       01360004
         BNE   ITEMNEXT           .. NO, CAN'T BE THIS ONE              01370000
         EX    R15,ITEMCLC        *** CLC 0(*-*,R1),ITMNM               01380004
         BE    ITEMFND            .. HIT                                01390000
ITEMNEXT EQU   *                  GO LOOK AT NEXT ITEM                  01400000
         AH    TBPTR,ITMNMLN      ADD LENGTH OF COMMAND TEXT-1          01410004
         LA    TBPTR,ITMTBLN+1(,TBPTR) ADD LENGTH OF FIXED DATA + 1     01420004
         B     ITEMLOOP           TRY, TRY AGAIN                        01430000
ITEMERR  EQU   *                  INVALID ITEM                          01440002
         MVC   0(4,RCADDR),=F'4'  SET RC=4                              01450002
         B     LISTNEXT           GO ATTEMPT NEXT LIST ITEM             01460009
ITEMFND  EQU   *                  ITEM MATCHED IN TABLE                 01470000
         L     R15,=A(PROCRTNS)   LOAD ADDRESS OF PROCESSING ROUTINES   01480008
         AH    R15,ITMRTN         ADD ROUTINE OFFSET                    01490008
         BALR  R14,R15            BRANCH TO PROCESSING ROUTINE          01500001
         SPACE 2                                                        01510002
ITEMPROC DS    0H                 RE-ENTER HERE FROM PROCESSING ROUTINE 01520002
*        DATA ITEM ADDRESS AT 0(R15)                                    01530002
*        ITEM LENGTH       AT ITMVALN                                   01540004
*        OUTPUT ADDRESS    AT 0(BUFA)                                   01550002
         LH    R1,ITMVALN         LOAD ITEM LENGTH MINUS ONE            01560004
         CR    R1,BUFL            COMPARE TO REMAINING OUTPUT LENGTH    01570002
         BNL   BUFFULL            .. BUFFER IS FULL                     01580002
         EX    R1,ITEMMVC         *** MVC 0(*-*,BUFA),0(R15)            01590002
         LA    BUFA,1(R1,BUFA)    ADVANCE OUTPUT PTR                    01600002
         SR    BUFL,R1            DECREMENT LENGTH REMAINING            01610002
         BCTR  BUFL,0                                                   01620002
         B     LISTNEXT           GO PROCESS NEXT LIST ITEM             01630011
BUFFULL  EQU   *                  OUTPUT BUFFER IS FULL                 01640002
         MVC   0(4,RCADDR),=F'4'  SET RC=4                              01650002
EXIT     EQU   *                                                        01660000
         TM    FLAGS,FLVAR        Q/ BUF = VARYING STRING?              01670003
         BNO   RETURN             .. NO, EXIT                           01680003
         L     R1,ASTRLEN         .. YES, LOAD A(CURRENT_LEN)           01690003
         LA    R0,2(,R1)          POINT TO START OF TEXT                01700003
         SR    BUFA,R0            COMPUTE USED LENGTH                   01710003
         STH   BUFA,0(,R1)          AND STORE BACK INTO STRING          01720003
RETURN   EQU   *                  RETURN TO CALLER                      01730003
         PLIRET                                                         01740000
         SPACE 1                                                        01750000
*        EXECUTED INSTRUCTIONS                                          01760000
ITEMCLC  CLC   0(*-*,R1),ITMNM    LOOKUP ITEM IN TABLE                  01770004
ITEMMVC  MVC   0(*-*,BUFA),0(R15) MOVE ITEM TO OUTPUT BUFFER            01780002
         SPACE 1                                                        01790006
         DROP  TBPTR                                                    01800006
         TITLE 'PLIGETSI - TABLES AND CONSTANTS'                        01810002
ITEMTBL  DS    0H                  ITEM TABLE                           01820000
         DC    AL2(L'JOBNAME-1)                  L'NAME                 01830000
         DC    AL2(8-1),AL2(GETJBNM-PROCRTNS)    L'VALUE, RTN OFST      01840009
JOBNAME  DC    C'JOBNAME'                        TEXT OF ITEM NAME      01850014
         DC    AL2(L'JOBID-1)                    .                      01860012
         DC    AL2(8-1),AL2(GETJBID-PROCRTNS)    .                      01870012
JOBID    DC    C'JOBID'                          .                      01880012
         DC    2X'FF'              END OF ITEM TABLE                    01890000
         SPACE 3                                                        01900002
ITMTBDS  DSECT ,                   ITEM TABLE ENTRY                     01910004
ITMNMLN  DS    H                   L'NAME MINUS ONE                     01920004
ITMVALN  DS    H                   L'VALUE MINUS ONE                    01930004
ITMRTN   DS    AL2                 OFFSET OF PROCESSING ROUTINE         01940004
ITMNM    EQU   *                   OFFSET OF ITEM NAME                  01950004
ITMTBLN  EQU   *-ITMTBDS           LENGTH OF FIXED PART OF ENTRY        01960004
         SPACE 3                                                        01970004
PLIGETSI CSECT ,                   BACK TO CSECT                        01980004
         LTORG                                                          01990002
         SPACE 1                                                        02000008
         DROP  CODE                                                     02010008
         TITLE 'PLIGETSI - ITEM PROCESSING ROUTINES'                    02020002
PROCRTNS DS    0F                                                       02030008
*                                                                       02040002
*        ALL ROUTINES ARE CALLED WITH RETURN ADDRESS IN R14             02050008
*            ADDRESS OF ENTRY POINT IN R15                              02060008
*        ADDRESS OF ITEM IS RETURNED IN R15                             02070008
*                                                                       02080002
GETJBNM  DS    0H                 GET JOBNAME ITEM                      02090002
         USING *,R15              (IF NECESSARY)                        02100008
         L     R1,X'10'(,R0)       LOAD A(CVT)                          02110015
         L     R1,0(,R1)                A(TCB_WORDS)                    02120015
         L     R1,0(,R1)                A(CURR_TCB)                     02130015
         L     R15,X'0C'(,R1)           A(TIOT)                         02140017
         BR    R14                FIRST 8 BYTES OF TIOT ARE JOBNAME     02150015
                                                                        02160012
GETJBID  DS    0H                 GET JOB ID ITEM                       02170012
         USING *,R15              (IF NECESSARY)                        02180012
         L     R1,X'10'(,R0)       LOAD A(CVT)                          02190012
         L     R1,0(,R1)                A(TCB_WORDS)                    02200012
         L     R1,0(,R1)                A(CURR_TCB)                     02210012
         L     R1,X'A4'(,R1)            A(TCT)                          02220012
         L     R1,X'8C'(,R1)            A(SMF30 PREFIX)                 02230012
         LA    R1,8(,R1)           POINT TO SMF TYPE 30 RECORD          02240012
         A     R1,X'20'(,R1)            A(IDENT SECT)                   02250012
         LA    R15,X'20'(R1)      A(JOB IDENTIFIER)                     02260012
         BR    R14                AND USE THAT                          02270012
                                                                        02280012
         END                                                            02290000
