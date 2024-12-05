         TITLE 'DYNAMIC ALLOCATION SUBROUTINE'
ALLOCATE CSECT
***********************************************************************
*                                                                     *
*        ALLOCATE: INTERFACE FOR DYNALLOC (SVC99) SERVICES.           *
*                                                                     *
*        AUTHOR: PETER FLASS                                          *
*                NYS LEGISLATIVE BILL DRAFTING COMMISSION             *
*                MAY, 1990                                            *
*                                                                     *
*        FUNCTION: PROGRAM IS CALLED WITH A CHARACTER STRING          *
*                  CONTAINING A TSO-LIKE 'ALLOCATE' COMMAND.          *
*                  AN SVC99 PARAMETER LIST IS BUILT USING THIS        *
*                  INFORMATION AND ALLOCATION IS PERFORMED.           *
*                                                                     *
*        RETURN CODES:                                                *
*                  0 NORMAL SUCCESSFUL COMPLETION                     *
*                  8 INVALID ITEM FOUND IN COMMAND SCAN               *
*                    PROCESSING CONTINUED WITH THIS ITEM SKIPPED,     *
*                    DYNALLOC ISSUED.                                 *
*                 12 INVALID COMMAND LIST FORMAT                      *
*                    PROCESSING TERMINATED, DYNALLOC NOT ISSUED.      *
*                 16 DYNALLOC ERROR 04 - UNAVAILABLE RESOURCE         *
*                 20 DYNALLOC ERROR 08 - INSTALLATION VALIDATION      *
*                 24 DYNALLOC ERROR 12 - INVALID PARAMETER LIST       *
*                                                                     *
*        MODIFICATIONS:                                               *
*                 09/10/90 - FOR DYNALLOC ERROR ADD 12 TO SVC99RC   PRF
*                 04/09/91 - CORRECT ABBREVIATION OF 'CYLINDERS'    PRF
*                 08/01/91 - ADD 'LIKE' PARAMETER                   PRF
*                            CORRECT FOR CHANGES TO 'VERSION' MACRO PRF
*                 06/02/94 - ALLOW DA(*)                            PRF
*                 01/09/95 - ADD 'SPIN' PARAMETER                   PRF
*                 01/12/95 - ALLOW 8-CHARACTER 'DEST' VALUES        PRF
*                                                                     *
***********************************************************************
         PRINT OFF
         MACRO
.*
.*       GENERATE ITEM TABLE ENTRIES
.*
         ITEM  &TXT,&MIN=,&NO=,&KEY=,&FLG=,&TXTL=0,                    X
               &PARM=
         LCLA  &L,&M
         LCLB  &F(8)
         LCLC  &PFX
&L       SETA  K'&TXT
&M       SETA  &MIN               MIN CHARS TO ABBREV
         AIF   (T'&MIN NE 'O').M
&M       SETA  &L                 DEFAULT MIN TO ACTUAL
.M       ANOP
         AIF   (T'&PARM EQ 'O').NOPARM
         AIF   ('&PARM' EQ 'NUM').NUMPARM
         AIF   ('&PARM' EQ 'AL').ALPARM
         AIF   ('&PARM' EQ 'CMP').CMPARM
         AIF   ('&PARM' EQ 'KEY').KYPARM
         MNOTE 4,'INVALID PARM OPTION &PARM'
         AGO   .NOPARM
.CMPARM  ANOP
&F(1)    SETB  1
&F(2)    SETB  1
&F(3)    SETB  1
         AGO   .GEN
.NUMPARM ANOP
&F(1)    SETB  1
         AGO   .GEN
.ALPARM  ANOP
&F(2)    SETB  1
         AGO   .GEN
.KYPARM  ANOP
&F(3)    SETB  1
         AGO   .GEN
.NOPARM  ANOP
.*
.GEN     ANOP
         DC    AL2(&L-1),AL2(&M-1)
         DC    AL1(&NO)
         DC    B'&F(1)&F(2)&F(3)&F(4)&F(5)&F(6)&F(7)&F(8)'
         AIF   (T'&KEY EQ 'O').NOKY
&PFX     SETC  'DAL'
         AIF   ('&KEY'(1,3) NE 'VRB').DOKY
&PFX     SETC  'S99'
.DOKY    DC    AL2(&PFX&KEY)
         AGO   .TXTL
.NOKY    DC    AL2(0)
         MNOTE *,'TEXT UNIT KEY MISSING'
.TXTL    DC    AL2(&TXTL)
         DC    C'&TXT'
         MEND
         PRINT ON
         EJECT
         REGS  ,                  REGISTER EQUATES
TXTA     EQU   R6                 PTR TO TXT BLD AREA
TUPA     EQU   R7                 PTR TO CURRENT TXT UNIT PTR
CMNDA    EQU   R8                 A(ALLOCATE_COMMAND)
CMNDL    EQU   R9                 L'ALLOCATE_COMMAND
TBPTR    EQU   R10
LINK     EQU   R11                LINK REGISTER
CODE     EQU   R12                CSECT BASE
         SPACE 3
LOCAL    DSECT                    LOCAL VARIABLE STORAGE
SAVEAREA DS    9D
BANNER   DS    CL8
IFLGS    DS    D                  ITEM FLAGS
ITST     DS    D                  TESTER FOR ITEM FLAGS
MAXRC    DS    F                  HIGHEST RETURN CODE
S99RC    DS    F                  DYNALLOC RETURN CODE
STATUS   DS    A                  A(USERS STATUS FIELD) OR ZERO
RBP      DS    A                  S99RBP
RB       DS    XL(S99RBEND-S99RB) S99RB
*
TXTAREA  DS    XL1024             TEXT UNITS AND POINTER LIST
*                                 TEXT UNITS BUILD UP FROM BOTTOM
*                                 PTR LIST BUILDS DOWN FROM TOP
LOCALL   EQU   *-LOCAL            LENGTH OF LOCAL STORAGE
         EJECT
ALLOCATE CSECT ,                  RESUME CSECT
         ENTRY ALLOC              ALTERNATE NAME
ALLOC    DS    0H
         B     BEGIN-*(,R15)      SKIP OVER VERSION
         DC    AL1(VERSIONL)      L'VERSION INFO               PF080191
ALLOCATE VERSION 1.3                                           PF011295
         PRINT NOGEN
BEGIN    DS    0H
         SAVE  (14,12)
         LR    CODE,R15           LOAD BASE REGISTER
         USING ALLOCATE,CODE
         LR    R3,R1              SAVE PARAM LIST ADDRESS
         GETMAIN RU,LV=LOCALL,LOC=BELOW GET LOCAL STORAGE
         ST    R1,8(,R13)         CHAIN SAVEAREAS
         ST    R13,4(,R1)               +
         LR    R13,R1                   +
         USING LOCAL,R13                +
         LR    R1,R3              RESTORE PLIST ADDRESS
         MVC   BANNER,=C'ALLOCATE' MOVE FLAG FOR DUMPS
         L     CMNDA,0(,R1)       PICK UP ADDRESS OF ALLOC COMMAND
         LH    CMNDL,0(,CMNDA)    LOAD LENGTH
         LA    CMNDA,2(,CMNDA)    SKIP OVER LENGTH
         XC    MAXRC,MAXRC        CLEAR RETURN CODE
         XC    S99RC,S99RC              +
         XC    STATUS,STATUS      CLEAR A(RETURNED STATUS)
         TM    0(R1),X'80'        Q/ DID USER REQUEST STATUS?
         BO    NOSTATUS           .. NO
         MVC   STATUS,4(R1)       .. YES, SAVE THE ADDRESS
NOSTATUS EQU   *                  BYPASS MOVE OF STATUS ADDR
         XC    IFLGS,IFLGS        ZERO ITEM FLAGS
         LA    R1,RB              LOAD ADDR OF SVC99 REQ BLK
         ST    R1,RBP             SAVE FOR SVC
         OI    RBP,X'80'          SET HIGH-ORDER BIT
         XC    RB(L'RB),RB        CLEAR RQST BLK
         MVI   S99RBLN-S99RB+RB,L'RB INITIALIZE RB LENGTH
         LA    TXTA,TXTAREA       POINT TO TXT UNIT WORKAREA
         LA    TUPA,TXTAREA+L'TXTAREA   POINT TO TXT PTR LIST
         BCTR  CMNDA,0            BACK UP ONE FOR START OF SCAN
         EJECT
***********************************************************************
*             SCAN ALLOCATE COMMAND                                   *
***********************************************************************
ITEMSCAN EQU   *
         BAL   LINK,SCANNER       SCAN COMMAND LIST
         LTR   CMNDL,CMNDL        TEST FOR LAST LIST ITEM
         BM    SVC99              .. YES
         LA    TBPTR,ITEMTBL      POINT TO ITEM TABLE
         USING ITMTBDS,TBPTR
ITEMLOOP EQU   *                  IDENTIFY ITEM
         CLI   0(TBPTR),X'FF'     Q/ END OF TABLE
         BE    ITEMERR            .. YES, IGNORE THIS ITEM
         CH    R15,ITMNMIN        Q/ AT LEAST MIN LENGTH?
         BL    ITEMNEXT           .. NO, CAN'T BE THIS ONE
         CH    R15,ITMNMLN        Q/ AT MOST MAX LENGTH?
         BH    ITEMNEXT           .. NO, CAN'T BE THIS ONE
         EX    R15,ITEMCLC        *** CLC 0(*-*,R1),ITMNM
         BE    ITEMFND            .. HIT
ITEMNEXT EQU   *                  GO LOOK AT NEXT ITEM
         AH    TBPTR,ITMNMLN      ADD LENGTH OF COMMAND TEXT-1
         LA    TBPTR,ITMTBLN+1(,TBPTR) ADD LENGTH OF FIXED DATA + 1
         B     ITEMLOOP           TRY, TRY AGAIN
ITEMERR  EQU   *                  INVALID ITEM
         MVC   MAXRC,=F'8'        SET RC=8
         B     ITEMSCAN           GO ATTEMPT NEXT LIST ITEM
ITEMFND  EQU   *                  ITEM MATCHED IN TABLE
         SR    R0,R0              DUP CHECK:
         LA    R1,1                 LOAD ONE BIT
         SR    R2,R2
         IC    R2,ITMNO             LOAD ITEM NUMBER
         SLDL  R0,0(R2)             SHIFT BIT INTO CORRECT LOC
         STM   R0,R1,ITST           SAVE DOUBLEWORD FOR TEST
         NC    ITST,IFLGS           TEST TO SEE IF ALREADY SET
         BNZ   ITEMERR              .. YES, ERROR
         STM   R0,R1,ITST         SAVE AGAIN
         OC    IFLGS,ITST         NOW SET THE BIT ON
         EJECT
***********************************************************************
*        BUILD ENTRY FOR NEW ITEM                                     *
***********************************************************************
         USING S99TUNIT,TXTA
         CLI   ITMNO,X'00'        TEST FOR ITEM 0 (VERB)
         BE    BLDVERB            .. YES
         CLI   S99VERB-S99RB+RB,X'00'   HAVE WE DONE VERB?
         BNE   BLDBGN             .. NO
         MVC   MAXRC,=F'12'       .. YES, SET RC=12
         B     EXIT                  AND LEAVE
BLDBGN   EQU   *
         MVC   S99TUKEY,ITMKY     MOVE TEXT UNIT KEY
         XC    S99TUNUM,S99TUNUM  CLEAR COUNT FIELD
         TM    ITMFLGS,ITMFNPR    Q/ NO TEXT REQUIRED
         BO    BLDDONE            .. YES
         BZ    BLDSDI             .. SELF-DEFINING KEYWORD
         MVI   S99TUNUM+1,1       MOVE # OF ENTRIES
         MVC   S99TULNG,ITMTXTL   MOVE ENTRY LENGTH
         LH    R1,ITMTXTL         PICK UP TEXT LENGTH
         LTR   R1,R1              TEST FOR ZERO LENGTH
         BZ    BLDPARM            .. NO, ZERO OUT TEXT FIELD
         BCTR  R1,0
         EX    R1,BLDXC           *** XC S99TUPAR(*-*),S99TUPAR
BLDPARM  EQU   *                  PROCESS PARAMETER TEXT
         LTR   CMNDL,CMNDL        TEST FOR LAST LIST ITEM
         BM    PARMERR            .. YES, ERROR
         CLI   1(CMNDA),C'('      Q/ IS PARAMETER PRESENT?
         BNE   PARMERR            .. NO, IT'S AN ERROR
         BAL   LINK,SCANNER       SCAN COMMAND LIST FOR PARAMETER
         LTR   CMNDL,CMNDL        DID WE FIND A CLOSING ')'?
         BM    PARMERR            .. NO, ERROR
         LTR   R15,R15            TEST SCANNED ITEM LENGTH
         BM    PARMERR
         TM    ITMFLGS,ITMFNP     Q/ IS THIS PARAMETER NUMERIC?
         BO    BLDNP              .. YES, GO EXTRACT NUMBER
         TM    ITMFLGS,ITMFAP     Q/ IS THIS PARAMETER A/N TEXT?
         BO    BLDAP              .. YES, GO DO THAT
         B     BLDKP              OTHERWISE MUST BE KEYWD PARM
         SPACE 1
BLDVERB  EQU   *                  MOVE VERB CODE TO RB
         MVC   S99VERB-S99RB+RB,ITMVRB  MOVE VERB CODE TO RB
         CLI   ITMVRB,S99VRBAL    Q/ IS THIS ALLOCATE?
         BNE   BLDUNALC           .. NO, PLUG 'UNALLOCATE'     PF060294
         MVC   S99TUNIT(RTORGL),RTORG MOVE TEXT UNIT
         B     BLDDONE            CONTINUE SCAN
BLDUNALC EQU   *                  ADD 'UNALLOCATE' TO LIST     PF060294
         MVC   S99TUNIT(UNALCL),UNALC MOVE TEXT UNIT           PF060294
         B     BLDDONE            CONTINUE SCAN                PF060294
         SPACE 1
BLDSDI   EQU   *                  PROCESS SELF-DEFINING KEYWORD
         MVI   S99TUNUM+1,1       MOVE # OF ENTRIES
         MVC   S99TULNG,=H'1'     MOVE LENGTH
         MVC   S99TUPAR(1),ITMVALU MOVE VALUE FOR SPECIAL ITEMS
         B     BLDDONE
         EJECT
***********************************************************************
*             PROCESS ALPHANUMERIC SUBPARAMETER                       *
***********************************************************************
BLDAP    EQU   *                  EXTRACT ALPHANUMERIC PARAMETER
         CLC   S99TUKEY,=AL2(DALDSNAM) Q/ IS THIS DSNAME?      PF060294
         BNE   BLDAP1             .. NO, CONTINUE              PF060294
         LTR   R15,R15            CHECK PARAMETER LENGTH       PF060294
         BNZ   BLDAP1             .. NOT 1, CAN'T BE DA(*)     PF060294
         CLI   0(R1),C'*'         Q/ IS IT DA(*)?              PF060294
         BNE   BLDAP1             .. NO, CONTINUE              PF060294
         MVC   S99TUKEY,=AL2(DALTERM) MOVE 'TERM' TXT UNIT KEY PF060294
         MVI   S99TUNUM+1,0       . NO ENTRIES                 PF060294
         MVI   S99TULNG+1,0       . ENTRY LENGTH=0             PF060294
         B     BLDDONE            EXIT                         PF060294
BLDAP1   EQU   *                  DA(*) BYPASS                 PF060294
         CH    R15,ITMTXTL        COMPARE LENGTH TO MAX
         BNL   PARMERR
         EX    R15,BLDMVC         *** MVC S99TUPAR(*-*),0(R1)
         LA    R15,1(,R15)        COMPUTE ACTUAL LENGTH
         STH   R15,S99TULNG       STORE ACTUAL LENGTH INTO ITEM
         CLC   S99TUKEY,=AL2(DALDSNAM) Q/ IS THIS DSNAME?
         BE    BLDMEMB            .. YES, LOOK FOR MEMBER
         CLC   S99TUKEY,=AL2(DALVLSER) Q/ IS THIS VOL SER #'S
         BNE   BLDDONE            .. NO, EXIT NOW
         LA    R2,S99TUENT        POINT TO FIRST TU ENTRY
         SPACE 1
BLDVOL   EQU   *                  BUILD LIST OF VOL SER #'S
         CLI   1(CMNDA),C')'      Q/ IS ANOTHER VOLSER PRESENT?
         BE    BLDDONE            .. NO, ALL DONE
         BAL   LINK,SCANNER       SCAN FOR NEXT VOLSER
         LTR   CMNDL,CMNDL        DID WE FIND A CLOSING ')'?
         BM    PARMERR            .. NO, ERROR
         LTR   R15,R15            TEST SCANNED ITEM LENGTH
         BM    PARMERR
         CH    R15,ITMTXTL        COMPARE LENGTH TO MAX
         BNL   PARMERR
         AH    R2,S99TULEN-S99TUFLD(,R2) SKIP OVER PREV VOLSER
         LA    R2,S99TUPRM-S99TUFLD(,R2)       +
         EX    R15,BLDMVC1 *** MVC S99TUPRM-S99TUFLD(*-*,R2),0(R1) 1693
         LA    R15,1(,R15)        COMPUTE ACTUAL LENGTH
         STH   R15,S99TULEN-S99TUFLD(,R2) STORE LENGTH USED
         LH    R15,S99TUNUM       LOAD NUM OF ENTRIES
         LA    R15,1(,R15)        UP BY ONE
         STH   R15,S99TUNUM       AND BACK AGAIN
         B     BLDVOL             GO LOOK FOR MORE
         SPACE 1
BLDMEMB  EQU   *                  CHECK FOR MEMBER NAME
         CLI   1(CMNDA),C'('      Q/ IS MEMBER NAME PRESENT?
         BNE   BLDDONE            .. NO, ALL DONE
         BAL   LINK,ENDTU         END DSNAME TEXT UNIT
         BAL   LINK,SCANNER       SCAN FOR MEMBER NAME
         LTR   CMNDL,CMNDL        DID WE FIND A CLOSING ')'?
         BM    PARMERR            .. NO, ERROR
         LTR   R15,R15            TEST SCANNED ITEM LENGTH
         BM    PARMERR
         CH    R15,=H'8'          COMPARE LENGTH TO MAX
         BNL   PARMERR
         MVC   S99TUNIT(MEMBRL),MEMBR MOVE TU FOR MEMBER NAME
         EX    R15,BLDMVC         *** MVC S99TUPAR(*-*),0(R1)
         LA    R15,1(,R15)        COMPUTE ACTUAL LENGTH
         STH   R15,S99TULNG       STORE ACTUAL LENGTH INTO ITEM
         B     BLDDONE            DONE WITH MEMBER NAME
         EJECT
***********************************************************************
*             PROCESS NUMERIC SUBPARAMETER                            *
***********************************************************************
BLDNP    EQU   *                  EXTRACT NUMERIC PARAMETER
         CH    R15,=H'8'          TEST LENGTH
         BNL   PARMERR            .. TOO BIG
         BAL   LINK,SCANNUM       VALIDATE NUMERIC PARAMETER
         B     PARMERR            (DONE IF ERROR)
         ST    R1,ITST            SAVE NUMERIC VALUE
         LH    R1,S99TULNG        PICK UP DESIRED LENGTH
         BCTR  R1,0               MINUS ONE FOR EX
         LA    R15,ITST+3         FIND WHERE TO START
         SR    R15,R1
         EX    R1,BLDMVCB         *** MVC S99TUPAR(*-*),0(R15)
         CLC   S99TUKEY,=AL2(DALPRIME) Q/ IS THIS PRIM SPACE ALLOC?
         BNE   BLDDONE            .. NO, EXIT NOW
BLDSEC   EQU   *                  PROCESS SECONDARY ALLOCATION
         CLI   1(CMNDA),C')'      Q/ IS ANOTHER VALUE PRESENT?
         BE    BLDDONE            .. NO, ALL DONE
         BAL   LINK,ENDTU         .. YES, END PRIM ALLOC TEXT UNIT
         BAL   LINK,SCANNER       SCAN FOR SECONDARY ALLOC
         LTR   CMNDL,CMNDL        DID WE FIND A CLOSING ')'?
         BM    PARMERR            .. NO, ERROR
         LTR   R15,R15            TEST SCANNED ITEM LENGTH
         BM    PARMERR
         CH    R15,=H'8'          TEST LENGTH
         BNL   PARMERR            .. TOO BIG
         BAL   LINK,SCANNUM       VALIDATE NUMERIC PARAMETER
         B     PARMERR            (DONE IF ERROR)
         MVC   S99TUNIT(SECNDL),SECND MOVE TU FOR SCND ALLOC
         ST    R1,ITST            SAVE NUMERIC VALUE
         LH    R1,S99TULNG        PICK UP DESIRED LENGTH
         BCTR  R1,0               MINUS ONE FOR EX
         LA    R15,ITST+3         FIND WHERE TO START
         SR    R15,R1
         EX    R1,BLDMVCB         *** MVC S99TUPAR(*-*),0(R15)
         B     BLDDONE            ALL SONE WITH SEC ALLOC
         EJECT
***********************************************************************
*        SPECIAL PROCESSING FOR KEYWORD-TYPE SUBPARAMETERS            *
*           EROPT, RECFM, DSORG, DEN, LABEL, SPIN              PF010995
***********************************************************************
BLDKP    EQU   *                  PROCESS KEYWORD PARAMETER
         CLC   S99TUKEY,=AL2(DALSPIN) Q/ IS THIS SPIN?         PF010995
         BE    BLDSP0             .. YES                       PF010995
         CH    R15,=H'3'          TEST FOR MAX LENGTH
         BH    PARMERR            .. TOO BIG TO BE GOOD
         SLL   R15,1              MULT LEN X 2
         LH    R15,BITM(R15)      CONVERT LENGTH TO BIT MASK FOR ICM
         L     R0,=4X'40'         LOAD SPACES FOR PADDING
         EX    R15,BLDICM         *** ICM R0,*-*,0(R1)
         LA    R15,EROPTD         ASSUME EROPT
         CLC   S99TUKEY,=AL2(DALEROPT) Q/ IS THIS EROPT?
         BE    BLDKP1             .. YES
         LA    R15,DEND           .. NO, ASSUME DEN
         CLC   S99TUKEY,=AL2(DALDEN)   Q/ IS THIS DEN?
         BE    BLDKP1             .. YES
         LA    R15,LABELD         .. NO, ASSUME LABEL
         CLC   S99TUKEY,=AL2(DALLABEL) Q/ IS THIS LABEL?
         BE    BLDKP1             .. YES
         LA    R15,DSORGD         .. NO, ASSUME DSORG
         CLC   S99TUKEY,=AL2(DALDSORG) Q/ IS THIS DSORG?
         BE    BLDKP1             .. YES
         LA    R15,RECFMD         .. NO, ASSUME RECFM
         MVI   S99TUPAR,X'00'        INIT VALUE
         CLC   S99TUKEY,=AL2(DALRECFM) Q/ IS THIS RECFM?
         BE    BLDKP1             .. YES
         B     PARMERR            .. NO, SHOULD NEVER OCCUR
BLDSP0   EQU   *                  SPECIAL HANDLING FOR SPIN    PF010995
         CH    R15,=H'1'          Q/ IS IT 'SPIN(NO)'?         PF010995
         BE    BLDSP0N            .. MAYBE ...                 PF010995
         CH    R15,=H'6'          Q/ 'SPIN(UNALLOC)'?          PF010995
         BNE   PARMERR            .. NO, MUST BE ERROR         PF010995
         CLC   =C'UNALLOC',0(R1)                               PF010995
         BNE   PARMERR                                         PF010995
         LA    R15,SPINDU         .. YES, SET SPIN(UNALLOC)    PF010995
         B     BLDKP2                                          PF010995
BLDSP0N EQU    *                  TEST FOR 'SPIN(NO)'          PF010995
         CLC   =C'NO',0(R1)                                    PF010995
         BNE   PARMERR                                         PF010995
         LA    R15,SPINDN         .. YES, SET SPIN(NO)         PF010995
         B     BLDKP2                                          PF010995
BLDKP1   EQU   *                  LOOKUP KWD VALUE IN TBL
         CLM   R0,X'F',2(R15)     Q/ IS THIS THE VALUE?
         BE    BLDKP2             .. YEP, FOUND IT
         CLC   0(2,R15),=H'0'     Q/ END OF TABLE?
         BZ    PARMERR            .. YEP, INVALID VALUE
         LA    R15,6(,R15)        .. NOPE, GO TO NEXT ENTRY
         B     BLDKP1                AND TRY AGAIN
BLDKP2   EQU   *
         CLC   S99TUKEY,=AL2(DALRECFM) Q/ IS THIS RECFM?
         BE    BLDRECFM           .. YES (RECFM TAKES SPEC HANDLING)
         MVC   S99TUPAR(1),1(R15) MOVE ONE-BYTE PARM
         CLI   S99TULNG+1,2       Q/ TWO-BYTE FIELD? (DSORG)
         BNE   BLDDONE            .. NO, WE'RE DONE
         MVC   S99TUPAR(2),0(R15) .. ELSE MOVE TWO BYTES
         B     BLDDONE
         SPACE 1
BLDRECFM EQU   *                  SPECIAL PROCESSING FOR RECFM
*        (NO TESTS ARE MADE FOR ILLOGICAL COMBINATIONS LIKE UAM)
         OC    S99TUPAR(1),1(R15) 'OR' IN NEXT RECFM VALUE
         CLI   1(CMNDA),C')'      Q/ IS ANOTHER VALUE PRESENT?
         BE    BLDDONE            .. NO, ALL DONE
         BAL   LINK,SCANNER       SCAN FOR NEXT VALUE
         LTR   CMNDL,CMNDL        DID WE FIND A CLOSING ')'?
         BM    PARMERR            .. NO, ERROR
         LTR   R15,R15            TEST SCANNED ITEM LENGTH
         BM    PARMERR
         CH    R15,=H'3'          COMPARE LENGTH TO MAX
         BNL   PARMERR
         SLL   R15,1              MULT LEN X 2
         LH    R15,BITM(R15)      CONVERT LENGTH TO BIT MASK FOR ICM
         L     R0,=4X'40'         LOAD SPACES FOR PADDING
         EX    R15,BLDICM         *** ICM R0,*-*,0(R1)
         LA    R15,RECFMD         RELOAD TABLE ADDRESS
         B     BLDKP1               LOOP TO PROCESS NEXT VALUE
         EJECT
***********************************************************************
*        END OF ONE COMPLETE TEXT UNIT                                *
***********************************************************************
BLDDONE  DS    0H                 DONE WITH TEXT UNIT
         BAL   LINK,ENDTU         FINISH THIS TEXT UNIT
         B     ITEMSCAN           CONTINUE SCAN
         SPACE 1
PARMERR  DS    0H                 PARAMETER ERROR
         MVC   MAXRC,=F'8'        SET RC=8
         B     ITEMSCAN
         EJECT
***************************************************************PF060994
*        EXECUTE SVC99                                         PF060994
***************************************************************PF060994
SVC99    DS    0H                 DO SVC99
         OI    TXTAREA+L'TXTAREA-4,X'80' FLAG END OF PTR LIST
         ST    TUPA,S99TXTPP-S99RB+RB STORE ADDRESS OF PTR LIST
         LA    R1,RBP             LOAD ADDRESS OF RB PTR
         DYNALLOC                 ISSUE SVC99
         ST    R15,S99RC          SAVE DYNALLOC RETURN CODE
         SPACE 1
EXIT     DS    0H                 RETURN TO CALLER
         OC    S99RC,S99RC        WAS THERE A DYNALLOC ERROR? PF091090
         BZ    EXIT1              .. NO                       PF091090
         L     R2,S99RC           .. YES, LOAD DYNALLOC ERROR PF091090
         LA    R2,12(,R2)            ADD 12                   PF091090
         ST    R2,MAXRC           SAVE UPDATED RETURN CODE    PF091090
EXIT1    EQU   *                                              PF091090
         OC    STATUS,STATUS      Q/ DID USER REQUEST FEEDBACK
         BZ    RETURN             .. NO, DON'T GIVE HIM ANY
         L     R1,STATUS          LOAD A(FEEDBACK FIELDS)
         USING USTATUS,R1
         MVC   USRC,MAXRC         MOVE HIGHEST RETURN CODE
         MVC   US99RC,S99RC       MOVE DYNALLOC RETURN CODE
         MVC   USRSC,S99RSC-S99RB+RB MOVE DYNALLOC STATUS
         LA    TUPA,TXTAREA+L'TXTAREA-4 POINT TO RTORG TUNIT
         MVC   USDSORG,=CL4' '    INITIALIZE DSORG
         L     TXTA,0(,TUPA)
         CLC   S99TUKEY,=AL2(DALRTORG) IS THIS THE ONE?
         BNE   RETURN             .. NO, BYPASS
         SR    R15,R15
         ICM   R15,B'0011',S99TUPAR PICK UP RTN'D DSORG
         LA    R14,DSORGD        POINT TO DSORG TRANSLATE TBL
DSORGLU  EQU   *                 DSORG LOOKUP
         CLM   R15,B'0011',0(R14) Q/ IS THIS THE ENTRY?
         BE    DSORGMV            .. YES
         CLC   0(2,R14),=H'0'     Q/ END OF TABLE?
         BZ    DSORGMV            .. YES, DEFAULT
         LA    R14,6(,R14)        .. NO, GO TO NEXT ENTRY
         B     DSORGLU               AND TRY AGAIN
DSORGMV  EQU   *
         MVC   USDSORG,2(R14)     MOVE DSORG TO FEEDBACK FLD
RETURN   EQU   *
         L     R2,MAXRC           PICK UP RETURN CODE
RET1     EQU   *
         LR    R1,R13             SAVE SAVEAREA ADDRESS FOR FREEMAIN
         L     R13,4(,R13)        LOAD CALLERS SAVEAREA ADDRESS
         FREEMAIN RU,LV=LOCALL,A=(1) FREE MY SAVEAREA
         LR    R15,R2             GET RC IN REG 15
         RETURN (14,12),RC=(15)   RETURN TO CALLER
         EJECT
***********************************************************************
*        MISCELLANEOUS SUBROUTINES                                    *
***********************************************************************
*
*        TEXT UNIT CLEANUP
*
ENDTU    DS    0H                 COMPLETE TEXT UNIT PROCESSING
         SH    TUPA,=H'4'         BACK UP PTR ADDRESS
         ST    TXTA,0(,TUPA)      STORE NEXT TEXT UNIT PTR
         LA    R1,S99TUENT        POINT PAST BASIC PORTION
         LH    R0,S99TUNUM        LOAD NUMBER OF ENTRIES
         LTR   R0,R0              Q/ # OF ENTRIES = ZERO?
         BZ    ENDTU2             .. YES, ALL DONE
ENDTU1   EQU   *
         AH    R1,S99TULEN-S99TUFLD(,R1) ADD TEXT LENGTH
         LA    R1,S99TUPRM-S99TUFLD(,R1) POINT PAST LENGTH
         BCT   R0,ENDTU1          DO ALL ENTRIES IN UNIT
ENDTU2   EQU   *
         LR    TXTA,R1            ADVANCE TEXT UNIT PTR
         LA    TXTA,1(,TXTA)      JUST FOR LAUGHS
         N     TXTA,=X'FFFFFFFE'    ROUND TO HALFWORD BOUNDARY
         CR    TXTA,TUPA          CHECK FOR AREA OVERFLOW
         BLR   LINK               .. NO, RETURN
         MVC   MAXRC,=F'12'       .. YES, MOVE MAX RETURN CODE
         B     EXIT               AND GET OUT
*
*        VALIDATE NUMERIC PARAMETER
*
SCANNUM  DS    0H                 VALIDATE NUMERIC PARAMETER
         STM   R15,R1,16(R13)     SAVE REGISTERS 15-1
         LA    R15,1(,R15)        BUMP LENGTH FOR LOOP
BLDNP1   EQU   *
         CLI   0(R1),C'0'         VALIDATE NUMERIC DATA
         BL    NUMERR
         CLI   0(R1),C'9'
         BH    NUMERR
         LA    R1,1(,R1)
         BCT   R15,BLDNP1
         LM    R15,R1,16(R13)     RESTORE REGISTERS 15-1
         EX    R15,BLDPACK        *** PACK ITST(8),0(*-*,R1)
         CVB   R1,ITST            CONVERT TO BIN
         B     4(,LINK)           EXIT TO CALLER
NUMERR   EQU   *                  INVALID NUMERIC PARAMETER
         LM    R15,R1,16(R13)     RESTORE REGISTERS 15-1
         SR    R1,R1              ZERO VALUE
         BR    LINK               TAKE ERROR RETURN
*
*        SCAN COMMAND LIST
*
SCANNER  DS    0H                 LIST SCAN ROUTINE
         LA    CMNDA,1(,CMNDA)    ADVANCE LIST PTR
         BCTR  CMNDL,0            DECREMENT LIST LENGTH
         LTR   CMNDL,CMNDL        TEST ARGUMENT LENGTH
         BMR   LINK               .. DONE, DO ALLOCATE
         CLI   0(CMNDA),C' '      TEST FOR BLANK
         BE    SCANNER
         CLI   0(CMNDA),C')'      OR CLOSE PAREN
         BE    SCANNER
         CLI   0(CMNDA),C'('      OR OPEN PAREN
         BE    SCANNER
         CLI   0(CMNDA),C','      OR COMMA
         BE    SCANNER
         SPACE 1
         LR    R1,CMNDA           SAVE ITEM START ADDRESS
SCANCON  EQU   *                  SCAN ONE ITEM NAME
         LTR   CMNDL,CMNDL        TEST ARGUMENT LENGTH
         BNP   SCANEND            .. DONE
         CLI   1(CMNDA),C' '      TEST FOR BLANK
         BE    SCANEND
         CLI   1(CMNDA),C')'      OR CLOSE PAREN
         BE    SCANEND
         CLI   1(CMNDA),C'('      OR OPEN PAREN
         BE    SCANEND
         CLI   1(CMNDA),C','      OR COMMA
         BE    SCANEND
         LA    CMNDA,1(,CMNDA)    ADVANCE LIST PTR
         BCTR  CMNDL,0            DECREMENT LENGTH
         B     SCANCON            AND CONTINUE SCAN
SCANEND  EQU   *                  ITEM NAME IDENTIFIED
         LR    R15,CMNDA          GET ITEM END ADDR
         SR    R15,R1             COMPUTE LENGTH MINUS ONE
         BR    LINK               RETURN TO CALLER
         SPACE 2
*        EXECUTED INSTRUCTIONS
ITEMCLC  CLC   0(*-*,R1),ITMNM        LOOKUP ITEM IN TABLE
BLDXC    XC    S99TUPAR(*-*),S99TUPAR CLEAR ITEM IN LIST
BLDMVC   MVC   S99TUPAR(*-*),0(R1)    MOVE A/N PARAMETER
BLDMVC1  MVC   S99TUPRM-S99TUFLD(*-*,R2),0(R1) MOVE 2ND VOLSER PF061693
BLDPACK  PACK  ITST(8),0(*-*,R1)      PACK NUMERIC PARAMETER
BLDMVCB  MVC   S99TUPAR(*-*),0(R15)   MOVE NUM PARAM TO LIST
BLDICM   ICM   R0,*-*,0(R1)           LOAD KEYWORD PARAMETER
         SPACE 1
         DROP  TXTA
         DROP  TBPTR
         SPACE 3
RTORG    DS    0H                  TEXT UNIT FOR RETURN DSORG
         DC    AL2(DALRTORG),AL2(1),AL2(2),AL2(0)
RTORGL   EQU   *-RTORG
*
UNALC    DS    0H                  TEXT UNIT FOR UNALLOCATE    PF060294
         DC    AL2(DUNUNALC),AL2(0)                            PF060294
UNALCL   EQU   *-UNALC                                         PF060294
*                                                              PF060294
SECND    DS    0H                  TEXT UNIT FOR SECNDRY ALLOC
         DC    AL2(DALSECND),AL2(1),AL2(3),AL3(0)
SECNDL   EQU   *-SECND
*
MEMBR    DS    0H                  TEXT UNIT FOR MEMBER NAME
         DC    AL2(DALMEMBR),AL2(1) .. LENGTH AND VALUE ADDED LATER
MEMBRL   EQU   *-MEMBR
         TITLE 'ALLOCATE - MISCELLANEOUS TABLES AND CONSTANTS'
DSORGD   DS    0H                  DSORG TRANSLATE TABLE
         DC    X'0004',CL4'TR  '
         DC    X'0008',CL4'VS  '
         DC    X'0020',CL4'TQ  '
         DC    X'0040',CL4'TX  '
         DC    X'0080',CL4'GS  '
         DC    X'0200',CL4'PO  '
         DC    X'0300',CL4'POU '
         DC    X'0400',CL4'MQ  '
         DC    X'0800',CL4'CQ  '
         DC    X'1000',CL4'CX  '
         DC    X'2000',CL4'DA  '
         DC    X'2100',CL4'DAU '
         DC    X'4000',CL4'PS  '
         DC    X'4100',CL4'PSU '
         DC    X'8000',CL4'IS  '
         DC    X'8100',CL4'ISU '
         DC    X'0000',CL4'UN  '   END OF TABLE
*
LABELD   DS    0H                  LABEL TYPE TRANSLATE TABLE
         DC    X'0001',CL4'NL  '
         DC    X'0002',CL4'SL  '
         DC    X'0004',CL4'NSL '
         DC    X'000A',CL4'SUL '
         DC    X'0010',CL4'BLP '
         DC    X'0021',CL4'LTM '
         DC    X'0040',CL4'AL  '
         DC    X'0048',CL4'AUL '
         DC    X'0000',CL4'===='
*
DEND     DS    0H                  DENSITY TRANSLATE TABLE
         DC    X'0003',CL4'0   '
         DC    X'0043',CL4'1   '
         DC    X'0083',CL4'2   '
         DC    X'00C3',CL4'3   '
         DC    X'00D3',CL4'4   '
         DC    X'0000',CL4'===='
*
EROPTD   DS    0H                  EROPT TRANSLATE TABLE
         DC    X'0080',CL4'ACC '
         DC    X'0040',CL4'SKP '
         DC    X'0020',CL4'ABE '
         DC    X'0000',CL4'===='
*
RECFMD   DS    0H                  RECFM TRANSLATE TABLE
         DC    X'0080',CL4'F   '
         DC    X'0040',CL4'V   '
         DC    X'00C0',CL4'U   '
         DC    X'0010',CL4'B   '
         DC    X'0002',CL4'M   '
         DC    X'0004',CL4'A   '
         DC    X'0000',CL4'    '   (LAST VALUE MUST BE SPACES)
*
SPIND    DS    0H                  SPIN TRANSLATE TABLE        PF010995
SPINDU   DC    X'0080',CL4'UNAL'                               PF010995
SPINDN   DC    X'0040',CL4'NO  '                               PF010995
*                                                              PF010995
BITM     DS    0H                  CONVERT LENGTH VALUES TO BITMASKS
         DC    BL2'1000'             L=1
         DC    BL2'1100'             L=2
         DC    BL2'1110'             L=3
         DC    BL2'1111'             L=4
*
         SPACE 3
         LTORG
         TITLE 'ALLOCATE - DSECTS'
ITMTBDS  DSECT ,                   ITEM TABLE ENTRY
ITMNMLN  DS    H                   L'NAME MINUS ONE
ITMNMIN  DS    H                   MINIMUM L'NAME MINUS ONE
ITMNO    DS    AL1                 ITEM NUMBER
ITMFLGS  DS    BL1                 FLAG BYTE
ITMFNP   EQU   X'80'               100. .... THIS ITEM TAKES NUM PARAM
ITMFAP   EQU   X'40'               010. .... THIS ITEM TAKES A/N PARM
ITMFKP   EQU   X'20'               001. .... THIS ITEM TAKES KWD PARM
ITMFNPR  EQU   X'E0'               111. .... THIS ITEM HAS NO TEXT
ITMFSDI  EQU   255-ITMFNPR         000. .... THIS ITEM IS SELF-DEFIN'G
ITMKY    DS    XL2                 DYNAMIC ALLOCATION KEY
         ORG   ITMKY
         DS    X
ITMVRB   DS    X                   VERB CODE
         ORG   ,
ITMTXTL  DS    XL2                 MAX ITEM TEXT LENGTH
         ORG   ITMTXTL
         DS    X
ITMVALU  DS    X                   TEXT VALUE FOR STATUS AND DISP
         ORG   ,
ITMNM    DS    0C                  OFFSET OF ITEM NAME TEXT
ITMTBLN  EQU   *-ITMTBDS           LENGTH OF FIXED PART OF ENTRY
         SPACE 3
USTATUS  DSECT                     USER STATUS AREA (16 BYTES)
USDSORG  DS    CL4                 RETURNED DSORG FOR ALLOCATE (TEXT)
USRC     DS    XL4                 SUBROUTINE RETURN CODE
US99RC   DS    XL4                 DYNALLOC RETURN CODE
USRSC    DS    XL4                 DYNALLOC REASON AND STATUS CODES
         SPACE 3
         PRINT NOGEN
         IEFZB4D0
         IEFZB4D2
ALLOCATE CSECT ,                   BACK TO CSECT
*
         TITLE 'ALLOCATE - ITEM TABLES'
*
*        THE FOLLOWING 'ALLOCATE' OPTIONS ARE NOT CURRENTLY SUPPORTED
*          MSVGP,    ALTFILE,  UCOUNT,   PARALLEL, ACCCODE,
*          MAXVOL,   PRIVATE,            NCP,      BFALN,
*          OPTCD,    BFTEK,    DIAGNS,   LIMCT,    BUFOFF,
*          TRTCH,    PROTECT,  BURST,    NOBURST,  FLASH,
*          MODIFY,   FCB,      OUTDES,   UCS,                  PF050897
*          REUSE,    INPUT,    OUTPUT.                         PF080191
*        USING(ATTRLIST) IS NOT APPLICABLE IN THIS ENVIRONMENT.
*
*        "NO" IS USED TO INDICATE MUTUALLY-EXCLUSIVE PARAMETERS.
*        LOGIC USED WILL ALLOW UP TO NO=63 MAXIMUM.
*        HIGHEST NO CURRENTLY USED IS -34-.                    PF050897
*
         PRINT NOGEN
ITEMTBL  DS    0H                  ITEM TABLE
*
*        * * * VERB KEYWORDS * * *
*
         ITEM  ALLOCATE,NO=0,MIN=5,KEY=VRBAL
         ITEM  FREE,NO=0,KEY=VRBUN
*
*        * * * KEYWORDS WHICH REQUIRE NO PARAMETERS * * *
*
         ITEM  DUMMY,KEY=DUMMY,PARM=CMP,NO=1,MIN=2
         ITEM  TRACKS,KEY=TRK,PARM=CMP,NO=6,MIN=3
         ITEM  CYLINDERS,KEY=CYL,PARM=CMP,NO=6,MIN=3           PF040991
         ITEM  HOLD,KEY=SHOLD,PARM=CMP,NO=10
         ITEM  RELEASE,KEY=RLSE,PARM=CMP,NO=15,MIN=3
         ITEM  ROUND,KEY=ROUND,PARM=CMP,NO=16,MIN=2
*
*        * * * SELF-DEFINING KEYWORDS * * *
*
*              'TXTL' IS PARM VALUE FOR STATUS
         ITEM  OLD,KEY=STATS,NO=3,TXTL=1
         ITEM  MOD,KEY=STATS,NO=3,TXTL=2
         ITEM  NEW,KEY=STATS,NO=3,TXTL=4
         ITEM  SHR,KEY=STATS,NO=3,TXTL=8
*              'TXTL' IS PARM VALUE FOR DISPOSITION
         ITEM  UNCATALOG,KEY=NDISP,NO=17,MIN=3,TXTL=1
         ITEM  CATALOG,KEY=NDISP,NO=17,MIN=3,TXTL=2
         ITEM  DELETE,KEY=NDISP,NO=17,MIN=3,TXTL=4
         ITEM  KEEP,KEY=NDISP,NO=17,MIN=3,TXTL=8
         ITEM  BURST,KEY=BURST,NO=29,TXTL=2
         ITEM  NOBURST,KEY=BURST,NO=29,TXTL=4
*
*        * * * KEYWORDS WHICH TAKE KEYWORD PARAMETERS * * *
*
         ITEM  LABEL,KEY=LABEL,PARM=KEY,NO=12,MIN=2,TXTL=1
         ITEM  EROPT,KEY=EROPT,PARM=KEY,NO=22,TXTL=1
         ITEM  RECFM,KEY=RECFM,PARM=KEY,NO=24,MIN=4,TXTL=1
         ITEM  DSORG,KEY=DSORG,PARM=KEY,NO=25,MIN=3,TXTL=2
         ITEM  DEN,KEY=DEN,PARM=KEY,NO=26,TXTL=1
         ITEM  SPIN,KEY=SPIN,PARM=KEY,NO=33,TXTL=1             PF010995
*
*        * * * KEYWORDS WHICH TAKE ALPHANUMERIC PARAMETERS * * *
*
         ITEM  DATASET,KEY=DSNAM,PARM=AL,NO=1,MIN=2,TXTL=44
         ITEM  DSNAME,KEY=DSNAM,PARM=AL,NO=1,MIN=2,TXTL=44
         ITEM  FILE,KEY=DDNAM,PARM=AL,NO=2,MIN=2,TXTL=8
         ITEM  DDNAME,KEY=DDNAM,PARM=AL,NO=2,MIN=2,TXTL=8
         ITEM  SYSOUT,KEY=SYSOU,PARM=AL,NO=3,MIN=3,TXTL=1
         ITEM  VOLUME,KEY=VLSER,PARM=AL,NO=4,MIN=3,TXTL=6
         ITEM  DEST,KEY=SUSER,PARM=AL,NO=9,TXTL=8              PF011295
         ITEM  UNIT,KEY=UNIT,PARM=AL,NO=11,MIN=2,TXTL=8
         ITEM  EXPDT,KEY=EXPDT,PARM=AL,NO=21,MIN=3,TXTL=5
         ITEM  CHARS,KEY=CHARS,PARM=AL,NO=30,TXTL=4
         ITEM  FORMS,KEY=SFMNO,PARM=AL,NO=31,TXTL=4
         ITEM  LIKE,KEY=DCBDS,PARM=AL,NO=32,TXTL=44            PF080191
         ITEM  WRITER,KEY=SPGNM,PARM=AL,NO=34,MIN=3,TXTL=8     PF050897
*
*        * * * KEYWORDS WHICH TAKE NUMERIC PARAMETERS * * *
*
         ITEM  SPACE,KEY=PRIME,PARM=NUM,NO=5,MIN=2,TXTL=3
         ITEM  BLOCK,KEY=BLKLN,PARM=NUM,NO=6,MIN=3,TXTL=3
         ITEM  AVBLOCK,KEY=BLKLN,PARM=NUM,NO=6,MIN=3,TXTL=3
         ITEM  BLKSIZE,KEY=BLKSZ,PARM=NUM,NO=7,MIN=3,TXTL=2
         ITEM  DIR,KEY=DIR,PARM=NUM,NO=8,TXTL=3
         ITEM  POSITION,KEY=DSSEQ,PARM=NUM,NO=13,MIN=3,TXTL=2
         ITEM  VSEQ,KEY=VLSEQ,PARM=NUM,NO=14,MIN=2,TXTL=2
         ITEM  BUFL,KEY=BUFL,PARM=NUM,NO=18,TXTL=2
         ITEM  BUFNO,KEY=BUFNO,PARM=NUM,NO=19,MIN=4,TXTL=1
         ITEM  LRECL,KEY=LRECL,PARM=NUM,NO=20,MIN=4,TXTL=2
         ITEM  RETPD,KEY=RETPD,PARM=NUM,NO=21,MIN=3,TXTL=2
         ITEM  KEYLEN,KEY=KYLEN,PARM=NUM,NO=27,MIN=3,TXTL=1
         ITEM  COPIES,KEY=COPYS,PARM=NUM,NO=28,MIN=3,TXTL=1
         DC    2X'FF'              END OF ITEM TABLE
         END
 MODE AMODE(31),RMODE(ANY)
 ALIAS ALLOC
 ALIAS RXALLOC
 NAME ALLOCATE(R)
