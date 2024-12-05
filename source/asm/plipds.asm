PLIPDS   CSECT
         TITLE 'PDS -- PL/I PDS SERVICES V2.0'
*/********************************************************************/
*/*                                                                  */
*/*      PDS - PL/I PDS SERVICES                                     */
*/*      P.FLASS - NYS LBDC -- MAY, 1995                             */
*/*                                                                  */
*/*      DESCRIPTION:                                                */
*/*            PROGRAM SELECTS A MEMBER OF A PDS.                    */
*/*            RETURNS 0 IF MEMBER FOUND,                            */
*/*                    4 IF MEMBER NOT FOUND,                        */
*/*                    8 IF ANY ERRORS.                              */
*/*                                                                  */
*/*      CALLING SEQUENCE:                                           */
*/*            DCL PDSMEM ENTRY RETURNS(FIXED BIN(15));              */
*/*            DCL DDNAME CHAR(8);                                   */
*/*            DCL MEMBER CHAR(8);                                   */
*/*            RC = PDSMEM('READ'|'WRITE',DDNAME,MEMBER);            */
*/*                                                                  */
*/*      FUNCTION:                                                   */
*/*            'READ': READ THE JFCB FOR THE REQUESTED DDNAME,       */
*/*             1. IF NOT PRESENT THE DDNAME IS INVALID, EXIT RC=8.  */
*/*             2. ISSUE BLDL FOR REQUESTED MEMBER, IF MEMBER NOT    */
*/*                FOUND EXIT WITH RC=4.                             */
*/*             3. PLACE THE MEMBER NAME IN THE JFCB, UPDATE JFCBIND1*/
*/*                TO INDICATE THAT THE DD POINTS TO A PDS MEMBER,   */
*/*                EXIT RC=0.                                        */
*/*            'WRITE': READ THE JFCB FOR THE REQUESTED DDNAME,      */
*/*             1. IF NOT PRESENT THE DDNAME IS INVALID, EXIT RC=8.  */
*/*             2. PLACE THE MEMBER NAME IN THE JFCB, UPDATE JFCBIND1*/
*/*                TO INDICATE THAT THE DD POINTS TO A PDS MEMBER,   */
*/*                EXIT RC=0.  THE FILE MUST THEN BE OPENED OUTPUT.  */
*/*                                                                  */
*/*      ENVIRONMENT:                                                */
*/*             MVS/ESA 5.1                                          */
*/*             PL/I FOR MVS AND VM 1.1.1                            */
*/*             AMODE(31), RMODE(ANY) - HEAP BELOW 16M LINE          */
*/*                                                                  */
*/*      REGISTER USAGE:                                             */
*/*            R1  = PARAMETER LIST ADDRESS                          */
*/*            R2  = BASE REGISTER                                   */
*/*            R11 = ADDRESS OF RETURN CODE                          */
*/*            R12 = ADDRESS OF TCA                                  */
*/*            R13 = ADDRESS OF DSA                                  */
*/*                                                                  */
*/*      MODIFICATIONS:                                              */
*/*            ADD READ/WRITE - V2.0, 9 JAN 1996 - PRF               */
*/*                                                                  */
*/********************************************************************/
         SPACE 3
         REGS
         PLIREGS
         EJECT
PLIPDS   CSECT
PLIPDS   RMODE 24
PLIPDS   AMODE ANY
PDSMEM   PLIENTRY ,LOCAL=(LOCAL,LOCALL)
*---------------------------*
* PROCESS PARAMETER DATA    *
*---------------------------*
         L     R11,12(PLIPL)       LOAD A(RETURN_CODE)             V2.0
         MVC   0(4,R11),=F'8'      DEFAULT RETURN CODE=8
         MVC   BLDLFF(4),=X'0001000C'  INIT BLDL LIST
         MVC   BLDLTTR(4),=X'FFFFFFFF' .
         MVC   PDS(PDSSLEN),PDSS   INITIALIZE DCB
         LA    R10,PDS
         ST    R10,OPENLIST        SAVE FOR OPEN
         OI    OPENLIST,X'80'      FLAG END-OF-LIST
         PLIARG 1,STRING,ADDR=R3,LEN=R4 GET READ/WRITE             V2.0
         C     R4,=F'1'            CHECK R/W LENGTH                V2.0
         BL    RC8                 .. ERROR                        V2.0
         MVC   READWRIT,0(R3)      MOVE FLAG                       V2.0
         CLI   READWRIT,C'R'       Q/ FLAG='R'?                    V2.0
         BE    GETARG2             .. YES                          V2.0
         CLI   READWRIT,C'W'       Q/ FLAG='W'?                    V2.0
         BNE   RC8                 .. NO                           V2.0
GETARG2  EQU   *                                                   V2.0
         PLIARG 2,STRING,ADDR=R3,LEN=R4 GET DDNAME                 V2.0
         C     R4,=F'8'            CHECK DDNAME LENGTH
         BNH   *+8                 .. OKAY
         LA    R4,8                .. TRUNCATE TO 8
         BCTR  R4,0
         MVC   DCBDDNAM-IHADCB(8,R10),=CL8' '
         EX    R4,MVCDDNAM         MOVE DDNAME TO DCB
         LA    R0,EXITLIST         PLUG IN EXITLIST ADDRESS
         STCM  R0,B'0111',DCBEXLSA-IHADCB(R10)
         LA    R0,JFCBAREA         BUILD EXITLIST
         ICM   R0,B'1000',=X'87'   .
         ST    R0,EXITLIST         .
         PLIARG 3,STRING,ADDR=R3,LEN=R4 GET MEMBER NAME            V2.0
         C     R4,=F'8'            CHECK MEMBER NAME LENGTH
         BNH   *+8                 .. OKAY
         LA    R4,8                .. TRUNCATE TO 8
         BCTR  R4,0
         MVC   MEMBER,=CL8' '
         EX    R4,MVCMEMB          SAVE MEMBER NAME
         SPACE 1
*---------------------------*
* READ THE JFCB             *
*---------------------------*
         LA    R1,OPENLIST         READ THE JFCB
         RDJFCB ,MF=(E,(1))        .
         LTR   R15,R15             TEST RETURN CODE
         BNZ   RC8                 .. ERROR
         MVC   JFCBELNM-JFCDSECT+JFCBAREA(8),=CL8' '
         NI    JFCBIND1-JFCDSECT+JFCBAREA,255-JFCPDS
         LA    R1,OPENLIST         OPEN THE DCB
         OPEN  TYPE=J,MF=(E,(1))   .
         LTR   R15,R15             TEST RETURN CODE
         BNZ   RC8                 .. ERROR
         SPACE 1
*---------------------------*
* ISSUE BLDL FOR MEMBER     *
*---------------------------*
         CLI   READWRIT,C'R'       Q/ FLAG='R'?                    V2.0
         BNE   RC0                 .. NO, SKIP BLDL                V2.0
         LA    R1,PDS              LOAD PDS ADDRESS
         LA    R0,BLDLLIST         LOAD LIST ADDRESS
         BLDL  (1),(0)             ISSUE BLDL
         LTR   R15,R15             TEST RETURN CODE
         BZ    RC0                 .. MEMBER FOUND
         C     R15,=F'4'           TEST FOR NOT FOUND
         BNE   CLOSE               .. NO, EXIT W/RC=8
         CLI   BLDLTTR+2,X'00'     Q/ WAS SEARCH DONE
         BNE   CLOSE               .. NO, I/O ERROR, ETC.
         MVC   0(4,R11),=F'4'      .. YES, SET RETURN CODE=4
         B     CLOSE               GO CLOSE THE PDS
RC0      EQU   *                   MEMBER FOUND
         MVC   0(4,R11),=F'0'      SET RETURN CODE=0
         SPACE 1
*---------------------------*
* CLOSE THE PDS             *
*---------------------------*
CLOSE    EQU   *
         LA    R1,OPENLIST         CLOSE THE DCB
         CLOSE ,MF=(E,(1))         .
         CLC   0(4,R11),=F'0'      Q/ RC=0?
         BNE   RETURN
         SPACE 1
*---------------------------*
* SET MEMBER INFO IN JFCB   *
*---------------------------*
         CLI   READWRIT,C'R'       Q/ FLAG='R'?                    V2.0
         BE    SETJFCB             .. YES                          V2.0
         OI    OPENLIST,X'0F'      .. NO. SET 'OUTPUT' FLAG        V2.0
SETJFCB  EQU   *                                                   V2.0
         MVC   JFCBELNM-JFCDSECT+JFCBAREA(8),MEMBER MOVE MEMBER
         OI    JFCBIND1-JFCDSECT+JFCBAREA,JFCPDS IND PDS MEMBER
         LA    R1,OPENLIST         OPEN THE DCB
         OPEN  TYPE=J,MF=(E,(1))   .
         LTR   R15,R15             TEST RETURN CODE
         BNZ   RC8                 .. ERROR
         LA    R1,OPENLIST         OPEN THE DCB
         CLOSE ,MF=(E,(1))         .
         B     RETURN
RC8      EQU   *
         MVC   0(4,R11),=F'8'      SET RETURN CODE=8
         SPACE 1
*---------------------------*
* RETURN TO CALLER          *
*---------------------------*
RETURN   EQU   *
         PLIRET ,                  RETURN TO CALLER
         SPACE 1
*---------------------------*
* EXECUTED INSTRUCTIONS     *
*---------------------------*
MVCDDNAM MVC   DCBDDNAM-IHADCB(*-*,R10),0(R3)
MVCMEMB  MVC   MEMBER(*-*),0(R3)
         SPACE 1
         LTORG *
         PRINT NOGEN
PDSS     DCB   DSORG=PO,MACRF=(R,W)                                V2.0
PDSSLEN  EQU   *-PDSS
         SPACE 1
         PRINT OFF
IHADCB   DSECT
         DCBD  DSORG=PO,DEVD=DA
JFCDSECT DSECT
         IEFJFCBN
         PRINT ON
         SPACE 1
***********************************************************************
*        LOCAL STORAGE (ALL BELOW 16MB)                               *
***********************************************************************
LOCAL    DSECT                     DSECT FOR DSA STORAGE
OPENLIST DS    A
PDS      DS    XL(PDSSLEN)         RESERVE STORAGE FOR DCB
EXITLIST DS    A                   RDJFCB EXIT LIST
JFCBAREA DS    XL(JFCBLGTH)
*
BLDLLIST DS    0F                  WORKAREA FOR BLDL
BLDLFF   DS    H                   NUMBER OF ENTRIES
BLDLLL   DS    H                   LENGTH OF ENTRY
MEMBER   DS    CL8                 MEMBER NAME
BLDLTTR  DS    CL4                 TTR
*
READWRIT DS    C                   READ/WRITE FLAG                 V2.0
         DS    0D                  *ALIGNMENT*
LOCALL   EQU   *-LOCAL             LENGTH OF DSA STORAGE
         SPACE 1
         END    PLIPDS
 MODE AMODE(31)
 MODE RMODE(24)
 ALIAS PDSMEM
 NAME PLIPDS(R)
