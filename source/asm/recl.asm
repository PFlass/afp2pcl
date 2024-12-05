PLIRECL  CSECT
*/********************************************************************/
*/*                                                                  */
*/*      RETRIEVE VARIABLE RECORD LENGTH AFTER READ                  */
*/*      P. FLASS -- EMPIRE STATE COLLEGE (SUNY) -- AUGUST 1985      */
*/*                                                                  */
*/*      CALLING SEQUENCE                                            */
*/*            DCL RL FIXED BIN(31);                                 */
*/*            DCL FN RECORD FILE;                                   */
*/*            DCL RECL ENTRY(*) RETURNS(FIXED BIN(31));             */
*/*            READ FILE(FN) ...                                     */
*/*            RL=RECL(FN);                                          */
*/*                                                                  */
*/*      LOGIC: CALLING SEQUENCE PASSES ADDRESS OF PL/I DCLCB        */
*/*             DCLCB +00 -> FCB                                     */
*/*             FCB   +58 -> IOCB            (VSAM)                  */
*/*             I0CB  +OC -> RPL              "                      */
*/*             RPL   +30 =  L'RECORD READ    "                      */
*/*                                                                  */
*/*      MODIFICATIONS:                                              */
*/*             021397 RETURN DATA LENGTH FOR VARIABLE-LENGTH RECORD.*/
*/*                                                                  */
*/*                                                                  */
*/********************************************************************/
         SPACE 3
         REGS
         PLIREGS
         EJECT
PLIRECL  AMODE 31
PLIRECL  RMODE ANY
RECL     PLIENTRY
         LR    R10,R1              SAVE PARAMETER LIST ADDRESS
         L     R3,0(,R10)          LOAD A(FILE_VARIABLE)
         L     R3,0(,R3)           LOAD A(DCLCB)
         L     R11,0(,R3)          LOAD A(FCB_ADDRESS) IF INTERNAL
         OC    0(2,R3),0(R3)       Q/ IS FILE INTERNAL?
         BNZ   TESTOPEN            .. YES
         A     R11,4(,PLITCA)      .. NO, ADD A(PRV)
TESTOPEN EQU   *
         L     R11,0(,R11)         LOAD A(FCB)
         C     R11,X'64'(,PLITCA)  Q/ IS FILE OPEN?
         BE    NOTOPEN             .. NO
         CLI   X'2A'(R11),X'1C'    Q/ VSAM FILE?
         BE    GETVSAM             .. YES
         L     R15,X'14'(,R11)     .. NO, LOAD A(DCB)          PF021397
         USING IHADCB,R15                                      PF021397
         LH    R11,DCBLRECL           LOAD DCBLRECL            PF021397
         TM    DCBRECFM,DCBRECU       Q/ RECFM=U?              PF021397
         BO    EXIT                   .. YES                   PF021397
         TM    DCBRECFM,DCBRECV       Q/ RECFM=V?              PF021397
         BNO   EXIT                   .. NO                    PF021397
         SH    R11,=H'4'              .. YES, SUBTRACT VRL     PF021397
         B     EXIT                   AND GET OUT
         DROP  R15                                             PF021397
GETVSAM  EQU   *                   VSAM LOGIC
         L     R11,X'54'(,R11)     LOAD A(IOCB_CHAIN)
         LTR   R11,R11             Q/ DO WE HAVE AN IOCB?
         BZ    NOIOCB              .. NO
         L     R11,X'3C'(,R11)     .. YES, LOAD A(RPL)
         L     R11,X'30'(,R11)        LOAD RPLRLEN
         B     EXIT                   AND GET OUT
NOTOPEN  EQU   *                   FILE NOT OPEN
NOIOCB   EQU   *                   NO IOCB
         L     R11,=F'-1'          .. INDICATE ERROR
EXIT     EQU   *                   PASS RECL TO CALLER
         L     R3,4(,R10)          LOAD A(RETURNED_VALUE)
         ST    R11,0(,R3)          PASS BACK RECL
         PLIRET                    RETURN TO CALLER
         SPACE 3
         LTORG *
*                                                              PF021397
         PRINT NOGEN                                           PF021397
         DCBD  DSORG=PS,DEVD=DA                                PF021397
*                                                              PF021397
         END   PLIRECL
 MODE AMODE(31),RMODE(ANY)
 ALIAS RECL
 NAME PLIRECL(R)
